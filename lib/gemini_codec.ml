(** Google Gemini REST API encode/decode. *)

let member k j = Yojson.Safe.Util.member k j
let to_string_opt j = try Some (Yojson.Safe.Util.to_string j) with _ -> None
let to_int_opt j = try Some (Yojson.Safe.Util.to_int j) with _ -> None
let to_list_opt j = try Some (Yojson.Safe.Util.to_list j) with _ -> None
let to_float_opt j = try Some (Yojson.Safe.Util.to_float j) with _ -> None

let make_error ?raw kind message : Types.error =
  { kind; message; provider = "gemini"; raw }

(** Gemini function names must match [a-zA-Z0-9_-]. Callers
    that namespace their tools with ':' (e.g. [core:memory_get])
    would be silently dropped from the function-call list.
    Replace ':' with '_' on the wire; the receiver can invert
    the mapping if it tracks a prefix whitelist. *)
let sanitize_tool_name (s : string) : string =
  String.map (fun c -> if c = ':' then '_' else c) s

(** Gemini rejects parameter schemas of shape [{"type":"object"}]
    that omit [properties]. Inject an empty object so the
    request validates. *)
let normalize_parameters (schema : Yojson.Safe.t) : Yojson.Safe.t =
  match schema with
  | `Assoc fields ->
    if List.mem_assoc "properties" fields then schema
    else `Assoc (fields @ [("properties", `Assoc [])])
  | other -> other

let encode_part = function
  | Types.Text t ->
    `Assoc [("text", `String t)]
  | Types.Image { url; _ } ->
    `Assoc [("fileData", `Assoc [("fileUri", `String url)])]
  | Types.Tool_use { name; input; thought_signature; _ } ->
    let parts = [("functionCall", `Assoc [
      ("name", `String (sanitize_tool_name name));
      ("args", input);
    ])] in
    let parts = match thought_signature with
      | Some ts -> parts @ [("thoughtSignature", `String ts)]
      | None -> parts
    in
    `Assoc parts
  | Types.Tool_result { id; content; _ } ->
    (* Gemini requires [functionResponse.name] to match the
       [functionCall.name] in the earlier turn — that's how
       it correlates results with calls (it has no opaque
       tool_use ids). Callers should put the same sanitized
       tool name in [Tool_result.id]. *)
    let name = sanitize_tool_name id in
    (match Yojson.Safe.from_string content with
     | exception _ ->
       `Assoc [("functionResponse", `Assoc [
         ("name", `String name);
         ("response", `Assoc [("content", `String content)]);
       ])]
     | json ->
       `Assoc [("functionResponse", `Assoc [
         ("name", `String name);
         ("response", json);
       ])])

let encode_message (msg : Types.message) =
  let role_str = match msg.role with
    | Types.User      -> "user"
    | Types.Tool      -> "user"
    | Types.Assistant -> "model"
    | Types.System    -> "user"
  in
  let parts = List.map encode_part msg.content in
  `Assoc [("role", `String role_str); ("parts", `List parts)]

let encode_tool (t : Types.tool) =
  `Assoc (
    [("name", `String (sanitize_tool_name t.name));
     ("parameters", normalize_parameters t.schema)]
    @ (match t.description with
       | Some d -> [("description", `String d)]
       | None   -> [])
  )

let encode_request (req : Types.request) =
  let system = List.filter_map (fun (m : Types.message) ->
    if m.role = Types.System then
      match m.content with
      | [Types.Text t] -> Some t
      | parts ->
        Some (String.concat "" (List.filter_map (function Types.Text t -> Some t | _ -> None) parts))
    else None
  ) req.messages in
  let messages = List.filter (fun (m : Types.message) -> m.role <> Types.System) req.messages in
  let contents = List.map encode_message messages in
  let fields = ref [("contents", `List contents)] in
  (match system with
   | [] -> ()
   | parts ->
     fields := !fields @ [("system_instruction", `Assoc [("parts", `List [
       `Assoc [("text", `String (String.concat "\n" parts))]
     ])])]);
  let gen_config = ref [] in
  (match req.max_tokens with Some n -> gen_config := !gen_config @ [("maxOutputTokens", `Int n)] | None -> ());
  (match req.temperature with Some f -> gen_config := !gen_config @ [("temperature", `Float f)] | None -> ());
  (match req.top_p with Some f -> gen_config := !gen_config @ [("topP", `Float f)] | None -> ());
  (if req.stop <> [] then
    gen_config := !gen_config @ [("stopSequences", `List (List.map (fun s -> `String s) req.stop))]);
  (match req.top_k with
   | Some n -> gen_config := !gen_config @ [("topK", `Int n)]
   | None -> ());
  (match req.seed with
   | Some n -> gen_config := !gen_config @ [("seed", `Int n)]
   | None -> ());
  (match req.response_format with
   | None
   | Some Types.Fmt_text -> ()
   | Some Types.Fmt_json_object ->
     gen_config := !gen_config @ [
       ("responseMimeType", `String "application/json")
     ]
   | Some (Types.Fmt_json_schema schema) ->
     gen_config := !gen_config @ [
       ("responseMimeType", `String "application/json");
       ("responseSchema", schema)
     ]);
  (* Reasoning -> generationConfig.thinkingConfig.thinkingBudget.
     Gemini uses integer token budgets; the effort enum maps to
     rough defaults, Budget takes the raw value, Dynamic becomes -1. *)
  (match req.reasoning with
   | None -> ()
   | Some r ->
     let budget =
       match r with
       | Minimal   -> 0
       | Low       -> 1024
       | Medium    -> 4096
       | High      -> 16384
       | Budget n  -> n
       | Dynamic   -> -1
     in
     gen_config := !gen_config @ [
       ("thinkingConfig", `Assoc [("thinkingBudget", `Int budget)])
     ]);
  (if !gen_config <> [] then
    fields := !fields @ [("generationConfig", `Assoc !gen_config)]);
  (match req.safety_settings with
   | None -> ()
   | Some s -> fields := !fields @ [("safetySettings", s)]);
  (if req.tools <> [] then
    fields := !fields @ [("tools", `List [`Assoc [("function_declarations", `List (List.map encode_tool req.tools))]])]);
  let all_fields = Json_merge.merge !fields req.extra in
  Ok (`Assoc all_fields)

let decode_finish_reason = function
  | "STOP"       -> Some Types.Stop
  | "MAX_TOKENS" -> Some Types.Length
  | "SAFETY" | "RECITATION" | "BLOCKLIST"
  | "OTHER" | "PROHIBITED_CONTENT" -> Some Types.Content_filter
  | _            -> None

let decode_usage j : Types.usage =
  let pt = to_int_opt (member "promptTokenCount" j) |> Option.value ~default:0 in
  let ct = to_int_opt (member "candidatesTokenCount" j) |> Option.value ~default:0 in
  let thoughts    = to_int_opt (member "thoughtsTokenCount" j) in
  let cache_read  = to_int_opt (member "cachedContentTokenCount" j) in
  let total = match to_int_opt (member "totalTokenCount" j) with
    | Some n -> n
    | None   -> pt + ct + Option.value thoughts ~default:0
  in
  {
    prompt_tokens      = pt;
    completion_tokens  = ct;
    total_tokens       = total;
    cache_read_tokens  = cache_read;
    cache_write_tokens = None;
    thinking_tokens    = thoughts;
  }

let decode_part j =
  let text_j    = member "text" j in
  let fn_call_j = member "functionCall" j in
  match text_j with
  | `String t when t <> "" ->
    Some (Types.Text t)
  | _ ->
    (match fn_call_j with
     | `Null -> None
     | fc ->
       let name  = member "name" fc |> to_string_opt |> Option.value ~default:"" in
       let args  = member "args" fc in
       (* Newer Gemini models (2.5+, 3.x-preview) emit an opaque
          [id] field per functionCall — read it when present so
          callers can distinguish multiple calls to the same
          tool in one response. Older models omit it; fall back
          to [name] so round-trip tool_result matching by name
          still works. *)
       let id =
         match member "id" fc |> to_string_opt with
         | Some s when s <> "" -> s
         | _ -> name
       in
       let thought_signature = member "thoughtSignature" j |> to_string_opt in
       Some (Types.Tool_use { id; name; input = args; thought_signature }))

let decode_response j =
  let usage      = match member "usageMetadata" j with
    | `Null -> None
    | u     -> Some (decode_usage u)
  in
  let candidates = to_list_opt (member "candidates" j) |> Option.value ~default:[] in
  let choices    = List.mapi (fun i cand ->
    let fr        = member "finishReason" cand |> to_string_opt |> (fun o -> Option.bind o decode_finish_reason) in
    let content_j = member "content" cand in
    let parts     = to_list_opt (member "parts" content_j) |> Option.value ~default:[] in
    let content   = List.filter_map decode_part parts in
    let tool_calls = List.filter_map (function
      | Types.Tool_use { id; name; input; _ } ->
        Some { Types.id; name; arguments = Yojson.Safe.to_string input }
      | _ -> None
    ) content in
    (* Keep Tool_use items in message.content so thought_signature
       round-trips through conversation history. *)
    let message : Types.message = {
      role = Types.Assistant; content; cache = None
    } in
    { Types.index = i; message; tool_calls; finish_reason = fr }
  ) candidates in
  Ok { Types.id = ""; model = ""; choices; usage; created = 0 }

let decode_error ~status j : Types.error =
  let err = member "error" j in
  let msg = match err with
    | `Null -> to_string_opt j |> Option.value ~default:"Unknown error"
    | e     -> member "message" e |> to_string_opt |> Option.value ~default:"Unknown error"
  in
  let kind : Types.error_kind = match status with
    | 401 | 403       -> Types.Auth_error
    | 404             -> Types.Not_found
    | 429             -> Types.Rate_limit { retry_after = None }
    | 400             -> Types.Invalid_request msg
    | n when n >= 500 -> Types.Server_error n
    | _               -> Types.Invalid_request msg
  in
  { kind; message = msg; provider = "gemini"; raw = Some j }

(** Gemini streaming: each chunk is a full response object. *)
let decode_chunk () (ev : Types.sse_event) =
  let data = String.trim ev.data in
  if data = "" then Ok None
  else begin
    match Yojson.Safe.from_string data with
    | exception _ -> Error (make_error (Types.Invalid_request "Invalid JSON in stream") data)
    | j ->
      (match decode_response j with
       | Error e -> Error e
       | Ok resp ->
         let usage = resp.Types.usage in
         let (delta, fr) = match resp.Types.choices with
           | [] ->
             { Types.role = None; content = None; tool_calls = [] }, None
           | c :: _ ->
             let text = List.find_map (function
               | Types.Text t -> Some t | _ -> None
             ) c.Types.message.content in
             (* Build deltas from message.content to preserve
                thought_signature (which tool_calls list lacks).

                Each Tool_use gets its OWN index. Consumers
                (e.g. LMI's llaml_provider) key their
                accumulator arrays by [td.index]; if every
                delta shares [index = 0] (as the pre-fix
                version did), multiple tool_calls in the same
                response collide — the consumer's args buffer
                receives concatenated JSON fragments like
                [{"a":1}{"b":2}], fails to parse, and every
                call surfaces with empty [input = {}]. See
                bug report in LMI's m15-pilot-interface
                dogfood session, 2026-04-16. *)
             let tool_uses =
               List.filter (function Types.Tool_use _ -> true | _ -> false)
                 c.Types.message.content
             in
             let tc_deltas = List.mapi (fun i tu ->
               match tu with
               | Types.Tool_use { id; name; input; thought_signature } ->
                 { Types.index = i; id = Some id; name = Some name;
                   arguments = Some (Yojson.Safe.to_string input);
                   thought_signature }
               | _ -> assert false  (* filtered above *)
             ) tool_uses in
             { Types.role = Some Types.Assistant; content = text; tool_calls = tc_deltas },
             c.Types.finish_reason
         in
         Ok (Some { Types.id = resp.Types.id; model = resp.Types.model; index = 0;
                    delta; finish_reason = fr; usage }))
  end

let encode_embed_request (_req : Types.embed_request) =
  Error { Types.kind = Types.Unsupported "batch embedding";
          message = "Use single embed calls for Gemini";
          provider = "gemini"; raw = None }

let decode_embed_response j =
  let embedding_j = member "embedding" j in
  let values      = to_list_opt (member "values" embedding_j) |> Option.value ~default:[] in
  let vector      = Array.of_list (List.filter_map to_float_opt values) in
  Ok { Types.model = ""; data = [{ index = 0; vector }]; usage = None }

let base_url = "https://generativelanguage.googleapis.com/v1beta/models"

let endpoint_for model ~streaming =
  if streaming then
    (* ?alt=sse makes Gemini emit proper SSE (data: {...} lines) instead of a JSON array *)
    Uri.of_string (base_url ^ "/" ^ model ^ ":streamGenerateContent?alt=sse")
  else
    Uri.of_string (base_url ^ "/" ^ model ^ ":generateContent")

let embed_endpoint model =
  Uri.of_string (base_url ^ "/" ^ model ^ ":embedContent")
