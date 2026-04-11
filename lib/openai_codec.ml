(** OpenAI wire protocol encode/decode. *)

let member k j = Yojson.Safe.Util.member k j
let to_string_opt j = try Some (Yojson.Safe.Util.to_string j) with _ -> None
let to_int_opt j = try Some (Yojson.Safe.Util.to_int j) with _ -> None
let to_float_opt j = try Some (Yojson.Safe.Util.to_float j) with _ -> None
let to_list_opt j = try Some (Yojson.Safe.Util.to_list j) with _ -> None

let make_error ?raw kind message : Types.error =
  { kind; message; provider = "openai"; raw }

let encode_role = function
  | Types.System    -> "system"
  | Types.User      -> "user"
  | Types.Assistant -> "assistant"
  | Types.Tool      -> "tool"

(** Encode a message to OpenAI JSON format. *)
let encode_message (msg : Types.message) =
  match msg.role with
  | Types.Tool ->
    let (id_opt, content_str) = match msg.content with
      | [Types.Tool_result { id; content; _ }] -> (Some id, content)
      | [Types.Text t] -> (None, t)
      | _ -> (None, "")
    in
    let fields = [("role", `String "tool"); ("content", `String content_str)] in
    let fields = match id_opt with
      | Some id -> ("tool_call_id", `String id) :: fields
      | None -> fields
    in
    `Assoc (List.rev fields)
  | Types.Assistant ->
    let tool_uses = List.filter_map (function
      | Types.Tool_use { id; name; input } -> Some (id, name, input)
      | _ -> None
    ) msg.content in
    let text_parts = List.filter_map (function
      | Types.Text t -> Some t
      | _ -> None
    ) msg.content in
    let tool_calls_json = List.map (fun (tu_id, tu_name, tu_input) ->
      `Assoc [
        ("id", `String tu_id);
        ("type", `String "function");
        ("function", `Assoc [
          ("name", `String tu_name);
          ("arguments", `String (Yojson.Safe.to_string tu_input))
        ])
      ]
    ) tool_uses in
    let content_val = match text_parts with
      | [] -> `Null
      | [t] -> `String t
      | parts -> `String (String.concat "" parts)
    in
    let fields = [("role", `String "assistant"); ("content", content_val)] in
    let fields = if tool_calls_json <> [] then
      fields @ [("tool_calls", `List tool_calls_json)]
    else fields in
    `Assoc fields
  | role ->
    let role_str = encode_role role in
    let content_val = match msg.content with
      | [Types.Text t] -> `String t
      | parts ->
        let has_complex = List.exists (function Types.Text _ -> false | _ -> true) parts in
        if has_complex then
          `List (List.filter_map (function
            | Types.Text t -> Some (`Assoc [("type", `String "text"); ("text", `String t)])
            | Types.Image { url; detail } ->
              let detail_str = match detail with
                | Some `Low -> "low" | Some `High -> "high" | _ -> "auto"
              in
              Some (`Assoc [
                ("type", `String "image_url");
                ("image_url", `Assoc [("url", `String url); ("detail", `String detail_str)])
              ])
            | _ -> None
          ) parts)
        else
          `String (String.concat "" (List.filter_map (function Types.Text t -> Some t | _ -> None) parts))
    in
    `Assoc [("role", `String role_str); ("content", content_val)]

let encode_tool (t : Types.tool) =
  `Assoc [
    ("type", `String "function");
    ("function", `Assoc (
      [("name", `String t.name);
       ("parameters", t.schema)]
      @ (match t.description with
         | Some d -> [("description", `String d)]
         | None -> [])
    ))
  ]

let encode_tool_choice = function
  | Types.Auto     -> `String "auto"
  | Types.None     -> `String "none"
  | Types.Required -> `String "required"
  | Types.Tool name -> `Assoc [("type", `String "function"); ("function", `Assoc [("name", `String name)])]

let encode_request (req : Types.request) =
  let messages = List.map encode_message req.messages in
  let fields = ref [
    ("model", `String req.model);
    ("messages", `List messages);
  ] in
  (match req.max_tokens with Some n -> fields := !fields @ [("max_tokens", `Int n)] | None -> ());
  (match req.temperature with Some f -> fields := !fields @ [("temperature", `Float f)] | None -> ());
  (match req.top_p with Some f -> fields := !fields @ [("top_p", `Float f)] | None -> ());
  (if req.stop <> [] then fields := !fields @ [("stop", `List (List.map (fun s -> `String s) req.stop))]);
  (if req.stream then fields := !fields @ [("stream", `Bool true); ("stream_options", `Assoc [("include_usage", `Bool true)])]);
  (if req.tools <> [] then begin
    fields := !fields @ [("tools", `List (List.map encode_tool req.tools))];
    fields := !fields @ [("tool_choice", encode_tool_choice req.tool_choice)]
  end);
  (match req.user with Some u -> fields := !fields @ [("user", `String u)] | None -> ());
  let all_fields = !fields @ req.extra in
  Ok (`Assoc all_fields)

let decode_finish_reason = function
  | "stop"           -> Some Types.Stop
  | "length"         -> Some Types.Length
  | "tool_calls"     -> Some Types.Tool_calls
  | "content_filter" -> Some Types.Content_filter
  | _                -> None

let decode_usage j : Types.usage =
  let pt = member "prompt_tokens" j |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
  let ct = member "completion_tokens" j |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
  let cache_read = member "prompt_tokens_details" j
    |> member "cached_tokens" |> to_int_opt in
  {
    prompt_tokens     = pt;
    completion_tokens = ct;
    total_tokens      = pt + ct;
    cache_read_tokens = cache_read;
    cache_write_tokens = None;
    thinking_tokens   = None;
  }

let decode_tool_calls j : Types.tool_call list =
  match to_list_opt j with
  | None -> []
  | Some tcs ->
    List.filter_map (fun tc ->
      let id   = member "id" tc |> to_string_opt |> Option.value ~default:"" in
      let fn   = member "function" tc in
      let name = member "name" fn |> to_string_opt |> Option.value ~default:"" in
      let arguments = member "arguments" fn |> to_string_opt |> Option.value ~default:"{}" in
      Some { Types.id; name; arguments }
    ) tcs

let decode_message role j : Types.message =
  let content_j = member "content" j in
  let text = to_string_opt content_j |> Option.value ~default:"" in
  let content = if text = "" then [] else [Types.Text text] in
  { role; content }

let decode_response j =
  let id      = member "id" j |> to_string_opt |> Option.value ~default:"" in
  let model   = member "model" j |> to_string_opt |> Option.value ~default:"" in
  let created = member "created" j |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
  let usage   = match member "usage" j with
    | `Null -> None
    | u     -> Some (decode_usage u)
  in
  let choices = match to_list_opt (member "choices" j) with
    | None -> []
    | Some cs ->
      List.mapi (fun i c ->
        let msg_j = member "message" c in
        let role  = match member "role" msg_j |> to_string_opt with
          | Some "system"    -> Types.System
          | Some "user"      -> Types.User
          | Some "tool"      -> Types.Tool
          | _                -> Types.Assistant
        in
        let message     = decode_message role msg_j in
        let tool_calls  = decode_tool_calls (member "tool_calls" msg_j) in
        let fr          = member "finish_reason" c |> to_string_opt
          |> (fun o -> Option.bind o decode_finish_reason) in
        { Types.index = i; message; tool_calls; finish_reason = fr }
      ) cs
  in
  Ok { Types.id; model; choices; usage; created }

let decode_error ~status j : Types.error =
  let msg = (match member "error" j with
    | `Null -> to_string_opt j |> Option.value ~default:"Unknown error"
    | err   -> member "message" err |> to_string_opt |> Option.value ~default:"Unknown error"
  ) in
  let kind : Types.error_kind = match status with
    | 401                    -> Types.Auth_error
    | 404                    -> Types.Not_found
    | 429                    -> Types.Rate_limit { retry_after = None }
    | 400                    -> Types.Invalid_request msg
    | n when n >= 500        -> Types.Server_error n
    | _                      -> Types.Invalid_request msg
  in
  { kind; message = msg; provider = "openai"; raw = Some j }

(** Stateless decoder — OpenAI SSE is self-contained per event. *)
let decode_chunk () (ev : Types.sse_event) =
  let data = ev.data in
  if data = "[DONE]" then Ok None
  else begin
    match Yojson.Safe.from_string data with
    | exception _ -> Error (make_error (Types.Invalid_request "Invalid JSON in SSE") data)
    | j ->
      let id    = member "id" j |> to_string_opt |> Option.value ~default:"" in
      let model = member "model" j |> to_string_opt |> Option.value ~default:"" in
      let usage = match member "usage" j with
        | `Null -> None
        | u     ->
          (match to_int_opt (member "prompt_tokens" u) with
           | None   -> None
           | Some _ -> Some (decode_usage u))
      in
      let choices = to_list_opt (member "choices" j) |> Option.value ~default:[] in
      (match choices with
       | [] ->
         let delta : Types.delta = { role = None; content = None; tool_calls = [] } in
         Ok (Some { Types.id; model; index = 0; delta; finish_reason = None; usage })
       | c :: _ ->
         let delta_j = member "delta" c in
         let content  = member "content" delta_j |> to_string_opt in
         let role     = match member "role" delta_j |> to_string_opt with
           | Some "system"    -> Some Types.System
           | Some "user"      -> Some Types.User
           | Some "assistant" -> Some Types.Assistant
           | Some "tool"      -> Some Types.Tool
           | _                -> None
         in
         let tool_call_deltas : Types.tool_call_delta list =
           match to_list_opt (member "tool_calls" delta_j) with
           | None -> []
           | Some tcs ->
             List.filter_map (fun tc ->
               let index   = member "index" tc |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
               let id_opt  = member "id" tc |> to_string_opt in
               let fn      = member "function" tc in
               let name_opt = member "name" fn |> to_string_opt in
               let args_opt = member "arguments" fn |> to_string_opt in
               Some { Types.index; id = id_opt; name = name_opt; arguments = args_opt }
             ) tcs
         in
         let fr    = member "finish_reason" c |> to_string_opt |> (fun o -> Option.bind o decode_finish_reason) in
         let delta : Types.delta = { role; content; tool_calls = tool_call_deltas } in
         let index = member "index" c |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
         Ok (Some { Types.id; model; index; delta; finish_reason = fr; usage }))
  end

let encode_embed_request (req : Types.embed_request) =
  let fields = [
    ("model", `String req.model);
    ("input", `List (List.map (fun s -> `String s) req.input));
  ] in
  let fields = match req.dimensions with
    | Some d -> fields @ [("dimensions", `Int d)]
    | None   -> fields
  in
  Ok (`Assoc fields)

let decode_embed_response j =
  let model = member "model" j |> to_string_opt |> Option.value ~default:"" in
  let data  = match to_list_opt (member "data" j) with
    | None -> []
    | Some items ->
      List.filter_map (fun item ->
        let index = member "index" item |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
        let vec   = match member "embedding" item with
          | `List floats ->
            Array.of_list (List.filter_map to_float_opt floats)
          | _ -> [||]
        in
        Some { Types.index; vector = vec }
      ) items
  in
  let usage = match member "usage" j with
    | `Null -> None
    | u     -> Some (decode_usage u)
  in
  Ok { Types.model; data; usage }
