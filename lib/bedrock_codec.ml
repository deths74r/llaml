(** AWS Bedrock Converse API encode/decode. *)

let member k j = Yojson.Safe.Util.member k j
let to_string_opt j = try Some (Yojson.Safe.Util.to_string j) with _ -> None
let to_int_opt j = try Some (Yojson.Safe.Util.to_int j) with _ -> None
let to_list_opt j = try Some (Yojson.Safe.Util.to_list j) with _ -> None

let make_error ?raw kind message : Types.error =
  { kind; message; provider = "bedrock"; raw }

let encode_content = function
  | Types.Text t ->
    `Assoc [("text", `String t)]
  | Types.Image { url; _ } ->
    `Assoc [("text", `String ("[Image: " ^ url ^ "]"))]
  | Types.Tool_use { id; name; input; _ } ->
    `Assoc [("toolUse", `Assoc [
      ("toolUseId", `String id);
      ("name", `String name);
      ("input", input)
    ])]
  | Types.Tool_result { id; content; is_error } ->
    let status = if is_error then [("status", `String "error")] else [] in
    `Assoc [("toolResult", `Assoc (
      [("toolUseId", `String id);
       ("content", `List [`Assoc [("text", `String content)]])]
      @ status
    ))]

let encode_message (msg : Types.message) =
  let role_str = match msg.role with
    | Types.User      -> "user"
    | Types.Tool      -> "user"
    | Types.Assistant -> "assistant"
    | Types.System    -> "user"
  in
  let content = List.map encode_content msg.content in
  `Assoc [("role", `String role_str); ("content", `List content)]

let encode_tool (t : Types.tool) =
  `Assoc [("toolSpec", `Assoc (
    [("name", `String t.name);
     ("inputSchema", `Assoc [("json", t.schema)])]
    @ (match t.description with
       | Some d -> [("description", `String d)]
       | None   -> [])
  ))]

let encode_request (req : Types.request) =
  let system = List.filter_map (fun (m : Types.message) ->
    if m.role = Types.System then
      match m.content with
      | [Types.Text t] -> Some (`Assoc [("text", `String t)])
      | parts ->
        let text = String.concat "" (List.filter_map (function Types.Text t -> Some t | _ -> None) parts) in
        Some (`Assoc [("text", `String text)])
    else None
  ) req.messages in
  let messages = List.filter (fun (m : Types.message) -> m.role <> Types.System) req.messages in
  let messages_json = List.map encode_message messages in
  let fields = ref [("messages", `List messages_json)] in
  (if system <> [] then fields := !fields @ [("system", `List system)]);
  let inf_config = ref [] in
  (match req.max_tokens with Some n -> inf_config := !inf_config @ [("maxTokens", `Int n)] | None -> ());
  (match req.temperature with Some f -> inf_config := !inf_config @ [("temperature", `Float f)] | None -> ());
  (match req.top_p with Some f -> inf_config := !inf_config @ [("topP", `Float f)] | None -> ());
  (if req.stop <> [] then
    inf_config := !inf_config @ [("stopSequences", `List (List.map (fun s -> `String s) req.stop))]);
  (if !inf_config <> [] then
    fields := !fields @ [("inferenceConfig", `Assoc !inf_config)]);
  (if req.tools <> [] then
    fields := !fields @ [("toolConfig", `Assoc [("tools", `List (List.map encode_tool req.tools))])]);
  let all_fields = Json_merge.merge !fields req.extra in
  Ok (`Assoc all_fields)

let decode_finish_reason = function
  | "end_turn"      -> Some Types.Stop
  | "max_tokens"    -> Some Types.Length
  | "tool_use"      -> Some Types.Tool_calls
  | "stop_sequence" -> Some Types.Stop
  | _               -> None

let decode_usage j : Types.usage =
  let it = to_int_opt (member "inputTokens" j) |> Option.value ~default:0 in
  let ot = to_int_opt (member "outputTokens" j) |> Option.value ~default:0 in
  {
    prompt_tokens      = it;
    completion_tokens  = ot;
    total_tokens       = it + ot;
    cache_read_tokens  = None;
    cache_write_tokens = None;
    thinking_tokens    = None;
  }

let decode_content_block j =
  let text_j     = member "text" j in
  let tool_use_j = member "toolUse" j in
  match text_j with
  | `String t -> Some (Types.Text t)
  | _ ->
    (match tool_use_j with
     | `Null -> None
     | tu ->
       let id    = member "toolUseId" tu |> to_string_opt |> Option.value ~default:"" in
       let name  = member "name" tu |> to_string_opt |> Option.value ~default:"" in
       let input = member "input" tu in
       Some (Types.Tool_use { id; name; input; thought_signature = None; metadata = None }))

let decode_response j =
  let output    = member "output" j in
  let message_j = member "message" output in
  let fr        = member "stopReason" j |> to_string_opt |> (fun o -> Option.bind o decode_finish_reason) in
  let usage     = match member "usage" j with
    | `Null -> None
    | u     -> Some (decode_usage u)
  in
  let content_blocks = to_list_opt (member "content" message_j) |> Option.value ~default:[] in
  let content   = List.filter_map decode_content_block content_blocks in
  let tool_calls = List.filter_map (function
    | Types.Tool_use { id; name; input; _ } ->
      Some { Types.id; name; arguments = Yojson.Safe.to_string input }
    | _ -> None
  ) content in
  let message_content = List.filter (function Types.Tool_use _ -> false | _ -> true) content in
  let message : Types.message = {
    role = Types.Assistant; content = message_content; cache = None
  } in
  let choice  = { Types.index = 0; message; tool_calls; finish_reason = fr } in
  Ok { Types.id = ""; model = ""; choices = [choice]; usage; created = 0 }

let decode_error ~status j : Types.error =
  let msg = match member "message" j with
    | `Null -> to_string_opt j |> Option.value ~default:"Unknown error"
    | m     -> to_string_opt m |> Option.value ~default:"Unknown error"
  in
  let kind : Types.error_kind = match status with
    | 401 | 403       -> Types.Auth_error
    | 404             -> Types.Not_found
    | 429             -> Types.Rate_limit { retry_after = None }
    | 400             -> Types.Invalid_request msg
    | n when n >= 500 -> Types.Server_error n
    | _               -> Types.Invalid_request msg
  in
  { kind; message = msg; provider = "bedrock"; raw = Some j }

(** {1 Binary event-stream parser} *)

let read_uint32_be s pos =
  let b i = Char.code (String.get s (pos + i)) in
  (b 0 lsl 24) lor (b 1 lsl 16) lor (b 2 lsl 8) lor (b 3)

let read_uint16_be s pos =
  let b i = Char.code (String.get s (pos + i)) in
  (b 0 lsl 8) lor b 1

(** Parse headers from header section of an event-stream message.
    Returns a list of (name, value) string pairs. *)
let parse_event_headers s hdr_start hdr_len =
  let headers = ref [] in
  let p = ref hdr_start in
  let hdr_end = hdr_start + hdr_len in
  while !p < hdr_end do
    let name_len = Char.code (String.get s !p) in
    p := !p + 1;
    if !p + name_len > hdr_end then p := hdr_end (* safety *)
    else begin
      let name = String.sub s !p name_len in
      p := !p + name_len;
      let value_type = Char.code (String.get s !p) in
      p := !p + 1;
      if value_type = 7 then begin (* string *)
        let val_len = read_uint16_be s !p in
        p := !p + 2;
        let value = String.sub s !p val_len in
        p := !p + val_len;
        headers := (name, value) :: !headers
      end else begin
        (* Other types: skip. Value length is type-dependent.
           For safety, just stop parsing headers. *)
        p := hdr_end
      end
    end
  done;
  List.rev !headers

(** Parse as many complete event-stream messages as possible from [s] starting at offset 0.
    Returns [(events, bytes_consumed)] where [events] is a list of [(event_type, payload_json)]. *)
let parse_messages s =
  let len = String.length s in
  let pos = ref 0 in
  let events = ref [] in
  let continue_ = ref true in
  while !continue_ do
    if !pos + 12 > len then
      continue_ := false  (* need at least prelude *)
    else begin
      let total_len = read_uint32_be s !pos in
      if total_len < 16 || !pos + total_len > len then
        continue_ := false  (* incomplete message *)
      else begin
        let headers_len = read_uint32_be s (!pos + 4) in
        let hdr_start = !pos + 12 in
        let payload_start = hdr_start + headers_len in
        (* total_len includes: 12 (prelude) + headers_len + payload + 4 (trailing CRC) *)
        let payload_end = !pos + total_len - 4 in
        let payload_len = payload_end - payload_start in
        if payload_len < 0 then
          continue_ := false
        else begin
          let headers = parse_event_headers s hdr_start headers_len in
          let payload = if payload_len > 0 then String.sub s payload_start payload_len else "" in
          let event_type = List.assoc_opt ":event-type" headers
            |> Option.value ~default:"" in
          if event_type <> "" then
            events := (event_type, payload) :: !events;
          pos := !pos + total_len
        end
      end
    end
  done;
  (List.rev !events, !pos)

(** {1 Streaming decoder state} *)

type state = {
  mutable in_tokens  : int;
  mutable out_tokens : int;
  chunk_id : string;  (* generated once, used for all chunks *)
}

let make_state () = {
  in_tokens  = 0;
  out_tokens = 0;
  chunk_id   = string_of_int (int_of_float (Unix.time ()));
}

let decode_chunk (state : state) (ev : Types.sse_event) =
  let event_type = ev.event |> Option.value ~default:"" in
  match event_type with
  | "contentBlockStart" | "contentBlockStop" -> Ok None
  | "contentBlockDelta" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ -> Ok None
     | j ->
       let delta_j = member "delta" j in
       (* Check for text delta *)
       (match to_string_opt (member "text" delta_j) with
        | Some text ->
          let delta : Types.delta = { role = None; content = Some text; tool_calls = [] } in
          Ok (Some { Types.id = state.chunk_id; model = ""; index = 0;
                     delta; finish_reason = None; usage = None })
        | None ->
          (* Check for toolUse delta *)
          let tool_use_j = member "toolUse" delta_j in
          (match to_string_opt (member "input" tool_use_j) with
           | Some partial ->
             let tc_delta : Types.tool_call_delta =
               { index = 0; id = None; name = None; arguments = Some partial; thought_signature = None } in
             let delta : Types.delta = { role = None; content = None; tool_calls = [tc_delta] } in
             Ok (Some { Types.id = state.chunk_id; model = ""; index = 0;
                        delta; finish_reason = None; usage = None })
           | None -> Ok None)))
  | "messageStop" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ ->
       let delta : Types.delta = { role = None; content = None; tool_calls = [] } in
       Ok (Some { Types.id = state.chunk_id; model = ""; index = 0;
                  delta; finish_reason = Some Types.Stop; usage = None })
     | j ->
       let fr = member "stopReason" j |> to_string_opt
         |> (fun o -> Option.bind o decode_finish_reason)
         |> Option.value ~default:Types.Stop in
       let delta : Types.delta = { role = None; content = None; tool_calls = [] } in
       Ok (Some { Types.id = state.chunk_id; model = ""; index = 0;
                  delta; finish_reason = Some fr; usage = None }))
  | "metadata" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ -> Ok None
     | j ->
       let usage_j = member "usage" j in
       let it = to_int_opt (member "inputTokens" usage_j) |> Option.value ~default:0 in
       let ot = to_int_opt (member "outputTokens" usage_j) |> Option.value ~default:0 in
       state.in_tokens <- it;
       state.out_tokens <- ot;
       let usage = Some {
         Types.prompt_tokens     = it;
         completion_tokens       = ot;
         total_tokens            = it + ot;
         cache_read_tokens       = None;
         cache_write_tokens      = None;
         thinking_tokens         = None;
       } in
       let delta : Types.delta = { role = None; content = None; tool_calls = [] } in
       Ok (Some { Types.id = state.chunk_id; model = ""; index = 0;
                  delta; finish_reason = None; usage }))
  | _ -> Ok None

let base_url region =
  Printf.sprintf "https://bedrock-runtime.%s.amazonaws.com" region

let endpoint (req : Types.request) ~streaming =
  let region = "us-east-1" in
  let suffix = if streaming then "/converse-stream" else "/converse" in
  Uri.of_string (base_url region ^ "/model/" ^ Uri.pct_encode req.model ^ suffix)

let endpoint_with_region (req : Types.request) ~region ~streaming =
  let suffix = if streaming then "/converse-stream" else "/converse" in
  Uri.of_string (base_url region ^ "/model/" ^ Uri.pct_encode req.model ^ suffix)
