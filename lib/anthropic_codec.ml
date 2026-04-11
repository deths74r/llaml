(** Anthropic Messages API encode/decode. *)

let member k j = Yojson.Safe.Util.member k j
let to_string_opt j = try Some (Yojson.Safe.Util.to_string j) with _ -> None
let to_int_opt j = try Some (Yojson.Safe.Util.to_int j) with _ -> None
let to_list_opt j = try Some (Yojson.Safe.Util.to_list j) with _ -> None

let make_error ?raw kind message : Types.error =
  { kind; message; provider = "anthropic"; raw }

(** Stateful decoder for Anthropic SSE streams. *)
type state = {
  mutable msg_id    : string;
  mutable model     : string;
  mutable in_tokens : int;
  mutable blocks    : (int * [ `Text | `Tool of string * string ]) list;
}

let make_state () : state = {
  msg_id    = "";
  model     = "";
  in_tokens = 0;
  blocks    = [];
}

let encode_content_block = function
  | Types.Text t ->
    `Assoc [("type", `String "text"); ("text", `String t)]
  | Types.Image { url; _ } ->
    `Assoc [
      ("type", `String "image");
      ("source", `Assoc [
        ("type", `String "url");
        ("url", `String url)
      ])
    ]
  | Types.Tool_use { id; name; input } ->
    `Assoc [
      ("type", `String "tool_use");
      ("id", `String id);
      ("name", `String name);
      ("input", input)
    ]
  | Types.Tool_result { id; content; is_error } ->
    `Assoc [
      ("type", `String "tool_result");
      ("tool_use_id", `String id);
      ("content", `String content);
      ("is_error", `Bool is_error)
    ]

let encode_message (msg : Types.message) =
  let role_str = match msg.role with
    | Types.User      -> "user"
    | Types.Tool      -> "user"
    | Types.Assistant -> "assistant"
    | Types.System    -> "user"
  in
  let content = match msg.content with
    | [Types.Text t] -> `String t
    | parts -> `List (List.map encode_content_block parts)
  in
  `Assoc [("role", `String role_str); ("content", content)]

let encode_tool (t : Types.tool) =
  `Assoc (
    [("name", `String t.name);
     ("input_schema", t.schema)]
    @ (match t.description with
       | Some d -> [("description", `String d)]
       | None   -> [])
  )

let encode_tool_choice = function
  | Types.Auto      -> `Assoc [("type", `String "auto")]
  | Types.None      -> `Assoc [("type", `String "none")]
  | Types.Required  -> `Assoc [("type", `String "any")]
  | Types.Tool name -> `Assoc [("type", `String "tool"); ("name", `String name)]

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
  let messages_json = List.map encode_message messages in
  let fields = ref [
    ("model", `String req.model);
    ("messages", `List messages_json);
    ("max_tokens", `Int (Option.value req.max_tokens ~default:4096));
  ] in
  (match system with
   | []    -> ()
   | parts -> fields := !fields @ [("system", `String (String.concat "\n" parts))]);
  (match req.temperature with Some f -> fields := !fields @ [("temperature", `Float f)] | None -> ());
  (match req.top_p with Some f -> fields := !fields @ [("top_p", `Float f)] | None -> ());
  (if req.stop <> [] then
    fields := !fields @ [("stop_sequences", `List (List.map (fun s -> `String s) req.stop))]);
  (if req.stream then fields := !fields @ [("stream", `Bool true)]);
  (if req.tools <> [] then begin
    fields := !fields @ [("tools", `List (List.map encode_tool req.tools))];
    fields := !fields @ [("tool_choice", encode_tool_choice req.tool_choice)]
  end);
  let all_fields = !fields @ req.extra in
  Ok (`Assoc all_fields)

let decode_finish_reason = function
  | "end_turn"      -> Some Types.Stop
  | "max_tokens"    -> Some Types.Length
  | "tool_use"      -> Some Types.Tool_calls
  | "stop_sequence" -> Some Types.Stop
  | _               -> None

let decode_usage_from_response j : Types.usage =
  let it = member "input_tokens" j |> to_int_opt |> Option.value ~default:0 in
  let ot = member "output_tokens" j |> to_int_opt |> Option.value ~default:0 in
  let cache_read  = member "cache_read_input_tokens" j |> to_int_opt in
  let cache_write = member "cache_creation_input_tokens" j |> to_int_opt in
  {
    prompt_tokens      = it;
    completion_tokens  = ot;
    total_tokens       = it + ot;
    cache_read_tokens  = cache_read;
    cache_write_tokens = cache_write;
    thinking_tokens    = None;
  }

let decode_response j =
  let id    = member "id" j |> to_string_opt |> Option.value ~default:"" in
  let model = member "model" j |> to_string_opt |> Option.value ~default:"" in
  let fr    = member "stop_reason" j |> to_string_opt |> (fun o -> Option.bind o decode_finish_reason) in
  let usage = match member "usage" j with
    | `Null -> None
    | u     -> Some (decode_usage_from_response u)
  in
  let content_blocks = to_list_opt (member "content" j) |> Option.value ~default:[] in
  let all_content = List.filter_map (fun block ->
    let typ = member "type" block |> to_string_opt |> Option.value ~default:"" in
    match typ with
    | "text" ->
      member "text" block |> to_string_opt |> Option.map (fun t -> Types.Text t)
    | "tool_use" ->
      let id    = member "id" block |> to_string_opt |> Option.value ~default:"" in
      let name  = member "name" block |> to_string_opt |> Option.value ~default:"" in
      let input = member "input" block in
      Some (Types.Tool_use { id; name; input })
    | _ -> None
  ) content_blocks in
  let tool_calls = List.filter_map (function
    | Types.Tool_use { id; name; input } ->
      Some { Types.id; name; arguments = Yojson.Safe.to_string input }
    | _ -> None
  ) all_content in
  let message_content = List.filter (function Types.Tool_use _ -> false | _ -> true) all_content in
  let message = { Types.role = Types.Assistant; content = message_content } in
  let choice  = { Types.index = 0; message; tool_calls; finish_reason = fr } in
  Ok { Types.id; model; choices = [choice]; usage; created = 0 }

let decode_error ~status j : Types.error =
  let err = member "error" j in
  let msg = match err with
    | `Null -> to_string_opt j |> Option.value ~default:"Unknown error"
    | e     -> member "message" e |> to_string_opt |> Option.value ~default:"Unknown error"
  in
  let kind : Types.error_kind = match status with
    | 401             -> Types.Auth_error
    | 404             -> Types.Not_found
    | 429             -> Types.Rate_limit { retry_after = None }
    | 400             -> Types.Invalid_request msg
    | n when n >= 500 -> Types.Server_error n
    | _               -> Types.Invalid_request msg
  in
  { kind; message = msg; provider = "anthropic"; raw = Some j }

let decode_chunk (state : state) (ev : Types.sse_event) =
  let event_type = ev.event |> Option.value ~default:"" in
  match event_type with
  | "ping"         -> Ok None
  | "message_stop" -> Ok None
  | "message_start" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ -> Error (make_error (Types.Invalid_request "Invalid JSON") ev.data)
     | j ->
       let msg_j = member "message" j in
       state.msg_id <- (member "id" msg_j |> to_string_opt |> Option.value ~default:"");
       state.model  <- (member "model" msg_j |> to_string_opt |> Option.value ~default:"");
       (match member "usage" msg_j with
        | `Null -> ()
        | u     -> state.in_tokens <- (to_int_opt (member "input_tokens" u) |> Option.value ~default:0));
       Ok None)
  | "content_block_start" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ -> Error (make_error (Types.Invalid_request "Invalid JSON") ev.data)
     | j ->
       let index = member "index" j |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
       let block = member "content_block" j in
       let typ   = member "type" block |> to_string_opt |> Option.value ~default:"" in
       (match typ with
        | "text" ->
          state.blocks <- (index, `Text) :: state.blocks;
          Ok None
        | "tool_use" ->
          let id   = member "id" block |> to_string_opt |> Option.value ~default:"" in
          let name = member "name" block |> to_string_opt |> Option.value ~default:"" in
          state.blocks <- (index, `Tool (id, name)) :: state.blocks;
          let tc_delta : Types.tool_call_delta =
            { index; id = Some id; name = Some name; arguments = None } in
          let delta : Types.delta =
            { role = None; content = None; tool_calls = [tc_delta] } in
          Ok (Some { Types.id = state.msg_id; model = state.model; index = 0;
                     delta; finish_reason = None; usage = None })
        | _ -> Ok None))
  | "content_block_delta" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ -> Error (make_error (Types.Invalid_request "Invalid JSON") ev.data)
     | j ->
       let index      = member "index" j |> Yojson.Safe.Util.to_int_option |> Option.value ~default:0 in
       let delta_j    = member "delta" j in
       let delta_type = member "type" delta_j |> to_string_opt |> Option.value ~default:"" in
       (match delta_type with
        | "text_delta" ->
          let text  = member "text" delta_j |> to_string_opt |> Option.value ~default:"" in
          let delta : Types.delta = { role = None; content = Some text; tool_calls = [] } in
          Ok (Some { Types.id = state.msg_id; model = state.model; index = 0;
                     delta; finish_reason = None; usage = None })
        | "input_json_delta" ->
          let partial  = member "partial_json" delta_j |> to_string_opt |> Option.value ~default:"" in
          let tc_delta : Types.tool_call_delta =
            { index; id = None; name = None; arguments = Some partial } in
          let delta : Types.delta = { role = None; content = None; tool_calls = [tc_delta] } in
          Ok (Some { Types.id = state.msg_id; model = state.model; index = 0;
                     delta; finish_reason = None; usage = None })
        | _ -> Ok None))
  | "content_block_stop" -> Ok None
  | "message_delta" ->
    (match Yojson.Safe.from_string ev.data with
     | exception _ -> Error (make_error (Types.Invalid_request "Invalid JSON") ev.data)
     | j ->
       let delta_j   = member "delta" j in
       let fr        = member "stop_reason" delta_j |> to_string_opt
         |> (fun o -> Option.bind o decode_finish_reason) in
       let usage_j   = member "usage" j in
       let out_tokens = to_int_opt (member "output_tokens" usage_j) |> Option.value ~default:0 in
       let usage = Some {
         Types.prompt_tokens     = state.in_tokens;
         completion_tokens       = out_tokens;
         total_tokens            = state.in_tokens + out_tokens;
         cache_read_tokens       = None;
         cache_write_tokens      = None;
         thinking_tokens         = None;
       } in
       let delta : Types.delta = { role = None; content = None; tool_calls = [] } in
       Ok (Some { Types.id = state.msg_id; model = state.model; index = 0;
                  delta; finish_reason = fr; usage }))
  | _ ->
    (* Unknown event type — skip *)
    Ok None
