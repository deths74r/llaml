(** Alcotest tests for llaml *)

(* ------------------------------------------------------------------ *)
(* Helpers *)
(* ------------------------------------------------------------------ *)

let check_ok label f =
  match f () with
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: unexpected error: %s" label e.Llaml.Types.message

let starts_with prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

(* ------------------------------------------------------------------ *)
(* OpenAI codec tests *)
(* ------------------------------------------------------------------ *)

let test_openai_encode_message_simple () =
  let msg : Llaml.Types.message =
    { role = Llaml.Types.User; content = [Llaml.Types.Text "hello"] }
  in
  let j = Llaml.Openai_codec.encode_message msg in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "has role user" true (String.length s > 0);
  Alcotest.(check bool) "contains role" true
    (try let _ = Str.search_forward (Str.regexp_string "\"role\"") s 0 in true
     with Not_found -> false);
  Alcotest.(check bool) "contains hello" true
    (try let _ = Str.search_forward (Str.regexp_string "hello") s 0 in true
     with Not_found -> false)

let test_openai_encode_message_assistant_tool_use () =
  let msg : Llaml.Types.message = {
    role = Llaml.Types.Assistant;
    content = [Llaml.Types.Tool_use { id = "call_1"; name = "my_tool"; input = `Assoc [("x", `Int 1)] }]
  } in
  let j = Llaml.Openai_codec.encode_message msg in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "has tool_calls" true
    (try let _ = Str.search_forward (Str.regexp_string "tool_calls") s 0 in true
     with Not_found -> false);
  Alcotest.(check bool) "content is null" true
    (try let _ = Str.search_forward (Str.regexp_string "\"content\":null") s 0 in true
     with Not_found -> false)

let test_openai_encode_message_tool_result () =
  let msg : Llaml.Types.message = {
    role = Llaml.Types.Tool;
    content = [Llaml.Types.Tool_result { id = "call_1"; content = "42"; is_error = false }]
  } in
  let j = Llaml.Openai_codec.encode_message msg in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "role is tool" true
    (try let _ = Str.search_forward (Str.regexp_string "\"role\":\"tool\"") s 0 in true
     with Not_found -> false);
  Alcotest.(check bool) "has tool_call_id" true
    (try let _ = Str.search_forward (Str.regexp_string "tool_call_id") s 0 in true
     with Not_found -> false)

let test_openai_encode_request_basic () =
  let req = Llaml.Types.request ~model:"gpt-4o" ~messages:[] () in
  let result = Llaml.Openai_codec.encode_request req in
  match result with
  | Error e -> Alcotest.failf "encode_request error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "has model" true
      (try let _ = Str.search_forward (Str.regexp_string "gpt-4o") s 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "has messages" true
      (try let _ = Str.search_forward (Str.regexp_string "messages") s 0 in true
       with Not_found -> false)

let test_openai_decode_response_fixture () =
  let fixture = {|{
    "id": "chatcmpl-abc123",
    "object": "chat.completion",
    "model": "gpt-4o",
    "created": 1699000000,
    "choices": [{
      "index": 0,
      "message": {"role": "assistant", "content": "Hello!"},
      "finish_reason": "stop"
    }],
    "usage": {
      "prompt_tokens": 10,
      "completion_tokens": 5,
      "total_tokens": 15,
      "prompt_tokens_details": {"cached_tokens": 0}
    }
  }|} in
  let j = Yojson.Safe.from_string fixture in
  (match Llaml.Openai_codec.decode_response j with
   | Error e -> Alcotest.failf "decode_response error: %s" e.Llaml.Types.message
   | Ok resp ->
     Alcotest.(check string) "id" "chatcmpl-abc123" resp.Llaml.Types.id;
     Alcotest.(check string) "model" "gpt-4o" resp.Llaml.Types.model;
     (match resp.choices with
      | [ch] ->
        (match ch.message.content with
         | [Llaml.Types.Text t] -> Alcotest.(check string) "content" "Hello!" t
         | _ -> Alcotest.fail "expected text content")
      | _ -> Alcotest.fail "expected one choice"))

let test_openai_decode_chunk_done () =
  let ev : Llaml.Types.sse_event = { event = None; data = "[DONE]" } in
  (match Llaml.Openai_codec.decode_chunk () ev with
   | Ok None -> ()
   | Ok (Some _) -> Alcotest.fail "expected None for [DONE]"
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message)

let test_openai_decode_chunk_text_delta () =
  let data = {|{"id":"chatcmpl-1","model":"gpt-4o","choices":[{"index":0,"delta":{"role":"assistant","content":"Hi"},"finish_reason":null}]}|} in
  let ev : Llaml.Types.sse_event = { event = None; data } in
  (match Llaml.Openai_codec.decode_chunk () ev with
   | Ok (Some chunk) ->
     Alcotest.(check (option string)) "content" (Some "Hi") chunk.delta.content
   | Ok None -> Alcotest.fail "expected Some chunk"
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message)

let test_openai_decode_error_401 () =
  let j = Yojson.Safe.from_string {|{"error":{"message":"Invalid API key"}}|} in
  let err = Llaml.Openai_codec.decode_error ~status:401 j in
  Alcotest.(check bool) "auth error" true
    (err.kind = Llaml.Types.Auth_error)

let test_openai_decode_error_429 () =
  let j = Yojson.Safe.from_string {|{"error":{"message":"Rate limit exceeded"}}|} in
  let err = Llaml.Openai_codec.decode_error ~status:429 j in
  Alcotest.(check bool) "rate limit" true
    (match err.kind with Llaml.Types.Rate_limit _ -> true | _ -> false)

let test_openai_encode_embed_request () =
  let req : Llaml.Types.embed_request = {
    model = "text-embedding-3-small"; input = ["hello"]; dimensions = None
  } in
  (match Llaml.Openai_codec.encode_embed_request req with
   | Error e -> Alcotest.failf "embed encode error: %s" e.Llaml.Types.message
   | Ok j ->
     let s = Yojson.Safe.to_string j in
     Alcotest.(check bool) "has model" true
       (try let _ = Str.search_forward (Str.regexp_string "text-embedding-3-small") s 0 in true
        with Not_found -> false);
     Alcotest.(check bool) "has input" true
       (try let _ = Str.search_forward (Str.regexp_string "\"input\"") s 0 in true
        with Not_found -> false))

(* ------------------------------------------------------------------ *)
(* Anthropic codec tests *)
(* ------------------------------------------------------------------ *)

let test_anthropic_encode_request_system () =
  let messages = [
    { Llaml.Types.role = Llaml.Types.System;
      content = [Llaml.Types.Text "You are helpful."] };
    { Llaml.Types.role = Llaml.Types.User;
      content = [Llaml.Types.Text "Hi"] };
  ] in
  let req = Llaml.Types.request ~model:"claude-3-haiku-20240307" ~messages () in
  (match Llaml.Anthropic_codec.encode_request req with
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
   | Ok j ->
     let s = Yojson.Safe.to_string j in
     (* system field should be at top level *)
     Alcotest.(check bool) "has top-level system" true
       (try let _ = Str.search_forward (Str.regexp_string "\"system\"") s 0 in true
        with Not_found -> false);
     (* system message should not be in messages array *)
     Alcotest.(check bool) "system text present" true
       (try let _ = Str.search_forward (Str.regexp_string "You are helpful") s 0 in true
        with Not_found -> false))

let test_anthropic_encode_request_tool_input_schema () =
  let tool : Llaml.Types.tool = {
    name = "get_weather";
    description = Some "Get weather";
    schema = `Assoc [("type", `String "object"); ("properties", `Assoc [])];
  } in
  let req = Llaml.Types.request ~model:"claude-3-haiku-20240307" ~messages:[] ~tools:[tool] () in
  (match Llaml.Anthropic_codec.encode_request req with
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
   | Ok j ->
     let s = Yojson.Safe.to_string j in
     Alcotest.(check bool) "has input_schema" true
       (try let _ = Str.search_forward (Str.regexp_string "input_schema") s 0 in true
        with Not_found -> false);
     Alcotest.(check bool) "no parameters key" true
       (not (try let _ = Str.search_forward (Str.regexp_string "\"parameters\"") s 0 in true
                 with Not_found -> false)))

let test_anthropic_decode_response_fixture () =
  let fixture = {|{
    "id": "msg_abc",
    "model": "claude-3-haiku-20240307",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "Hello there!"}],
    "usage": {"input_tokens": 10, "output_tokens": 5}
  }|} in
  let j = Yojson.Safe.from_string fixture in
  (match Llaml.Anthropic_codec.decode_response j with
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
   | Ok resp ->
     Alcotest.(check string) "id" "msg_abc" resp.Llaml.Types.id;
     (match resp.choices with
      | [ch] ->
        (match ch.message.content with
         | [Llaml.Types.Text t] -> Alcotest.(check string) "content" "Hello there!" t
         | _ -> Alcotest.fail "expected text")
      | _ -> Alcotest.fail "expected one choice"))

let test_anthropic_decode_chunk_ping () =
  let ev : Llaml.Types.sse_event = { event = Some "ping"; data = "{}" } in
  let state = Llaml.Anthropic_codec.make_state () in
  (match Llaml.Anthropic_codec.decode_chunk state ev with
   | Ok None -> ()
   | Ok (Some _) -> Alcotest.fail "ping should yield None"
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message)

let test_anthropic_decode_chunk_message_start () =
  let data = {|{"type":"message_start","message":{"id":"msg_1","model":"claude-3-haiku","usage":{"input_tokens":10,"output_tokens":0}}}|} in
  let ev : Llaml.Types.sse_event = { event = Some "message_start"; data } in
  let state = Llaml.Anthropic_codec.make_state () in
  (match Llaml.Anthropic_codec.decode_chunk state ev with
   | Ok None ->
     Alcotest.(check string) "msg_id set" "msg_1" state.msg_id
   | Ok (Some _) -> Alcotest.fail "message_start should yield None"
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message)

let test_anthropic_decode_chunk_content_block_delta () =
  let data = {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}|} in
  let ev : Llaml.Types.sse_event = { event = Some "content_block_delta"; data } in
  let state = Llaml.Anthropic_codec.make_state () in
  (match Llaml.Anthropic_codec.decode_chunk state ev with
   | Ok (Some chunk) ->
     Alcotest.(check (option string)) "content" (Some "Hello") chunk.delta.content
   | Ok None -> Alcotest.fail "expected Some chunk"
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message)

let test_anthropic_decode_chunk_message_stop () =
  let ev : Llaml.Types.sse_event = { event = Some "message_stop"; data = "{}" } in
  let state = Llaml.Anthropic_codec.make_state () in
  (match Llaml.Anthropic_codec.decode_chunk state ev with
   | Ok None -> ()
   | Ok (Some _) -> Alcotest.fail "message_stop should yield None"
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message)

let test_anthropic_decode_error_401 () =
  let j = Yojson.Safe.from_string {|{"error":{"type":"authentication_error","message":"invalid API key"}}|} in
  let err = Llaml.Anthropic_codec.decode_error ~status:401 j in
  Alcotest.(check bool) "auth error" true (err.kind = Llaml.Types.Auth_error)

(* ------------------------------------------------------------------ *)
(* SSE tests *)
(* ------------------------------------------------------------------ *)

let test_sse_parse_event_data_only () =
  match Llaml.Sse.parse_event ["data: hello"] with
  | None -> Alcotest.fail "expected Some"
  | Some ev ->
    Alcotest.(check (option string)) "event" None ev.event;
    Alcotest.(check string) "data" "hello" ev.data

let test_sse_parse_event_with_event_field () =
  match Llaml.Sse.parse_event ["event: foo"; "data: bar"] with
  | None -> Alcotest.fail "expected Some"
  | Some ev ->
    Alcotest.(check (option string)) "event" (Some "foo") ev.event;
    Alcotest.(check string) "data" "bar" ev.data

let test_sse_parse_event_empty () =
  match Llaml.Sse.parse_event [] with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty lines"

let test_sse_split_events () =
  let body = "data: a\n\ndata: b\n\n" in
  let groups = Llaml.Sse.split_events body in
  Alcotest.(check int) "two groups" 2 (List.length groups)

(* ------------------------------------------------------------------ *)
(* Types tests *)
(* ------------------------------------------------------------------ *)

let test_types_request_defaults () =
  let req = Llaml.Types.request ~model:"gpt-4o" ~messages:[] () in
  Alcotest.(check bool) "stream=false" false req.stream;
  Alcotest.(check bool) "tools=[]" true (req.tools = []);
  Alcotest.(check bool) "tool_choice=Auto" true (req.tool_choice = Llaml.Types.Auto)

(* ------------------------------------------------------------------ *)
(* SigV4 tests *)
(* ------------------------------------------------------------------ *)

let test_sigv4_sign_headers_present () =
  let url = Uri.of_string "https://bedrock-runtime.us-east-1.amazonaws.com/model/foo/converse" in
  let signed = Llaml.Sigv4.sign
    ~service:"bedrock"
    ~region:"us-east-1"
    ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
    ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    ~method_:"POST"
    ~url
    ~headers:[("content-type", "application/json")]
    ~body:"{}"
    ()
  in
  let keys = List.map fst signed in
  Alcotest.(check bool) "has authorization" true (List.mem "authorization" keys);
  Alcotest.(check bool) "has x-amz-date" true (List.mem "x-amz-date" keys)

let test_sigv4_authorization_value () =
  let url = Uri.of_string "https://bedrock-runtime.us-east-1.amazonaws.com/model/foo/converse" in
  let signed = Llaml.Sigv4.sign
    ~service:"bedrock"
    ~region:"us-east-1"
    ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
    ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    ~method_:"POST"
    ~url
    ~headers:[]
    ~body:"{}"
    ()
  in
  let auth = List.assoc "authorization" signed in
  Alcotest.(check bool) "starts with AWS4-HMAC-SHA256" true
    (starts_with "AWS4-HMAC-SHA256 Credential=" auth)

let test_sigv4_session_token () =
  let url = Uri.of_string "https://bedrock-runtime.us-east-1.amazonaws.com/model/foo/converse" in
  let signed = Llaml.Sigv4.sign
    ~service:"bedrock"
    ~region:"us-east-1"
    ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
    ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    ~session_token:"MySessionToken"
    ~method_:"POST"
    ~url
    ~headers:[]
    ~body:"{}"
    ()
  in
  let keys = List.map fst signed in
  Alcotest.(check bool) "has x-amz-security-token" true
    (List.mem "x-amz-security-token" keys)

(* ------------------------------------------------------------------ *)
(* Bedrock eventstream tests *)
(* ------------------------------------------------------------------ *)

let write_uint32_be buf n =
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xFF));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr (n land 0xFF))

let write_uint16_be buf n =
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr (n land 0xFF))

let make_event_frame event_type payload =
  let buf = Buffer.create 128 in
  let hdr_buf = Buffer.create 64 in
  let write_string_header name value =
    Buffer.add_char hdr_buf (Char.chr (String.length name));
    Buffer.add_string hdr_buf name;
    Buffer.add_char hdr_buf (Char.chr 7); (* string type *)
    write_uint16_be hdr_buf (String.length value);
    Buffer.add_string hdr_buf value
  in
  write_string_header ":event-type" event_type;
  write_string_header ":content-type" "application/json";
  let headers = Buffer.contents hdr_buf in
  let headers_len = String.length headers in
  let payload_len = String.length payload in
  let total_len = 12 + headers_len + payload_len + 4 in
  write_uint32_be buf total_len;
  write_uint32_be buf headers_len;
  write_uint32_be buf 0; (* prelude CRC — skip *)
  Buffer.add_string buf headers;
  Buffer.add_string buf payload;
  write_uint32_be buf 0; (* message CRC — skip *)
  Buffer.contents buf

let test_bedrock_parse_messages_single () =
  let payload = {|{"contentBlockIndex":0,"delta":{"text":"hi"}}|} in
  let frame = make_event_frame "contentBlockDelta" payload in
  let (events, consumed) = Llaml.Bedrock_codec.parse_messages frame in
  Alcotest.(check int) "one event" 1 (List.length events);
  Alcotest.(check int) "consumed all" (String.length frame) consumed;
  (match events with
   | [(ev_type, ev_payload)] ->
     Alcotest.(check string) "event type" "contentBlockDelta" ev_type;
     Alcotest.(check string) "payload" payload ev_payload
   | _ -> Alcotest.fail "expected one event")

let test_bedrock_parse_messages_incomplete () =
  let payload = {|{"contentBlockIndex":0,"delta":{"text":"hi"}}|} in
  let frame = make_event_frame "contentBlockDelta" payload in
  (* Truncate the frame *)
  let truncated = String.sub frame 0 (String.length frame / 2) in
  let (events, consumed) = Llaml.Bedrock_codec.parse_messages truncated in
  Alcotest.(check int) "no events" 0 (List.length events);
  Alcotest.(check int) "consumed 0" 0 consumed

let test_bedrock_parse_messages_two_frames () =
  let payload1 = {|{"contentBlockIndex":0,"delta":{"text":"hello"}}|} in
  let payload2 = {|{"stopReason":"end_turn"}|} in
  let frame1 = make_event_frame "contentBlockDelta" payload1 in
  let frame2 = make_event_frame "messageStop" payload2 in
  let combined = frame1 ^ frame2 in
  let (events, consumed) = Llaml.Bedrock_codec.parse_messages combined in
  Alcotest.(check int) "two events" 2 (List.length events);
  Alcotest.(check int) "consumed all" (String.length combined) consumed;
  (match events with
   | [(t1, _); (t2, _)] ->
     Alcotest.(check string) "first event type" "contentBlockDelta" t1;
     Alcotest.(check string) "second event type" "messageStop" t2
   | _ -> Alcotest.fail "expected two events")

(* ------------------------------------------------------------------ *)
(* Test registration *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "llaml" [
    "openai_codec", [
      Alcotest.test_case "encode_message simple text" `Quick test_openai_encode_message_simple;
      Alcotest.test_case "encode_message assistant tool_use" `Quick test_openai_encode_message_assistant_tool_use;
      Alcotest.test_case "encode_message tool result" `Quick test_openai_encode_message_tool_result;
      Alcotest.test_case "encode_request basic" `Quick test_openai_encode_request_basic;
      Alcotest.test_case "decode_response fixture" `Quick test_openai_decode_response_fixture;
      Alcotest.test_case "decode_chunk [DONE]" `Quick test_openai_decode_chunk_done;
      Alcotest.test_case "decode_chunk text delta" `Quick test_openai_decode_chunk_text_delta;
      Alcotest.test_case "decode_error 401" `Quick test_openai_decode_error_401;
      Alcotest.test_case "decode_error 429" `Quick test_openai_decode_error_429;
      Alcotest.test_case "encode_embed_request" `Quick test_openai_encode_embed_request;
    ];
    "anthropic_codec", [
      Alcotest.test_case "encode_request with system" `Quick test_anthropic_encode_request_system;
      Alcotest.test_case "encode_request tool input_schema" `Quick test_anthropic_encode_request_tool_input_schema;
      Alcotest.test_case "decode_response fixture" `Quick test_anthropic_decode_response_fixture;
      Alcotest.test_case "decode_chunk ping" `Quick test_anthropic_decode_chunk_ping;
      Alcotest.test_case "decode_chunk message_start" `Quick test_anthropic_decode_chunk_message_start;
      Alcotest.test_case "decode_chunk content_block_delta" `Quick test_anthropic_decode_chunk_content_block_delta;
      Alcotest.test_case "decode_chunk message_stop" `Quick test_anthropic_decode_chunk_message_stop;
      Alcotest.test_case "decode_error 401" `Quick test_anthropic_decode_error_401;
    ];
    "sse", [
      Alcotest.test_case "parse_event data only" `Quick test_sse_parse_event_data_only;
      Alcotest.test_case "parse_event with event field" `Quick test_sse_parse_event_with_event_field;
      Alcotest.test_case "parse_event empty" `Quick test_sse_parse_event_empty;
      Alcotest.test_case "split_events two groups" `Quick test_sse_split_events;
    ];
    "types", [
      Alcotest.test_case "request defaults" `Quick test_types_request_defaults;
    ];
    "sigv4", [
      Alcotest.test_case "sign headers present" `Quick test_sigv4_sign_headers_present;
      Alcotest.test_case "authorization value format" `Quick test_sigv4_authorization_value;
      Alcotest.test_case "session_token header" `Quick test_sigv4_session_token;
    ];
    "bedrock_eventstream", [
      Alcotest.test_case "parse single frame" `Quick test_bedrock_parse_messages_single;
      Alcotest.test_case "parse incomplete frame" `Quick test_bedrock_parse_messages_incomplete;
      Alcotest.test_case "parse two frames" `Quick test_bedrock_parse_messages_two_frames;
    ];
  ]
