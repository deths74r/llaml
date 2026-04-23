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
    { role = Llaml.Types.User; content = [Llaml.Types.Text "hello"]; cache = None }
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
    content = [Llaml.Types.Tool_use { id = "call_1"; name = "my_tool"; input = `Assoc [("x", `Int 1)]; thought_signature = None; metadata = None }];
    cache = None;
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
    content = [Llaml.Types.Tool_result { id = "call_1"; content = "42"; is_error = false }];
    cache = None;
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
      content = [Llaml.Types.Text "You are helpful."];
      cache = None };
    { Llaml.Types.role = Llaml.Types.User;
      content = [Llaml.Types.Text "Hi"];
      cache = None };
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
(* Json_merge — D2 *)
(* ------------------------------------------------------------------ *)

let contains needle s =
  try let _ = Str.search_forward (Str.regexp_string needle) s 0 in true
  with Not_found -> false

let test_json_merge_disjoint () =
  let base = [("a", `Int 1)] in
  let overlay = [("b", `Int 2)] in
  let merged = Llaml.Json_merge.merge base overlay in
  let s = Yojson.Safe.to_string (`Assoc merged) in
  Alcotest.(check bool) "a present" true (contains "\"a\":1" s);
  Alcotest.(check bool) "b present" true (contains "\"b\":2" s)

let test_json_merge_nested_objects () =
  let base = [
    ("generationConfig", `Assoc [("temperature", `Float 0.0)])
  ] in
  let overlay = [
    ("generationConfig",
     `Assoc [("thinkingConfig",
              `Assoc [("thinkingBudget", `Int 2048)])])
  ] in
  let merged = Llaml.Json_merge.merge base overlay in
  let j = `Assoc merged in
  let s = Yojson.Safe.to_string j in
  (* Exactly one [generationConfig] key *)
  let count =
    let rec loop i n =
      match Str.search_forward (Str.regexp_string "generationConfig") s i with
      | j -> loop (j + 1) (n + 1)
      | exception Not_found -> n
    in
    loop 0 0
  in
  Alcotest.(check int) "single generationConfig key" 1 count;
  Alcotest.(check bool) "temperature preserved" true
    (contains "\"temperature\":0.0" s);
  Alcotest.(check bool) "thinkingBudget present" true
    (contains "\"thinkingBudget\":2048" s)

let test_json_merge_overlay_wins_on_scalar () =
  let base = [("x", `Int 1)] in
  let overlay = [("x", `Int 99)] in
  let merged = Llaml.Json_merge.merge base overlay in
  let s = Yojson.Safe.to_string (`Assoc merged) in
  Alcotest.(check bool) "overlay wins" true (contains "\"x\":99" s);
  Alcotest.(check bool) "base dropped" false (contains "\"x\":1," s)

let test_gemini_encode_merges_extras_with_thinking () =
  (* Regression for the historical collision: setting temperature
     AND passing extras with generationConfig.thinkingConfig used to
     emit TWO generationConfig keys at the top level. *)
  let req =
    Llaml.Types.request
      ~model:"gemini-2.5-pro"
      ~messages:[]
      ~temperature:0.2
      ~extra:[("generationConfig",
               `Assoc [("thinkingConfig",
                        `Assoc [("thinkingBudget", `Int 1024)])])]
      ()
  in
  (match Llaml.Gemini_codec.encode_request req with
   | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
   | Ok j ->
     let s = Yojson.Safe.to_string j in
     let count =
       let rec loop i n =
         match Str.search_forward (Str.regexp_string "generationConfig") s i with
         | k -> loop (k + 1) (n + 1)
         | exception Not_found -> n
       in
       loop 0 0
     in
     Alcotest.(check int) "single generationConfig key" 1 count;
     Alcotest.(check bool) "temperature landed" true
       (contains "\"temperature\":0.2" s);
     Alcotest.(check bool) "thinkingBudget landed" true
       (contains "\"thinkingBudget\":1024" s))

(* ------------------------------------------------------------------ *)
(* Reasoning knob — D3 *)
(* ------------------------------------------------------------------ *)

let test_gemini_reasoning_budget_emits_thinking_config () =
  let req =
    Llaml.Types.request
      ~model:"gemini-2.5-pro"
      ~messages:[]
      ~reasoning:(Llaml.Types.Budget 4096)
      ()
  in
  match Llaml.Gemini_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "has thinkingConfig" true
      (contains "thinkingConfig" s);
    Alcotest.(check bool) "thinkingBudget=4096" true
      (contains "\"thinkingBudget\":4096" s)

let test_gemini_reasoning_dynamic_emits_minus_one () =
  let req =
    Llaml.Types.request
      ~model:"gemini-2.5-pro"
      ~messages:[]
      ~reasoning:Llaml.Types.Dynamic
      ()
  in
  match Llaml.Gemini_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "thinkingBudget=-1" true
      (contains "\"thinkingBudget\":-1" s)

let test_anthropic_reasoning_high_emits_thinking_block () =
  let req =
    Llaml.Types.request
      ~model:"claude-sonnet-4-5"
      ~messages:[]
      ~reasoning:Llaml.Types.High
      ()
  in
  match Llaml.Anthropic_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "has thinking block" true (contains "\"thinking\":" s);
    Alcotest.(check bool) "type enabled" true
      (contains "\"type\":\"enabled\"" s);
    Alcotest.(check bool) "budget_tokens=16384" true
      (contains "\"budget_tokens\":16384" s)

let test_openai_reasoning_medium_emits_effort_label () =
  let req =
    Llaml.Types.request
      ~model:"o3-mini"
      ~messages:[]
      ~reasoning:Llaml.Types.Medium
      ()
  in
  match Llaml.Openai_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "reasoning_effort=medium" true
      (contains "\"reasoning_effort\":\"medium\"" s)

let test_openai_reasoning_budget_rounds_to_label () =
  (* Budget 16384 should bucket into "high". *)
  let req =
    Llaml.Types.request
      ~model:"gpt-5"
      ~messages:[]
      ~reasoning:(Llaml.Types.Budget 16384)
      ()
  in
  match Llaml.Openai_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "reasoning_effort=high" true
      (contains "\"reasoning_effort\":\"high\"" s)

(* ------------------------------------------------------------------ *)
(* Cache control — B5 *)
(* ------------------------------------------------------------------ *)

let test_anthropic_cache_control_on_last_block () =
  let req =
    Llaml.Types.request
      ~model:"claude-sonnet-4-5"
      ~messages:[
        { Llaml.Types.role = User;
          content = [ Llaml.Types.Text "expensive context...";
                      Llaml.Types.Text "question" ];
          cache = Some Llaml.Types.Ephemeral };
      ]
      ()
  in
  match Llaml.Anthropic_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "has cache_control" true
      (contains "cache_control" s);
    Alcotest.(check bool) "type ephemeral" true
      (contains "\"type\":\"ephemeral\"" s)

let test_anthropic_cache_control_system_message () =
  let req =
    Llaml.Types.request
      ~model:"claude-sonnet-4-5"
      ~messages:[
        { Llaml.Types.role = System;
          content = [Llaml.Types.Text "You are a helpful assistant."];
          cache = Some Llaml.Types.Ephemeral };
        { Llaml.Types.role = User;
          content = [Llaml.Types.Text "Hi"];
          cache = None };
      ]
      ()
  in
  match Llaml.Anthropic_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "system is list form" true
      (contains "\"system\":[" s);
    Alcotest.(check bool) "system has cache_control" true
      (contains "cache_control" s)

(* ------------------------------------------------------------------ *)
(* top_k / seed / response_format — B3 *)
(* ------------------------------------------------------------------ *)

(* Regression: multiple functionCall parts in one Gemini response
   must each get a distinct [index] in the tool_call_delta list.
   Earlier versions hardcoded [index = 0] for every Tool_use,
   which caused consumers (e.g. LMI's llaml_provider accumulator)
   to merge all calls into one slot — concatenating their args
   into invalid JSON and surfacing every call with empty input.
   See llaml commit fixing this + LMI 2026-04-16 dogfood. *)
let test_gemini_decode_chunk_multiple_tool_calls () =
  let resp_json = `Assoc [
    ("candidates", `List [
      `Assoc [
        ("content", `Assoc [
          ("parts", `List [
            `Assoc [
              ("functionCall", `Assoc [
                ("name", `String "read_file");
                ("args", `Assoc [("path", `String "a.ml")]);
                ("id", `String "call_a")
              ])
            ];
            `Assoc [
              ("functionCall", `Assoc [
                ("name", `String "read_file");
                ("args", `Assoc [("path", `String "b.ml")]);
                ("id", `String "call_b")
              ])
            ];
            `Assoc [
              ("functionCall", `Assoc [
                ("name", `String "read_file");
                ("args", `Assoc [("path", `String "c.ml")]);
                ("id", `String "call_c")
              ])
            ]
          ])
        ]);
        ("finishReason", `String "STOP")
      ]
    ])
  ] in
  let ev : Llaml.Types.sse_event =
    { event = None; data = Yojson.Safe.to_string resp_json }
  in
  match Llaml.Gemini_codec.decode_chunk () ev with
  | Error e -> Alcotest.failf "decode_chunk error: %s" e.Llaml.Types.message
  | Ok None -> Alcotest.fail "expected a chunk, got None"
  | Ok (Some chunk) ->
    let deltas = chunk.Llaml.Types.delta.tool_calls in
    Alcotest.(check int) "three deltas" 3 (List.length deltas);
    let indices = List.map (fun (d : Llaml.Types.tool_call_delta) -> d.index) deltas in
    Alcotest.(check (list int)) "unique ascending indices"
      [0; 1; 2] indices;
    let ids = List.filter_map
      (fun (d : Llaml.Types.tool_call_delta) -> d.id) deltas in
    Alcotest.(check (list string)) "distinct ids preserved"
      ["call_a"; "call_b"; "call_c"] ids;
    let args = List.filter_map
      (fun (d : Llaml.Types.tool_call_delta) -> d.arguments) deltas in
    Alcotest.(check int) "three arg strings" 3 (List.length args);
    Alcotest.(check bool) "first args contain a.ml" true
      (contains "a.ml" (List.nth args 0));
    Alcotest.(check bool) "second args contain b.ml" true
      (contains "b.ml" (List.nth args 1));
    Alcotest.(check bool) "third args contain c.ml" true
      (contains "c.ml" (List.nth args 2))

let test_gemini_decode_part_reads_opaque_id () =
  (* Newer Gemini models (2.5+, 3.x-preview) emit an opaque
     [id] per functionCall. The codec should round-trip it. *)
  let resp_json = `Assoc [
    ("candidates", `List [
      `Assoc [
        ("content", `Assoc [
          ("parts", `List [
            `Assoc [
              ("functionCall", `Assoc [
                ("name", `String "some_tool");
                ("args", `Assoc [("k", `String "v")]);
                ("id", `String "xvg3g7sk")
              ])
            ]
          ])
        ]);
        ("finishReason", `String "STOP")
      ]
    ])
  ] in
  match Llaml.Gemini_codec.decode_response resp_json with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok resp ->
    (match resp.Llaml.Types.choices with
     | [] -> Alcotest.fail "expected a choice"
     | c :: _ ->
       (match c.Llaml.Types.message.content with
        | [Llaml.Types.Tool_use { id; _ }] ->
          Alcotest.(check string) "opaque id round-tripped"
            "xvg3g7sk" id
        | _ -> Alcotest.fail "expected a single Tool_use block"))

let test_gemini_decode_part_falls_back_to_name_when_id_missing () =
  (* Older Gemini models omit [id]. Fallback to name so
     tool_result round-trip matching by name still works. *)
  let resp_json = `Assoc [
    ("candidates", `List [
      `Assoc [
        ("content", `Assoc [
          ("parts", `List [
            `Assoc [
              ("functionCall", `Assoc [
                ("name", `String "some_tool");
                ("args", `Assoc [])
              ])
            ]
          ])
        ]);
        ("finishReason", `String "STOP")
      ]
    ])
  ] in
  match Llaml.Gemini_codec.decode_response resp_json with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok resp ->
    (match resp.Llaml.Types.choices with
     | [] -> Alcotest.fail "expected a choice"
     | c :: _ ->
       (match c.Llaml.Types.message.content with
        | [Llaml.Types.Tool_use { id; name; _ }] ->
          Alcotest.(check string) "id falls back to name"
            "some_tool" id;
          Alcotest.(check string) "name preserved"
            "some_tool" name
        | _ -> Alcotest.fail "expected a single Tool_use block"))

let test_gemini_tool_name_sanitization () =
  (* LMI tool names use ':' as a namespace separator. Gemini
     rejects function names that aren't [a-zA-Z0-9_-], so the
     codec should rewrite ':' to '_' on the wire. Schemas
     without a [properties] key should also get one injected. *)
  let tool : Llaml.Types.tool = {
    name = "core:memory_get";
    description = Some "Fetch a memory by id";
    schema = `Assoc [("type", `String "object")];
  } in
  let req =
    Llaml.Types.request
      ~model:"gemini-2.5-flash"
      ~messages:[]
      ~tools:[tool]
      ()
  in
  match Llaml.Gemini_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "colon replaced" true
      (contains "core_memory_get" s);
    Alcotest.(check bool) "no raw colon in name" false
      (contains "core:memory_get" s);
    Alcotest.(check bool) "properties injected" true
      (contains "\"properties\"" s)

let test_gemini_top_k_and_seed () =
  let req =
    Llaml.Types.request
      ~model:"gemini-2.5-flash"
      ~messages:[]
      ~top_k:40
      ~seed:12345
      ()
  in
  match Llaml.Gemini_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "topK=40" true (contains "\"topK\":40" s);
    Alcotest.(check bool) "seed=12345" true (contains "\"seed\":12345" s)

let test_anthropic_top_k () =
  let req =
    Llaml.Types.request
      ~model:"claude-sonnet-4-5"
      ~messages:[]
      ~top_k:50
      ()
  in
  match Llaml.Anthropic_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "top_k=50" true (contains "\"top_k\":50" s)

let test_openai_response_format_json_object () =
  let req =
    Llaml.Types.request
      ~model:"gpt-4o"
      ~messages:[]
      ~response_format:Llaml.Types.Fmt_json_object
      ()
  in
  match Llaml.Openai_codec.encode_request req with
  | Error e -> Alcotest.failf "error: %s" e.Llaml.Types.message
  | Ok j ->
    let s = Yojson.Safe.to_string j in
    Alcotest.(check bool) "response_format json_object" true
      (contains "\"type\":\"json_object\"" s)

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
    "json_merge", [
      Alcotest.test_case "disjoint keys" `Quick test_json_merge_disjoint;
      Alcotest.test_case "nested objects merge" `Quick test_json_merge_nested_objects;
      Alcotest.test_case "scalar overlay wins" `Quick test_json_merge_overlay_wins_on_scalar;
      Alcotest.test_case "gemini extras merge with thinking" `Quick test_gemini_encode_merges_extras_with_thinking;
    ];
    "reasoning", [
      Alcotest.test_case "gemini Budget -> thinkingConfig" `Quick test_gemini_reasoning_budget_emits_thinking_config;
      Alcotest.test_case "gemini Dynamic -> -1" `Quick test_gemini_reasoning_dynamic_emits_minus_one;
      Alcotest.test_case "anthropic High -> thinking block" `Quick test_anthropic_reasoning_high_emits_thinking_block;
      Alcotest.test_case "openai Medium -> effort label" `Quick test_openai_reasoning_medium_emits_effort_label;
      Alcotest.test_case "openai Budget bucketed to label" `Quick test_openai_reasoning_budget_rounds_to_label;
    ];
    "cache_control", [
      Alcotest.test_case "anthropic cache on last block" `Quick test_anthropic_cache_control_on_last_block;
      Alcotest.test_case "anthropic cache on system message" `Quick test_anthropic_cache_control_system_message;
    ];
    "extra_knobs", [
      Alcotest.test_case "gemini top_k and seed" `Quick test_gemini_top_k_and_seed;
      Alcotest.test_case "anthropic top_k" `Quick test_anthropic_top_k;
      Alcotest.test_case "openai response_format json_object" `Quick test_openai_response_format_json_object;
      Alcotest.test_case "gemini tool name sanitization" `Quick test_gemini_tool_name_sanitization;
      Alcotest.test_case "gemini decode chunk multiple tool calls have distinct indices" `Quick
        test_gemini_decode_chunk_multiple_tool_calls;
      Alcotest.test_case "gemini decode part reads opaque id" `Quick
        test_gemini_decode_part_reads_opaque_id;
      Alcotest.test_case "gemini decode part falls back to name when id missing" `Quick
        test_gemini_decode_part_falls_back_to_name_when_id_missing;
    ];
  ]
