(* bin/gemini_modes.ml — Exercise Gemini across sampling, thinking, JSON, tools, multi-turn. *)

let make_https authenticator =
  fun uri raw_flow ->
    let host =
      Uri.host uri |> Option.value ~default:""
      |> Domain_name.of_string_exn |> Domain_name.host_exn
    in
    let tls_config = match Tls.Config.client ~authenticator () with
      | Ok cfg -> cfg
      | Error (`Msg m) -> failwith ("TLS config error: " ^ m)
    in
    Tls_eio.client_of_flow tls_config ~host raw_flow

let () =
  Mirage_crypto_rng_unix.use_default ();
  let key = match Sys.getenv_opt "GEMINI_API_KEY" with
    | Some k when k <> "" -> k
    | _ -> failwith "GEMINI_API_KEY not set"
  in
  let authenticator = match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg m) -> failwith ("CA load: " ^ m)
  in
  Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      let module Http : Llaml.Client.Http = struct
        type t = { client : Cohttp_eio.Client.t; sw : Eio.Switch.t }
        let create () = {
          client = Cohttp_eio.Client.make ~https:(Some (make_https authenticator)) env#net;
          sw;
        }
        let make_headers pairs =
          List.fold_left (fun acc (k, v) -> Cohttp.Header.add acc k v)
            (Cohttp.Header.init ()) pairs
        let post t ~url ~headers ~body =
          let hdrs = make_headers headers in
          let bsrc = Cohttp_eio.Body.of_string body in
          let resp, rbody = Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          let rh = Cohttp.Response.headers resp |> Cohttp.Header.to_list in
          let s = Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int in
          Ok (status, s, rh)
        let post_stream t ~url ~headers ~body ~on_line =
          let hdrs = make_headers headers in
          let bsrc = Cohttp_eio.Body.of_string body in
          let resp, rbody = Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status >= 400 then
            Ok (status, Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int)
          else begin
            let r = Eio.Buf_read.of_flow rbody ~max_size:max_int in
            (try while true do on_line (Eio.Buf_read.line r) done
             with End_of_file -> ());
            Ok (status, "")
          end
        let post_stream_raw t ~url ~headers ~body ~on_data =
          let hdrs = make_headers headers in
          let bsrc = Cohttp_eio.Body.of_string body in
          let resp, rbody = Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status >= 400 then
            Ok (status, Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int)
          else begin
            let r = Eio.Buf_read.of_flow rbody ~max_size:max_int in
            (try
              let all = Eio.Buf_read.take_all r in
              if String.length all > 0 then on_data all
            with End_of_file -> ());
            Ok (status, "")
          end
      end in
      let module Client = Llaml.Client.Make (Llaml.Providers.Gemini) (Http) in
      let http = Http.create () in
      let client = Client.create ~auth:(Llaml.Auth.Api_key key) http in

      let text_of_resp (resp : Llaml.Types.response) =
        match resp.choices with
        | ch :: _ ->
          String.concat "" (List.filter_map
            (function Llaml.Types.Text t -> Some t | _ -> None) ch.message.content)
        | [] -> "(empty)"
      in
      let show_usage (resp : Llaml.Types.response) =
        match resp.usage with
        | Some u ->
          let thinking = match u.thinking_tokens with
            | Some n -> Printf.sprintf " (thinking=%d)" n
            | None   -> ""
          in
          Printf.sprintf "%d+%d=%d%s"
            u.prompt_tokens u.completion_tokens u.total_tokens thinking
        | None -> "?"
      in
      let show_finish (resp : Llaml.Types.response) =
        match resp.choices with
        | ch :: _ ->
          (match ch.finish_reason with
           | Some Stop -> "stop"
           | Some Length -> "length"
           | Some Tool_calls -> "tool_calls"
           | Some Content_filter -> "filter"
           | None -> "-")
        | [] -> "-"
      in
      let run label req =
        Printf.printf "\n--- %s ---\n%!" label;
        match Client.complete client req with
        | Error e -> Format.printf "ERROR: %a\n%!" Llaml.Types.pp_error e
        | Ok resp ->
          let text = text_of_resp resp in
          let preview =
            if String.length text > 200 then String.sub text 0 200 ^ "..."
            else text
          in
          Printf.printf "text:   %s\n" preview;
          Printf.printf "usage:  %s\n" (show_usage resp);
          Printf.printf "finish: %s\n" (show_finish resp);
          (match resp.choices with
           | ch :: _ when ch.tool_calls <> [] ->
             List.iter (fun (tc : Llaml.Types.tool_call) ->
               Printf.printf "tool_call: %s(%s)\n" tc.name tc.arguments
             ) ch.tool_calls
           | _ -> ())
      in

      let u t : Llaml.Types.message = { role = User; content = [Text t] } in
      let a t : Llaml.Types.message = { role = Assistant; content = [Text t] } in
      let flash = "gemini-2.5-flash" in
      let pro   = "gemini-2.5-pro" in
      let g3    = "gemini-3-pro-preview" in

      (* 1. Low temperature *)
      run "temp=0.0"
        (Llaml.Types.request ~model:flash
          ~messages:[u "Count to three. Respond in exactly one short line."]
          ~temperature:0.0 ());

      (* 2. High temperature *)
      run "temp=1.5"
        (Llaml.Types.request ~model:flash
          ~messages:[u "Invent a creative one-word name for a cat. Respond with just the name."]
          ~temperature:1.5 ());

      (* 3. max_tokens small — expect Length *)
      run "max_tokens=5"
        (Llaml.Types.request ~model:flash
          ~messages:[u "Tell me a long story about a dragon."]
          ~max_tokens:5 ());

      (* 4. Stop sequence *)
      run "stop sequence"
        (Llaml.Types.request ~model:flash
          ~messages:[u "List 5 fruits, one per line: apple, banana, cherry, date, elderberry."]
          ~stop:["cherry"] ());

      (* 5. JSON mode via extra *)
      run "JSON mode (responseMimeType)"
        (Llaml.Types.request ~model:flash
          ~messages:[u "Return a JSON object mapping France and Germany to their capital cities."]
          ~extra:[("generationConfig", `Assoc [
            ("responseMimeType", `String "application/json")
          ])] ());

      (* 6. Structured output via responseSchema *)
      run "JSON mode + responseSchema"
        (Llaml.Types.request ~model:flash
          ~messages:[u "Who wrote Hamlet? Reply via the schema."]
          ~extra:[("generationConfig", `Assoc [
            ("responseMimeType", `String "application/json");
            ("responseSchema", `Assoc [
              ("type", `String "object");
              ("properties", `Assoc [
                ("author", `Assoc [("type", `String "string")]);
                ("year",   `Assoc [("type", `String "integer")])
              ]);
              ("required", `List [`String "author"; `String "year"])
            ])
          ])] ());

      (* 7. thinkingBudget = 0 (off) on 2.5-flash *)
      run "2.5-flash thinking off (budget=0)"
        (Llaml.Types.request ~model:flash
          ~messages:[u "What is 17 * 23? Just the number."]
          ~extra:[("generationConfig", `Assoc [
            ("thinkingConfig", `Assoc [("thinkingBudget", `Int 0)])
          ])] ());

      (* 8. thinkingBudget = -1 (dynamic) on 2.5-pro *)
      run "2.5-pro thinking dynamic (budget=-1)"
        (Llaml.Types.request ~model:pro
          ~messages:[u "What is 17 * 23? Just the number."]
          ~extra:[("generationConfig", `Assoc [
            ("thinkingConfig", `Assoc [("thinkingBudget", `Int (-1))])
          ])] ());

      (* 9. thinkingBudget = 2048 (fixed) on 2.5-pro *)
      run "2.5-pro thinking fixed (budget=2048)"
        (Llaml.Types.request ~model:pro
          ~messages:[u "What is 17 * 23? Just the number."]
          ~extra:[("generationConfig", `Assoc [
            ("thinkingConfig", `Assoc [("thinkingBudget", `Int 2048)])
          ])] ());

      (* 10. gemini-3 thinkingLevel=low *)
      run "gemini-3 thinkingLevel=low"
        (Llaml.Types.request ~model:g3
          ~messages:[u "What is 17 * 23? Just the number."]
          ~extra:[("generationConfig", `Assoc [
            ("thinkingConfig", `Assoc [("thinkingLevel", `String "low")])
          ])] ());

      (* 11. gemini-3 thinkingLevel=high *)
      run "gemini-3 thinkingLevel=high"
        (Llaml.Types.request ~model:g3
          ~messages:[u "Prove there are infinitely many primes in one short paragraph."]
          ~extra:[("generationConfig", `Assoc [
            ("thinkingConfig", `Assoc [("thinkingLevel", `String "high")])
          ])] ());

      (* 12. Tool calling *)
      let get_weather : Llaml.Types.tool = {
        name = "get_weather";
        description = Some "Get the current weather for a city";
        schema = `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("city", `Assoc [
              ("type", `String "string");
              ("description", `String "City name")
            ])
          ]);
          ("required", `List [`String "city"])
        ]
      } in
      run "tool calling"
        (Llaml.Types.request ~model:flash
          ~messages:[u "What's the weather in Paris? Use the get_weather tool."]
          ~tools:[get_weather] ());

      (* 13. Multi-turn memory *)
      run "multi-turn"
        (Llaml.Types.request ~model:flash
          ~messages:[
            u "My name is Edward.";
            a "Nice to meet you, Edward!";
            u "What is my name?"
          ] ())
