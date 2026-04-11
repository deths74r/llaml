(* bin/gemini_chat.ml — Gemini chat example using litellm-ocaml *)

(** Build an HTTPS client function using tls-eio and system CA certificates. *)
let make_https authenticator =
  fun uri raw_flow ->
    let host =
      Uri.host uri
      |> Option.value ~default:""
      |> Domain_name.of_string_exn
      |> Domain_name.host_exn
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
    | _ -> failwith "GEMINI_API_KEY environment variable not set"
  in
  let model =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "gemini-2.5-flash"
  in
  Printf.printf "Model: %s\n%!" model;
  (* Load system CA certificates for TLS verification *)
  let authenticator = match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg m) -> failwith ("Failed to load CA certificates: " ^ m)
  in
  Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      (* Define Http backend using the outer Eio context *)
      let module Http : Llaml.Client.Http = struct
        type t = {
          client : Cohttp_eio.Client.t;
          sw     : Eio.Switch.t;
        }
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
          let resp_headers = Cohttp.Response.headers resp |> Cohttp.Header.to_list in
          let body_str = Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int in
          Ok (status, body_str, resp_headers)
        let post_stream t ~url ~headers ~body ~on_line =
          let hdrs = make_headers headers in
          let bsrc = Cohttp_eio.Body.of_string body in
          let resp, rbody = Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status >= 400 then
            let err_body = Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int in
            Ok (status, err_body)
          else begin
            let buf_reader = Eio.Buf_read.of_flow rbody ~max_size:max_int in
            (try while true do
              let line = Eio.Buf_read.line buf_reader in
              on_line line
            done with End_of_file -> ());
            Ok (status, "")
          end
        let post_stream_raw t ~url ~headers ~body ~on_data =
          let hdrs = make_headers headers in
          let bsrc = Cohttp_eio.Body.of_string body in
          let resp, rbody = Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status >= 400 then
            let err_body = Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int in
            Ok (status, err_body)
          else begin
            let buf_reader = Eio.Buf_read.of_flow rbody ~max_size:max_int in
            (try
              let all_data = Eio.Buf_read.take_all buf_reader in
              if String.length all_data > 0 then on_data all_data
            with End_of_file -> ());
            Ok (status, "")
          end
      end in

      let module Client = Llaml.Client.Make (Llaml.Providers.Gemini) (Http) in
      let http = Http.create () in
      let client = Client.create ~auth:(Llaml.Auth.Api_key key) http in

      let messages = [
        { Llaml.Types.role = Llaml.Types.User;
          content = [Llaml.Types.Text "What is the capital of France? Answer in one sentence."] }
      ] in
      let req = Llaml.Types.request ~model ~messages () in

      (* Non-streaming completion *)
      Printf.printf "=== Non-streaming ===\n%!";
      (match Client.complete client req with
       | Error e ->
         Format.printf "Error: %a\n%!" Llaml.Types.pp_error e
       | Ok resp ->
         let text = match resp.Llaml.Types.choices with
           | ch :: _ ->
             (match ch.Llaml.Types.message.content with
              | Llaml.Types.Text t :: _ -> t
              | _ -> "(no text)")
           | [] -> "(no choices)"
         in
         Printf.printf "Response: %s\n" text;
         (match resp.usage with
          | Some u -> Printf.printf "Usage: %d prompt + %d completion = %d total\n"
              u.Llaml.Types.prompt_tokens u.completion_tokens u.total_tokens
          | None -> ()));

      (* Streaming completion *)
      Printf.printf "\n=== Streaming ===\n%!";
      let stream_req = Llaml.Types.request ~model ~messages
        ~stream:true () in
      let on_chunk (chunk : Llaml.Types.chunk) =
        match chunk.delta.content with
        | Some text -> print_string text; flush stdout
        | None -> ()
      in
      (match Client.stream client stream_req ~on_chunk with
       | Error e -> Format.printf "\nStream error: %a\n%!" Llaml.Types.pp_error e
       | Ok (Some u) ->
         Printf.printf "\nUsage: %d prompt + %d completion\n"
           u.Llaml.Types.prompt_tokens u.completion_tokens
       | Ok None -> print_newline ())
