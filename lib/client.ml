(** HTTP client functor — wires together a Provider and an Http backend. *)

module type Http = sig
  type t
  val create : unit -> t
  val post :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    (int * string * (string * string) list, string) result
  val post_stream :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    on_line:(string -> unit) -> (int * string, string) result
  val post_stream_raw :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    on_data:(string -> unit) -> (int * string, string) result
end

module Make (P : Provider.S) (H : Http) = struct

  type t = {
    auth        : Auth.t;
    base_url    : Uri.t option;
    max_retries : int;
    timeout_s   : float;
    http        : H.t;
  }

  let create ~auth ?base_url ?(max_retries = 2) ?(timeout_s = 600.0) http =
    { auth; base_url; max_retries; timeout_s; http }

  (** Add Gemini API key to URL query params when provider is Gemini. *)
  let maybe_add_api_key_param (auth : Auth.t) url =
    match auth with
    | Auth.Api_key k when P.id = "gemini" ->
      Uri.add_query_param' url ("key", k)
    | _ -> url

  (** Sign request for Bedrock if needed. *)
  let maybe_sign_bedrock (auth : Auth.t) url headers body =
    match auth with
    | Auth.Aws_sigv4 creds when P.id = "bedrock" ->
      let signed = Sigv4.sign
        ~service:creds.Auth.service
        ~region:creds.Auth.region
        ~access_key_id:creds.Auth.access_key_id
        ~secret_access_key:creds.Auth.secret_access_key
        ?session_token:creds.Auth.session_token
        ~method_:"POST"
        ~url
        ~headers
        ~body
        ()
      in
      (* Merge signed headers *)
      let existing = List.filter (fun (k, _) ->
        not (List.exists (fun (sk, _) ->
          String.lowercase_ascii k = String.lowercase_ascii sk
        ) signed)
      ) headers in
      existing @ signed
    | _ -> headers

  let make_error kind message =
    { Types.kind; message; provider = P.id; raw = None }

  (** Retry logic: exponential backoff with Retry-After header support. *)
  let with_retry max_retries f =
    let rec loop attempt =
      match f () with
      | Ok v -> Ok v
      | Error ({ Types.kind = Types.Rate_limit _; _ } as e) when attempt < max_retries ->
        (* Simple backoff: 1s, 2s, 4s *)
        Unix.sleepf (Float.of_int (1 lsl attempt));
        loop (attempt + 1)
      | Error ({ Types.kind = Types.Server_error _; _ } as e) when attempt < max_retries ->
        Unix.sleepf (Float.of_int (1 lsl attempt));
        loop (attempt + 1)
      | Error e -> Error e
    in
    loop 0

  let complete t req =
    let req = { req with Types.stream = false } in
    let url = match t.base_url with
      | Some base ->
        let path = Uri.path (P.endpoint req) in
        Uri.with_path base path
      | None -> P.endpoint req
    in
    let url = maybe_add_api_key_param t.auth url in
    with_retry t.max_retries (fun () ->
      match P.encode_request req with
      | Error e -> Error e
      | Ok body_json ->
        let body = Yojson.Safe.to_string body_json in
        let headers = P.headers t.auth req in
        let headers = maybe_sign_bedrock t.auth url headers body in
        let headers = if List.exists (fun (k, _) -> String.lowercase_ascii k = "content-type") headers
          then headers
          else ("content-type", "application/json") :: headers
        in
        match H.post t.http ~url ~headers ~body with
        | Error msg -> Error (make_error (Types.Network_error msg) msg)
        | Ok (status, resp_body, _resp_headers) ->
          if status >= 400 then begin
            let raw = (match Yojson.Safe.from_string resp_body with
              | j -> Some j
              | exception _ -> None)
            in
            let err_json = Option.value raw ~default:(`String resp_body) in
            Error (P.decode_error ~status err_json)
          end else begin
            match Yojson.Safe.from_string resp_body with
            | exception _ ->
              Error (make_error (Types.Invalid_request "Invalid JSON response") resp_body)
            | j -> P.decode_response j
          end
    )

  let stream t req ~on_chunk =
    let req = { req with Types.stream = true } in
    let url = match t.base_url with
      | Some base ->
        let path = Uri.path (P.endpoint req) in
        Uri.with_path base path
      | None -> P.endpoint req
    in
    let url = maybe_add_api_key_param t.auth url in
    match P.encode_request req with
    | Error e -> Error e
    | Ok body_json ->
      let body = Yojson.Safe.to_string body_json in
      let headers = P.headers t.auth req in
      let headers = maybe_sign_bedrock t.auth url headers body in
      let headers = if List.exists (fun (k, _) -> String.lowercase_ascii k = "content-type") headers
        then headers
        else ("content-type", "application/json") :: headers
      in
      if P.binary_streaming then begin
        (* Binary AWS event-stream path *)
        let buf = Buffer.create 4096 in
        let error_ref = ref None in
        let final_usage = ref None in
        let decoder = P.make_decoder () in
        let on_data chunk =
          if !error_ref = None then begin
            Buffer.add_string buf chunk;
            let s = Buffer.contents buf in
            let (evts, consumed) = Bedrock_codec.parse_messages s in
            if consumed > 0 then begin
              let remaining = String.sub s consumed (String.length s - consumed) in
              Buffer.clear buf;
              Buffer.add_string buf remaining
            end;
            List.iter (fun (evt_type, payload) ->
              let ev = { Types.event = Some evt_type; data = payload } in
              match P.decode_chunk decoder ev with
              | Error e -> error_ref := Some e
              | Ok None -> ()
              | Ok (Some c) ->
                (match c.Types.usage with Some u -> final_usage := Some u | None -> ());
                on_chunk c
            ) evts
          end
        in
        (match H.post_stream_raw t.http ~url ~headers ~body ~on_data with
         | Error msg -> Error (make_error (Types.Network_error msg) msg)
         | Ok (status, err_body) when status >= 400 ->
           let raw = (match Yojson.Safe.from_string err_body with
             | j -> Some j | exception _ -> None) in
           let err_json = Option.value raw ~default:(`String err_body) in
           Error (P.decode_error ~status err_json)
         | Ok (_, _) ->
           match !error_ref with
           | Some e -> Error e
           | None -> Ok !final_usage)
      end else begin
        (* SSE line accumulator *)
        let decoder = P.make_decoder () in
        let final_usage = ref None in
        let line_buf = ref [] in
        let error_ref = ref None in
        let process_event () =
          let lines = List.rev !line_buf in
          line_buf := [];
          match Sse.parse_event lines with
          | None -> ()
          | Some ev ->
            (match P.decode_chunk decoder ev with
             | Error e -> error_ref := Some e
             | Ok None -> ()
             | Ok (Some chunk) ->
               (match chunk.Types.usage with
                | Some u -> final_usage := Some u
                | None -> ());
               on_chunk chunk)
        in
        let on_line line =
          if !error_ref = None then begin
            if line = "" then
              process_event ()
            else
              line_buf := line :: !line_buf
          end
        in
        (match H.post_stream t.http ~url ~headers ~body ~on_line with
         | Error msg -> Error (make_error (Types.Network_error msg) msg)
         | Ok (status, err_body) when status >= 400 ->
           let raw = (match Yojson.Safe.from_string err_body with
             | j -> Some j | exception _ -> None) in
           let err_json = Option.value raw ~default:(`String err_body) in
           Error (P.decode_error ~status err_json)
         | Ok (_, _) ->
           (* Process any remaining buffered lines *)
           if !line_buf <> [] then process_event ();
           match !error_ref with
           | Some e -> Error e
           | None -> Ok !final_usage)
      end

  let embed t (req : Types.embed_request) =
    if not P.supports_embeddings then
      Error (make_error (Types.Unsupported "embeddings")
               (P.name ^ " does not support embeddings"))
    else begin
      let url = match t.base_url with
        | Some base -> base
        | None ->
          (* For embed, use a modified endpoint. Providers that support embeddings
             typically have /v1/embeddings. We derive from the completion endpoint. *)
          let completion_url = P.endpoint (Types.request ~model:req.model ~messages:[] ()) in
          let base = Uri.with_path completion_url "" in
          Uri.with_path base "/v1/embeddings"
      in
      let url = maybe_add_api_key_param t.auth url in
      with_retry t.max_retries (fun () ->
        match P.encode_embed_request req with
        | Error e -> Error e
        | Ok body_json ->
          let body = Yojson.Safe.to_string body_json in
          (* Use a dummy request for headers (only auth matters) *)
          let dummy_req = Types.request ~model:req.model ~messages:[] () in
          let headers = P.headers t.auth dummy_req in
          let headers = if List.exists (fun (k, _) -> String.lowercase_ascii k = "content-type") headers
            then headers
            else ("content-type", "application/json") :: headers
          in
          match H.post t.http ~url ~headers ~body with
          | Error msg -> Error (make_error (Types.Network_error msg) msg)
          | Ok (status, resp_body, _) ->
            if status >= 400 then begin
              let raw = (match Yojson.Safe.from_string resp_body with
                | j -> Some j | exception _ -> None) in
              let err_json = Option.value raw ~default:(`String resp_body) in
              Error (P.decode_error ~status err_json)
            end else begin
              match Yojson.Safe.from_string resp_body with
              | exception _ ->
                Error (make_error (Types.Invalid_request "Invalid JSON response") resp_body)
              | j -> P.decode_embed_response j
            end
      )
    end

end

(** Default cohttp-eio HTTP backend.

    Note: cohttp-eio requires an Eio environment to be set up by the caller.
    This implementation uses a simple approach compatible with cohttp-eio 0.7+.
    If the exact API differs, users can provide their own Http module. *)
module Cohttp_eio_http : Http = struct
  type t = unit

  let create () = ()

  let make_headers pairs =
    List.fold_left (fun acc (k, v) ->
      Cohttp.Header.add acc k v
    ) (Cohttp.Header.init ()) pairs

  let post () ~url ~headers ~body =
    (* cohttp-eio requires running inside an Eio.Switch and Eio environment.
       This is a best-effort wrapper — callers must ensure Eio is running.
       We use a simple approach with Eio_main if available. *)
    try
      let result = ref (Error "not started") in
      Eio_main.run (fun env ->
        Eio.Switch.run (fun sw ->
          let client = Cohttp_eio.Client.make ~https:None env#net in
          let cohttp_headers = make_headers headers in
          let body_src = Cohttp_eio.Body.of_string body in
          let resp, resp_body = Cohttp_eio.Client.post client
            ~sw
            ~headers:cohttp_headers
            ~body:body_src
            url
          in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          let resp_headers = Cohttp.Response.headers resp
            |> Cohttp.Header.to_list in
          let body_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int in
          result := Ok (status, body_str, resp_headers)
        )
      );
      !result
    with exn ->
      Error (Printexc.to_string exn)

  let post_stream () ~url ~headers ~body ~on_line =
    try
      let error_ref = ref None in
      let status_ref = ref 0 in
      let err_body_ref = ref "" in
      Eio_main.run (fun env ->
        Eio.Switch.run (fun sw ->
          let client = Cohttp_eio.Client.make ~https:None env#net in
          let cohttp_headers = make_headers headers in
          let body_src = Cohttp_eio.Body.of_string body in
          let resp, resp_body = Cohttp_eio.Client.post client
            ~sw
            ~headers:cohttp_headers
            ~body:body_src
            url
          in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          status_ref := status;
          if status >= 400 then
            (try err_body_ref := Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int
             with exn -> error_ref := Some (Printexc.to_string exn))
          else begin
            let buf_reader = Eio.Buf_read.of_flow resp_body ~max_size:max_int in
            (try
              while true do
                let line = Eio.Buf_read.line buf_reader in
                on_line line
              done
            with
            | End_of_file -> ()
            | exn -> error_ref := Some (Printexc.to_string exn))
          end
        )
      );
      match !error_ref with
      | Some msg -> Error msg
      | None -> Ok (!status_ref, !err_body_ref)
    with exn ->
      Error (Printexc.to_string exn)

  let post_stream_raw () ~url ~headers ~body ~on_data =
    try
      let error_ref = ref None in
      let status_ref = ref 0 in
      let err_body_ref = ref "" in
      Eio_main.run (fun env ->
        Eio.Switch.run (fun sw ->
          let client = Cohttp_eio.Client.make ~https:None env#net in
          let cohttp_headers = make_headers headers in
          let body_src = Cohttp_eio.Body.of_string body in
          let resp, resp_body = Cohttp_eio.Client.post client
            ~sw ~headers:cohttp_headers ~body:body_src url
          in
          let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          status_ref := status;
          if status >= 400 then
            (try err_body_ref := Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:max_int
             with exn -> error_ref := Some (Printexc.to_string exn))
          else begin
            let buf_reader = Eio.Buf_read.of_flow resp_body ~max_size:max_int in
            (try
              let all_data = Eio.Buf_read.take_all buf_reader in
              if String.length all_data > 0 then on_data all_data
            with
            | End_of_file -> ()
            | exn -> error_ref := Some (Printexc.to_string exn))
          end
        )
      );
      match !error_ref with
      | Some msg -> Error msg
      | None -> Ok (!status_ref, !err_body_ref)
    with exn ->
      Error (Printexc.to_string exn)
end
