(** HTTP client functor — wires together a Provider and an Http backend. *)

module type Http = sig
  type t
  val post :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    (int * string * (string * string) list, string) result
  val post_stream :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    on_line:(string -> unit) -> (int * string, string) result
  val post_stream_raw :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    on_data:(string -> unit) -> (int * string, string) result
  val sleep : t -> float -> unit
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

  (** Parse a [Retry-After] header, accepting either seconds
      (the common case) or an HTTP-date (rare — we ignore that
      form and treat it as absent). *)
  let parse_retry_after (headers : (string * string) list) : float option =
    let lc k = String.lowercase_ascii k in
    List.find_map
      (fun (k, v) ->
         if lc k = "retry-after" then
           try Some (float_of_string (String.trim v))
           with _ -> None
         else None)
      headers

  (** If [err] is a [Rate_limit] with no [retry_after] hint,
      fill it in from the response headers. Leaves other error
      kinds untouched. *)
  let enrich_rate_limit (err : Types.error)
      (resp_headers : (string * string) list) : Types.error =
    match err.Types.kind with
    | Types.Rate_limit { retry_after = None } ->
      (match parse_retry_after resp_headers with
       | None -> err
       | Some hint ->
         { err with
           Types.kind = Types.Rate_limit { retry_after = Some hint } })
    | _ -> err

  (** Retry logic: exponential backoff with Retry-After header support.
      Uses [H.sleep] on the HTTP backend so eio fibers yield instead of
      blocking the domain with [Unix.sleepf]. *)
  let with_retry http max_retries f =
    let rec loop attempt =
      match f () with
      | Ok v -> Ok v
      | Error ({ Types.kind = Types.Rate_limit { retry_after }; _ } as _e)
        when attempt < max_retries ->
        let backoff = Float.of_int (1 lsl attempt) in
        let delay = match retry_after with
          | Some hint -> Float.max hint backoff
          | None      -> backoff
        in
        H.sleep http delay;
        loop (attempt + 1)
      | Error ({ Types.kind = Types.Server_error _; _ } as _e)
        when attempt < max_retries ->
        H.sleep http (Float.of_int (1 lsl attempt));
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
    with_retry t.http t.max_retries (fun () ->
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
        | Ok (status, resp_body, resp_headers) ->
          if status >= 400 then begin
            let raw = (match Yojson.Safe.from_string resp_body with
              | j -> Some j
              | exception _ -> None)
            in
            let err_json = Option.value raw ~default:(`String resp_body) in
            let err = P.decode_error ~status err_json in
            Error (enrich_rate_limit err resp_headers)
          end else begin
            match Yojson.Safe.from_string resp_body with
            | exception _ ->
              Error (make_error (Types.Invalid_request "Invalid JSON response") resp_body)
            | j ->
              (* C5: backfill empty model field with the request's
                 model — Gemini and some others don't echo it. *)
              (match P.decode_response j with
               | Error e -> Error e
               | Ok r ->
                 let r =
                   if r.Types.model = "" then
                     { r with Types.model = req.model }
                   else r
                 in
                 Ok r)
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
      with_retry t.http t.max_retries (fun () ->
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

(* The default Eio+Cohttp HTTP backend lives in the
   [llaml.eio] sub-library as [Llaml_eio.Http_eio]. Callers
   that want the canonical wiring should use [Llaml_eio.make]
   rather than instantiating [Client.Make] by hand.

   Keeping the core [llaml] library dep-light
   (yojson + uri + digestif + base64 + str + unix) means
   consumers who already ship their own HTTP backend — or
   only want the codecs for offline testing — don't pay the
   eio + cohttp-eio + tls-eio + ca-certs footprint. *)
