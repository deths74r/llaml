(** Eio + Cohttp-eio HTTP backend for Llaml.

    Extracted from the original inline module in
    [bin/gemini_chat.ml] and promoted to a first-class
    submodule so downstream consumers (LMI, other agent
    harnesses) don't have to hand-roll TLS + CA + cohttp
    plumbing. *)

type t = {
  client : Cohttp_eio.Client.t;
  sw     : Eio.Switch.t;
  sleep_fn : float -> unit;
}

let make_https authenticator =
  fun uri raw_flow ->
    let host =
      Uri.host uri
      |> Option.value ~default:""
      |> Domain_name.of_string_exn
      |> Domain_name.host_exn
    in
    let tls_config =
      match Tls.Config.client ~authenticator () with
      | Ok cfg -> cfg
      | Error (`Msg m) -> failwith ("TLS config: " ^ m)
    in
    Tls_eio.client_of_flow tls_config ~host raw_flow

let default_authenticator () =
  match Ca_certs.authenticator () with
  | Ok a -> a
  | Error (`Msg m) -> failwith ("Ca_certs: " ^ m)

let make ~env ~sw ?authenticator () =
  let authenticator =
    match authenticator with
    | Some a -> a
    | None -> default_authenticator ()
  in
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (make_https authenticator))
      env#net
  in
  let clock = env#clock in
  let sleep_fn sec = Eio.Time.sleep clock sec in
  { client; sw; sleep_fn }

let sleep t sec = t.sleep_fn sec

(* --- Llaml.Client.Http implementation ----------------------------- *)

let make_headers pairs =
  List.fold_left
    (fun acc (k, v) -> Cohttp.Header.add acc k v)
    (Cohttp.Header.init ())
    pairs

let post t ~url ~headers ~body =
  let hdrs = make_headers headers in
  let bsrc = Cohttp_eio.Body.of_string body in
  match
    Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url
  with
  | exception exn -> Error (Printexc.to_string exn)
  | (resp, rbody) ->
    let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let rh = Cohttp.Response.headers resp |> Cohttp.Header.to_list in
    (try
       let s = Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int in
       Ok (status, s, rh)
     with exn -> Error (Printexc.to_string exn))

let post_stream t ~url ~headers ~body ~on_line =
  let hdrs = make_headers headers in
  let bsrc = Cohttp_eio.Body.of_string body in
  match
    Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url
  with
  | exception exn -> Error (Printexc.to_string exn)
  | (resp, rbody) ->
    let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    if status >= 400 then
      (try
         let err =
           Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int
         in
         Ok (status, err)
       with exn -> Error (Printexc.to_string exn))
    else begin
      let r = Eio.Buf_read.of_flow rbody ~max_size:max_int in
      (try
         while true do
           on_line (Eio.Buf_read.line r)
         done
       with End_of_file -> ());
      Ok (status, "")
    end

let post_stream_raw t ~url ~headers ~body ~on_data =
  let hdrs = make_headers headers in
  let bsrc = Cohttp_eio.Body.of_string body in
  match
    Cohttp_eio.Client.post t.client ~sw:t.sw ~headers:hdrs ~body:bsrc url
  with
  | exception exn -> Error (Printexc.to_string exn)
  | (resp, rbody) ->
    let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    if status >= 400 then
      (try
         let err =
           Eio.Buf_read.(parse_exn take_all) rbody ~max_size:max_int
         in
         Ok (status, err)
       with exn -> Error (Printexc.to_string exn))
    else begin
      let r = Eio.Buf_read.of_flow rbody ~max_size:max_int in
      (try
         let all = Eio.Buf_read.take_all r in
         if String.length all > 0 then on_data all
       with End_of_file -> ());
      Ok (status, "")
    end
