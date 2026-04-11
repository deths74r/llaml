(** AWS SigV4 signing implementation. *)

let sha256_hex s =
  let digest = Digestif.SHA256.digest_string s in
  Digestif.SHA256.to_hex digest

let hmac_sha256 key data =
  Digestif.SHA256.hmac_string ~key data

let hmac_sha256_hex key data =
  let mac = hmac_sha256 key data in
  Digestif.SHA256.to_hex mac

(** Format Unix time as YYYYMMDDTHHMMSSZ *)
let format_datetime t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let format_date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d%02d%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday

(** URI-encode a string component (percent-encoding). *)
let uri_encode ?(slash = false) s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9'
    | '-' | '_' | '.' | '~' -> Buffer.add_char buf c
    | '/' when slash -> Buffer.add_char buf '/'
    | c ->
      Buffer.add_char buf '%';
      Buffer.add_string buf (Printf.sprintf "%02X" (Char.code c))
  ) s;
  Buffer.contents buf

(** Canonicalize headers: lowercase names, trim values, sort. *)
let canonical_headers headers =
  let normalized = List.map (fun (k, v) ->
    (String.lowercase_ascii k, String.trim v)
  ) headers in
  let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) normalized in
  let lines = List.map (fun (k, v) -> k ^ ":" ^ v ^ "\n") sorted in
  String.concat "" lines

let signed_headers_str headers =
  let names = List.map (fun (k, _) -> String.lowercase_ascii k) headers in
  let sorted = List.sort String.compare names in
  String.concat ";" sorted

(** Canonicalize the query string from a Uri. *)
let canonical_query_string uri =
  let query = Uri.query uri in
  let pairs = List.concat_map (fun (k, vs) ->
    List.map (fun v -> (uri_encode k, uri_encode v)) vs
  ) query in
  let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) pairs in
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) sorted)

(** The canonical URI path (normalized, encoded). *)
let canonical_uri uri =
  let path = Uri.path uri in
  let path = if path = "" then "/" else path in
  uri_encode ~slash:true path

let sign ~service ~region ~access_key_id ~secret_access_key
    ?session_token ~method_ ~url ~headers ~body () =
  let now = Unix.time () in
  let datetime = format_datetime now in
  let date = format_date now in
  let host = Uri.host url |> Option.value ~default:"" in
  (* Add required headers *)
  let base_headers = headers
    |> List.filter (fun (k, _) -> String.lowercase_ascii k <> "host")
    |> List.filter (fun (k, _) -> String.lowercase_ascii k <> "x-amz-date") in
  let sign_headers = base_headers
    @ [("host", host); ("x-amz-date", datetime)]
    @ (match session_token with
       | Some t -> [("x-amz-security-token", t)]
       | None -> []) in
  (* Build canonical request *)
  let body_hash = sha256_hex body in
  let canon_uri = canonical_uri url in
  let canon_query = canonical_query_string url in
  let canon_hdrs = canonical_headers sign_headers in
  let signed_hdrs = signed_headers_str sign_headers in
  let canonical_request = String.concat "\n" [
    method_;
    canon_uri;
    canon_query;
    canon_hdrs;
    signed_hdrs;
    body_hash;
  ] in
  (* Credential scope *)
  let scope = String.concat "/" [date; region; service; "aws4_request"] in
  (* String to sign *)
  let string_to_sign = String.concat "\n" [
    "AWS4-HMAC-SHA256";
    datetime;
    scope;
    sha256_hex canonical_request;
  ] in
  (* Signing key *)
  let k_secret = "AWS4" ^ secret_access_key in
  let k_date = hmac_sha256 k_secret date in
  let k_region = hmac_sha256 (Digestif.SHA256.to_raw_string k_date) region in
  let k_service = hmac_sha256 (Digestif.SHA256.to_raw_string k_region) service in
  let k_signing = hmac_sha256 (Digestif.SHA256.to_raw_string k_service) "aws4_request" in
  let signature = hmac_sha256_hex (Digestif.SHA256.to_raw_string k_signing) string_to_sign in
  (* Authorization header *)
  let authorization = Printf.sprintf
    "AWS4-HMAC-SHA256 Credential=%s/%s, SignedHeaders=%s, Signature=%s"
    access_key_id scope signed_hdrs signature in
  (* Return headers to add/replace *)
  let result = [
    ("x-amz-date", datetime);
    ("authorization", authorization);
  ] in
  let result = match session_token with
    | Some t -> ("x-amz-security-token", t) :: result
    | None -> result
  in
  result
