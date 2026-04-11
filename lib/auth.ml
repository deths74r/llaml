(** Authentication strategies. *)

type aws_creds = {
  access_key_id     : string;
  secret_access_key : string;
  session_token     : string option;
  region            : string;
  service           : string;
}

type gcp_creds = {
  token    : string;
  project  : string;
  location : string;
}

type t =
  | Api_key   of string
  | Aws_sigv4 of aws_creds
  | Gcp_token of gcp_creds
  | Custom    of (string * string) list

let from_env var =
  match Sys.getenv_opt var with
  | None | Some "" -> Error (Printf.sprintf "Environment variable %s is unset or empty" var)
  | Some key -> Ok (Api_key key)
