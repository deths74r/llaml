(** Authentication strategies.

    Each provider knows which [Auth.t] variant it expects.
    The [Client] functor passes [auth] to [Provider.headers] at request time,
    allowing providers that need per-request signing (e.g. AWS SigV4) to
    generate fresh credentials each call. *)

type aws_creds = {
  access_key_id     : string;
  secret_access_key : string;
  session_token     : string option;
  region            : string;
  service           : string;  (** e.g. "bedrock" *)
}

type gcp_creds = {
  token    : string;
  project  : string;
  location : string;  (** e.g. "us-central1" *)
}

type t =
  | Api_key   of string
  (** Sent as [Authorization: Bearer <key>].
      Used by: OpenAI, Anthropic, Gemini, most openai-compatible providers. *)

  | Aws_sigv4 of aws_creds
  (** AWS Signature V4 — signs the full request.
      Used by: Bedrock, SageMaker. *)

  | Gcp_token of gcp_creds
  (** OAuth2 Bearer — used by: Vertex AI. *)

  | Custom    of (string * string) list
  (** Arbitrary HTTP headers — escape hatch for anything else. *)

val from_env : string -> (t, string) result
(** [from_env var] reads an API key from the named environment variable.
    Returns [Error msg] if the variable is unset or empty. *)
