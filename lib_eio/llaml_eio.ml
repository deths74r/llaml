module Http_eio = Http_eio

type client = {
  provider_id : string;
  complete :
    Llaml.Types.request ->
    (Llaml.Types.response, Llaml.Types.error) result;
  stream :
    Llaml.Types.request ->
    on_chunk:(Llaml.Types.chunk -> unit) ->
    (Llaml.Types.usage option, Llaml.Types.error) result;
  embed :
    Llaml.Types.embed_request ->
    (Llaml.Types.embed_response, Llaml.Types.error) result;
  list_models : unit -> (string list, string) result;
}

let make ~env ~sw ?authenticator
    ?base_url ?(max_retries = 2) ?(timeout_s = 600.0)
    ~provider ~auth () =
  let http = Http_eio.make ~env ~sw ?authenticator () in
  let (module P : Llaml.Provider.S) = provider in
  let module C = Llaml.Client.Make (P) (Http_eio) in
  let c =
    C.create ~auth ?base_url ~max_retries ~timeout_s http
  in
  let do_list_models () =
    match Llaml.Providers.models_endpoint P.id with
    | None -> Error (Printf.sprintf "provider %s has no model listing endpoint" P.id)
    | Some url_str ->
      let url = match auth with
        | Llaml.Auth.Api_key k when P.id = "gemini" ->
          Uri.add_query_param' (Uri.of_string url_str) ("key", k)
        | _ -> Uri.of_string url_str
      in
      let headers = match auth with
        | Llaml.Auth.Api_key k when P.id <> "gemini" ->
          [("Authorization", "Bearer " ^ k)]
        | _ -> []
      in
      match Http_eio.get http ~url ~headers with
      | Error msg -> Error msg
      | Ok (status, body, _) ->
        if status >= 400 then
          Error (Printf.sprintf "HTTP %d: %s" status
                   (String.sub body 0 (min 200 (String.length body))))
        else
          Llaml.Providers.parse_models_response ~provider_id:P.id body
  in
  {
    provider_id = P.id;
    complete = (fun req -> C.complete c req);
    stream   = (fun req ~on_chunk -> C.stream c req ~on_chunk);
    embed    = (fun req -> C.embed c req);
    list_models = do_list_models;
  }

(* Model-prefix to provider mapping — used by [auto]. *)
let provider_for_model (model : string) : (module Llaml.Provider.S) option =
  Llaml.Providers.by_prefix model

(* Map a provider id to its conventional API-key env var.
   [`Api_key_var v] means read [v] from the environment;
   [`No_auth] means the provider takes no credentials
   (e.g. Ollama at localhost); [`Unsupported] means the
   provider needs something more complex than a single
   string (AWS SigV4, GCP OAuth2 token) and [auto] cannot
   resolve it — caller must use [make] directly. *)
let auth_plan_for_provider_id = function
  | "openai"     -> `Api_key_var "OPENAI_API_KEY"
  | "anthropic"  -> `Api_key_var "ANTHROPIC_API_KEY"
  | "gemini"     -> `Api_key_var "GEMINI_API_KEY"
  | "groq"       -> `Api_key_var "GROQ_API_KEY"
  | "together"   -> `Api_key_var "TOGETHER_API_KEY"
  | "fireworks"  -> `Api_key_var "FIREWORKS_API_KEY"
  | "deepseek"   -> `Api_key_var "DEEPSEEK_API_KEY"
  | "xai"        -> `Api_key_var "XAI_API_KEY"
  | "cerebras"   -> `Api_key_var "CEREBRAS_API_KEY"
  | "openrouter" -> `Api_key_var "OPENROUTER_API_KEY"
  | "mistral"    -> `Api_key_var "MISTRAL_API_KEY"
  | "ollama"     -> `No_auth
  | "bedrock"    -> `Unsupported "bedrock requires AWS SigV4; use Llaml_eio.make directly"
  | id           -> `Unsupported (Printf.sprintf "unknown provider %S" id)

let auto ~env ~sw ~model ?authenticator () =
  match provider_for_model model with
  | None ->
    Error (Printf.sprintf "llaml: no provider matches model %S" model)
  | Some ((module P : Llaml.Provider.S) as provider) ->
    let auth_result =
      match auth_plan_for_provider_id P.id with
      | `No_auth           -> Ok (Llaml.Auth.Api_key "")
      | `Api_key_var var   -> Llaml.Auth.from_env var
      | `Unsupported msg   -> Error msg
    in
    match auth_result with
    | Error msg -> Error msg
    | Ok auth ->
      Ok (make ~env ~sw ?authenticator ~provider ~auth ())
