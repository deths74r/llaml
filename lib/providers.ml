(** Concrete provider implementations. *)

(** {1 OpenAI} *)

module Openai : Provider.S = struct
  let id   = "openai"
  let name = "OpenAI"

  type decoder = unit
  let make_decoder () = ()

  let endpoint _req =
    Uri.of_string "https://api.openai.com/v1/chat/completions"

  let headers (auth : Auth.t) _req =
    match auth with
    | Auth.Api_key k -> [("Authorization", "Bearer " ^ k)]
    | Auth.Custom hs -> hs
    | _ -> []

  let encode_request = Openai_codec.encode_request
  let decode_response = Openai_codec.decode_response
  let decode_chunk () ev = Openai_codec.decode_chunk () ev
  let decode_error = Openai_codec.decode_error

  let supports_streaming  = true
  let binary_streaming    = false
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = true

  let encode_embed_request req =
    Openai_codec.encode_embed_request req

  let decode_embed_response = Openai_codec.decode_embed_response
end

(** {1 Anthropic} *)

module Anthropic : Provider.S = struct
  let id   = "anthropic"
  let name = "Anthropic"

  type decoder = Anthropic_codec.state
  let make_decoder = Anthropic_codec.make_state

  let endpoint _req =
    Uri.of_string "https://api.anthropic.com/v1/messages"

  let headers (auth : Auth.t) _req =
    let base = [
      ("anthropic-version", "2023-06-01");
      ("content-type", "application/json");
    ] in
    match auth with
    | Auth.Api_key k -> ("x-api-key", k) :: base
    | Auth.Custom hs -> hs @ base
    | _ -> base

  let encode_request = Anthropic_codec.encode_request
  let decode_response = Anthropic_codec.decode_response
  let decode_chunk state ev = Anthropic_codec.decode_chunk state ev
  let decode_error = Anthropic_codec.decode_error

  let supports_streaming  = true
  let binary_streaming    = false
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = false

  let encode_embed_request _req =
    Error { Types.kind = Types.Unsupported "embeddings";
            message = "Anthropic does not support embeddings";
            provider = "anthropic"; raw = None }

  let decode_embed_response _j =
    Error { Types.kind = Types.Unsupported "embeddings";
            message = "Anthropic does not support embeddings";
            provider = "anthropic"; raw = None }
end

(** {1 Gemini} *)

module Gemini : Provider.S = struct
  let id   = "gemini"
  let name = "Gemini"

  type decoder = unit
  let make_decoder () = ()

  let endpoint (req : Types.request) =
    Gemini_codec.endpoint_for req.model ~streaming:req.stream

  let headers (auth : Auth.t) _req =
    let base = [("content-type", "application/json")] in
    match auth with
    | Auth.Api_key k -> ("x-goog-api-key", k) :: base
    | Auth.Custom hs -> hs @ base
    | _ -> base
  (** Gemini API keys ride in the [x-goog-api-key] header. Historically
      this provider put the key in the URL query string (?key=...), but
      URLs leak into access logs and exception traces whereas headers
      are redacted by most HTTP clients. *)

  let encode_request = Gemini_codec.encode_request
  let decode_response = Gemini_codec.decode_response
  let decode_chunk () ev = Gemini_codec.decode_chunk () ev
  let decode_error = Gemini_codec.decode_error

  let supports_streaming  = true
  let binary_streaming    = false
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = true

  let encode_embed_request = Gemini_codec.encode_embed_request
  let decode_embed_response = Gemini_codec.decode_embed_response
end

(** {1 Bedrock} *)

module Bedrock : Provider.S = struct
  let id   = "bedrock"
  let name = "AWS Bedrock"

  type decoder = Bedrock_codec.state
  let make_decoder = Bedrock_codec.make_state

  let endpoint req =
    Bedrock_codec.endpoint req ~streaming:req.Types.stream

  let headers (auth : Auth.t) req =
    match auth with
    | Auth.Aws_sigv4 creds ->
      (* We need the body to sign, but we don't have it here.
         The actual signing is done in Client with the encoded body.
         Return minimal headers; Client will call sign with the body. *)
      let _ = req in
      let base = [("content-type", "application/json")] in
      (* Store creds hint for client — client checks provider id and does signing *)
      let _ = creds in
      base
    | Auth.Custom hs -> hs
    | _ -> [("content-type", "application/json")]

  let encode_request = Bedrock_codec.encode_request
  let decode_response = Bedrock_codec.decode_response
  let decode_chunk state ev = Bedrock_codec.decode_chunk state ev
  let decode_error = Bedrock_codec.decode_error

  let supports_streaming  = true
  let binary_streaming    = true
  let supports_tools      = true
  let supports_vision     = false
  let supports_embeddings = false

  let encode_embed_request _req =
    Error { Types.kind = Types.Unsupported "embeddings";
            message = "Bedrock embeddings not yet implemented";
            provider = "bedrock"; raw = None }

  let decode_embed_response _j =
    Error { Types.kind = Types.Unsupported "embeddings";
            message = "Bedrock embeddings not yet implemented";
            provider = "bedrock"; raw = None }
end

(** {1 Tier 2 — OpenAI-compatible} *)

module Together = Provider.Make_openai_compat (struct
  let id   = "together"
  let name = "Together AI"
  let base_url = "https://api.together.xyz/v1"
  let supports_tools      = true
  let supports_vision     = false
  let supports_embeddings = true
end)

module Fireworks = Provider.Make_openai_compat (struct
  let id   = "fireworks"
  let name = "Fireworks AI"
  let base_url = "https://api.fireworks.ai/inference/v1"
  let supports_tools      = true
  let supports_vision     = false
  let supports_embeddings = false
end)

module Groq = Provider.Make_openai_compat (struct
  let id   = "groq"
  let name = "Groq"
  let base_url = "https://api.groq.com/openai/v1"
  let supports_tools      = true
  let supports_vision     = false
  let supports_embeddings = false
end)

module Deepseek = Provider.Make_openai_compat (struct
  let id   = "deepseek"
  let name = "DeepSeek"
  let base_url = "https://api.deepseek.com/v1"
  let supports_tools      = true
  let supports_vision     = false
  let supports_embeddings = false
end)

module Xai = Provider.Make_openai_compat (struct
  let id   = "xai"
  let name = "xAI (Grok)"
  let base_url = "https://api.x.ai/v1"
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = false
end)

module Cerebras = Provider.Make_openai_compat (struct
  let id   = "cerebras"
  let name = "Cerebras"
  let base_url = "https://api.cerebras.ai/v1"
  let supports_tools      = true
  let supports_vision     = false
  let supports_embeddings = false
end)

module Ollama = Provider.Make_openai_compat (struct
  let id   = "ollama"
  let name = "Ollama"
  let base_url = "http://localhost:11434/v1"
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = true
end)

module Openrouter = Provider.Make_openai_compat (struct
  let id   = "openrouter"
  let name = "OpenRouter"
  let base_url = "https://openrouter.ai/api/v1"
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = false
end)

module Mistral = Provider.Make_openai_compat (struct
  let id   = "mistral"
  let name = "Mistral AI"
  let base_url = "https://api.mistral.ai/v1"
  let supports_tools      = true
  let supports_vision     = true
  let supports_embeddings = true
end)

(** {1 Model aliases}

    Short, memorable names that expand to canonical model ids before
    any prefix-based routing happens. Aliases are additive: users
    who already type canonical names ([gpt-5], [claude-opus-4-6])
    continue to work as-is. *)
let aliases : (string * string) list = [
  (* Anthropic *)
  "opus",      "claude-opus-4-6";
  "sonnet",    "claude-sonnet-4-6";
  "haiku",     "claude-haiku-4-5";
  (* Gemini *)
  "flash",     "gemini-2.5-flash";
  "pro",       "gemini-2.5-pro";
  (* OpenAI *)
  "gpt5",      "gpt-5";
  "gpt5-mini", "gpt-5-mini";
  "gpt5-nano", "gpt-5-nano";
  "gpt4o",     "gpt-4o";
  "gpt4o-mini","gpt-4o-mini";
]

(** Resolve a possibly-aliased model name to its canonical form.
    Unknown names pass through unchanged. *)
let resolve_alias (model : string) : string =
  match List.assoc_opt model aliases with
  | Some canonical -> canonical
  | None -> model

(** {1 Prefix lookup table} *)

let by_prefix raw_model =
  let model = resolve_alias raw_model in
  let has_prefix p = String.length model >= String.length p &&
    String.sub model 0 (String.length p) = p in
  (* Bedrock prefixes — check before others *)
  if has_prefix "us." || has_prefix "eu." || has_prefix "ap."
  || has_prefix "amazon." || has_prefix "anthropic."
  || has_prefix "meta." || has_prefix "mistral.amazonaws"
  then Some (module Bedrock : Provider.S)
  (* Tier 1 *)
  else if has_prefix "gpt-" || has_prefix "o1" || has_prefix "o3" || has_prefix "o4"
  then Some (module Openai : Provider.S)
  else if has_prefix "claude-"
  then Some (module Anthropic : Provider.S)
  else if has_prefix "gemini-"
  then Some (module Gemini : Provider.S)
  (* Tier 2 *)
  else if has_prefix "together/"
  then Some (module Together : Provider.S)
  else if has_prefix "fireworks/" || has_prefix "accounts/fireworks"
  then Some (module Fireworks : Provider.S)
  else if has_prefix "groq/"
  then Some (module Groq : Provider.S)
  else if has_prefix "deepseek-" || has_prefix "deepseek/"
  then Some (module Deepseek : Provider.S)
  else if has_prefix "grok-" || has_prefix "xai/"
  then Some (module Xai : Provider.S)
  else if has_prefix "cerebras/"
  then Some (module Cerebras : Provider.S)
  else if has_prefix "ollama/"
  then Some (module Ollama : Provider.S)
  else if has_prefix "openrouter/"
  then Some (module Openrouter : Provider.S)
  else if has_prefix "mistral-" || has_prefix "mistral/"
  then Some (module Mistral : Provider.S)
  else None

(** {1 Curated model catalog}

    Hand-maintained list of known models per provider. The
    ordering inside each provider is roughly newest-first so a
    downstream [/model] listing puts the fresh models near the
    top. This is NOT an exhaustive enumeration — new models
    appear faster than we can update — and it is NOT validated
    against the provider's [/models] endpoint. It exists so a
    REPL tab-completion UI has something sensible to autocomplete
    against without a live API call at startup. *)

let catalog : (string * string) list = [
  (* OpenAI *)
  "gpt-5",                         "openai";
  "gpt-5-mini",                    "openai";
  "gpt-5-nano",                    "openai";
  "gpt-4.1",                       "openai";
  "gpt-4o",                        "openai";
  "gpt-4o-mini",                   "openai";
  "o3",                            "openai";
  "o3-mini",                       "openai";
  "o4-mini",                       "openai";

  (* Anthropic *)
  "claude-opus-4-6",               "anthropic";
  "claude-opus-4-5",               "anthropic";
  "claude-sonnet-4-6",             "anthropic";
  "claude-sonnet-4-5",             "anthropic";
  "claude-haiku-4-5",              "anthropic";
  "claude-3-5-sonnet-latest",      "anthropic";
  "claude-3-5-haiku-latest",       "anthropic";

  (* Gemini *)
  "gemini-3-pro-preview",          "gemini";
  "gemini-2.5-pro",                "gemini";
  "gemini-2.5-flash",              "gemini";
  "gemini-2.5-flash-lite",         "gemini";

  (* Groq *)
  "groq/llama-3.3-70b-versatile",  "groq";
  "groq/llama-3.1-8b-instant",     "groq";
  "groq/mixtral-8x7b-32768",       "groq";

  (* xAI *)
  "grok-4",                        "xai";
  "grok-3",                        "xai";

  (* DeepSeek *)
  "deepseek-chat",                 "deepseek";
  "deepseek-reasoner",             "deepseek";

  (* Mistral *)
  "mistral-large-latest",          "mistral";
  "mistral-small-latest",          "mistral";

  (* Cerebras *)
  "cerebras/llama-3.3-70b",        "cerebras";

  (* Together *)
  "together/meta-llama/Llama-3.3-70B-Instruct-Turbo", "together";

  (* Fireworks *)
  "fireworks/llama-v3p3-70b-instruct", "fireworks";

  (* OpenRouter *)
  "openrouter/anthropic/claude-sonnet-4-5", "openrouter";
  "openrouter/openai/gpt-5",                 "openrouter";

  (* Ollama (local) *)
  "ollama/llama3.3",               "ollama";
  "ollama/qwen2.5-coder",          "ollama";
]

(** {1 Live model listing}

    Maps provider IDs to their model-listing endpoint URL.
    Returns [None] for providers that don't expose one
    (Anthropic, Bedrock). *)

let models_endpoint = function
  | "gemini"     -> Some "https://generativelanguage.googleapis.com/v1beta/models"
  | "openai"     -> Some "https://api.openai.com/v1/models"
  | "groq"       -> Some "https://api.groq.com/openai/v1/models"
  | "mistral"    -> Some "https://api.mistral.ai/v1/models"
  | "together"   -> Some "https://api.together.xyz/v1/models"
  | "fireworks"  -> Some "https://api.fireworks.ai/inference/v1/models"
  | "deepseek"   -> Some "https://api.deepseek.com/v1/models"
  | "xai"        -> Some "https://api.x.ai/v1/models"
  | "cerebras"   -> Some "https://api.cerebras.ai/v1/models"
  | "openrouter" -> Some "https://openrouter.ai/api/v1/models"
  | "ollama"     -> Some "http://localhost:11434/v1/models"
  | _            -> None

type model_info = {
  id : string;
  description : string;
  supports_reasoning : bool;
}

(** Parse model entries from a provider's /models JSON response.
    Gemini uses a different schema from the OpenAI-compatible
    providers, so we dispatch on [provider_id]. *)
let parse_models_response ~provider_id body =
  try
    let j = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    match provider_id with
    | "gemini" ->
      let models = j |> member "models" |> to_list in
      let entries = List.filter_map (fun m ->
          let name = m |> member "name" |> to_string in
          let desc = m |> member "displayName" |> to_string_option
                     |> Option.value ~default:"" in
          let methods = try m |> member "supportedGenerationMethods" |> to_list
                            |> List.map to_string
            with _ -> [] in
          if List.mem "generateContent" methods then
            let prefix = "models/" in
            let plen = String.length prefix in
            let id =
              if String.length name > plen && String.sub name 0 plen = prefix then
                String.sub name plen (String.length name - plen)
              else name
            in
            let supports_reasoning =
              (try m |> member "thinking" |> to_bool with _ -> false)
            in
            Some { id; description = desc; supports_reasoning }
          else None) models
      in
      Ok entries
    | _ ->
      let data = j |> member "data" |> to_list in
      let is_reasoning_model id =
        let has p =
          let plen = String.length p in
          String.length id >= plen && String.sub id 0 plen = p
        in
        has "o1" || has "o3" || has "o4"
        || has "deepseek-reasoner"
      in
      let entries = List.map (fun m ->
          let id = m |> member "id" |> to_string in
          let desc = (match m |> member "name" with
            | `String s -> s
            | _ -> match m |> member "owned_by" with
              | `String s -> s
              | _ -> "") in
          { id; description = desc;
            supports_reasoning = is_reasoning_model id }) data
      in
      Ok entries
  with exn ->
    Error (Printf.sprintf "failed to parse models response: %s" (Printexc.to_string exn))
