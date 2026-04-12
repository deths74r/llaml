(** Pre-built provider modules.

    Each is a [Provider.S] implementation ready to pass to [Client.Make].

    Typical usage goes through the {!Llaml_eio.make} convenience
    constructor rather than instantiating the [Client.Make] functor directly:

    {[
      let client =
        Llaml_eio.make ~env ~sw
          ~provider:(module Llaml.Providers.Openai)
          ~auth:(Llaml.Auth.Api_key (Sys.getenv "OPENAI_API_KEY"))
          ()
      in
      client.complete req
    ]}
*)

(** {1 Tier 1 — Distinct wire protocols} *)

module Openai    : Provider.S
module Anthropic : Provider.S
module Gemini    : Provider.S
module Bedrock   : Provider.S

(** {1 Tier 2 — OpenAI-compatible profiles} *)

module Together   : Provider.S
module Fireworks  : Provider.S
module Groq       : Provider.S
module Deepseek   : Provider.S
module Xai        : Provider.S
module Cerebras   : Provider.S
module Ollama     : Provider.S
module Openrouter : Provider.S
module Mistral    : Provider.S

(** {1 Convenience} *)

val by_prefix : string -> (module Provider.S) option
(** Look up a provider by its model name prefix.
    [by_prefix "gpt-4o"]         → [Some (module Openai)]
    [by_prefix "claude-3-5"]     → [Some (module Anthropic)]
    [by_prefix "gemini-2.0"]     → [Some (module Gemini)]
    [by_prefix "together/"]      → [Some (module Together)]
    Returns [None] for unrecognized prefixes. *)
