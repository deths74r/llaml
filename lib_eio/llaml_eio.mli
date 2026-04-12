(** Plug-and-play entry point for Llaml on top of Eio.

    Hides the {!Llaml.Client.Make} functor behind a
    record-of-closures. Callers say "I want a client for this
    provider" and get back a uniform interface without
    touching HTTP backends, switches, or CA authenticators.

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let client =
        Llaml_eio.make ~env ~sw
          ~provider:(module Llaml.Providers.Anthropic)
          ~auth:(Llaml.Auth.Api_key (Sys.getenv "ANTHROPIC_API_KEY"))
          ()
      in
      match
        client.complete
          (Llaml.Types.request
             ~model:"claude-sonnet-4-5"
             ~messages:[
               { role = User; content = [Text "hello"]; cache = None }
             ] ())
      with
      | Ok r  -> ...
      | Error e -> Format.eprintf "%a@." Llaml.Types.pp_error e
    ]}
*)

module Http_eio = Http_eio

(** {1 Boxed client}

    A record of closures over a provider + backend + auth.
    The functor is instantiated and the [Http_eio.t] captured
    inside each closure, so callers never see either. *)

type client = {
  provider_id : string;
  (** Short provider id — "openai", "anthropic", "gemini", … *)

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
}

val make :
  env:< net : _ Eio.Net.t ;
        clock : _ Eio.Time.clock ;
        .. > ->
  sw:Eio.Switch.t ->
  ?authenticator:X509.Authenticator.t ->
  ?base_url:Uri.t ->
  ?max_retries:int ->
  ?timeout_s:float ->
  provider:(module Llaml.Provider.S) ->
  auth:Llaml.Auth.t ->
  unit -> client
(** [make ~env ~sw ~provider ~auth ()] builds a boxed client
    for [provider] over the eio HTTP backend. Internally:

    - constructs an [Http_eio.t] bound to [env] and [sw]
    - instantiates [Llaml.Client.Make(P)(Http_eio)]
    - closes over the resulting client in each record field

    [base_url], [max_retries], and [timeout_s] pass through
    to [Client.Make.create]. *)

(** {1 Auto — model-string to client}

    One-liner that picks a provider from a model-name prefix
    and reads the matching env var. *)

val auto :
  env:< net : _ Eio.Net.t ;
        clock : _ Eio.Time.clock ;
        .. > ->
  sw:Eio.Switch.t ->
  model:string ->
  ?authenticator:X509.Authenticator.t ->
  unit -> (client, string) result
(** [auto ~env ~sw ~model ()] picks a provider via
    {!Llaml.Providers.by_prefix}, reads the conventional env
    var for that provider
    ([OPENAI_API_KEY], [ANTHROPIC_API_KEY], [GEMINI_API_KEY],
    [GROQ_API_KEY], …), and returns a boxed client. Returns
    [Error msg] if no provider matches the model prefix or
    the env var is unset. *)
