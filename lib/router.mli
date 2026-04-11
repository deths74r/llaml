(** Router: multiple deployments behind model-group names,
    with load balancing, automatic retries, cooldowns, and fallback chains.

    {[
      module R = Router.Make (Http)

      let router = R.create
        Router.{ default_config with
          strategy  = Lowest_latency;
          fallbacks = [("smart", ["fast"])] }
        [ { id = "sonnet-1"; group = "smart"; model = "claude-sonnet-4-5";
            weight = 1.0; provider = (module Providers.Anthropic);
            auth = Auth.Api_key (Sys.getenv "ANTHROPIC_API_KEY") };
          { id = "opus-1";   group = "smart"; model = "claude-opus-4-6";
            weight = 0.3; provider = (module Providers.Anthropic);
            auth = Auth.Api_key (Sys.getenv "ANTHROPIC_API_KEY") };
          { id = "groq-1";   group = "fast";  model = "llama-3.3-70b-versatile";
            weight = 1.0; provider = (module Providers.Groq);
            auth = Auth.Api_key (Sys.getenv "GROQ_API_KEY") } ]
        http

      let resp = R.complete router ~group:"smart" req
      (* → tries sonnet/opus by latency; on failure falls back to groq *)
    ]}
*)

type strategy =
  | Simple_shuffle
  (** Weighted random selection across active deployments. *)
  | Lowest_latency
  (** Pick the deployment with the lowest recent average latency.
      Unknown latency (no history yet) is treated as highest priority
      so new deployments get warmed up. *)
  | Lowest_tpm_rpm
  (** Pick the deployment with the fewest tokens + requests in the
      last 60 seconds.  Useful for staying under provider rate limits. *)

type config = {
  strategy      : strategy;
  fallbacks     : (string * string list) list;
  (** [(primary_group, [fallback1; fallback2; ...])] pairs.
      On exhausting retries in [primary_group], the router tries each
      fallback group in order. *)
  allowed_fails : int;
  (** Consecutive failures before a deployment enters cooldown. Default: 3 *)
  cooldown_s    : float;
  (** Seconds a cooled-down deployment is excluded from routing. Default: 60.0 *)
  max_retries   : int;
  (** Retry attempts within a group on retriable errors before falling back.
      Default: 2 *)
}

val default_config : config

type deployment_spec = {
  id       : string;
  (** Unique identifier — used in status reports and logs. *)
  group    : string;
  (** Model-group name callers use to address this deployment. *)
  model    : string;
  (** Actual model string sent to the provider, e.g. ["claude-sonnet-4-5"]. *)
  weight   : float;
  (** Relative selection weight for [Simple_shuffle]. Default: 1.0 *)
  provider : (module Provider.S);
  auth     : Auth.t;
}

module Make (H : Client.Http) : sig

  type t

  val create : config -> deployment_spec list -> H.t -> t

  val complete :
    t ->
    group:string ->
    Types.request ->
    (Types.response, Types.error) result
  (** Chat completion with retry + fallback.
      The [model] field of [req] is overridden by the chosen deployment. *)

  val stream :
    t ->
    group:string ->
    Types.request ->
    on_chunk:(Types.chunk -> unit) ->
    (Types.usage option, Types.error) result
  (** Streaming chat completion with retry + fallback. *)

  val embed :
    t ->
    group:string ->
    Types.embed_request ->
    (Types.embed_response, Types.error) result

  (** {1 Introspection} *)

  type deployment_status = {
    id             : string;
    group          : string;
    model          : string;
    active         : bool;         (** false = currently cooled down *)
    failures       : int;
    avg_latency_ms : float option; (** None = no requests yet *)
  }

  val status : t -> deployment_status list

end
