(** HTTP client — instantiated from a Provider + Auth.

    This is where the network lives.  [Client.Make] takes a provider module
    and returns a usable client that handles:
    - Request serialization (via the provider)
    - HTTP execution (via an injectable [Http] backend — testable)
    - SSE stream parsing (via the provider's [decode_chunk])
    - Error mapping
    - Retry on rate-limit (exponential backoff, respecting Retry-After) *)

(** {1 HTTP backend abstraction}

    Inject your HTTP implementation here. The canonical eio+cohttp backend
    lives in the [llaml.eio] sub-library as [Llaml_eio.Http_eio]; tests can
    inject a mock that returns canned responses. *)

module type Http = sig
  type t

  val post :
    t ->
    url:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (int * string * (string * string) list, string) result
  (** Blocking POST.  Returns [(status, body, response_headers)]. *)

  val post_stream :
    t ->
    url:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    on_line:(string -> unit) ->
    (int * string, string) result
  (** Streaming POST.  On 2xx the backend calls [on_line] for each SSE line
      as it arrives and returns [Ok (status, "")].  On a >=400 status the
      backend must NOT invoke [on_line]; it buffers the full response body
      and returns [Ok (status, body)] so the caller can route it through
      [decode_error].  [Error msg] is reserved for connection-level failures. *)

  val post_stream_raw :
    t -> url:Uri.t -> headers:(string * string) list -> body:string ->
    on_data:(string -> unit) ->
    (int * string, string) result
  (** Like [post_stream] but delivers raw byte chunks instead of lines.
      Same status-handling contract: [on_data] is only called on 2xx;
      on >=400 the buffered error body is returned in [Ok (status, body)]. *)

  val sleep : t -> float -> unit
  (** Fiber-friendly sleep. Called by [Client.Make]'s retry logic on
      [Rate_limit] / [Server_error]. The eio backend routes this to
      [Eio.Time.sleep] so it yields to other fibers; a naive backend
      may use [Unix.sleepf]. *)
end

(** {1 Client functor} *)

module Make (P : Provider.S) (H : Http) : sig

  type t

  val create :
    auth:Auth.t ->
    ?base_url:Uri.t ->
    ?max_retries:int ->
    ?timeout_s:float ->
    H.t ->
    t
  (** [create ~auth http] constructs a client for provider [P].

      - [base_url] overrides [P.endpoint] — useful for pointing at a local
        proxy or the litellm proxy itself.
      - [max_retries] defaults to 2; retries on [Rate_limit] and [Server_error].
      - [timeout_s] defaults to 600.0 (10 min — long for streaming). *)

  val complete :
    t ->
    Types.request ->
    (Types.response, Types.error) result
  (** Blocking chat completion.  Sets [request.stream = false] internally. *)

  val stream :
    t ->
    Types.request ->
    on_chunk:(Types.chunk -> unit) ->
    (Types.usage option, Types.error) result
  (** Streaming chat completion.  Calls [on_chunk] for each [Types.chunk]
      as it arrives from the provider's SSE stream.

      Returns the final [usage] if the provider included it in the stream
      (OpenAI sends it in the last chunk; Anthropic in a [message_delta] event). *)

  val embed :
    t ->
    Types.embed_request ->
    (Types.embed_response, Types.error) result
  (** Text embeddings.  Returns [Error (Unsupported _)] if
      [P.supports_embeddings = false]. *)

end
