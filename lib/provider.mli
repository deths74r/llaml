(** The [Provider] module type — the interface every provider must implement.

    A provider is a pure transformation layer: it knows how to convert
    canonical [Types.request] values into provider-specific JSON, and how
    to parse provider-specific JSON back into canonical [Types.response] values.
    It does not touch the network.

    The [Client] functor owns the HTTP machinery and calls into the provider
    at each stage of the request lifecycle. *)

module type S = sig

  val id   : string
  (** Short identifier, e.g. ["openai"], ["anthropic"], ["bedrock"].
      Used in error messages and routing. *)

  val name : string
  (** Display name, e.g. ["OpenAI"], ["Anthropic"]. *)

  (** {1 Routing} *)

  val endpoint : Types.request -> Uri.t
  (** Compute the request URL.  Called once per request, allowing per-model
      routing — e.g. Bedrock uses different ARNs per model family. *)

  (** {1 Authentication} *)

  val headers : Auth.t -> Types.request -> (string * string) list
  (** Produce HTTP headers for authentication.

      For most providers this just returns an [Authorization] header from the
      [Api_key].  For AWS SigV4 this signs the serialized body and returns
      the full set of signed headers — hence why [Types.request] is also
      passed (the canonical body must be signed). *)

  (** {1 Serialization} *)

  val encode_request : Types.request -> (Yojson.Safe.t, Types.error) result
  (** Translate a canonical request into the provider's JSON body.

      Returns [Error] if the request uses a feature the provider doesn't
      support (e.g. tool_choice=Required on a provider that only supports Auto). *)

  val decode_response : Yojson.Safe.t -> (Types.response, Types.error) result
  (** Parse a complete (non-streaming) response body. *)

  type decoder
  (** Stateful decoder for SSE streams.  For stateless providers (OpenAI), this
      is [unit].  For Anthropic, it tracks message ID, model, and block types. *)

  val make_decoder : unit -> decoder
  (** Create a fresh decoder for a new streaming request. *)

  val decode_chunk : decoder -> Types.sse_event -> (Types.chunk option, Types.error) result
  (** Parse one SSE event from the stream.

      - Returns [Ok (Some chunk)] for an event with content.
      - Returns [Ok None] when the event is a no-op or the stream is done.
      - Returns [Error _] on a malformed event.

      The [Client] accumulates SSE lines into events and calls this. *)

  val decode_error : status:int -> Yojson.Safe.t -> Types.error
  (** Map an HTTP error status code and body to a typed [Types.error].
      Called when the HTTP response status is >= 400. *)

  (** {1 Capabilities} *)

  val supports_streaming  : bool
  val supports_tools      : bool
  val supports_vision     : bool
  val supports_embeddings : bool
  val binary_streaming : bool
  (** If true, the provider uses AWS binary event-stream framing instead of SSE.
      The client will call [H.post_stream_raw] instead of [H.post_stream]. *)

  (** {1 Embeddings}

      Only called when [supports_embeddings = true]. *)

  val encode_embed_request  : Types.embed_request -> (Yojson.Safe.t, Types.error) result
  val decode_embed_response : Yojson.Safe.t -> (Types.embed_response, Types.error) result

end

(** {1 OpenAI-compatible shorthand}

    Many providers (Together, Fireworks, Groq, DeepSeek, Ollama, …) speak
    the OpenAI wire protocol verbatim — they just differ in base URL and
    which features they advertise.  This functor generates a full [S]
    implementation from a minimal config record, so adding a new
    OpenAI-compatible provider is ~10 lines. *)

module type Openai_compat_config = sig
  val id       : string
  val name     : string
  val base_url : string

  val supports_tools      : bool
  val supports_vision     : bool
  val supports_embeddings : bool
end

module Make_openai_compat (_ : Openai_compat_config) : S
