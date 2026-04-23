(** Canonical types shared across all providers.
    Every provider transforms to/from these. *)

(** {1 Messages} *)

type role =
  | System
  | User
  | Assistant
  | Tool

(** A single part of a message's content.
    Most messages are just [Text], but tool calling and vision add the rest. *)
type content =
  | Text        of string
  | Image       of { url : string; detail : [ `Low | `High | `Auto ] option }
  | Tool_use    of {
      id                : string;
      name              : string;
      input             : Yojson.Safe.t;
      thought_signature : string option;
    }
  | Tool_result of { id : string; content : string; is_error : bool }

(** Cache policy for a message.

    Only Anthropic honors this — its codec attaches a
    [cache_control: {type: "ephemeral"}] tag to the last
    content block of the message, telling the server to
    cache everything up to and including that block. Other
    providers ignore the field entirely.

    Put [`Ephemeral] on the last stable prefix of your
    prompt (system message, big doc, shared context) and
    change the trailing messages freely — cached-input
    billing kicks in on the unchanging prefix. *)
type cache_control =
  | Ephemeral

type message = {
  role    : role;
  content : content list;
  (** Most messages have a single [Text] part, but we always use a list
      so tool-use and multimodal content compose naturally. *)
  cache   : cache_control option;
  (** Provider-specific cache hint; only Anthropic reads it. *)
}

val message :
  role:role ->
  content:content list ->
  ?cache:cache_control ->
  unit ->
  message
(** Smart constructor — [cache] defaults to [None]. *)

(** {1 Tools} *)

type tool = {
  name        : string;
  description : string option;
  schema      : Yojson.Safe.t;
  (** JSON Schema object describing the tool's parameters. *)
}

type tool_choice =
  | Auto        (** Provider picks whether to call a tool *)
  | None        (** Never call tools *)
  | Required    (** Must call at least one tool *)
  | Tool of string  (** Call this specific tool *)

(** Reasoning / thinking budget for models that expose one.

    - OpenAI o-series and GPT-5 reasoning models accept
      [reasoning_effort: "minimal"|"low"|"medium"|"high"].
    - Anthropic extended thinking takes an explicit token budget
      via [thinking: { type: "enabled", budget_tokens: N }].
    - Gemini 2.5 exposes [generationConfig.thinkingConfig.thinkingBudget]
      as an integer token budget; [-1] means "dynamic".

    The effort variants map to provider-native defaults when
    the provider wants a token budget (Low=1024, Medium=4096,
    High=16384). [Budget n] is the explicit-token form for
    providers that accept it. [Dynamic] maps to Gemini's [-1]
    and is equivalent to [Auto] everywhere else. *)
type reasoning_effort =
  | Minimal
  | Low
  | Medium
  | High
  | Budget of int
  | Dynamic

(** {1 Requests} *)

(** Response-format hints for models that support structured output.

    - [Fmt_text] — default unstructured text.
    - [Fmt_json_object] — free-form JSON (no schema).
    - [Fmt_json_schema] — typed JSON output conforming to a
      provider-native schema (passed through as opaque JSON).

    OpenAI supports all three. Gemini supports [Fmt_json_object]
    and [Fmt_json_schema] via [responseMimeType]+[responseSchema].
    Anthropic has no native structured-output mode and codec
    silently ignores the field. *)
type response_format =
  | Fmt_text
  | Fmt_json_object
  | Fmt_json_schema of Yojson.Safe.t

type request = {
  model       : string;
  messages    : message list;
  tools       : tool list;
  tool_choice : tool_choice;
  max_tokens  : int option;
  temperature : float option;
  top_p       : float option;
  top_k       : int option;
  stop        : string list;
  stream      : bool;
  user        : string option;
  reasoning   : reasoning_effort option;
  seed        : int option;
  response_format : response_format option;
  safety_settings : Yojson.Safe.t option;
  (** Gemini-specific escape hatch — passed through as the
      top-level [safetySettings] field. Other providers
      ignore it. *)
  extra       : (string * Yojson.Safe.t) list;
  (** [reasoning] is optional reasoning / thinking budget; each codec maps
      it to its provider-native shape (Gemini thinkingConfig, Anthropic
      thinking block, OpenAI reasoning_effort).

      [extra] holds provider-specific fields not covered above. Shallow-
      recursively merged into the serialized JSON body: keys absent from
      the codec's output are appended, and keys whose values are objects
      on both sides merge recursively, so
      [~extra:[("generationConfig", `Assoc [("thinkingConfig", ...)])]]
      lands alongside [temperature] etc. without duplicating
      [generationConfig] at the top level. *)
}

val request :
  model:string ->
  messages:message list ->
  ?tools:tool list ->
  ?tool_choice:tool_choice ->
  ?max_tokens:int ->
  ?temperature:float ->
  ?top_p:float ->
  ?top_k:int ->
  ?stop:string list ->
  ?stream:bool ->
  ?user:string ->
  ?reasoning:reasoning_effort ->
  ?seed:int ->
  ?response_format:response_format ->
  ?safety_settings:Yojson.Safe.t ->
  ?extra:(string * Yojson.Safe.t) list ->
  unit ->
  request
(** Smart constructor — all optional fields default to sensible values. *)

(** {1 Responses} *)

type usage = {
  prompt_tokens     : int;
  completion_tokens : int;
  total_tokens      : int;
  cache_read_tokens : int option;  (** Anthropic / OpenAI prompt caching *)
  cache_write_tokens: int option;
  thinking_tokens   : int option;
  (** Reasoning / thought tokens billed separately from [completion_tokens]:
      Gemini thinking, OpenAI reasoning effort, Anthropic extended thinking. *)
}

type finish_reason =
  | Stop
  | Length         (** Hit max_tokens *)
  | Tool_calls     (** Stopped to invoke a tool *)
  | Content_filter (** Filtered by the provider *)

type tool_call = {
  id        : string;
  name      : string;
  arguments : string;  (** Raw JSON string — providers return it unparsed *)
}

type choice = {
  index         : int;
  message       : message;
  tool_calls    : tool_call list;
  finish_reason : finish_reason option;
}

type response = {
  id      : string;
  model   : string;
  choices : choice list;
  usage   : usage option;
  created : int;
}

(** {1 Streaming} *)

type sse_event = {
  event : string option;
  data  : string;
}
(** A parsed SSE event.  [event] is the [event:] field (if any); [data] is the
    concatenated [data:] lines. *)

(** Providers emit a sequence of deltas. Each delta carries only what changed. *)
type tool_call_delta = {
  index     : int;
  id        : string option;
  name      : string option;
  arguments : string option;  (** Accumulate across chunks to reconstruct *)
  thought_signature : string option;  (** Gemini thought signature for round-trip *)
}

type delta = {
  role       : role option;
  content    : string option;
  tool_calls : tool_call_delta list;
}

type chunk = {
  id            : string;
  model         : string;
  index         : int;
  delta         : delta;
  finish_reason : finish_reason option;
  usage         : usage option;  (** Present only in the final chunk, provider-dependent *)
}

(** {1 Embeddings} *)

type embed_request = {
  model      : string;
  input      : string list;
  dimensions : int option;
}

type embedding = {
  index  : int;
  vector : float array;
}

type embed_response = {
  model  : string;
  data   : embedding list;
  usage  : usage option;
}

(** {1 Errors} *)

type rate_limit_info = {
  retry_after : float option;  (** Seconds, from Retry-After header *)
}

type error_kind =
  | Auth_error                       (** 401 / bad credentials *)
  | Rate_limit      of rate_limit_info
  | Invalid_request of string        (** 400 — message is the provider's reason *)
  | Not_found                        (** 404 / unknown model *)
  | Server_error    of int           (** 5xx *)
  | Timeout                          (** Request exceeded deadline *)
  | Network_error   of string        (** Connection-level failure *)
  | Unsupported     of string        (** Feature not available for this provider *)

type error = {
  kind     : error_kind;
  message  : string;
  provider : string;
  raw      : Yojson.Safe.t option;  (** Original error body from the provider *)
}

val pp_error : Format.formatter -> error -> unit

val retry_after_from_message : string -> float option
(** Best-effort extraction of a retry-after hint from a provider's
    human-readable error message. Handles the three phrasings seen
    in the wild ("reset after Ns", "retry in Nms", "retry after Ns")
    with units of [ms] or [s]. Returns seconds on success. Used by
    the OpenAI/Anthropic/Gemini codecs when the server doesn't send
    a [Retry-After] header but does mention the delay in the
    message body. *)
