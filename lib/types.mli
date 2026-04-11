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
  | Tool_use    of { id : string; name : string; input : Yojson.Safe.t }
  | Tool_result of { id : string; content : string; is_error : bool }

type message = {
  role    : role;
  content : content list;
  (** Most messages have a single [Text] part, but we always use a list
      so tool-use and multimodal content compose naturally. *)
}

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

(** {1 Requests} *)

type request = {
  model       : string;
  messages    : message list;
  tools       : tool list;
  tool_choice : tool_choice;
  max_tokens  : int option;
  temperature : float option;
  top_p       : float option;
  stop        : string list;
  stream      : bool;
  user        : string option;
  (** Pass provider-specific fields not covered above.
      Merged into the serialized JSON body before sending. *)
  extra       : (string * Yojson.Safe.t) list;
}

val request :
  model:string ->
  messages:message list ->
  ?tools:tool list ->
  ?tool_choice:tool_choice ->
  ?max_tokens:int ->
  ?temperature:float ->
  ?top_p:float ->
  ?stop:string list ->
  ?stream:bool ->
  ?user:string ->
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
