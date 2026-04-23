(** Canonical types shared across all providers. *)

(** {1 Messages} *)

type role =
  | System
  | User
  | Assistant
  | Tool

type content =
  | Text        of string
  | Image       of { url : string; detail : [ `Low | `High | `Auto ] option }
  | Tool_use    of { id : string; name : string; input : Yojson.Safe.t;
                     thought_signature : string option }
  | Tool_result of { id : string; content : string; is_error : bool }

type cache_control =
  | Ephemeral

type message = {
  role    : role;
  content : content list;
  cache   : cache_control option;
}

let message ~role ~content ?cache () =
  { role; content; cache }

(** {1 Tools} *)

type tool = {
  name        : string;
  description : string option;
  schema      : Yojson.Safe.t;
}

type tool_choice =
  | Auto
  | None
  | Required
  | Tool of string

type reasoning_effort =
  | Minimal
  | Low
  | Medium
  | High
  | Budget of int
  | Dynamic

(** {1 Requests} *)

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
  extra       : (string * Yojson.Safe.t) list;
}

let request ~model ~messages
    ?(tools = [])
    ?(tool_choice = Auto)
    ?max_tokens
    ?temperature
    ?top_p
    ?top_k
    ?(stop = [])
    ?(stream = false)
    ?user
    ?reasoning
    ?seed
    ?response_format
    ?safety_settings
    ?(extra = [])
    () =
  { model; messages; tools; tool_choice; max_tokens; temperature;
    top_p; top_k; stop; stream; user; reasoning; seed;
    response_format; safety_settings; extra }

(** {1 Responses} *)

type usage = {
  prompt_tokens     : int;
  completion_tokens : int;
  total_tokens      : int;
  cache_read_tokens : int option;
  cache_write_tokens: int option;
  thinking_tokens   : int option;
}

type finish_reason =
  | Stop
  | Length
  | Tool_calls
  | Content_filter

type tool_call = {
  id        : string;
  name      : string;
  arguments : string;
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

type tool_call_delta = {
  index     : int;
  id        : string option;
  name      : string option;
  arguments : string option;
  thought_signature : string option;
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
  usage         : usage option;
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
  retry_after : float option;
}

type error_kind =
  | Auth_error
  | Rate_limit      of rate_limit_info
  | Invalid_request of string
  | Not_found
  | Server_error    of int
  | Timeout
  | Network_error   of string
  | Unsupported     of string

type error = {
  kind     : error_kind;
  message  : string;
  provider : string;
  raw      : Yojson.Safe.t option;
}

let pp_error fmt e =
  let kind_str = match e.kind with
    | Auth_error -> "AuthError"
    | Rate_limit { retry_after } ->
      (match retry_after with
       | Some s -> Printf.sprintf "RateLimit(retry_after=%.1fs)" s
       | None -> "RateLimit")
    | Invalid_request msg -> Printf.sprintf "InvalidRequest(%s)" msg
    | Not_found -> "NotFound"
    | Server_error code -> Printf.sprintf "ServerError(%d)" code
    | Timeout -> "Timeout"
    | Network_error msg -> Printf.sprintf "NetworkError(%s)" msg
    | Unsupported feat -> Printf.sprintf "Unsupported(%s)" feat
  in
  Format.fprintf fmt "[%s] %s: %s" e.provider kind_str e.message

let telemetry_type = function
  | Auth_error        -> "auth"
  | Rate_limit _      -> "rate_limit"
  | Invalid_request _ -> "invalid_request"
  | Not_found         -> "not_found"
  | Server_error _    -> "server"
  | Timeout           -> "timeout"
  | Network_error _   -> "network"
  | Unsupported _     -> "unsupported"

let retry_after_from_message (msg : string) : float option =
  let parse_duration n unit_ =
    match float_of_string_opt n, unit_ with
    | Some v, "ms" -> Some (v /. 1000.0)
    | Some v, "s"  -> Some v
    | _            -> None
  in
  let search pattern =
    let re = Str.regexp_case_fold pattern in
    try
      ignore (Str.search_forward re msg 0);
      parse_duration (Str.matched_group 1 msg) (Str.matched_group 2 msg)
    with Not_found -> None
  in
  match search "reset after \\([0-9.]+\\)\\(ms\\|s\\)" with
  | Some _ as v -> v
  | None ->
    match search "retry in \\([0-9.]+\\)\\(ms\\|s\\)" with
    | Some _ as v -> v
    | None -> search "retry after \\([0-9.]+\\)\\(ms\\|s\\)"
