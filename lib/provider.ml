(** Provider module type and OpenAI-compatible functor. *)

module type S = sig
  val id   : string
  val name : string
  val endpoint : Types.request -> Uri.t
  val headers : Auth.t -> Types.request -> (string * string) list
  val encode_request : Types.request -> (Yojson.Safe.t, Types.error) result
  val decode_response : Yojson.Safe.t -> (Types.response, Types.error) result
  type decoder
  val make_decoder : unit -> decoder
  val decode_chunk : decoder -> Types.sse_event -> (Types.chunk option, Types.error) result
  val decode_error : status:int -> Yojson.Safe.t -> Types.error
  val supports_streaming  : bool
  val supports_tools      : bool
  val supports_vision     : bool
  val supports_embeddings : bool
  val binary_streaming : bool
  val encode_embed_request  : Types.embed_request -> (Yojson.Safe.t, Types.error) result
  val decode_embed_response : Yojson.Safe.t -> (Types.embed_response, Types.error) result
end

module type Openai_compat_config = sig
  val id       : string
  val name     : string
  val base_url : string
  val supports_tools      : bool
  val supports_vision     : bool
  val supports_embeddings : bool
end

module Make_openai_compat (C : Openai_compat_config) : S = struct  [@warning "-67"]
  let id   = C.id
  let name = C.name

  type decoder = unit
  let make_decoder () = ()

  let endpoint _req =
    Uri.of_string (C.base_url ^ "/chat/completions")

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
  let supports_tools      = C.supports_tools
  let supports_vision     = C.supports_vision
  let supports_embeddings = C.supports_embeddings
  let binary_streaming    = false

  let encode_embed_request req =
    if C.supports_embeddings then
      Openai_codec.encode_embed_request req
    else
      Error { Types.kind = Types.Unsupported "embeddings";
              message = C.name ^ " does not support embeddings";
              provider = C.id; raw = None }

  let decode_embed_response = Openai_codec.decode_embed_response
end
