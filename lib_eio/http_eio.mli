(** Eio + Cohttp-eio HTTP backend for Llaml.

    Bind one [t] per Eio context — the [env] and [sw] must
    outlive the client — then pass it to
    [Llaml.Client.Make(P)(Http_eio).create], or use
    [Llaml_eio.make] below to skip the functor entirely.

    On construction the backend loads the system CA bundle
    via [Ca_certs] and builds a TLS client config. Callers
    that want to trust a specific root (self-signed test
    servers, corporate MITM) can pass [?authenticator]. *)

type t

val make :
  env:< net : _ Eio.Net.t ;
        clock : _ Eio.Time.clock ;
        .. > ->
  sw:Eio.Switch.t ->
  ?authenticator:X509.Authenticator.t ->
  unit -> t
(** [make ~env ~sw ()] wires up a cohttp-eio client bound to
    [env]'s network cap and [sw]. If [authenticator] is
    omitted, the system CA bundle is loaded automatically.

    Raises [Failure] if CA loading or TLS config construction
    fails — caller must have [Mirage_crypto_rng_unix.use_default ()]
    in effect at call time. *)

include Llaml.Client.Http with type t := t
