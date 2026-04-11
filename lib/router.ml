(** Router implementation. *)

(* ------------------------------------------------------------------ *)
(* Public types                                                         *)
(* ------------------------------------------------------------------ *)

type strategy =
  | Simple_shuffle
  | Lowest_latency
  | Lowest_tpm_rpm

type config = {
  strategy      : strategy;
  fallbacks     : (string * string list) list;
  allowed_fails : int;
  cooldown_s    : float;
  max_retries   : int;
}

let default_config = {
  strategy      = Simple_shuffle;
  fallbacks     = [];
  allowed_fails = 3;
  cooldown_s    = 60.0;
  max_retries   = 2;
}

type deployment_spec = {
  id       : string;
  group    : string;
  model    : string;
  weight   : float;
  provider : (module Provider.S);
  auth     : Auth.t;
}

(* ------------------------------------------------------------------ *)
(* Internal types                                                       *)
(* ------------------------------------------------------------------ *)

(** Abstract callable interface for a deployment — erases the provider type. *)
type api = {
  complete : Types.request -> (Types.response, Types.error) result;
  stream   : Types.request -> on_chunk:(Types.chunk -> unit) ->
             (Types.usage option, Types.error) result;
  embed    : Types.embed_request -> (Types.embed_response, Types.error) result;
}

type deployment = {
  id     : string;
  group  : string;
  model  : string;
  weight : float;
  api    : api;
}

(** Per-deployment mutable state, always accessed under [mutex]. *)
type state = {
  mutex             : Mutex.t;
  mutable failures  : int;
  mutable last_fail : float option;   (* Unix timestamp of most recent failure *)
  mutable latencies : float list;     (* last 10 samples in ms, newest first *)
  mutable tpm_log   : (float * int) list; (* (timestamp, tokens) within window *)
  mutable rpm_log   : float list;         (* request timestamps within window *)
}

let make_state () = {
  mutex     = Mutex.create ();
  failures  = 0;
  last_fail = None;
  latencies = [];
  tpm_log   = [];
  rpm_log   = [];
}

let window_s = 60.0  (* sliding window for TPM/RPM *)
let max_lat_samples = 10

(* ------------------------------------------------------------------ *)
(* Functor                                                              *)
(* ------------------------------------------------------------------ *)

module Make (H : Client.Http) = struct

  type t = {
    config      : config;
    deployments : deployment list;
    states      : (string, state) Hashtbl.t;
  }

  (* Build a type-erased [api] record from a provider + auth + http. *)
  let make_api (type p) (module P : Provider.S) http auth model =
    let module C = Client.Make(P)(H) in
    let client = C.create ~auth http in
    { complete = (fun req -> C.complete client { req with Types.model });
      stream   = (fun req ~on_chunk ->
                   C.stream client { req with Types.model } ~on_chunk);
      embed    = (fun req -> C.embed client req); }

  let create config specs http =
    let deployments = List.map (fun (s : deployment_spec) ->
      { id     = s.id;
        group  = s.group;
        model  = s.model;
        weight = s.weight;
        api    = make_api s.provider http s.auth s.model }
    ) specs in
    let states = Hashtbl.create (List.length deployments) in
    List.iter (fun d -> Hashtbl.add states d.id (make_state ())) deployments;
    { config; deployments; states }

  let get_state t id = Hashtbl.find t.states id

  (* ---------------------------------------------------------------- *)
  (* Cooldown — call only while holding the state mutex               *)
  (* ---------------------------------------------------------------- *)

  let is_active_locked cfg (s : state) =
    match s.last_fail with
    | None -> true
    | Some ts ->
      s.failures < cfg.allowed_fails ||
      Unix.gettimeofday () -. ts >= cfg.cooldown_s

  let is_active t dep =
    let s = get_state t dep.id in
    Mutex.protect s.mutex (fun () -> is_active_locked t.config s)

  let get_active t group =
    List.filter (fun d -> d.group = group && is_active t d) t.deployments

  (* ---------------------------------------------------------------- *)
  (* Latency helpers                                                   *)
  (* ---------------------------------------------------------------- *)

  let avg_latency (s : state) =
    match s.latencies with
    | [] -> None
    | xs ->
      let n = float_of_int (List.length xs) in
      Some (List.fold_left ( +. ) 0.0 xs /. n)

  (* ---------------------------------------------------------------- *)
  (* TPM/RPM helpers                                                   *)
  (* ---------------------------------------------------------------- *)

  let count_tpm (s : state) now =
    List.fold_left (fun acc (ts, n) ->
      if now -. ts < window_s then acc + n else acc
    ) 0 s.tpm_log

  let count_rpm (s : state) now =
    List.length (List.filter (fun ts -> now -. ts < window_s) s.rpm_log)

  (* ---------------------------------------------------------------- *)
  (* Deployment selection                                              *)
  (* ---------------------------------------------------------------- *)

  let pick t active =
    match active with
    | []  -> None
    | [d] -> Some d
    | _   ->
      match t.config.strategy with

      | Simple_shuffle ->
        let total = List.fold_left (fun acc d -> acc +. d.weight) 0.0 active in
        let r = Random.float total in
        let rec go acc = function
          | []        -> Some (List.hd (List.rev active))
          | d :: rest ->
            let acc' = acc +. d.weight in
            if r <= acc' then Some d else go acc' rest
        in
        go 0.0 active

      | Lowest_latency ->
        (* None (no history) sorts before known latencies — warms up new deployments *)
        let scored = List.map (fun d ->
          let s = get_state t d.id in
          let lat = Mutex.protect s.mutex (fun () -> avg_latency s) in
          (lat, d)
        ) active in
        let sorted = List.sort (fun (a, _) (b, _) ->
          match a, b with
          | None,   None   -> 0
          | None,   Some _ -> -1   (* prefer unknown — get a sample *)
          | Some _, None   -> 1
          | Some x, Some y -> Float.compare x y
        ) scored in
        Some (snd (List.hd sorted))

      | Lowest_tpm_rpm ->
        let now = Unix.gettimeofday () in
        let scored = List.map (fun d ->
          let s = get_state t d.id in
          let score = Mutex.protect s.mutex (fun () ->
            float_of_int (count_tpm s now + count_rpm s now * 100)
          ) in
          (score, d)
        ) active in
        let sorted = List.sort (fun (a, _) (b, _) -> Float.compare a b) scored in
        Some (snd (List.hd sorted))

  (* ---------------------------------------------------------------- *)
  (* Record outcomes                                                   *)
  (* ---------------------------------------------------------------- *)

  let record_success t dep latency_ms tokens =
    let s = get_state t dep.id in
    Mutex.protect s.mutex (fun () ->
      s.failures  <- 0;
      let lats = latency_ms :: s.latencies in
      s.latencies <- (if List.length lats > max_lat_samples
                      then List.filteri (fun i _ -> i < max_lat_samples) lats
                      else lats);
      let now = Unix.gettimeofday () in
      s.tpm_log <- (now, tokens)
                   :: List.filter (fun (ts, _) -> now -. ts < window_s) s.tpm_log;
      s.rpm_log <- now
                   :: List.filter (fun ts -> now -. ts < window_s) s.rpm_log
    )

  let record_failure t dep =
    let s = get_state t dep.id in
    Mutex.protect s.mutex (fun () ->
      s.failures  <- s.failures + 1;
      s.last_fail <- Some (Unix.gettimeofday ())
    )

  (* ---------------------------------------------------------------- *)
  (* Error classification                                              *)
  (* ---------------------------------------------------------------- *)

  let is_retriable (e : Types.error) =
    match e.kind with
    | Types.Rate_limit _      -> true
    | Types.Server_error _    -> true
    | Types.Network_error _   -> true
    | Types.Timeout           -> true
    | Types.Auth_error        -> false  (* wrong key — retrying won't help *)
    | Types.Invalid_request _ -> false  (* bad request — same result *)
    | Types.Not_found         -> false  (* model doesn't exist *)
    | Types.Unsupported _     -> false

  let tokens_of_response (resp : Types.response) =
    match resp.Types.usage with
    | None   -> 0
    | Some u -> u.Types.total_tokens

  let no_dep_error group =
    { Types.kind    = Types.Unsupported ("no active deployments in group: " ^ group);
      message       = "All deployments are cooled down or the group is empty";
      provider      = "router";
      raw           = None }

  let exhausted_error () =
    { Types.kind    = Types.Unsupported "all fallback groups exhausted";
      message       = "Every group and fallback was tried and failed";
      provider      = "router";
      raw           = None }

  (* ---------------------------------------------------------------- *)
  (* Try deployments within a single group (with per-group retries)   *)
  (* ---------------------------------------------------------------- *)

  let try_group_complete t group req =
    let rec go attempt last_err =
      if attempt > t.config.max_retries then
        Error (Option.value last_err ~default:(no_dep_error group))
      else
        let active = get_active t group in
        match pick t active with
        | None -> Error (no_dep_error group)
        | Some dep ->
          let t0 = Unix.gettimeofday () in
          match dep.api.complete req with
          | Ok resp ->
            let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
            record_success t dep ms (tokens_of_response resp);
            Ok resp
          | Error e when is_retriable e ->
            record_failure t dep;
            go (attempt + 1) (Some e)
          | Error e ->
            record_failure t dep;
            Error e
    in
    go 0 None

  let try_group_stream t group req ~on_chunk =
    let rec go attempt last_err =
      if attempt > t.config.max_retries then
        Error (Option.value last_err ~default:(no_dep_error group))
      else
        let active = get_active t group in
        match pick t active with
        | None -> Error (no_dep_error group)
        | Some dep ->
          let t0 = Unix.gettimeofday () in
          match dep.api.stream req ~on_chunk with
          | Ok usage ->
            let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
            let tokens = match usage with
              | None   -> 0
              | Some u -> u.Types.total_tokens
            in
            record_success t dep ms tokens;
            Ok usage
          | Error e when is_retriable e ->
            record_failure t dep;
            go (attempt + 1) (Some e)
          | Error e ->
            record_failure t dep;
            Error e
    in
    go 0 None

  let try_group_embed t group req =
    let active = get_active t group in
    match pick t active with
    | None    -> Error (no_dep_error group)
    | Some dep -> dep.api.embed req

  (* ---------------------------------------------------------------- *)
  (* Fallback chain                                                    *)
  (* ---------------------------------------------------------------- *)

  let fallbacks_for t group =
    List.assoc_opt group t.config.fallbacks |> Option.value ~default:[]

  let complete t ~group req =
    let groups = group :: fallbacks_for t group in
    let rec go = function
      | []          -> Error (exhausted_error ())
      | g :: rest   ->
        match try_group_complete t g req with
        | Ok _ as ok -> ok
        | Error e when is_retriable e && rest <> [] -> go rest
        | Error e -> Error e
    in
    go groups

  let stream t ~group req ~on_chunk =
    let groups = group :: fallbacks_for t group in
    let rec go = function
      | []        -> Error (exhausted_error ())
      | g :: rest ->
        match try_group_stream t g req ~on_chunk with
        | Ok _ as ok -> ok
        | Error e when is_retriable e && rest <> [] -> go rest
        | Error e -> Error e
    in
    go groups

  let embed t ~group req =
    let groups = group :: fallbacks_for t group in
    let rec go = function
      | []        -> Error (exhausted_error ())
      | g :: rest ->
        match try_group_embed t g req with
        | Ok _ as ok -> ok
        | Error e when is_retriable e && rest <> [] -> go rest
        | Error e -> Error e
    in
    go groups

  (* ---------------------------------------------------------------- *)
  (* Introspection                                                     *)
  (* ---------------------------------------------------------------- *)

  type deployment_status = {
    id             : string;
    group          : string;
    model          : string;
    active         : bool;
    failures       : int;
    avg_latency_ms : float option;
  }

  let deployment_to_status t (dep : deployment) : deployment_status =
    let s = get_state t dep.id in
    let active         = Mutex.protect s.mutex (fun () -> is_active_locked t.config s) in
    let failures       = Mutex.protect s.mutex (fun () -> s.failures) in
    let avg_latency_ms = Mutex.protect s.mutex (fun () -> avg_latency s) in
    { id = dep.id; group = dep.group; model = dep.model;
      active; failures; avg_latency_ms }

  let status t = List.map (deployment_to_status t) t.deployments

end
