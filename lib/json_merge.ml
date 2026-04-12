(** Shallow-recursive JSON Assoc merge.

    Each provider codec uses [merge] to fold [req.extra] into
    the serialized body. Without this, extras were appended at
    the top level via plain list concat — which meant a user
    passing [~extra:[("generationConfig", ...)]] while also
    setting [temperature] ended up with two [generationConfig]
    keys in the output JSON, one of which the provider's parser
    silently dropped.

    Semantics:
    - If [key] is not in [base], append it.
    - If [key] IS in [base] and both values are [`Assoc _], recurse.
    - Otherwise, overlay wins (user's [extra] replaces codec's value). *)

let rec merge (base : (string * Yojson.Safe.t) list)
    (overlay : (string * Yojson.Safe.t) list)
  : (string * Yojson.Safe.t) list =
  List.fold_left
    (fun acc (k, v) ->
       match List.assoc_opt k acc with
       | None ->
         acc @ [(k, v)]
       | Some existing ->
         let merged = merge_json existing v in
         List.map
           (fun (k', v') -> if k' = k then (k, merged) else (k', v'))
           acc)
    base
    overlay

and merge_json (a : Yojson.Safe.t) (b : Yojson.Safe.t) : Yojson.Safe.t =
  match a, b with
  | `Assoc la, `Assoc lb -> `Assoc (merge la lb)
  | _, _ -> b
