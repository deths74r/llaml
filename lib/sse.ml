(** SSE (Server-Sent Events) stream parsing utilities. *)

(** Parse a list of lines from one SSE message block into an event.
    Returns [None] if no [data:] line is found. *)
let parse_event lines =
  let event_type = ref None in
  let data_parts = ref [] in
  List.iter (fun line ->
    if String.length line = 0 then ()
    else if String.length line >= 7 && String.sub line 0 7 = "event: " then
      event_type := Some (String.sub line 7 (String.length line - 7))
    else if String.length line >= 6 && String.sub line 0 6 = "event:" then
      event_type := Some (String.sub line 6 (String.length line - 6))
    else if String.length line >= 6 && String.sub line 0 6 = "data: " then
      data_parts := (String.sub line 6 (String.length line - 6)) :: !data_parts
    else if String.length line >= 5 && String.sub line 0 5 = "data:" then
      data_parts := (String.sub line 5 (String.length line - 5)) :: !data_parts
  ) lines;
  match !data_parts with
  | [] -> None
  | parts ->
    let data = String.concat "\n" (List.rev parts) in
    Some Types.{ event = !event_type; data }

(** Split a raw SSE response body into per-event line groups.
    Events are separated by blank lines. *)
let split_events body =
  let lines = String.split_on_char '\n' body in
  (* Normalize \r\n endings *)
  let lines = List.map (fun l ->
    let n = String.length l in
    if n > 0 && l.[n-1] = '\r' then String.sub l 0 (n-1) else l
  ) lines in
  let groups = ref [] in
  let current = ref [] in
  List.iter (fun line ->
    if line = "" then begin
      if !current <> [] then begin
        groups := List.rev !current :: !groups;
        current := []
      end
    end else
      current := line :: !current
  ) lines;
  if !current <> [] then
    groups := List.rev !current :: !groups;
  List.rev !groups
