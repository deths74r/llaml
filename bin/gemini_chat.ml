(* bin/gemini_chat.ml — Gemini chat example via Llaml_eio.make *)

let () =
  Mirage_crypto_rng_unix.use_default ();
  let key =
    match Sys.getenv_opt "GEMINI_API_KEY" with
    | Some k when k <> "" -> k
    | _ -> failwith "GEMINI_API_KEY environment variable not set"
  in
  let model =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "gemini-2.5-flash"
  in
  Printf.printf "Model: %s\n%!" model;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    Llaml_eio.make
      ~env ~sw
      ~provider:(module Llaml.Providers.Gemini)
      ~auth:(Llaml.Auth.Api_key key)
      ()
  in
  let messages =
    [ { Llaml.Types.role = Llaml.Types.User;
        content =
          [ Llaml.Types.Text
              "What is the capital of France? Answer in one sentence." ];
        cache = None } ]
  in
  let req = Llaml.Types.request ~model ~messages () in

  Printf.printf "=== Non-streaming ===\n%!";
  (match client.complete req with
   | Error e -> Format.printf "Error: %a\n%!" Llaml.Types.pp_error e
   | Ok resp ->
     let text =
       match resp.Llaml.Types.choices with
       | ch :: _ ->
         (match ch.Llaml.Types.message.content with
          | Llaml.Types.Text t :: _ -> t
          | _ -> "(no text)")
       | [] -> "(no choices)"
     in
     Printf.printf "Response: %s\n" text;
     match resp.usage with
     | Some u ->
       Printf.printf "Usage: %d prompt + %d completion = %d total\n"
         u.Llaml.Types.prompt_tokens u.completion_tokens u.total_tokens
     | None -> ());

  Printf.printf "\n=== Streaming ===\n%!";
  let stream_req =
    Llaml.Types.request ~model ~messages ~stream:true ()
  in
  let on_chunk (chunk : Llaml.Types.chunk) =
    match chunk.delta.content with
    | Some text -> print_string text; flush stdout
    | None -> ()
  in
  match client.stream stream_req ~on_chunk with
  | Error e ->
    Format.printf "\nStream error: %a\n%!" Llaml.Types.pp_error e
  | Ok (Some u) ->
    Printf.printf "\nUsage: %d prompt + %d completion\n"
      u.Llaml.Types.prompt_tokens u.completion_tokens
  | Ok None -> print_newline ()
