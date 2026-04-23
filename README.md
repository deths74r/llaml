# llaml

An OCaml client library for Large Language Model APIs. Talk to OpenAI,
Anthropic, Google Gemini, AWS Bedrock, and a dozen OpenAI-compatible
providers behind one set of canonical types — with streaming, tools,
reasoning-effort budgets, prompt caching, and a routing layer for
fallbacks and load balancing.

Inspired by the Python [litellm](https://github.com/BerriAI/litellm) —
a canonical-type LLM client for an ecosystem that didn't have one yet.

## Status

Pre-alpha, working. 47 tests green, builds clean on OCaml 5.2. Used as
the provider layer for [LMI](https://github.com/deths74r/lmi).

v0.2.0 (2026-04-23) — jittered backoff + `?transient_only`, Retry-After
body extraction, Gemini `x-goog-api-key` header (P0 security fix —
keys now travel in the header, not the URL query string), model
aliases (`sonnet`, `flash`, `gpt5`, …), `?session_id` on
`Llaml_eio.make` / `Client.create` for `x-lmi-session-id` correlation.
See [`CHANGELOG.md`](CHANGELOG.md) for detail.

## Features

- **Unified types** — one `request`, one `response`, one `chunk`, one
  `usage` across every provider. Smart constructor with sensible
  defaults for every optional field.
- **Four native wire protocols** — OpenAI, Anthropic, Google Gemini,
  AWS Bedrock Converse.
- **Nine OpenAI-compatible profiles** — Together, Fireworks, Groq,
  DeepSeek, xAI, Cerebras, Ollama, OpenRouter, Mistral. Each is ~10
  lines courtesy of a `Make_openai_compat` functor.
- **Streaming** — SSE and AWS binary event-stream both plumbed through
  as canonical `chunk` deltas. Per-provider stateful decoders handle
  message-level event machines (Anthropic).
- **Tool calling** — unified across providers via `Types.tool`, with
  a canonical `Tool_use` / `Tool_result` content model.
- **Reasoning-effort knob** — a single `reasoning` field on every
  request maps to Gemini `thinkingConfig.thinkingBudget`, Anthropic
  extended `thinking` blocks, and OpenAI `reasoning_effort`. Set it
  once, it routes to whichever provider's native shape.
- **Prompt caching** — `message.cache = Some Ephemeral` tags the last
  content block of that message with Anthropic's
  `cache_control: {type: "ephemeral"}`. System messages route through
  the list-of-blocks form automatically.
- **Production knobs** — `top_k`, `seed`, `response_format`
  (`Fmt_text` / `Fmt_json_object` / `Fmt_json_schema`),
  `safety_settings` (Gemini). All emit to whichever provider
  supports them; others silently ignore.
- **AWS SigV4** — full request signing for Bedrock (no AWS SDK
  dependency).
- **Injectable HTTP backend** — the canonical eio+cohttp-eio backend
  ships as `llaml_eio`, but any module matching `Client.Http` works
  (makes testing trivial — inject a mock that returns canned
  responses).
- **Router** — model groups with `Simple_shuffle`, `Lowest_latency`,
  `Lowest_tpm_rpm` strategies, cooldowns on repeated failure, and
  fallback chains between groups.
- **Provider-specific escape hatch** — `request.extra` is shallow-
  recursively merged into the serialized body, so passing
  `~extra:[("generationConfig", `Assoc [("thinkingConfig", ...)])]`
  lands alongside `temperature` etc. without duplicating top-level
  keys.
- **Honors `Retry-After`** — 429 responses route the server's
  retry hint through to the backoff path. Retry sleeps use the
  backend's fiber-friendly clock (Eio.Time.sleep), not a domain-
  blocking Unix sleep.

## Build

Requires OCaml 5+, `dune`, and the usual HTTP stack:

```
opam install dune yojson uri digestif base64 \
             eio_main cohttp-eio tls-eio ca-certs \
             mirage-crypto-rng domain-name alcotest
dune build
dune runtest
```

The core `llaml` library only needs `yojson + uri + digestif + base64
+ str + unix`. The eio/cohttp/tls/ca-certs dependencies are confined
to the `llaml_eio` sub-library; consumers who ship their own HTTP
backend don't pay for them.

## Quick start — one-liner

If the model name is enough to pick a provider and you have the
matching env var set:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
match
  Llaml_eio.auto ~env ~sw ~model:"claude-sonnet-4-5" ()
with
| Error msg -> prerr_endline msg
| Ok client ->
  let req =
    Llaml.Types.request
      ~model:"claude-sonnet-4-5"
      ~messages:[
        { role = User;
          content = [Text "What is the capital of France?"];
          cache = None }
      ]
      ()
  in
  match client.complete req with
  | Error e -> Format.eprintf "%a@." Llaml.Types.pp_error e
  | Ok resp ->
    (match resp.choices with
     | ch :: _ ->
       (match ch.message.content with
        | Text t :: _ -> print_endline t
        | _ -> ())
     | [] -> ())
```

`Llaml_eio.auto` inspects the model prefix via `Providers.by_prefix`,
picks the right provider module, reads the conventional env var
(`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, `GROQ_API_KEY`,
`TOGETHER_API_KEY`, …), and returns a boxed client. Bedrock returns
`Error` since it needs AWS SigV4; use `Llaml_eio.make` directly for
that.

## Quick start — explicit provider

When you want to pick the provider yourself:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let client =
  Llaml_eio.make
    ~env ~sw
    ~provider:(module Llaml.Providers.Gemini)
    ~auth:(Llaml.Auth.Api_key (Sys.getenv "GEMINI_API_KEY"))
    ()
in
let req =
  Llaml.Types.request
    ~model:"gemini-2.5-pro"
    ~messages:[
      { role = User;
        content = [Text "Prove there are infinitely many primes."];
        cache = None }
    ]
    ~reasoning:(Llaml.Types.Budget 8192)
    ~temperature:0.2
    ()
in
match client.complete req with
| Ok resp  -> (* ... *)
| Error e  -> Format.eprintf "%a@." Llaml.Types.pp_error e
```

`Llaml_eio.make` hides the `Client.Make` functor behind a
record-of-closures — `client.complete`, `client.stream`, `client.embed`.
Callers never touch HTTP backends, switches, or TLS authenticators.

Streaming:

```ocaml
let on_chunk (chunk : Llaml.Types.chunk) =
  match chunk.delta.content with
  | Some text -> print_string text; flush stdout
  | None -> ()
in
match
  client.stream { req with stream = true } ~on_chunk
with
| Ok (Some u) -> Printf.printf "\n[%d/%d tokens]\n" u.prompt_tokens u.completion_tokens
| Ok None     -> print_newline ()
| Error e     -> Format.eprintf "%a@." Llaml.Types.pp_error e
```

## Reasoning effort — one knob, three providers

The `reasoning` field on `Types.request` accepts
`Minimal | Low | Medium | High | Budget of int | Dynamic` and each
codec maps it to its provider-native shape:

- **Gemini 2.5** → `generationConfig.thinkingConfig.thinkingBudget`
  (int, or `-1` for `Dynamic`). `Low`/`Medium`/`High` map to
  1024/4096/16384 token budgets.
- **Anthropic extended thinking** → top-level
  `thinking: { type: "enabled", budget_tokens: N }`. Same default
  budget table as Gemini, floor of 1024 (Anthropic's minimum).
- **OpenAI o-series + GPT-5 reasoning** → top-level
  `reasoning_effort: "minimal"|"low"|"medium"|"high"`. Budget values
  bucket into the nearest label.

```ocaml
let req =
  Llaml.Types.request ~model:"gpt-5" ~messages
    ~reasoning:Llaml.Types.High ()
```

## Prompt caching — Anthropic

`message.cache = Some Ephemeral` tags the last content block of that
message with `cache_control: {type: "ephemeral"}`. Anthropic caches
everything up to and including the tagged block, so you'd typically
put the marker on the last stable prefix of your prompt (system
message, a big doc you're passing in context) and change the trailing
messages freely:

```ocaml
let req =
  Llaml.Types.request ~model:"claude-sonnet-4-5"
    ~messages:[
      (* System: expensive to prefill, stable across turns → cache it *)
      { role = System;
        content = [Text big_system_prompt];
        cache = Some Llaml.Types.Ephemeral };
      (* User turn: changes every request, not cached *)
      { role = User;
        content = [Text "what's next?"];
        cache = None };
    ]
    ()
```

Other providers ignore `cache` entirely.

## Provider overview

| Provider     | Auth            | Streaming | Tools | Embeddings | Reasoning |
|--------------|-----------------|-----------|-------|------------|-----------|
| OpenAI       | API key         | SSE       | ✅    | ✅         | ✅        |
| Anthropic    | API key         | SSE       | ✅    | ❌         | ✅        |
| Gemini       | API key (query) | SSE       | ✅    | ✅         | ✅        |
| Bedrock      | AWS SigV4       | binary    | ✅    | ❌         | ❌        |
| Together     | API key         | SSE       | ✅    | ✅         | ❌        |
| Fireworks    | API key         | SSE       | ✅    | ✅         | ❌        |
| Groq         | API key         | SSE       | ✅    | ❌         | ❌        |
| DeepSeek     | API key         | SSE       | ✅    | ❌         | ❌        |
| xAI          | API key         | SSE       | ✅    | ❌         | ❌        |
| Cerebras     | API key         | SSE       | ✅    | ❌         | ❌        |
| Ollama       | none            | SSE       | ✅    | ✅         | ❌        |
| OpenRouter   | API key         | SSE       | ✅    | ❌         | ❌        |
| Mistral      | API key         | SSE       | ✅    | ✅         | ❌        |

## Router

`Llaml.Router` puts multiple deployments behind a single model-group
name with load balancing, cooldowns, retries, and fallback chains:

```ocaml
module R = Llaml.Router.Make (Llaml_eio.Http_eio)

let http = Llaml_eio.Http_eio.make ~env ~sw () in
let router =
  R.create
    Llaml.Router.{ default_config with
      strategy  = Lowest_latency;
      fallbacks = [("smart", ["fast"])] }
    [ { id = "sonnet-1"; group = "smart"; model = "claude-sonnet-4-5";
        weight = 1.0;
        provider = (module Llaml.Providers.Anthropic);
        auth = Llaml.Auth.Api_key (Sys.getenv "ANTHROPIC_API_KEY") };
      { id = "groq-1"; group = "fast"; model = "llama-3.3-70b-versatile";
        weight = 1.0;
        provider = (module Llaml.Providers.Groq);
        auth = Llaml.Auth.Api_key (Sys.getenv "GROQ_API_KEY") } ]
    http

let resp = R.complete router ~group:"smart" req
(* Tries sonnet by latency; on failure falls back to groq. *)
```

Strategies: `Simple_shuffle`, `Lowest_latency`, `Lowest_tpm_rpm`.
Deployments enter cooldown after `allowed_fails` consecutive failures
and are excluded from routing until `cooldown_s` elapses.

## Project layout

```
lib/           — core library (yojson + uri only)
  types.ml(i)       canonical request/response/message types
  provider.ml(i)    provider signature + openai-compat functor
  providers.ml(i)   13 pre-built provider modules + by_prefix lookup
  auth.ml(i)        Api_key / Aws_sigv4 / Gcp_token / Custom
  client.ml(i)      Client.Make functor, retry + error handling
  router.ml(i)      model-group routing with strategies and fallbacks
  sigv4.ml          AWS Signature V4 — no SDK dependency
  sse.ml            SSE line-to-event parser
  openai_codec.ml   \
  anthropic_codec.ml \  per-provider encode/decode
  gemini_codec.ml   /
  bedrock_codec.ml  /
  json_merge.ml     shallow-recursive Assoc merge for req.extra

lib_eio/       — eio+cohttp-eio backend (opt-in)
  http_eio.ml(i)    Client.Http implementation
  llaml_eio.ml(i)   boxed client record + make + auto

bin/           — live examples (need an API key)
  gemini_chat.ml    minimal chat (non-streaming + streaming)
  gemini_modes.ml   exercise temperature/thinking/JSON/tools/multi-turn

test/          — alcotest unit tests
  test_llaml.ml     47 tests: codecs, json_merge, reasoning, cache,
                    knobs, SSE, SigV4, Bedrock event-stream,
                    aliases, retry_after_from_message
```

## License

MIT. See `LICENSE`.
