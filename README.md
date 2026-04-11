# llaml

An OCaml client library for Large Language Model APIs. Talk to OpenAI,
Anthropic, Google Gemini, AWS Bedrock, and a dozen OpenAI-compatible
providers behind one set of canonical types.

## Features

- **Unified types** — one `request`, one `response`, one `chunk`, one `usage`
  across every provider
- **Four native wire protocols** — OpenAI, Anthropic, Google Gemini, AWS
  Bedrock Converse
- **Nine OpenAI-compatible profiles** — Together, Fireworks, Groq, DeepSeek,
  xAI, Cerebras, Ollama, OpenRouter, Mistral
- **Streaming** — SSE and AWS binary event-stream both plumbed through as
  canonical `chunk` deltas
- **Tool calling** — unified across providers via `Types.tool`
- **Reasoning token accounting** — `thinking_tokens` field captures Gemini
  thoughts, OpenAI reasoning effort, and Anthropic extended thinking
- **AWS SigV4** — full request signing for Bedrock (no AWS SDK dependency)
- **Injectable HTTP backend** — ships with a cohttp-eio implementation, but
  any module matching `Client.Http` works (makes testing trivial)
- **Router** — model groups with load balancing, cooldowns, retries, and
  fallback chains
- **Provider-specific escape hatch** — `request.extra` merges arbitrary JSON
  into the request body for things like Gemini's `thinkingConfig` or JSON
  mode

## Build

Requires OCaml 5+, `dune`, and the usual HTTP stack:

```
opam install dune cohttp-eio tls-eio ca-certs mirage-crypto-rng \
             yojson uri alcotest
dune build
dune runtest
```

## Quick start — Gemini

```ocaml
(* See bin/gemini_chat.ml for the full HTTP backend wiring. *)
module Client = Llaml.Client.Make (Llaml.Providers.Gemini) (My_http)

let () =
  let key    = Sys.getenv "GEMINI_API_KEY" in
  let http   = My_http.create () in
  let client = Client.create ~auth:(Llaml.Auth.Api_key key) http in
  let req    = Llaml.Types.request
    ~model:"gemini-2.5-flash"
    ~messages:[
      { role = User; content = [Text "What is the capital of France?"] }
    ] ()
  in
  match Client.complete client req with
  | Ok resp -> print_endline (* ... extract text ... *)
  | Error e -> Format.printf "%a\n" Llaml.Types.pp_error e
```

Streaming works the same way with `Client.stream client req ~on_chunk`.

## Provider overview

| Provider     | Auth            | Streaming | Tools | Embeddings |
|--------------|-----------------|-----------|-------|------------|
| OpenAI       | API key         | SSE       | ✅    | ✅         |
| Anthropic    | API key         | SSE       | ✅    | ❌         |
| Gemini       | API key (query) | SSE       | ✅    | ✅         |
| Bedrock      | AWS SigV4       | binary    | ✅    | ❌         |
| Together     | API key         | SSE       | ✅    | ✅         |
| Fireworks    | API key         | SSE       | ✅    | ✅         |
| Groq         | API key         | SSE       | ✅    | ❌         |
| DeepSeek     | API key         | SSE       | ✅    | ❌         |
| xAI          | API key         | SSE       | ✅    | ❌         |
| Cerebras     | API key         | SSE       | ✅    | ❌         |
| Ollama       | none / custom   | SSE       | ✅    | ✅         |
| OpenRouter   | API key         | SSE       | ✅    | ❌         |
| Mistral      | API key         | SSE       | ✅    | ✅         |

## Router

`Llaml.Router` puts multiple deployments behind a single model-group name:

```ocaml
module R = Llaml.Router.Make (My_http)

let router = R.create
  Llaml.Router.{ default_config with
    strategy  = Lowest_latency;
    fallbacks = [("smart", ["fast"])] }
  [ { id = "sonnet-1"; group = "smart"; model = "claude-sonnet-4-5";
      weight = 1.0; provider = (module Llaml.Providers.Anthropic);
      auth = Api_key (Sys.getenv "ANTHROPIC_API_KEY") };
    { id = "groq-1"; group = "fast"; model = "llama-3.3-70b-versatile";
      weight = 1.0; provider = (module Llaml.Providers.Groq);
      auth = Api_key (Sys.getenv "GROQ_API_KEY") } ]
  http

let resp = R.complete router ~group:"smart" req
(* Tries sonnet by latency; on failure falls back to groq. *)
```

Strategies: `Simple_shuffle`, `Lowest_latency`, `Lowest_tpm_rpm`. Deployments
enter a cooldown after `allowed_fails` consecutive failures and are excluded
from routing until `cooldown_s` elapses.

## Examples

- `bin/gemini_chat.ml` — minimal chat example (non-streaming + streaming).
  Takes an optional model name on the command line; defaults to
  `gemini-2.5-flash`.
- `bin/gemini_modes.ml` — exercises temperature, max_tokens, stop sequences,
  JSON mode, `responseSchema`, thinking budgets, `thinkingLevel`, tool
  calling, and multi-turn conversations against live Gemini models.

## License

MIT. See `LICENSE`.
