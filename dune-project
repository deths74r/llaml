(lang dune 3.16)
(name llaml)
(license MIT)

(generate_opam_files true)

(source (github deths74r/llaml))
(authors "Edward Edmonds")
(maintainers "Edward Edmonds")

(package
 (name llaml)
 (synopsis "Canonical-type LLM client for OCaml")
 (description
  "One set of canonical types across 13 LLM providers (OpenAI, Anthropic,
Gemini, Bedrock, Groq, Together, Fireworks, DeepSeek, xAI, Cerebras,
Ollama, OpenRouter, Mistral). Streaming, tool calling, reasoning-effort
budgets, Anthropic prompt caching, AWS SigV4 for Bedrock, and a router
with load balancing / cooldowns / fallback chains. The core library
depends only on yojson + uri + digestif + base64; the eio+cohttp backend
lives in a separate llaml.eio sub-library so consumers who bring their
own HTTP stack don't pay for it.")
 (depends
  (ocaml (>= 5.2))
  dune
  yojson
  uri
  digestif
  base64
  (alcotest :with-test)))

(package
 (name llaml-eio)
 (synopsis "Eio + cohttp-eio HTTP backend for llaml")
 (description
  "Plug-and-play eio backend for llaml. Provides Llaml_eio.Http_eio
(a Client.Http implementation) and Llaml_eio.make / Llaml_eio.auto —
functor-hiding convenience constructors that return a boxed client
record of closures. Callers never touch Client.Make or TLS/CA
plumbing.")
 (depends
  (ocaml (>= 5.2))
  dune
  (llaml (= :version))
  eio
  eio_main
  cohttp
  cohttp-eio
  tls-eio
  ca-certs
  domain-name
  mirage-crypto-rng))
