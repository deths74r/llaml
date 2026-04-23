# Changelog

## 0.2.0 (2026-04-23)

Reliability + security pass on the client layer, plus the
infrastructure kept after the Gemini Code Assist OAuth experiment
was reverted upstream in LMI. 47 unit tests, builds clean.

### Added

- **Jittered exponential backoff** with `?transient_only` on
  `Client.Make.create`. Jitter (0.8×–1.2× multiplier seeded from
  `/dev/urandom` at module load) prevents thundering-herd retries
  when multiple fibers hit the same rate limit. `transient_only =
  true` narrows the retry loop to 429 / 5xx so background daemons
  surface network errors immediately instead of burning their
  retry budget on connectivity loss.
- **Retry-After from error body** — `Types.retry_after_from_message`
  parses "reset after Ns", "retry in Nms", and "retry after Ns"
  phrasings from rate-limit error text. Wired into the OpenAI and
  Anthropic 429 paths so `Rate_limit.retry_after` is populated
  even when the server omits the header.
- **Model aliases** — `Llaml.Providers.aliases` is a curated
  short-name → canonical-id table (`sonnet` → `claude-sonnet-4-6`,
  `flash` → `gemini-2.5-flash`, `gpt5` → `gpt-5`, etc.).
  `Llaml.Providers.resolve_alias` resolves a possibly-aliased name;
  unknown names pass through unchanged so canonical ids keep
  working.
- **Session-id header injection** — `?session_id` on
  `Client.Make.create` and (now also) `Llaml_eio.make`. When set,
  every outbound request carries `x-lmi-session-id: <id>` so
  provider-side logs + consumer debug logs share a correlation
  key across concurrent fibers.
- **`?transient_only` on `Llaml_eio.make`** — previously exposed
  on `Client.Make.create` but not forwarded through the Eio
  wrapper, leaving callers unable to set it. Now plumbed end-to-end.

### Fixed

- **Gemini API key travels in `x-goog-api-key` header** instead
  of `?key=` URL query param. This is a P0 security fix:
  query-string secrets land in web-server access logs, proxy
  histories, and any error-reporting backend that captures URLs.
  Header delivery keeps the key off the transport.

### Removed

- `Tool_use.metadata` (added experimentally during the OAuth work
  as a "future LMI provenance hook") — deleted. Zero producers,
  zero consumers across both llaml and LMI, and LMI's design
  doc 15 explicitly rejects per-tool-call provenance fields in
  favor of ledger-envelope metadata.
- `Types.telemetry_type` — zero callers anywhere. LMI's
  `Provider.error` taxonomy collapses llaml's 8 categories to 3
  (Api/Network/Unsupported), so the finer-grained helper had no
  natural wiring point today. Can come back when a real consumer
  materializes (e.g. a future ops metrics hook).

### Context

Between 2026-04-20 and 2026-04-22 llaml shipped an `Auth.Oauth`
variant, `Llaml.Oauth` module, and `Gemini_oauth` provider to
support Gemini Code Assist subscription auth. LMI integrated it.
We then discovered Code Assist's Flash Standard tier enforces an
undocumented ~16K TPM cap that made the second turn of any
non-trivial session 429. The OAuth layer was reverted wholesale
(llaml reset to v0.1.5 = `2d7b8f7`, non-OAuth gaps re-applied on
top). This release packages what survived: the reliability +
security fixes above, which are useful independently of OAuth.

See [LMI's `docs/design/21_gemini_oauth.md`](https://github.com/deths74r/lmi/blob/main/docs/design/21_gemini_oauth.md)
for the full REVERTED post-mortem.

## 0.1.5 (2026-04-17)

fix: gemini multi-tool-call streaming (index collision + opaque id).

## 0.1.4

model_info: add supports_reasoning field.

## 0.1.3

fix: thread thought_signature through streaming path.

## 0.1.2

list_models: return model_info with descriptions.

## 0.1.1

fix: round-trip Gemini thought_signature on function calls.

## 0.1.0 (2026-04-12)

First tagged release. Gemini: recognize SAFETY / RECITATION /
BLOCKLIST as `Content_filter` finish reason.
