# Public API tiering and the functions deliberately un-exported / renamed after the core rewrite

The typed-AST core rewrite left the exported surface partly stale: `_pkgdown.yml` referenced ~12 functions that no longer exist, two exports (`ptr_runtime_input_spec`, `ptr_ns_id`) had only internal callers or documented removed contracts, and a dead `placeholders=` argument (residue of the pre-rewrite per-call placeholder registry) was threaded through two exported copy functions. We audited the whole surface, fixed it to five explicit tiers — end users / Shiny embedders / advanced embedders / placeholder authors / LLM tooling — un-exported `ptr_runtime_input_spec` and `ptr_ns_id`, removed `placeholders=`, renamed `ptr_merge_ui_text`→`ptr_ui_text` and `ptr_server_state`→`ptr_init_state` for clarity, and decided that headless / non-Shiny use (`ptr_translate`, `ptr_run_formula`, `ptr_setup_*`) stays intentionally internal because exposing it would commit the still-settling node-class contract as public API. Recorded so a future reader doesn't "fix" the missing functions back into the namespace. Action checklist: `dev/plans/api-audit.md`.

## Status

accepted

## Consequences

- Breaking changes for anyone on a pre-`0.9.x` dev snapshot: `ptr_merge_ui_text` / `ptr_server_state` / `ptr_runtime_input_spec` / `ptr_ns_id` gone or renamed; `placeholders=` and `ns=` (on `ptr_app`/`ptr_app_bslib`) removed; `ptr_resolve_ui_text` argument order changed. None of these had real external users (rewrite branch only), so no deprecation cycle.
- "Exported function whose *required* companions are internal" is treated as a portal smell going forward — it's what made `ptr_server_state` ambiguous (resolved by promoting it to a real, documented advanced-embedder entry point rather than half-exposing it). `ptr_init_state` standalone is honestly billed as a state container for programmatic driving + `testServer`, not a from-scratch reactive-app builder.
