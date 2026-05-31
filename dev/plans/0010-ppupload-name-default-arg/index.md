# ADR 0010 — Implementation plans

Source: [dev/adr/0010-ppupload-name-default-arg.html](../../adr/0010-ppupload-name-default-arg.html)
Generated: 2026-05-21 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0010-ppupload-name-default-arg/` (run under `/goal`)

## Promotion note

ADR 0010 was originally drafted with Status **DEFERRED**. It was promoted to **ACCEPTED** in this /decision-to-plan session (2026-05-21) after grilling resolved four under-specified points; the resulting Clarifications C1–C6 are appended to the ADR itself and are the authoritative source for these plans. In particular, Clarification C3 expands the scope from "display name only" to "data-with-upload-override" (auto-resolve from the caller env via the existing `eval_env` parent chain — see C2). Source verification (2026-05-21) showed several of the original ADR's claimed touches are already in place; the plans below reflect what actually needs to change.

## Merge order

`{01, 02} → 03`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01, 02 — disjoint files (`R/paintr-builtins.R` vs `R/paintr-render.R`); safe to implement concurrently.
- **G2**: 03 — depends on both members of G1 being merged into the orchestrator branch first.

## Plans

| #  | Slug                          | Group | Depends on | Status | One-line summary |
|----|-------------------------------|-------|------------|--------|------------------|
| 01 | registry-and-ui-seeding       | G1    | —          | draft  | Add `default_arg = ptr_default_symbol_or_string()` to ppUpload registry; seed the companion textInput from `node$default`; flip top-level `ppUpload` from guard-only to identity-on-arg + guard-on-noarg; update `pp_placeholders` roxygen. |
| 02 | preserve-mode-current-pick    | G1    | —          | draft  | Rewire `stamp_current_pick_walk.ptr_ph_data_source` to prefer `snapshot[[node$companion_id]]` when companion_id is set, falling back to the existing `snapshot[[node$id]]` path for companion-less sources. |
| 03 | e2e-and-integration           | G2    | 01, 02     | draft  | New R-level integration test + shinytest2 browser fixture `adr10-ppupload-name` asserting `ppUpload(penguins)` round-trip, auto-resolve from caller env (no upload), and re-edit to `ppUpload(iris)`. No production R-source touched. |

## Blocked plans

(Populated by auto-verdict loop if any plan stays FAIL after 3 /implementable attempts.)

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in the same parallel_group all branch off the same point (the latest merged commit before the group).
- Plans 01 and 02 share parallel group G1 but touch disjoint files: plan 01 owns `R/paintr-builtins.R` + new test file; plan 02 owns one function body in `R/paintr-render.R` + new test file. Safe to implement concurrently.
- Plan 03 owns only new test/fixture files. It must not modify any `R/paintr-*.R` file — Clarification C2 establishes the auto-resolve plumbing (`eval_env = new.env(parent = envir)` at `R/paintr-server.R:207`) is already in place.
- Definition-of-Done command is in every plan; run it on the orchestrator branch after every merge.

## Pre-existing source state (verified 2026-05-21)

These were claimed by the original ADR Implementation Sketch as work items but already exist in source:

- `ptr_define_placeholder_source` already accepts a `default_arg` parameter (`R/paintr-registry.R:627`). PLAN-01 only adds the argument at the call site, not the validator wiring.
- `extract_placeholder_args` already routes positional args through `entry$default_arg` and stamps `node$default` (`R/paintr-translate.R:390–419`). No parser change is needed.
- `ptr_ph_data_source` node constructor already has a `default` slot (`R/paintr-nodes.R:61–70`). No node-shape change is needed.
- `format_placeholder_value` already lists `"ppUpload"` in `bareword_kw` (`R/paintr-render.R:391–412`). No render-dispatch change is needed.
- `substitute_walk.ptr_ph_data_source` already reads `ctx$snapshot[[node$companion_id]]` and emits a bare symbol via `rlang::sym(name_value)` in final mode (`R/paintr-substitute.R:145–179`). No substitute-walk change is needed — Clarification C1.
- `eval_env = new.env(parent = envir)` (`R/paintr-server.R:207`) and `assign(binding_name, df, envir = state$eval_env)` (`R/paintr-server.R:504`) deliver auto-resolve via the parent-env chain — Clarification C2/C3. No new env-threading code is needed in PLAN-03.

These pre-verified facts pin the actual implementation surface to: (1) one new arg to a registry call, (2) one literal change in a `shiny::textInput` value, (3) one signature change to the top-level `ppUpload` (+ its roxygen), (4) one branch added to `stamp_current_pick_walk.ptr_ph_data_source`. Everything else is tests + a fixture.

## Deferred to future rounds

- Vignette / docs announcing ADR 0010 (the named-upload form, the auto-resolve semantics, the migration path from `ppUpload()` to `ppUpload(<name>)`).
- Migration of existing `ppUpload` usages in this project's vignettes and fixtures to the named form (per ADR 0010 "Vignette / fixture migration burden" row: optional, incremental).
- Behavior of the companion textInput when `node$default` is set AND the user clears the input at runtime — current spec: empty string → preserve mode emits `ppUpload()` (legacy form). If this turns out to confuse users, file a follow-up plan.

<!-- implementable: PASS date=2026-05-21 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=413a48e756c4 -->
