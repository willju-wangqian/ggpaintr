# ADR 0023 — Implementation plans

Source: [dev/adr/0023-panel-owned-shared-sources-end-to-end.html](../../adr/0023-panel-owned-shared-sources-end-to-end.html)
Generated: 2026-05-26 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0023-panel-owned-shared-sources-end-to-end/` (run under `/goal`)

## Merge order

`01 → {02, 03} → 04 → {05, 06} → 07 → 08`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01 — foundational bundle field; every later plan depends on it.
- **G2**: 02, 03 — pure refactor (02) and pure parser invariant (03); disjoint files (`R/paintr-server.R::ptr_setup_pipelines` vs `R/paintr-shared-coordinator.R::ptr_shared`).
- **G3**: 04 — host-side `ptr_setup_panel_sources()`; depends on 01 + 02.
- **G4**: 05, 06 — both touch `R/paintr-server.R` but different functions (`ptr_setup_pipelines` vs `ptr_setup_source_uis`); implement concurrently, merge serially to avoid same-file merge conflicts.
- **G5**: 07 — consumer-picker dep + `has_rendered` preservation; depends on 04.
- **G6**: 08 — end-to-end multi-instance browser fixture + `CONTEXT.md` correction; depends on 01–07.

## Plans

| #  | Slug                                       | Group | Depends on    | Status | One-line summary |
|----|--------------------------------------------|-------|---------------|--------|------------------|
| 01 | bundle-panel-sources-field                 | G1    | —             | PASS   | Add `panel_sources = list()` to `new_ptr_shared_state` / validator / print method **and** extend `ptr_init_state()` so the field reaches `state$panel_sources` per-instance via the existing `do.call` channel. |
| 02 | resolve-upload-source-helper               | G2    | —             | PASS   | Extract upload-format detection + default-arg fallback + binding-name derivation from `ptr_setup_pipelines()` into one internal helper. |
| 03 | parse-time-invariant                       | G2    | —             | PASS   | `ptr_shared()` aborts (class `ptr_panel_consumer_source_mismatch`) when a panel-owned consumer has formula-local sources. |
| 04 | host-setup-panel-sources                   | G3    | 01, 02        | PASS   | `ptr_shared_server()` populates `state$panel_sources` for panel-owned source keys via new internal `ptr_setup_panel_sources()` helper. |
| 05 | per-instance-pipeline-substitution         | G4    | 01, 04        | PASS   | `ptr_setup_pipelines()` substitutes panel-owned source ids — depends on `state$panel_sources[[sid]]()` instead of input slots, mirrors into per-instance `state$resolved_sources`/`bound_names`. |
| 06 | partition-aware-skip-source-uis            | G4    | 01, 04        | PASS   | `ptr_setup_source_uis()` skips panel-owned source ids (`if (s$id %in% names(state$panel_sources %||% list())) next`), restoring the Cut-1-dropped skip in partition-aware form. |
| 07 | consumer-picker-panel-sources-dep          | G5    | 04            | PASS   | `ptr_bind_shared_consumer_uis()` / `ptr_bind_local_shared_consumers()` gain `panel_sources` arg; renderUI deps on `panel_sources[[sid]]()`; `has_rendered` closure-flag preserved verbatim. |
| 08 | e2e-multi-instance-panel-source            | G6    | 01–07         | PASS   | Shinytest2 fixture asserting ADR worked examples #1 / #2 / #3 / #4 / R1; CONTEXT.md line 125 corrected. |

**Note on accessor naming.** ADR §4 and §7 use `dots$panel_sources` as shorthand for "the bundle's `panel_sources` field reaching the per-instance scope". The actual reachable accessor inside `ptr_setup_pipelines()` / `ptr_setup_source_uis()` is `state$panel_sources` — `dots` is a local variable inside `ptr_server()` (R/paintr-server.R:756) that is consumed by `do.call(ptr_init_state, c(list(formula, envir), dots))` at R/paintr-server.R:781. Plan 01 extends `ptr_init_state()` with a `panel_sources = list()` arg so the bundle field flows in unchanged and lands on `state`. Plans 05 / 06 / 07 read `state$panel_sources` directly.

## Blocked plans

(none — all 8 plans PASS /implementable, per-plan drift audit, and global worked-example coverage audit.)

## Worked-example coverage

(VERDICT: PASS from /decision-to-plan step 8.5, coverage audit cycle 2.)

| anchor | kind | summary | status | plan:scenario |
|--------|------|---------|--------|---------------|
| #1     | positive | Two plots sharing one uploaded dataset (the textbook Bug 1 reproducer) | COVERED | 08: ADR worked example #1 — two plots sharing one uploaded dataset |
| #2     | positive | default_arg primes panel-shared source so pickers populate at boot     | COVERED | 04: Default-arg fallback primes panel_sources at boot; 08: ADR worked example #2 — default_arg primes pickers at boot |
| #3     | positive | Mixed scope: panel-owned source, formula-local consumers               | COVERED | 05: Two instances sharing one panel_sources entry; 08: ADR worked example #3 — mixed scope wires correctly |
| #4     | positive | Single-instance shared source — Cut-1 path preserved post-Cut-2         | COVERED | 08: ADR worked example #4 — single-instance shared source still works; 06: Single-instance config (NULL bundle) |
| #5     | positive | Pipeline-head ppUpload chained through intermediate stages (dplyr::filter / mutate) | COVERED | 05: ADR worked example #5 — pipeline-head ppUpload chained through intermediate stages |
| #6     | positive | Custom source keyword inherits panel-scope wiring via registry dispatch | COVERED | 04: ADR worked example #6 — custom source keyword inherits panel-scope wiring |
| #7     | positive | Seed panel-owned source via companion id at boot                       | EXEMPT  | ADR Out-of-scope: panel-scope `spec=` seeding deferred ("the seed shape needs its own design and is out of this cut") |
| R1     | negative | Panel-owned consumer + formula-local sources aborts at ptr_shared()    | COVERED | 03: ADR worked example R1 — different bare-data sources; 08: R1 e2e smoke (class + names) |
| R2     | negative | Two bare ppUpload() with shared consumer — falls to R1                 | COVERED | 03: ADR worked example R2 — two bare ppUpload() with shared consumer falls to R1 |
| R3     | negative | Spec-seeding fileInput with a data frame is silently skipped (browser-driven) | COVERED | 04: ADR worked example R3 — spec-seeding the fileInput silently skipped |
| R4     | negative | Embedder-supplied panel_sources reactive cannot replace fileInput tag  | COVERED | 04: ADR worked example R4 — panel still emits the fileInput tag; 01: R4 escape-hatch validates (reactive accepted) |
| R5     | negative | Single-formula-referenced source falls to inline section, not the panel | COVERED | 03: Mixed scope (asserts panel_keys excludes single-formula keys); 06: Formula-local shared source rendered per-instance |

## Uncovered worked examples

(none — see "Worked-example coverage" above. #7 is EXEMPT-BY-ADR, not uncovered.)

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in the same parallel_group all branch off the same point (the latest merged commit before the group).
- Definition-of-Done command is in every plan; run it on the orchestrator branch after every merge.
- The full DoD command (copied into every plan): `NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_dir("tests/testthat", reporter="progress", stop_on_failure=FALSE)'` — expected FAIL 0 / ERROR 0 / SKIP 0 / PASS ≥ pre-plan baseline + the plan's claimed delta.
- Proxy traps (per `CLAUDE.md`): raw `Rscript` without `NOT_CRAN=true` silently SKIPs every shinytest2 test; `devtools::check()` is NOT the gate (it clean-skips the e2e file via the source-root guard); the only valid gate is the command above.
- For Plan 07's `has_rendered` invariant: the auditor must additionally re-run `tests/testthat/test-shared-source-rendering.R` specifically to confirm the post-add-expr-merge regression test still PASSes.
- For Plan 08's fixture: the auditor must inspect the first line of `tests/testthat/fixtures/vignette-apps/shared-source-panel-multi/app.R` for the `pkgload::load_all(Sys.getenv("GGP_PKG"), ...)` incantation — without it, the shinytest2 child runs against the stale system install and every assertion silently tests dead code.

<!-- implementable: PASS date=2026-05-26 gate="N/A (manifest)" hash=2f54537e3563 -->
