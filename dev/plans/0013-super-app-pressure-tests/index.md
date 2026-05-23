# ADR 0013 — Implementation plans

Source: [dev/adr/0013-super-app-pressure-test-suite.html](../../adr/0013-super-app-pressure-test-suite.html)
Generated: 2026-05-23 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0013-super-app-pressure-tests/` (run under `/goal`)

## Merge order

`01 → {02, 03, 04, 05, 06}`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01 — creates the test-file scaffold + helpers. Everything else depends on its anchor markers existing.
- **G2**: 02, 03, 04, 05, 06 — each owns a disjoint fixture subdir + one anchor region in `test-super-pressure.R`. Anchor regions never overlap (PLAN-01 enforces order + identity), so any merge order within G2 is conflict-free. Implementers can develop concurrently in sibling worktrees branched off the post-PLAN-01 HEAD.

## Plans

| #  | Slug                                | Group | Depends on | Status | One-line summary |
|----|-------------------------------------|-------|------------|--------|------------------|
| 01 | test-file-scaffold-and-helpers      | G1    | —          | draft  | Create `tests/testthat/test-super-pressure.R` with 5 anchor-delimited empty regions + `tests/testthat/helper-super-pressure.R` exposing sentinel-propagation helpers that refuse to weaken into presence proxies. Zero new test_that blocks. |
| 02 | app-1-kitchen-sink                  | G2    | 01         | draft  | Super-app 1 (`ptr_app()` + mtcars literal + G2 bare-expr + window panel). Custom `ppRange` value placeholder. Sentinel propagation for ppNum/ppVar/ppText/ppExpr; C6 shared-default tiebreak; B3 toggle canonical-host instance. |
| 03 | app-2a-upload-registry              | G2    | 01         | draft  | Super-app 2a (`ptr_app()` + ppUpload mid-pipeline + G1 string). Custom `ppPower` value + `ppMultiVar` consumer (returns `interaction(...)`). Two positional `ppUpload(df_main)` / `ppUpload(df_aux)` per ADR-0010; layer-data scope verified at picker level; D9 inline error. |
| 04 | app-2b-customsource-splice          | G2    | 01         | draft  | Super-app 2b (`ptr_app()` + custom `ppSample` D3 source at head + G3 `!!` splice with placeholders **inside**). Custom `ppCoef`/`ppFactor`/`ppSample` (3 hooks: value, consumer, source). Layer-data `ppUpload` (NOT at head). |
| 05 | app-3-l3-multi-shared-plotly        | G2    | 01         | draft  | Super-app 3 (hand-rolled L3 + 2× per-cell mtcars + G4 `rlang::expr` vars + G6 forwarded-symbol + plotly custom output). Custom `ppRange`. Shared `linked` key drives BOTH cells; B3 toggle next to plotly output; F4/F7/F8 state-API surface. |
| 06 | app-4-user-css-safety-adversarial   | G2    | 01         | draft  | Super-app 4 (`ptr_app() + css = "user.css"` + literal mtcars + G5 paste0/sprintf force-eval). Custom `ppColor` value (validate_input hex). K2+K3 coexistence; K4 user_css pruning; J1+J3 test-injected adversarial ppExpr probe (deny-list + walker reject with canonical literal `"is not allowed in an \`expr\` input"`); I5 validate_input. K1 (bslib) dropped — `ptr_app_bslib()` slated for removal. |

## Blocked plans

(Empty.)

## ADR amendments made during this /decision-to-plan session (2026-05-23)

Drift audit surfaced seven items where the ADR text disagreed with the actual project API. All were **corrected at source** in the same session (not recorded as plan-level drift notes). The ADR file (`dev/adr/0013-super-app-pressure-test-suite.html`) now reflects the true API shape, and the plans match the corrected ADR.

1. **App-1 `copy = list(...)` → nested `ui_text$params`.** `ptr_app()` has no `copy =` argument (verified at `R/paintr-app.R:136-143`). The canonical channel for per-arg overrides is `ui_text$params` (`R/paintr-copy.R:64`). ADR §App-1 code block fixed.
2. **App-2a/2b `ppUpload(name = '…')` → positional `ppUpload(<sym>)`.** The ADR-0010 implementation uses `default_arg = ptr_default_symbol_or_string()` (`R/paintr-builtins.R:332-339`) — the companion arg is positional (bareword or string), not a `name =` keyword. ADR §App-2a code block + prose, ADR §App-2b code block, ADR §D7 multi-data-source paragraph, and ADR §D6 G1 row all fixed.
3. **App-2a `copy = list(params = ...)` → nested `ui_text$params`.** Same as item 1.
4. **App-2b `ptr_default_character()` → `ptr_default_string()`.** `ptr_default_character()` does not exist; the closest validator for `ppSample("iris")`'s string-literal default is `ptr_default_string()` (`R/paintr-default-args.R:217`). ADR §App-2b code block fixed.
5. **App-3 citation `R/paintr-app.R:1271` clarified.** Line 1271 is where `ptr_app_grid` opens; the string-only assertion `all(vapply(plots, rlang::is_string, ...))` is in `ptr_app_grid_components` at `R/paintr-app.R:1309-1313`. ADR §App-3 note now cites both functions and the correct assertion location.
6. **App-4 host swap: `ptr_app_bslib()` + `user_css =` → `ptr_app() + css =`.** `ptr_app_bslib()` has no `user_css =` (or `css =`) argument; `ptr_app()` has `css =` (`R/paintr-app.R:142`). Since the user indicated `ptr_app_bslib()` is slated for removal, App-4's host swapped to `ptr_app()`, K1 (bslib) dropped from the partition, and K2+K3+K4 retained via `ptr_app(... css = "user.css")`. ADR §D1 host enum, ADR §D1 theme row, ADR §App-4 heading + slot picks + code block + pressure axes + coverage matrix all fixed.
7. **Deny-list location and rejection-text phrasing pinned.** The deny list `unsafe_expr_denylist` lives at `R/paintr-utils.R:448`; the canonical rejection literal at runtime is `` "`<name>` is not allowed in an `expr` input (<where>)" `` (`R/paintr-utils.R:609`). PLAN-06's BDD `Then` clause asserts on that literal substring.

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in G2 all branch off the same point (the latest merged commit after PLAN-01).
- Definition-of-Done command is in every plan; run it on the orchestrator branch after every merge.
- The G2 merge order is numeric (02 → 03 → 04 → 05 → 06). Each plan's edit to `tests/testthat/test-super-pressure.R` lives in its own anchor region; merges are conflict-free. A merge that produces a conflict in `test-super-pressure.R` means an anchor marker was renamed or removed, violating PLAN-01's anchor-immutability guard — that is a regression of whichever G2 plan introduced it, and must be fixed in that plan's worktree, not absorbed at merge time.
- Browser-stack gate: the orchestrator must run the authoritative gate on a machine where `shinytest2`, `chromote`, Chrome (for any plan), and `plotly` (for PLAN-05) are installed. Without those, the new tests SKIP cleanly — that is correct behavior but does NOT verify PASS+1. The orchestrator owns this verification.

## Deferred to future rounds (named in ADR §"Coverage gaps deliberately left unfilled")

These are explicitly NOT in scope for this ADR — no plan is generated for them, and the orchestrator must not attempt to absorb them into G2 plans.

- E3, E4 (shared label / default tiebreak across differing keys).
- E7 (mid-pipeline shared= on a data source).
- E8 (preserve-mode shared= round-trip under L3).
- D7 (keyword shadow rejection — adversarial registration).
- D8 (LHS-name vs `keyword=` drift warning).
- F5 (`ptr_extract_plot / _error / _code` end-to-end).
- G7, G8 (capture-time rejection of non-string non-call symbols, multi-expression formulas).
- I6, I7 (parse-time abort with malformed / empty formulas).
- J2 (default-arg constant-fold registry).
- J5 (keyword shadow-guard — duplicates D7).
- J6 (parser uses no `eval/parse/get` on AST — source-grep test, not app-shaped).
- Cross-data-source `shared=` semantics.
- ggiraph / DT custom outputs (covered by existing narrow fixtures).
- K1 (bslib theming) — `ptr_app_bslib()` is slated for removal; no super-app exercises bslib hosting under this ADR.

<!-- implementable: PASS date=2026-05-23 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=b257531f5b75 -->
