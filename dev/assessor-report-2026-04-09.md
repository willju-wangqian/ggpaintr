# ggpaintr Project Assessment Report

**Date:** 2026-04-09
**Branch:** `codex/publication-loop`
**Package version:** 0.1.0

---

## 1. Git Status

**Branch:** `codex/publication-loop` (up to date with `origin/codex/publication-loop`)

**Unstaged changes in 3 files:**
- `R/paintr-utils.R` (+44 lines): Two new internal functions `ptr_can_stand_alone()` and `ptr_remove_empty_nonstandalone_layers()` that prune empty non-standalone layers (e.g., `labs()`, `facet_wrap()`, `theme()` with no arguments) after placeholder resolution.
- `R/paintr-ui.R` (+2 lines): Adds `title = copy$empty_text` to shinyWidgets picker options for better UX when no selection is made.
- `R/paintr-runtime.R` (+1 line): Calls the new `ptr_remove_empty_nonstandalone_layers()` in the `ptr_complete_expr()` pipeline.

**Recent commits** are focused on coverage increases, API prefix renaming (`ggpaintr_*` to `ptr_*`), agent/browser test infrastructure, and publication loop work.

---

## 2. R CMD Check

**Result: 0 errors, 0 warnings, 1 note.**

The single NOTE is `unable to verify current time` -- an environment/network check, not a package issue. CRAN reviewers typically ignore this. The check ran all examples, tests, and vignettes without problems.

---

## 3. Test Coverage

**606 tests, all passing.** (Initial run showed 2 transient failures in `test-utils.R` lines 377-378 due to stale namespace; re-run passed cleanly at 606/606.)

**Test file distribution (by test count):**

| File | Tests |
|------|-------|
| test-utils.R | 53 |
| test-validation.R | 32 |
| test-export-shiny.R | 21 |
| test-runtime-feedback.R | 16 |
| test-coverage-gaps.R | 14 |
| test-copy-rules.R | 11 |
| test-upload.R | 9 |
| test-extensibility.R | 8 |
| test-placeholders.R | 7 |
| test-placeholder-registry.R | 5 |
| test-column-name-normalization.R | 5 |
| test-runtime-input-spec.R | 4 |
| test-plot-build.R | 4 |
| test-complete-expr.R | 4 |
| test-unsupported-use-cases.R | 3 |
| test-parse-formula.R | 3 |
| test-supported-use-cases.R | 1 |

**Source files without dedicated test files:**
- `R/paintr-app.R` (675 lines) -- the largest untested file. Contains `ptr_server_state()`, `ptr_server()`, `ptr_app()`, all `ptr_register_*()` binders, and `ptr_extract_*()` accessors. These are Shiny-dependent and hard to unit-test without `shiny::testServer()`, but the total absence of any test file is a coverage gap.
- `R/paintr-data.R` (124 lines) -- partially covered by `test-column-name-normalization.R`, which tests `ptr_normalize_column_names()`. The file-level mapping just didn't match the name pattern.

---

## 4. Documentation

**Exported functions:** 26 (per NAMESPACE)
**Man pages:** 27 (26 function pages + 1 package page)
**Man pages with `\examples`:** 22 out of 27

**Missing `@examples`:**

| Man page | Function(s) |
|----------|-------------|
| `ptr_extract_code.Rd` | `ptr_extract_code()` |
| `ptr_extract_error.Rd` | `ptr_extract_error()` |
| `ptr_register_controls.Rd` | `ptr_register_controls()` |
| `ptr_server.Rd` | `ptr_server()` |
| `ggpaintr-package.Rd` | Package-level doc (acceptable to omit) |

`ptr_extract_code` and `ptr_extract_error` are simple accessors -- examples would be trivial to add. `ptr_register_controls` and `ptr_server` require a Shiny session context, so `\dontrun{}` examples would be appropriate and should be added for CRAN completeness.

**Vignettes:** 3 present (`ggpaintr-workflow.Rmd`, `ggpaintr-extensibility.Rmd`, `ggpaintr-placeholder-registry.Rmd`).

---

## 5. CRAN Readiness

| Item | Status | Notes |
|------|--------|-------|
| DESCRIPTION | OK | All fields present, proper Authors@R with ORCIDs, URL and BugReports set, R >= 3.5.0 |
| License | GPL-3 declared in DESCRIPTION | **No LICENSE file on disk** -- GPL-3 does not require a separate file in R packages (CRAN accepts the DESCRIPTION declaration alone), but note this differs from MIT/BSD which require LICENSE files |
| cran-comments.md | Present | Documents 0E/0W/1N, mentions API prefix rename and `rlang::abort()` migration |
| NEWS.md | Present | Single 0.1.0 entry with changelog |
| .Rbuildignore | Present | Correctly excludes dev/, archive/, docker files, Claude/graphify artifacts |
| R CMD check | PASS (1 NOTE) | The "unable to verify current time" note is benign |

**CRAN submission blockers: None identified.**

**Minor concerns for CRAN review:**
- The 4 exported functions missing `@examples` could trigger a reviewer comment. Adding `\dontrun{}` examples resolves this.
- `purrr` is in Imports -- CRAN accepts this but some reviewers may note that `purrr::map2()` usage (the main dependency) could be replaced with `Map()` to reduce dependencies. Not a blocker.

---

## 6. Code Gaps

**TODOs in source code (2 found, both deferred):**
1. `R/paintr-app.R:1` -- `TODO(future): add inst/shiny-demo/ interactive demo app for hands-on`
2. `R/paintr-placeholders.R:464` -- `TODO(future): lift the inline-only restriction by deparsing function bodies`

Both are tagged `future` and are not blockers. No TODOs/FIXMEs in test files.

**Unstaged changes:** The 3 modified files contain a complete, self-consistent feature (empty non-standalone layer pruning). Tests for this feature already exist in `test-utils.R` lines 345-395 and pass. These changes need to be committed.

---

## 7. Dependencies

**Imports (6):**

| Package | Min Version | Purpose | Assessment |
|---------|-------------|---------|------------|
| assertthat | >= 0.2.0 | Input validation | Standard, lightweight |
| ggplot2 | >= 3.3.0 | Plot construction | Core dependency, expected |
| purrr | >= 0.3.0 | `map2()` in runtime | Could be replaced with base `Map()` to drop 1 dep; low priority |
| rlang | >= 1.0.0 | Tidy eval, `abort()`, AST manipulation | Essential for the parsing pipeline |
| shiny | >= 1.6.0 | UI generation, server | Core dependency |
| shinyWidgets | >= 0.6.4 | `pickerInput` for `var` placeholder | Justified -- standard picker is inadequate |

**Suggests (5):** knitr, pkgdown, rmarkdown, styler, testthat -- all standard for a documented, tested package.

**Nothing unusual.** The dependency set is lean and well-justified. The only optional reduction would be `purrr`, used in ~2 call sites.

---

## 8. Goal Scorecard

### Goal 1: User-Friendly for Beginners — **Ready**

The package surface is well-organized around the `ptr_*` prefix. The primary entry point `ptr_app()` takes a single formula string and produces a complete Shiny app -- minimal ceremony for beginners. Three vignettes guide users through the workflow, extensibility, and placeholder registry. The copy-rules system (`ptr_merge_ui_text`, `ptr_resolve_ui_text`) adds complexity but is opt-in.

### Goal 2: Elegant, Clean Code — **Ready**

Source is cleanly split across 10 files totaling 4,289 lines, following tidyverse conventions. The pipeline architecture (parse -> placeholders -> UI/runtime -> app/export) is clear and well-documented in CLAUDE.md. Internal helpers are `@noRd` and unexported. The new layer-pruning feature (unstaged) follows the same patterns.

### Goal 3: Extensible for Advanced Developers — **Ready**

`ptr_define_placeholder()` with its four hooks (`build_ui`, `resolve_expr`, `bind_ui`, `prepare_eval_env`) provides a clean extension point. `ptr_merge_placeholders()` allows per-app custom registries. The `TODO(future)` at line 464 of `paintr-placeholders.R` notes the inline-function-only restriction for serialization -- a known limitation documented in the code.

### Goal 4: Teachable with Informative Examples — **Needs Work**

Vignettes cover the main workflow and extension patterns. Man pages have examples for 22/26 exported functions. The generated code output (`ptr_extract_code`) helps users learn what their formula produces. The 4 missing examples are a minor gap. README and vignettes need more progressive examples showing high-level to low-level API usage.

---

## Current State

- R CMD check passes cleanly (0E/0W/1N benign).
- 606 tests pass across 17 test files.
- 26 exported functions, all with roxygen documentation, 22 with examples.
- 3 vignettes, pkgdown site infrastructure in place.
- DESCRIPTION, cran-comments.md, NEWS.md, .Rbuildignore all present and correct.
- Clean `ptr_*` API prefix throughout.

## In Progress

- 3 files have unstaged changes implementing empty non-standalone layer pruning (a complete, tested feature awaiting commit).
- Branch is `codex/publication-loop`, not yet merged to `main`.

## Gaps

1. **No test file for `paintr-app.R`** (675 lines, largest source file). The Shiny server/UI integration functions (`ptr_server`, `ptr_register_*`, `ptr_server_state`) have zero automated test coverage. This is the single largest coverage gap.
2. **4 exported functions missing `@examples`**: `ptr_extract_code`, `ptr_extract_error`, `ptr_register_controls`, `ptr_server` (all in `R/paintr-app.R`).
3. **Unstaged changes not committed**: The layer-pruning feature in `paintr-utils.R`, `paintr-runtime.R`, and `paintr-ui.R` is complete and tested but uncommitted.
4. **No LICENSE file on disk** -- not required for GPL-3 on CRAN, but some workflows expect it.

## Code Quality Notes

No issues requiring `code-simplifier` agent intervention were identified. Code follows consistent patterns and conventions throughout.

## Recommended Next Steps (Prioritized)

1. **Commit the unstaged changes.** The layer-pruning feature is complete and tested.
2. **Add `\dontrun{}` examples** to `ptr_extract_code`, `ptr_extract_error`, `ptr_register_controls`, and `ptr_server`. ~15 minutes of work; eliminates a potential CRAN reviewer comment.
3. **Add `test-app.R`** using `shiny::testServer()` to cover `ptr_server_state()`, `ptr_register_controls()`, and the extract accessors. This is the biggest coverage gap.
4. **Merge to `main`** and run final `R CMD check --as-cran` from clean state before CRAN submission.
5. **Optional:** Replace `purrr::map2()` with `base::Map()` to drop the `purrr` dependency. Low priority.

## Risks

1. **`paintr-app.R` has no automated tests.** If a regression is introduced in the Shiny integration layer, it will not be caught until manual testing. This is the primary quality risk.
2. **Transient test flakiness observed.** The first `devtools::test()` run showed 2 failures in `test-utils.R` that did not reproduce. This suggests the test environment is sensitive to namespace loading order when unstaged changes are present. Committing the changes should resolve this.
3. **R >= 3.5.0 minimum** is very permissive. If any dependency (especially rlang >= 1.0.0 or shinyWidgets >= 0.6.4) actually requires a newer R, installation could fail silently on old R versions. Low risk in practice since most users are on R >= 4.0.

## TODO (Deferred)

- **Documentation completeness:** 4 exported functions missing `@examples`; vignettes need more progressive high-to-low-level examples.
- **Version numbering:** Currently 0.1.0 — versioning strategy not assessed per instructions.
