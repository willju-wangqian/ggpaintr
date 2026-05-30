# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

An R package that turns ggplot-like formula strings into Shiny apps.
Placeholder tokens in the formula (`var`, `text`, `num`, `expr`, `upload`) become Shiny input widgets automatically.

## Before implementing — context-sufficiency check (do this FIRST)

Treat any handoff / bug report / "locked decision" / "N-line fix" as a **hypothesis about scope**, never as the full picture. Bugs here often span multiple sessions and branches (e.g. the consumer-seeding / source-binding / boot-default / auto-name complex). Before writing code, **verify breadth, not just depth**:

- Read the cited ADR(s) in `dev/adr/` **in full** (not by reference) and `CONTEXT.md` for the domain model.
- `git log --oneline -- <touched files/area>` and scan recent commits for prior related work; check **sibling branches/worktrees** (`git branch -vv`, `git worktree list`) and `.scratch/` + `dev/` notes — an active multi-session campaign usually leaves tracks there.
- Grep `MEMORY.md` + the memory dir for the area; recalled memories naming a related bug are a signal to go read it.
- Then **state, before coding: what larger complex this belongs to, what's already been tried, and what context is still missing.** If the answer is "this is one facet of X," map X first.

A handoff's "complete / verified / 2-line" claim gets the *same skepticism as its code claims* — and the breadth check is **proactive** (up front), not reactive (only when a test breaks). When in doubt, fan out and read widely before narrowing. See memory `verify-breadth-not-just-depth`.

## Commands

```r
devtools::test()                          # run all tests
devtools::test(filter = "parse-formula")  # run one test file (matches test-<filter>.R)
devtools::document()                      # regenerate NAMESPACE + man/ from roxygen
devtools::check()                         # full R CMD check (0 errors, 0 warnings required)
devtools::load_all()                      # load package for interactive use
source("dev/build-pkgdown.R"); build_pkgdown_clean()  # build documentation site
```

## Conventions

Style (`ptr_` prefix, snake_case, 2-space), error/message idioms (`rlang::abort`, `cli::cli_warn`), validation (`assertthat`), and the NAMESPACE / `devtools::document()` workflow live in `.claude/rules/coding.md`. Project-specific: source `R/paintr-*.R`, tests `tests/testthat/test-*.R`; dependencies — rlang (tidy eval + error signaling), assertthat (validation), cli (messages), ggplot2 (rendering), purrr (functional helpers), shiny + shinyWidgets (UI), bslib (optional, powers `ptr_app_bslib()`).

## Code Exploration Tools

Serena symbolic tools pay off only for navigating *named* functions in large `R/paintr-*.R` files; use Bash `grep` / `Read` for cross-file search and non-R files, and `Rscript` probes for runtime/reactive behavior. Full routing table, diagnosis ordering, and the sub-agent worktree edit-leak pitfall: `.claude/rules/serena-tools.md`.

## Source of Truth Rules

- `README.Rmd` is the editable README source; `README.md` is generated output — edit `README.Rmd`, then re-knit.
- `dev/developer-notes.md` is human-maintained only — do not edit unless the user explicitly asks.
- Stored task files (`dev/tasks/*.md`) stay unchanged unless the user explicitly asks to edit them.
- Do not create ad-hoc markdown files in `.claude/specs/`.
- Architectural decision records live in `dev/adr/` (not `docs/` — that's pkgdown's generated, gitignored output). Agent-skill config lives in `dev/agents/` (see `## Agent skills` below).
- Audit results (pre-merge / post-merge / drift / roxygen / e2e / LLM / etc.) save to `dev/audit/audit-<topic>-<time>.html` — single home for every audit artifact, not `dev/notes/`.
- `dev/feature_bank/` is **deprecated — NOT a source of truth** (demoted 2026-05-28; its substring-match coverage went stale). Do not cite, regenerate, or treat its rows as authoritative. Derive feature/coverage status from the R source + `NAMESPACE` + `CONTEXT.md` + `dev/adr/` + the test suite.
- `vignettes/*.Rmd` are **currently OUTDATED — do NOT use as a source of truth** (flagged 2026-05-28; not yet rewritten). Confirmed drift: `ggpaintr-use-cases.Rmd` documents the **removed** `shared_ui=` argument on `ptr_shared()` / `ptr_app_grid()` (examples will error), and vignettes may carry pre-ADR-0009/0022/0023 syntax. Until a vignette rewrite lands, verify any vignette claim/example against the R source + `NAMESPACE` + `CONTEXT.md` + `dev/adr/`; never quote a vignette as authoritative.
- Generated HTML files (audits, notes, plans, ADRs, any agent-emitted artifact under `dev/`) MUST link the shared stylesheet via `<link rel="stylesheet" href="/path/to/dev/assets/doc.css">` (use the correct relative path from the file's location, e.g. `../assets/doc.css` from `dev/audit/`, `../../assets/doc.css` from `dev/plans/<slug>/`). Do NOT inline `<style>` blocks or duplicate rules already in `doc.css` — this saves output tokens and keeps doc styling consistent. If a class you need is missing from `doc.css`, add it there once rather than inlining locally.

## Harness Config

<!-- Generated by /ewh:doit init — edit values as needed -->

- Language: R
- Test command: devtools::test()
- Check command: devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))
- Source pattern: R/paintr-*.R
- Test pattern: tests/testthat/test-*.R
- Doc build: source("dev/build-pkgdown.R"); build_pkgdown_clean()  # excludes CLAUDE.md from the rendered site
- README render: devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())
- Conventions: tidyverse style, snake_case, 2-space indent
- Auto-approve start: true

### Authoritative gate (Definition of Done) — read before claiming green

The authoritative full-suite gate is the suite at **FAIL 0 / ERROR 0 / SKIP 0 / PASS N** (N grows; last ≈3388 on 2026-05-29 — the count is **not** the gate, **0/0/0 is**). Default run is **serial** (~590s); `devtools::test()` sets `NOT_CRAN=true` itself so browser tests RUN (0 SKIP):

```
Rscript -e 'devtools::test(reporter="progress", stop_on_failure=FALSE)'
```

**Parallelism is opt-in — use it ONLY for the post-merge full-suite gate or when I explicitly ask** (never the inner loop, never R CMD check). It cuts ~590s→~270s; enable with `TESTTHAT_PARALLEL=TRUE TESTTHAT_CPUS=4` (cap at 4 — browser tests are render-bound, so >4 concurrent Chromes oversubscribe CPU → slower *and* more boot-tail flakes). A residual flake under parallel load is not a product failure — re-confirm serially before calling it a regression.

> ⚠ Proxy trap — do not be fooled (corrected 2026-05-17 / 2026-05-29, empirically verified):
> - A raw `Rscript -e 'testthat::test_dir()/test_file()'` with **no `NOT_CRAN`** → `skip_on_cran()` fires → **every shinytest2 browser test silently SKIPs**. "green, SKIP n" off that is NOT the gate.
> - **Parallel runs only** (`TESTTHAT_PARALLEL=TRUE`): `testthat::test_dir()` defaults to `load_package="none"`, so its worker subprocesses don't load ggpaintr (the parent's `devtools::load_all()` doesn't propagate) → use `devtools::test()`, which pkgloads the source pkg per worker. Worker count = `TESTTHAT_CPUS` else `getOption("Ncpus")` else 2. Parallel exposes boot-tail races (a `renderUI` input read/uploaded before `bindAll()` wires it); the e2e boot helpers were hardened against this in 4b5f4d0 (poll/await bindings + `upload_file()` wrapper).
> - `devtools::check(... --as-cran)` does **NOT** make `skip_on_cran()` fire (devtools runs the check subprocess with `NOT_CRAN=true`). The browser fixtures instead **cleanly SKIP** via `boot_vignette_app()`'s source-root guard (no `DESCRIPTION` in the `.Rcheck` sandbox). If a check run shows the e2e file **ERRORing** (`R CMD check found ERRORs`, pkgload "DESCRIPTION not found"), that guard is missing/broken — a **harness defect, not a product failure, and never the gate**.
> - Only `NOT_CRAN=true` / `devtools::test()` **runs** the browser tests (source tree present → 0 SKIP). That alone is the gate. Never claim the suite green, merge a branch, mark a step done, or clear context off the skipping/erroring harness.

Other gates: `devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran","--no-manual"))` → 0 errors / 0 warnings (by-design NOTEs expected: CRAN incoming "new submission", `.git`, top-level `CONTEXT.md`); the e2e browser file **cleanly SKIPs** here via the source-root guard — a non-zero-error check means that guard regressed, not a product failure (see proxy-trap block above). Docs: `source("dev/build-pkgdown.R"); build_pkgdown_clean()` → clean. Browser e2e needs `shinytest2` + `chromote` + Chrome; absent → clean all-skip is acceptable but must be reported, not hidden.

Every plan for this project takes its **Definition of Done** from this block. Audit/enforce a plan with `/implementable <plan-path>`.

### Test scope during iteration

**Default to the cheap inner loop, not the full e2e suite.** Unless I explicitly ask, do not run the full e2e browser suite while iterating: always run **all non-e2e tests** (serial `test_dir` with no `NOT_CRAN` → browser tests cleanly SKIP, ~2680 assertions in seconds), then only the e2e tests in the change's **blast radius** — which for a shared-runtime/codegen change (`paintr-runtime`/`server`/`substitute`/`ui.R`) widens to the whole seeding/binding/source cluster, or the full suite if you can't bound it. The full e2e suite runs **only** at a merge boundary (post-merge + worktree landing) or on explicit request, and is the **only** thing that grounds a "done"/"green" claim. Recipes + blast-radius detail: `.claude/rules/testing.md`.

## Agent skills

### Issue tracker

Issues and PRDs live as markdown files under `.scratch/<feature>/` in this repo — no remote tracker. See `dev/agents/issue-tracker.md`.

### Triage labels

Default canonical vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`), recorded as a `Status:` line in each issue file. See `dev/agents/triage-labels.md`.

### Domain docs

Single-context. The L2/L3 embedding model, UI-piece taxonomy, and shared-coordinator language live in `CONTEXT.md` (authoritative, created 2026-05-16, locked by ADR 0005 and amended through ADR 0023 — see CONTEXT.md's status header for the ADR chain). General project/build conventions stay in this file. Architectural decisions live in `dev/adr/`. See `dev/agents/domain.md`.

### Knowledge stamps

When you derive a reusable, source-grounded understanding, emit an inline `⟦FINDING⟧ … ⟦/FINDING⟧` stamp so it survives `/clear` and can be harvested+verified later. Full contract, grammar, and the bar for what's worth stamping: `.claude/rules/knowledge-stamps.md`. On-demand: `/stamp`. End-of-session dump (best-effort, unverified): `/summarize-knowledge`. The regular verify+store pass over `/export`-ed transcripts: the `harvest-finding` skill. Categorize the verified findings into a labeled, greppable map: `/process-finding`.

**Rebuild understanding from the index first.** Before re-deriving knowledge about a subsystem, a past bug, or *why* something is built a certain way, consult the project knowledge index: `grep -i "<topic>" .claude/harvest-findings/INDEX.md` → label, then `grep -rl "<label>" .claude/harvest-findings/raw_knowledge/` → the verified findings. This is the read side of the stamp→summarize→harvest→process pipeline; the pipeline (set up by `/harvest-init`) writes hard-won understanding, the index reads it back. See the "Recall" section of `.claude/rules/knowledge-stamps.md`.
