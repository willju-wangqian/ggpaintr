# Pitfalls — durable debugging lessons

What surprised someone enough to cost a full debugging cycle. Promoted from `~/.claude/projects/-Users-willju-Research-ggpaintr/memory/` (the project's auto-memory) and `.claude/rules/testing.md`. Each entry follows the same shape: **symptom → cause → fix → source**.

These are debugging time-savers, not API reference. If a question asks "why is my test silently passing?" or "why does the picker not populate in shinytest2?" — this is the file.

## A. shinytest2 / browser e2e

### A.1 — Tests silently pass against stale system-installed ggpaintr
**Symptom:** browser tests look green, but they're testing dead code (e.g. a pre-redesign 0.9.1 `ptr_shared_server` that no longer exists).
**Cause:** shinytest2 runs the app in a fresh child R process. A serialized `shiny.appobj` passed from the parent makes that child resolve `library(ggpaintr)` to the **system-installed** ggpaintr (pkgload `_build/` is empty). Old version + new fixture = green-but-meaningless test.
**Fix:** boot an app *directory* whose `app.R` calls `pkgload::load_all(Sys.getenv("GGP_PKG"), quiet=TRUE, helpers=FALSE, attach_testthat=FALSE)` as its first line. The test sets `withr::local_envvar(GGP_PKG = normalizePath(test_path("..","..")))` and `AppDriver$new(test_path("fixtures","vignette-apps", slug))`.
**Source:** `.claude/rules/testing.md` "Browser e2e gotchas"; auto-memory `shinytest2-appdir-pkgload`.

### A.2 — `app$get_values()` 500s with "invalid char in json text"
**Symptom:** `app$get_values()` errors on a ggpaintr app with no obvious cause.
**Cause:** `get_values()` snapshots *every* output, including the custom-renderer hosts that are in a pre-draw `shiny.silent.error` state. The error pane's silent-error condition can't be JSON-serialized.
**Fix:** Don't call `app$get_values()`. Use targeted reads: `app$get_html("#id")`, `app$get_value(output="id")`, `app$get_value(input="id")`.
**Source:** `.claude/rules/testing.md`; auto-memory `shinytest2-appdir-pkgload`.

### A.3 — `set_inputs(...)` times out with `wait_ = TRUE`
**Symptom:** placeholder widget value set; the call hangs and times out.
**Cause:** ggpaintr only re-renders on the **Update / Draw click** (`04-invariants.md` II.1). Setting a placeholder widget never invalidates an output by itself, so `wait_for_input` waits forever.
**Fix:** `app$set_inputs(id = value, wait_ = FALSE)` for every placeholder set; then click the draw button explicitly and `app$wait_for_idle()`.
**Source:** `.claude/rules/testing.md`.

### A.4 — "Unable to find input binding" on a `var` picker
**Symptom:** test sets `<layer>_var_<param>` and the binding doesn't exist.
**Cause:** `var` pickers for source/consumer placeholders live in a `renderUI` under the layer's **"Data" subtab** (suspended until that subtab is shown). The widget isn't bound at boot.
**Fix:** set the source/consumer input first, then `set_inputs(<layer>_subtab = "Controls", wait_ = FALSE)`, `wait_for_idle()`, *then* set the var pickers.
**Source:** `.claude/rules/testing.md`; auto-memory `shinytest2-appdir-pkgload`.

### A.5 — `R CMD check` ERRORs on e2e file
**Symptom:** `devtools::check()` shows `R CMD check found ERRORs` from `test-vignette-e2e-*.R` with `pkgload "DESCRIPTION not found"`.
**Cause:** `devtools::check` runs the subprocess with `NOT_CRAN=true`, so `skip_on_cran()` doesn't fire. The browser tests *try to run* inside the `.Rcheck` sandbox where there is no `DESCRIPTION` for `pkgload::load_all()` to find.
**Fix:** the fixture's `boot_vignette_app()` helper has a source-root guard that **cleanly SKIPs** when `DESCRIPTION` is missing. If you see ERRORs (not clean SKIPs) under `check`, that guard is missing or broken — it's a **harness defect, not a product failure**, and you should not claim the suite is green off this.
**Source:** project `CLAUDE.md` "Authoritative gate" → "Proxy trap" block; auto-memory `shinytest2-appdir-pkgload`.

### A.6 — Bare `Rscript -e 'testthat::test_dir()'` shows green but skipped browser tests
**Symptom:** "tests pass, SKIP n" off a raw `Rscript` invocation.
**Cause:** without `NOT_CRAN=true`, `skip_on_cran()` fires and every shinytest2 test SKIPs. The harness silently hides them.
**Fix:** the authoritative gate is `NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", reporter="progress", stop_on_failure=FALSE)'`. Expected: **FAIL 0 / ERROR 0 / SKIP 0 / PASS N**. `devtools::test()` is equivalent (sets `NOT_CRAN=true` itself).
**Source:** project `CLAUDE.md` "Authoritative gate".

### A.7 — `AppDriver$new()` emits benign warnings that hide real ones
**Symptom:** "Failed to locate globals" / htmlDependency-prefix warnings drown out real signals.
**Cause:** Both warnings are benign and well-understood (see auto-memory `htmldependency-resource-prefix`).
**Fix:** wrap `AppDriver$new(...)` in scoped `suppressWarnings(...)`. **Do not blanket-suppress** assertion output — only the constructor.
**Source:** `.claude/rules/testing.md`.

## B. Test harness — registry / mocking / proxies

### B.1 — `devtools::test()` is the gate; raw `test_file` can split the registry
**Symptom:** custom-placeholder test passes under `devtools::test()`, fails under `Rscript -e 'testthat::test_file(...)'`.
**Cause:** `devtools::load_all` can split `.ptr_registry` across the package env and the namespace env when invoked in some test contexts. Custom placeholders become invisible to `ptr_translate` because the lookup hits the wrong env.
**Fix:** use `devtools::test()` / `devtools::test_local()` — they heal the split by running each file in a clean process. The harness gate is `devtools::test()`, not bare `test_file` / `test_dir`.
**Source:** auto-memory `feedback-harness-test_file-vs-devtools-test`.

### B.2 — `testServer` is NOT a proxy for `ptr_app`
**Symptom:** a `testServer(ptr_server, ...)` test that "works" but the real `ptr_app` doesn't reproduce the behavior.
**Cause:** `testServer(ptr_server, ...)` does not include `ptr_capture_formula`, the upload prologue wiring, shared-coordinator binding, or several other reactive pieces that `ptr_app` composes. Upload + shared code-gen are not faithfully reproduced.
**Fix:** **Never file a bug from an unreproduced `testServer` result.** Reproduce against a real `ptr_app(...)` (or a shinytest2 fixture) before filing. For unit-testing the pure pipeline, use `ptr_exec_headless` (internal) directly — it isolates the deterministic core.
**Source:** auto-memory `feedback-testserver-not-ptr-app`.

### B.3 — `assignInNamespace` fails to spy under `load_all`
**Symptom:** `assignInNamespace("fn", spy, ns="ggpaintr")` doesn't intercept the call; the spy captures `NULL`.
**Cause:** under `pkgload::load_all`, `assignInNamespace` may not patch the symbol the package itself dispatches against. The NULL capture is **not** evidence the function isn't called.
**Fix:** use `testthat::local_mocked_bindings(fn = spy, .package = "ggpaintr")` instead. Inside shinytest2 child processes (where parent-process bindings don't reach), write the instrumentation into the fixture's `app.R` via `assignInNamespace(..., ns = "ggpaintr")` *after* `pkgload::load_all` — the child does see this.
**Source:** auto-memory `project-mock-internal-calls`.

### B.4 — Parent-process `trace()` / `debugonce()` invisible to shinytest2 child
**Symptom:** `debugonce(ptr_setup_runtime)` in your REPL has no effect on the running shinytest2 child.
**Cause:** shinytest2 spawns a fresh child R process. Parent-process tracing doesn't reach the child.
**Fix:** instrument inside the fixture's `app.R`. Write `assignInNamespace("fn_name", wrapped_fn, ns = "ggpaintr")` after the `pkgload::load_all` line; the wrapped function can `message()`/`cat()` to surface state into the child's stdout, which `AppDriver` captures.
**Source:** `.claude/rules/serena-tools.md` "Shinytest2 child-process instrumentation"; auto-memory `shinytest2-appdir-pkgload`.

## C. Reactive / runtime traps

### C.1 — `shiny::debounce` + paired-observer race (the ADR 0025 lesson)
**Symptom:** observer A (debounced) and observer B (raw input) read the same `input[[id]]`. Behavior diverges from what either observer should produce in isolation.
**Cause:** wrapping one observer's `input[[id]]` in `debounce` inverts flush order vs. any sibling raw-input observer. The raw-input observer fires on every keystroke; the debounced observer fires 400ms later. Any cross-talk through DOM / session state (e.g. a JS round-trip from observer B that mutates input the debounced observer reads) races.
**Fix:** when debouncing an input, **audit every observer on the same id** and pass the *same debounced reactive* into all of them. ADR 0025 §7 A2: `ptr_bind_source_mutex` takes a `shortcut_r` arg so the mutex's text-side observer gates on the same debounced reactive that `resolve_upload_source` reads.
**Source:** auto-memory `shiny-debounce-mutex-race`; `R/paintr-server.R:1546-1584` (the actual fix); `04-invariants.md` VII.5.

### C.2 — `lapply(x, bare-namespace-generic)` skips S3 dispatch
**Symptom:** `lapply(nodes, paintr:::prune_walk)` returns a list of identical "default-method" outputs; never dispatches on class.
**Cause:** the bare namespace reference (`pkg:::fn`) resolves to the *generic*, not the dispatch site. `lapply` passes each element as the first arg; S3 dispatch needs to see a `UseMethod` call, which `lapply` swallows.
**Fix:** wrap explicitly: `lapply(nodes, function(n) prune_walk(n))` — this re-establishes the call site so `UseMethod` dispatches normally.
**Source:** auto-memory `pipeline-head-data-source` (this bit the prune walker's mid-pipeline upload resolution in 2026-05).

### C.3 — Stage with `stage_id` but unbound reactive runs unconditionally
**Symptom:** a stage that "should be disabled" runs anyway.
**Cause:** `disable_walk` drops a stage iff `isFALSE(stage_enabled[[sid]])`. NULL / missing reactives are **no-ops** (`04-invariants.md` III.1). A stage with a `stage_id` but no UI checkbox to bind it never gets the `FALSE` value.
**Fix:** ensure a `ppVerbSwitch` or a layer-level checkbox actually exists (or that `state$stage_enabled` is seeded explicitly). `stage_id` alone does nothing.
**Source:** ADR 0021; `04-invariants.md` III.1.

### C.4 — `state$server_ns_fn` / `state$ui_ns_fn` not for user code
**Symptom:** documentation example uses `state$server_ns_fn("my_id")` to wire a custom output.
**Cause:** these are internal plumbing per ADR 0006. They exist on `state` but are explicitly *not* the L3 escape hatch — `state$runtime()` is. Wiring custom outputs off `server_ns_fn` couples the user to internal namespacing decisions.
**Fix:** users wire `output[[ns("my_widget")]] <- renderXyz({ state$runtime()$plot })` where `ns` is their own `shiny::NS(id)` matching the `id` they passed to `ptr_server`. No need to reach into `state` for the ns fn.
**Source:** CONTEXT.md L19-28; ADR 0006; `04-invariants.md` V.5.

## D. UI / asset traps

### D.1 — `htmlDependency` does NOT register `addResourcePath`
**Symptom:** absolute-path asset references (e.g. `/ggpaintr/header-logo.svg`) 404 in some host configurations.
**Cause:** `htmlDependency` does not register the legacy `addResourcePath("ggpaintr", …)` prefix that the bundle expects. Removing the explicit `addResourcePath` call (thinking `htmlDependency` covered it) breaks asset refs.
**Fix:** keep the explicit `addResourcePath` registration in `core_assets_dep()` (`R/paintr-build-ui.R`). Do not "simplify" it away as a duplicate.
**Source:** auto-memory `htmldependency-resource-prefix`; `04-invariants.md` VII.4.

### D.2 — User CSS leaks between sessions on `R CMD check`
**Symptom:** under `devtools::check` only, the user-css `resourcePath` from one test bleeds into another.
**Cause:** `addResourcePath` registrations are global to the R session; under `check` the same session runs many tests.
**Fix:** the package has a `cleanup` helper that removes user-css resourcePaths after each shinytest2 test. Use it in fixture `app.R`s that set custom CSS.
**Source:** auto-memory `shinytest2-appdir-pkgload`.

## E. Debugging-method traps

### E.1 — DOM probes can fabricate "picker empty" bugs
**Symptom:** a `dev/scripts/<probe>.js` says "the column picker has 0 options"; bug filed.
**Cause:** DOM probes can see the picker pre-binding (renderUI not yet flushed) and report empty options. The actual binding then arrives milliseconds later; the bug doesn't reproduce.
**Fix:** before filing a picker-empty bug, cross-check with the R-side state (e.g. `runtime_upstream_cols(state, snapshot)`) and use `dev/scripts/audit-probe.js` which has the proper timing. **Two filed bugs (BUG-4, BUG-A1) were DOM-probe artifacts** — they didn't reproduce in actual interaction.
**Source:** auto-memory `browser-probe-pitfalls`.

### E.2 — Probe uses `devtools::load_all` but fixture uses `pkgload::load_all` → registry split
**Symptom:** a custom placeholder is registered in the probe session but `ptr_translate` can't find it.
**Cause:** `devtools::load_all` and `pkgload::load_all` can populate different copies of `.ptr_registry` depending on env attachment. If your probe is on one path and the fixture is on the other, they have different registries.
**Fix:** mirror the fixture's `pkgload::load_all` setup in probe scripts (or use `devtools::load_all` consistently across both).
**Source:** auto-memory `probe-devtools-vs-pkgload-registry`.

### E.3 — Presence-proxy assertions can hide propagation bugs
**Symptom:** `expect_dom_id("...")` / `expect_code_nonempty()` all pass, but the user sees the wrong column rendered.
**Cause:** these assertions check that *some* DOM/code exists, not that the *expected literal value* propagated through. A bug where a placeholder picks the wrong upstream column passes the presence check.
**Fix:** strengthen e2e assertions with **literal-nowhere-propagation** proofs ("the string `bad_value` appears *nowhere* in `code_text` after a re-render") and **populated-picker** checks ("the picker has options `c("a","b","c")`, not `character(0)`"). The first hid bug B1; presence proxies are not enough.
**Source:** auto-memory `e2e-assertion-weakness-lens`.

### E.4 — Vignette↔fixture drift: fixture is the authoritative side
**Symptom:** a sub-agent audit reports "vignette says X, code does Y."
**Cause:** the fixture `app.R`s sometimes carry an `# EXCEPT:` header documenting where the vignette is stale — the fixture is the up-to-date version, the vignette text is the stale side.
**Fix:** when an audit finding mentions a vignette↔code drift, **re-verify the audit against source first** before changing either side. A sub-agent's "vignette is wrong" conclusion may be reading a stale vignette line that the fixture's EXCEPT header already documented.
**Source:** auto-memory `vignette-fixture-drift-ledger`.

## F. Source-code shape traps

### F.1 — Anonymous-fn pipe stages are unsupported
**Symptom:** a formula like `mtcars |> (\(d) d[d$mpg > 20, ])() |> ggplot(...)` produces a stage with an unusable id/label.
**Cause:** `layer_call_name` (`R/paintr-translate.R`) falls back to `expr_text(head)` for lambdas — which produces an opaque string unusable as an id or display label.
**Fix:** users should use a named helper function as a pipe stage instead of an inline lambda. This is documented in the use-cases vignette.
**Source:** auto-memory `anonymous-fn-pipe-stage`.

### F.2 — `ppExpr` user input is *not* a string, it's an expression
**Symptom:** `validate_input` hook for a custom `ppExpr`-like keyword gets a `language` value, not a `character`.
**Cause:** ADR 0009's expression-mode input — `ppExpr`'s `resolve_expr` receives the user's typed text parsed into a language object, then `substitute_walk.ptr_ph_value` wraps the result with `ptr_user_expr` (`R/paintr-substitute.R:124-126`). If your custom keyword wants the *string*, mark it as a `value` placeholder, not an `expr` shape.
**Source:** `02-key-paths.md` Path E; `R/paintr-substitute.R:121-127`.

### F.3 — `ptr_extract_code(state)` is multi-line; `_plot` / `_error` are one-liners
**Symptom:** treating all three `ptr_extract_*` as identical wrappers.
**Cause:** `ptr_extract_plot` / `ptr_extract_error` are one-line `shiny::isolate(state$runtime()$<field>)`. `ptr_extract_code` additionally runs `format_code_with_extras` + `emit_upload_prologue` so the output matches the Code panel verbatim (with the `<name> <- read.csv(...)` prologue lines).
**Fix:** when reproducing the Code panel programmatically, always go through `ptr_extract_code`. Don't read `state$runtime()$code_text` directly — you'll miss the prologue.
**Source:** `R/paintr-server.R:3234-3239`; `02-key-paths.md` Path G.

### F.4 — Placeholder surface forms: `pp*` works as bare symbol *and* call; legacy names parse-but-don't-detect
**Symptom (writing about placeholders):** assuming `ppVar` must always appear as a function call (`ppVar()` / `ppVar(mpg)`), or claiming that the legacy unprefixed names "no longer parse" after ADR 0009. Both are wrong and have already led to a defective audit report (2026-05-27, manuscript Chapter 3 audit).
**Cause:** `placeholder_keyword` (`R/paintr-translate.R:704-708`) accepts both `is.symbol(expr)` (bare symbol branch) and the call-headed branch immediately after; `detect_placeholder` (`R/paintr-translate.R:687-702`) builds on it and looks up the registry entry. There is no syntactic requirement of parens. So every registered `pp*` name is recognized in **all three** surface forms:

  - bare symbol: `aes(x = ppVar, y = ppVar)` → 2 `ptr_ph_data_consumer` nodes
  - empty call: `aes(x = ppVar())` → 1 `ptr_ph_data_consumer` node
  - seeded default: `aes(x = ppVar(mpg))` → 1 `ptr_ph_data_consumer` node (seeded with `mpg`)

For the unprefixed legacy names (`var`/`text`/`num`/`expr`/`upload`), the registry has no entry, so `detect_placeholder` returns `NULL`. The formula still **parses** as ordinary R — it's just left as a normal call/symbol that the framework never owns. That has three distinct failure modes downstream:

  - **silent wrong:** `aes(x = var(mpg))` resolves to `stats::var(mpg)` (the variance scalar) at draw time; no error, semantically broken;
  - **eval error:** `labs(title = text())` / `geom_point(size = num())` / `upload() |> ...` fail with "could not find function" inside the eval'd expression;
  - **semantic garbage:** `expr()` is a base function returning an unevaluated expression — it runs cleanly and produces meaningless output downstream.

**Fix (writing/auditing):** never claim "bare keywords don't parse" — say "are not recognized as placeholders" and name the actual failure mode. Whenever doing a manuscript / vignette / doc audit on placeholder syntax, **empirically verify the surface forms** by running:

```r
suppressMessages(devtools::load_all(".", quiet=TRUE))
root <- ggpaintr:::ptr_translate("<formula string>")
phs  <- ggpaintr:::ptr_collect(root, function(n,...) ggpaintr:::is_ptr_placeholder(n))
```

and counting `length(phs)` for each surface variant before writing a finding. The 2026-05-27 audit report's F1 finding was substantively right (manuscript ch3 uses legacy names) but wrong about the mechanism ("no longer parse") — the wrong mechanism propagated into the proposed patch text. The empirical-verify-each-form step is the cheapest way to catch this.
**Source:** `R/paintr-translate.R:704-708` (`placeholder_keyword` — bare-symbol branch + call branch), `R/paintr-translate.R:687-702` (`detect_placeholder`), `R/paintr-builtins.R:265, 401-426` (current registry surface). 2026-05-27 manuscript ch3 audit report at `/Users/willju/Research/paintrPaper/preconsideration/notes/ggpaintr-expert-review-2026-05-27-1955.html` for the worked example.

## How to add a pitfall here

Promote a debugging lesson when **all three** are true:

1. It cost a real debugging cycle (a fix would have been faster if the next person knew this).
2. The *symptom* is misleading (the right fix isn't obvious from the error message).
3. The lesson is durable (would still apply after a major refactor — not a one-off bug).

Format: **Symptom → Cause → Fix → Source**. Keep each entry compact. The goal is to short-circuit the next debugging cycle, not to be exhaustive.
