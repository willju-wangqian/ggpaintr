# Resolution table — 2026-05-13 audit follow-up

Source audit: `dev/audit/feature-coverage-2026-05-13.html` (commit `9ceedb1`).
Handoff plan: `dev/plans/bug-fix-2026-05-13.md`.

Branch: `core-rewrite-impl`. Test suite: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1152 ]`.
R CMD check (`--as-cran --no-manual`): `0 errors ✔ | 1 warning | 3 notes`.
The one warning (`ptr_init_state.Rd` undocumented `plots`) is pre-existing
on HEAD and untouched by this work.

| Bug   | Status     | Fix                                                                       | Regression test                                                          |
|-------|------------|---------------------------------------------------------------------------|--------------------------------------------------------------------------|
| BUG-A1 | **False positive — closed** (browser-verified, see note below) | None needed (R-side wiring already correct). | `tests/testthat/test-regression-bugs-2026-05-13.R::BUG-A1` — pins the invariant as a guard rail (every var consumer, including those under `aes()`, refreshes through `pds -> diamonds -> colvars subset` in a long pipeline). |
| BUG-A2 | Closed     | `collect_shared_placeholders()` + `ptr_make_app_server()`'s consumer rep-node + `ptr_app_grid()`'s rep-node now clear `node$param` when shared occurrences span more than one distinct param. Copy resolution then falls through to `defaults$<keyword>` instead of latching the first occurrence's `params$<param>$<keyword>` (which was overriding the user's `defaults$num$help` with the built-in alpha-specific hint). | `tests/testthat/test-regression-bugs-2026-05-13.R::BUG-A2` — unit (`ptr_resolve_ui_text("control", keyword="num", param=NULL)` returns the override) + integration (shared `num(shared='lvl')` widget rendered into `ptr_app()`'s sidebar carries the user's help, no alpha hint). |
| BUG-B1 | Closed     | `ptr_app_bslib(title = NULL)` default + new precedence block: an explicit `title=` wins; otherwise fall through to `ui_text$shell$title$label`, then the hardcoded default. New `@section Precedence` in `?ptr_app_bslib`. | `tests/testthat/test-regression-bugs-2026-05-13.R::BUG-B1` — two tests pinning both directions of the precedence. |

## BUG-A1 — verified false positive (R-side wiring is correct)

The audit's symptom — aes `var` pickers staying with empty option lists
after a `pick_ds` source switch, while per-stage `var` pickers refresh —
was reproduced in the browser only. An R-side reproduction under
`shiny::testServer()` cannot match it.

What was actually verified, via a `testServer` probe instrumented on
`ptr_builtin_var_build_ui()` to capture the `cols` vector reaching each
consumer's build_ui call on the long App-1 pipeline
(`pds |> head(num) |> select(colvars) |> mutate(metric = var + log(var)) |> filter(var > num) |> ggplot(aes(x = var, y = var, color = var))`):

```
--- pds=mpg ---
[var] id=ggplot_1_1_var_NA           ncols=11  cols=[manufacturer,model,displ,year,cyl,trans]
[var] id=ggplot_1_2_var_NA           ncols=11  cols=[manufacturer,model,displ,year,cyl,trans]
[var] id=ggplot_1_3_var_NA           ncols=11  cols=[manufacturer,model,displ,year,cyl,trans]
[var] id=ggplot_4_1_1_var_NA         ncols=11  cols=[manufacturer,model,displ,year,cyl,trans]
[var] id=ggplot_4_1_2_1_var_NA       ncols=11  cols=[manufacturer,model,displ,year,cyl,trans]
[var] id=ggplot_5_1_1_var_NA         ncols=11  cols=[manufacturer,model,displ,year,cyl,trans]

--- pds=diamonds ---
[var] id=ggplot_1_1_var_NA           ncols=10  cols=[carat,cut,color,clarity,depth,table]
... (every var consumer refreshes) ...

--- colvars=[carat,price,cut] ---
[var] id=ggplot_1_1_var_NA           ncols=3   cols=[carat,price,cut]
... (every var consumer narrows to the colvars subset) ...
```

That is, every consumer's `entry_reactive()` invalidates on the
`pick_ds`-style source change, `runtime_consumer_entry()` resolves the
upstream through `ptr_resolve_upstream()`, and the rebuilt picker's
`build_ui()` receives the new column vector. The aes-arg consumers and
the per-stage consumers go through the same code path; the audit's
hypothesis (aes consumers wired to a stale upstream-cols snapshot) is
contradicted by the trace.

The remaining (and, on present evidence, only) source of the audit's
observation is the same DOM-probe artifact that BUG-4 turned out to be
on 2026-05-12 (see `dev/tasks/bug-4-browser-followup.md`): a JS check
that reads `select.options.length` on a picker after a re-render misses
options that bootstrap-select / selectize.js has stashed off the
underlying `<select>`. The aes pickers and the per-stage pickers share
the same `shinyWidgets::pickerInput(..., multiple = TRUE, maxOptions = 1)`
constructor, so the same caveat applies to both — but a probe that
happens to read aes pickers at a different DOM lifecycle point than
per-stage pickers (e.g., post `renderUI` replacement vs post
`updatePickerInput`) can easily see different states.

### Browser verification (2026-05-13)

Booted `/tmp/ggp-launchers/app1a.R` against the current branch
(`core-rewrite-impl`), drove `pick_ds` from `mpg` to `diamonds` via
selectize (`sel.selectize.setValue('diamonds')`), waited 3s for the
producer-debounce, then read each aes picker through the
bootstrap-select-aware probe (`$(sel).find('option')`, since `var` uses
`shinyWidgets::pickerInput()` / bootstrap-select rather than
selectize). Result, post-switch:

```
ggplot_1_1_var_NA   nOpts=10  opts=[carat,cut,color,clarity,depth,table,price,x,y,z]
ggplot_1_2_var_NA   nOpts=10  opts=[carat,cut,color,clarity,depth,table,price,x,y,z]
ggplot_1_3_var_NA   nOpts=10  opts=[carat,cut,color,clarity,depth,table,price,x,y,z]
```

All three aes pickers do refresh to the diamonds columns. The audit's
"opts: []" reading is the same probe-level misread BUG-4 was: a JS
check that read the underlying `<select>`'s direct `<option>` children
at a moment when bootstrap-select / selectize had moved them out, on a
re-render. Marked closed in line with BUG-4.

The R-side guard rail in
`test-regression-bugs-2026-05-13.R::BUG-A1` stays as a backstop against
any future regression in the wiring path.

## Cross-references

- `dev/audit/feature-coverage-2026-05-13.html` — audit input.
- `dev/plans/bug-fix-2026-05-13.md` — handoff plan executed by this round.
- `dev/tasks/bug-4-browser-followup.md` — the prior selectize/picker DOM probe false positive (same shape as BUG-A1's evidence).
- `tests/testthat/test-regression-bugs-2026-05-13.R` — the three regression tests added by this round.
