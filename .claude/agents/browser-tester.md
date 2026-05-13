---
name: browser-tester
extends: ewh:browser-tester
tools:
  - mcp__serena__find_symbol
  - mcp__serena__get_symbols_overview
  - mcp__serena__read_memory
  - mcp__serena__list_memories
---

## ggpaintr — picker-empty bug filing rules

Two prior audits filed "picker is empty" bugs that were probe-level
artifacts, not R-side issues:

- BUG-4 (2026-05-12, `dev/tasks/bug-4-browser-followup.md`) — a
  `shiny::selectInput(..., multiple = TRUE)` widget: selectize.js had
  moved the `<option>` children off the underlying `<select>`, so
  `select.options.length === 0` even though the widget was populated.
- BUG-A1 (2026-05-13, `dev/audit/feature-coverage-2026-05-13-resolution.md`)
  — a `shinyWidgets::pickerInput()` widget: the audit's DOM probe read
  the wrong element / read at the wrong lifecycle moment, and again
  saw zero options against a populated picker.

Both fixes were "none needed". The R-side trace was clean both times.

Do NOT file a "picker shows 0 options" bug against ggpaintr until both
of these gates pass:

1. **Use the widget-class-aware reader at `dev/scripts/audit-probe.js`.**
   Eval the file in the page once at the start of the audit
   (`fetch('http://127.0.0.1:4321/dev/scripts/audit-probe.js')` is NOT
   served by Shiny — paste the file's contents into a `javascript_tool`
   call). Then call `window.ggpaintrAuditProbe.readPicker(id)`. The
   helper dispatches on `selectpicker` / `selectize` / native and
   returns the option-value array the user will actually see. Never
   call `document.getElementById(id).options` directly on a ggpaintr
   `var`/`colvars`/`pick_ds`-style picker.

2. **R-side cross-check before filing.** Before reporting a picker as
   empty, attach a spy to the relevant build_ui in the running app
   (e.g. `ptr_builtin_var_build_ui` for `var` consumers) that prints
   `cols` per consumer id, then re-execute the probe sequence. If R
   prints non-empty `cols` for the id the DOM probe says is empty, the
   bug is a probe artifact — file it as such (a one-line note in the
   audit report, mirroring `dev/tasks/bug-4-browser-followup.md`), do
   not include it in the "fix me" list.

The cross-check is cheap: a single `assignInNamespace()` swap in the
running R session takes seconds and conclusively rules out the
probe-artifact case. Skipping it cost the project two rounds of
unnecessary debugging.

## Files to read before starting an audit

- `dev/scripts/feature-coverage-examples.R` — the source for the audit
  formulas (`ex1_formula`, `ex2_*`, `ex3_*`) and the placeholder
  registrations the launchers depend on.
- `dev/scripts/audit-probe.js` — the widget-aware reader described
  above.
- `dev/tasks/bug-4-browser-followup.md` and
  `dev/audit/feature-coverage-2026-05-13-resolution.md` — both probe
  artifacts, both worth reading before filing a similar-shaped bug.
