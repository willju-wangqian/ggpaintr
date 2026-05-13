# BUG-4 — verified FALSE POSITIVE (resolved 2026-05-13)

Status: **not a bug**. Original report's DOM probe misread `shiny::selectInput()`'s rendered HTML.

## What the 2026-05-12 report claimed

App C (`ptr_app(ex3_formula)`) — after uploading a CSV into both `#ggplot_1_upload_NA` and `#geom_line_0_upload_NA`, the `select(colvars)` custom-consumer widget appears to render with `<select>` containing 0 options, while sibling built-in `var` consumers in the same pipeline populate normally.

## What's actually happening

`colvars`' `build_ui` (in `dev/scripts/feature-coverage-examples.R`) calls `shiny::selectInput(..., multiple = TRUE)`. That widget initializes **selectize.js**, which removes the `<option>` children from the underlying `<select>` element and stashes them on the `selectize` instance (`select.selectize.options`). Querying `selectInput.options.length` therefore returns 0 even when the widget is fully populated.

The built-in `var` build_ui uses `shinyWidgets::pickerInput()` (bootstrap-select), which keeps `<option>` children in place — so the same DOM probe (`select.options.length`) is meaningful for `var` but meaningless for `colvars`.

## Verification (2026-05-13, on `fix/feature-coverage-bugs-2026-05-12` @ 76cae03)

Live App C with instrumented `runtime_consumer_entry` and `invoke_build_ui`, drove the upload via Chrome MCP, then read the colvars widget's true state:

```
runtime_consumer_entry  -> cols = ["a","b"] for both colvars and every var consumer
invoke_build_ui         -> extra$cols = ["a","b"] reaches the colvars build_ui
colvars <select>.options.length === 0          (matches the report's reading)
colvars select.selectize.options === {a, b}    (real state — populated)
colvars select.selectize.open() dropdown       -> renders [a, b] as clickable items
```

Conclusion: the widget was always populated. The browser-test report's verdict was a probe-level misread, not a runtime failure.

## What stays as-is

- `61d8e98 fix(bug-4): pin runtime parity ...` — keep. The headless test it added (`tests/testthat/test-regression-bugs-2026-05-12.R::BUG-4`) pins a real, useful invariant: a custom `ptr_define_placeholder_consumer()` build_ui receives the same `cols` as the built-in `var` consumer at the same upstream pipeline depth. That invariant *would* break if anyone keyed `runtime_consumer_entry` or `invoke_build_ui`'s `extra` payload on the keyword in the future. Worth keeping as a guardrail.
- The HTML bug report's BUG-4 entry should be read alongside this file.

## Reusable browser probe for future custom-consumer checks

When verifying that a `selectInput`-based widget is populated, do NOT inspect `<option>` children. Instead, in browser JS:

```js
const sel = document.getElementById('<widget_id>');
sel.selectize
  ? Object.keys(sel.selectize.options)   // selectize-backed (shiny::selectInput)
  : Array.from(sel.options).map(o => o.value)  // plain <select> or pickerInput
```
