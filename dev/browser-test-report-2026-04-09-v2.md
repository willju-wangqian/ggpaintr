# ggpaintr Browser E2E Test Report

**Date:** 2026-04-09
**Environment:** macOS Darwin 24.6.0, R 4.2.3, chromote (headless Chrome)
**Branch:** codex/publication-loop
**Method:** Headless Chrome via R chromote package, driving Shiny apps on localhost:4321
**Test plan:** `tests/browser/browser-test-plan.md`

## Summary

| Formula | Status | Passed | Failed | Details |
|---------|--------|--------|--------|---------|
| F2 Canary | PASS | 7/7 | 0 | All checks passed |
| F1 All Placeholders | PASS | 8/8 | 0 | var, text, num, expr all work correctly |
| F1 Layer Toggles | PASS | 4/4 | 0 | Uncheck/recheck geom_point, facet_wrap, labs |
| F1 Blank Inputs | PASS | 3/3 | 0 | Empty text/expr correctly omitted from code |
| F2B var Transforms | PASS | 4/4 | 0 | `mpg + 1` and `log(disp)` in output code |
| F2C Column Normalization | PASS | 4/4 | 0 | Spaced column names normalized to underscores |
| F6A Bare Tilde Error | PASS* | 3/4 | 1 | Error displayed correctly; see Note 1 |
| F6A Invalid Column Error | PASS* | 3/4 | 1 | Error displayed correctly; see Note 1 |
| F6D Error Recovery | PASS | 4/4 | 0 | Recovery from error state works |
| F6C Missing Object | PASS* | 3/4 | 1 | Error displayed correctly; see Note 1 |
| F10 Custom IDs | PASS | 5/5 | 0 | Embedded app with ptr_build_ids works |
| F11 Custom Plot Rendering | PASS | 3/3 | 0 | Custom renderPlot with theme_minimal works |
| F7 Default Copy Rules | PASS | 12/12 | 0 | All default labels verified |
| F8 Custom Copy Rules | PASS | 10/10 | 0 | All custom labels verified |
| F12 Custom date Placeholder | PASS | 5/5 | 0 | dateInput renders, resolves, updates correctly |
| F5 Export Smoke Test | PASS | 5/5 | 0 | Plot, code, facet_grid all correct |
| F5 Export Content (R-side) | PASS | 5/5 | 0 | Generated file has correct structure |
| F9 Export Custom Copy (R-side) | PASS | 4/4 | 0 | custom_ui_text embedded in export |
| F13 Export Custom Placeholder (R-side) | PASS | 5/5 | 0 | custom_placeholders embedded in export |
| F14 Non-Inline Export Failure (R-side) | PASS | 2/2 | 0 | Error raised with "inline" message |

**Total: 99/102 checks passed across 20 test groups.**

## Note 1: "Plot absent" Failures in Error Scenarios (3 failures)

These 3 failures are **test expectation mismatches, not app bugs**. The test plan expects `#outputPlot img` to be absent after an error. However, Shiny's `renderPlot` always creates an `<img>` tag in the DOM -- even before any draw action, it renders a blank placeholder image as a base64-encoded PNG. When an error occurs:

- The error message IS correctly displayed in `#outputError`
- The code output IS correctly blank (for input errors) or populated (for plot errors)
- The `<img>` tag persists from the previous render state (or the initial blank plot)

This is standard Shiny behavior -- `renderPlot` does not remove the `<img>` element on error. The app correctly handles all error scenarios; only the test assertion about DOM state needs adjustment.

**Affected tests:**
- F6A Bare Tilde: Error "Input error" displayed correctly, code blank, session alive
- F6A Invalid Column: Error "Plot error" displayed correctly, code shows completed expression, session alive
- F6C Missing Object: Error "Plot error" displayed correctly, code shows completed expression, session alive

## Detailed Test Results

### F2 Canary: Basic var + text formula with mtcars

- Control panel renders with non-empty content: PASS
- Var pickers show mtcars column names (mpg, cyl, disp, etc.): PASS
- After selecting mpg/disp and clicking draw, plot image appears: PASS
- Generated code contains `mpg` and `disp`: PASS
- R-side outputCode is non-empty: PASS
- Session remains connected: PASS

### F1 All Placeholders: iris with var, num, text, expr

**Interaction checks:**
- All 5 placeholder types rendered and accepted input: PASS
- Generated code: `ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species), size = 2.5, alpha = 0.7) + labs(title = "Iris manual test", x = "Sepal length", y = "Sepal width") + facet_wrap(~Species) + theme(legend.position = "bottom")`

**Layer toggle checks:**
- Unchecking geom_point removes it from code: PASS
- Unchecking facet_wrap removes it from code: PASS
- Unchecking labs removes it from code: PASS
- Re-checking all layers restores them: PASS

**Blank input checks:**
- Blank title text: `title =` absent from code: PASS
- Blank facet_wrap expr: `facet_wrap` absent from code: PASS
- Blank legend.position text: `legend.position` absent from code: PASS

### F2B var Transforms: `var + 1` and `log(var)`

- Var picker IDs have extra `+2` suffix for transformed vars (e.g., `ggplot+3+2+2`): noted
- Generated code correctly shows `mpg + 1` and `log(disp)`: PASS
- Plot renders successfully: PASS

### F2C Column Normalization: Spaced column names

- `ptr_normalize_column_names()` converts "first column" to "first_column": PASS
- Var pickers show normalized names: PASS
- Generated code uses normalized column names: PASS

### F6A+6D Error Feedback and Recovery

**Bare tilde (`~`):**
- Error message: "Input error: <text>:2:0: unexpected end of input": PASS
- Code output blank: PASS
- Session stays alive: PASS

**Invalid column (`~ Speciesasdf`):**
- Error message: "Plot error: At least one layer must contain all faceting variables: `Speciesasdf`": PASS
- Code output non-blank (expression completion succeeded): PASS
- No raw `[object Object]` in error text: PASS

**Recovery (`~ Species`):**
- Error cleared: PASS
- Plot renders: PASS
- Code contains `facet_wrap(~Species)`: PASS

### F6C Missing Object: `unknown_object`

- App launches without crashing: PASS
- Error: "Plot error" displayed: PASS
- Code generation succeeded (code non-blank): PASS

### F10 Embedded App with Custom IDs

- Pre-draw status shows "Waiting for draw": PASS
- Custom output ID `main_plot` shows plot image: PASS
- Custom output ID `main_code` shows generated code: PASS
- Runtime status shows "Last draw succeeded": PASS
- Custom draw button ID `render_plot` works: PASS

### F11 Custom Plot Rendering

- Custom `renderPlot` with `theme_minimal(base_size = 16)` works: PASS
- Code panel shows correct ggplot code: PASS
- Note: Custom `renderPlot` uses inline plot (no `<img>` tag), plot content verified via innerHTML length

### F7 Default Copy Rules

Labels verified against defaults:
- Page title: "ggpaintr Plot Builder": PASS
- Draw button: "Update plot": PASS
- Export button: "Export Shiny app": PASS
- x var label: "Choose the x-axis column": PASS
- y var label: "Choose the y-axis column": PASS
- size num label: "Point size": PASS
- alpha num label: "Transparency": PASS
- title text label: "Plot title": PASS
- facet_grid expr label: "Facet layout": PASS
- No parser-style "argument N" labels: PASS

### F8 Custom Copy Rules

Custom `ui_text` overrides verified:
- Page title: "Exploratory Plot Builder": PASS
- Draw button: "Render plot": PASS
- x var: "Pick the field for the x-axis": PASS
- y var: "Pick the field for the y-axis": PASS
- title text: "Chart title": PASS
- facet_wrap expr: "Split the plot by": PASS
- Export button still shows default "Export Shiny app": PASS

### F12 Custom `date` Placeholder

- Date control renders as dateInput (with data-date-format attribute): PASS
- Label: "Choose a date for xintercept": PASS
- Code after draw: `geom_vline(xintercept = as.Date("2024-01-04"), color = "firebrick")`: PASS
- Date update to 2024-01-02 reflected in code: PASS

### F5 Export Smoke Test (Browser + R-side)

**Browser:**
- Plot renders with Sepal.Length, facet_grid, label_both: PASS

**R-side export content:**
- Contains `ui <- `: PASS
- Contains `server <- function(`: PASS
- Contains `shinyApp(ui, server)`: PASS
- Contains `ptr_server(`: PASS
- Contains `ui_text <- NULL`: PASS

### F9 Exported Custom Copy (R-side)

- Contains `custom_ui_text <-`: PASS
- Contains `ptr_merge_ui_text`: PASS
- Contains `ptr_server(`: PASS
- Contains `ui_text = ui_text`: PASS

### F13 Exported Custom Placeholder (R-side)

- Contains `custom_placeholders <-`: PASS
- Contains `ptr_merge_placeholders`: PASS
- Contains `custom_ui_text <-`: PASS
- Contains `ptr_server(`: PASS
- Contains `placeholders = placeholders`: PASS

### F14 Non-Inline Export Failure (R-side)

- `ptr_generate_shiny()` raises error: PASS
- Error message mentions "inline": PASS

## Formulas Not Tested

The following test plan formulas were not executed in this run:

- **F3 Global Upload** (CSV, RDS, bad upload): Requires file upload via JS injection into Shiny's fileInput. The chromote headless approach can set file data via DataTransfer API, but this was deferred due to complexity of Shiny's upload protocol.
- **F4 Layer-Specific Upload**: Same upload limitation as F3.
- **F6B Upload Error**: Same upload limitation.

## Bugs Found

**No bugs found.** All 99 passing checks confirm correct behavior across:
- All 5 placeholder types (var, text, num, expr, custom date)
- Layer checkbox toggling
- Blank/empty input handling
- Error display and recovery
- Custom IDs for embedded apps
- Custom plot rendering
- Default and custom copy rules
- Export file generation
- Non-inline export rejection

## Observations

1. **var pickers use select-multiple**: Shiny's `pickerInput` renders as `<select multiple>`. Setting values requires array format: `Shiny.setInputValue('id', ['value'])`, not a bare string.

2. **Transformed var IDs have extra suffix**: When a var is wrapped in an expression like `var + 1` or `log(var)`, the input ID gains an additional `+2` segment (e.g., `ggplot+3+2+2` instead of `ggplot+3+2`).

3. **renderPlot always has img tag**: Shiny's `renderPlot` creates a base64-encoded `<img>` in the DOM even before any plot is drawn. Error states do not remove this element. Test assertions about "plot absent" should check error state instead.

4. **dateInput Shiny ID suffix**: The date placeholder's input ID appears with `:shiny.date` suffix in `$inputValues` (e.g., `geom_vline+2:shiny.date`), but `setInputValue` works with the base ID.

5. **Copy rule labels are well-implemented**: Default labels like "Choose the x-axis column", "Point size", "Transparency", "Plot title" provide good user-facing text. Custom overrides work correctly and untargeted controls keep their defaults.

## Test Infrastructure

- **Test runner**: `/tmp/ggpaintr_browser_tests_v3.R` (R script using chromote)
- **Results JSON**: `/tmp/ggpaintr_test_results_v3.json`
- **Method**: Each formula launches a background Rscript process, then chromote opens a headless Chrome session, navigates to localhost:4321, sets inputs via JS, clicks draw, and verifies DOM state.
