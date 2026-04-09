# Manual Checklist for `ggpaintr`

For known unresolved boundaries and reproducible edge-case examples, see
`tests/manual/ggpaintr-edge-cases.Rmd`.

## Core Shiny checks

1. Launch a `ggpaintr_app()` app with static data and confirm the control tabs render.
2. Select `var`, `text`, `num`, and `expr` inputs and click `draw`.
3. Confirm the plot updates and `outputCode` matches the chosen inputs.
4. Disable one or more layer checkboxes and confirm the plot and code both update.
5. Enter a malformed `expr` value and confirm the app shows an inline error instead of failing silently.
6. Enter a faceting expression with a missing variable such as `~ Speciesasdf` and confirm the inline error channel shows the faceting error instead of raw Shiny `[object Object]`.
7. Launch a formula such as `aes(x = var + 1, y = log(var))`, confirm the picker still offers only column names, and confirm the generated code shows the selected columns inside those formula-level transforms.

## Local data normalization checks

1. Create a local data frame with spaced or punctuated column names and pass it through `ggpaintr_normalize_column_names()`.
2. Launch a `ggpaintr_app()` formula against the normalized object.
3. Confirm the `var` pickers show the normalized names and the plot/code render successfully.

## Unsupported boundary checks

1. Build a formula that uses `var` with no `data = ...` source.
2. Confirm `register_var_ui_outputs()` fails during UI preparation instead of creating broken `var` controls.
3. Confirm the error clearly says that data columns are not available for the affected layer.

## Upload checks

1. Launch a formula using `ggplot(data = upload, aes(x = var, y = var)) + geom_point()`.
2. Upload `simple_numeric.csv`.
3. Leave the dataset name blank and confirm the default name is derived from the filename.
4. Confirm `var` selectors appear only after the upload succeeds.
5. Draw the plot and confirm the code uses the resolved dataset object name.
6. Repeat with `simple_numeric.rds`.
7. Repeat with a custom dataset name and confirm the custom name is used.
8. Try `bad_extension.txt` and confirm the app shows a clear inline error without crashing.
9. Confirm the plot stays empty when upload completion fails.
10. Confirm upload-backed `var` pickers show normalized column names before draw.

## Layer-specific upload checks

1. Launch a formula where only one layer uses `data = upload`.
2. Confirm the uploaded dataset drives only the upload-backed `var` selectors.
3. Confirm the final plot still combines static-data and upload-data layers.

## Copy-rule checks

1. Launch an app that uses the default copy rules with upload, numeric, title, and `facet_wrap(expr)` controls.
2. Confirm the shell text shows `ggpaintr Plot Builder`, `Update plot`, and `Export Shiny app`.
3. Confirm the upload controls show `Choose a data file`, `Optional dataset name`, and the accepted-format help text.
4. After upload, confirm prompts such as `Choose the x-axis column`, `Choose the y-axis column`, `Point size`, `Transparency`, `Plot title`, and `Facet by` appear.
5. Confirm no default control label contains parser-style text such as `argument 1`.
6. Launch an app with a custom `copy_rules` list.
7. Confirm the targeted labels and placeholders use the custom wording while untargeted controls keep the defaults.
8. Confirm the customized app still renders the plot and generated code normally.

## Export checks

1. Export a generated app from a static-data formula and run it.
2. Export a generated app from an upload formula and run it.
3. In the exported upload app, repeat one CSV upload flow and one RDS upload flow.
4. Confirm the exported app still renders the plot and code correctly.
5. Confirm exported apps also show inline error messages for bad `expr` input and bad uploads.
6. Confirm exported apps clear the plot on failure and recover normally after a corrected redraw.
7. Confirm the default exported script exposes `copy_rules <- NULL`, explicit `ui <- ...`, explicit `server <- function(...)`, and `shinyApp(ui, server)`.
8. Confirm the exported shell labels are resolved with `ggpaintr_resolve_copy(..., copy_rules = copy_rules)`.
9. Confirm the exported server uses `ggpaintr_server(...)` and stores the returned `ggpaintr_state`.
10. Add one simple custom output or observer that reads `ggpaintr_state$runtime()` and confirm the app still works.
11. Export an app with custom `copy_rules` and confirm the exported script defines `custom_copy_rules <- ...`.
12. Confirm the exported script rebuilds `copy_rules` with `ggpaintr_effective_copy_rules(custom_copy_rules)`.
13. Confirm the exported script passes `copy_rules = copy_rules` into `ggpaintr_server(...)`.
14. Run the exported custom-copy app and confirm it preserves the customized wording from the live app.

## Shiny integration checks

1. Launch an app built from `ggpaintr_server_state()` plus the `ggpaintr_bind_*()` helpers.
2. Confirm the embedded app still renders the control panel, plot, inline errors, code, and export button.
3. Add one small custom output or observer that reads `ggpaintr_state$runtime()` and confirm it updates after `draw`.
4. Launch a variant with custom top-level ids from `ggpaintr_ids()` and confirm the app still behaves normally.
5. Launch a variant with a custom `renderPlot()` that uses `ggpaintr_plot_value()`.
6. Confirm the custom plot renderer can add styling while the default error and code binders still work.
7. Confirm the custom plot renderer clears the plot when `ggpaintr_plot_value()` returns `NULL`.

## Placeholder registry checks

1. Launch an app with a custom placeholder registry built by `ggpaintr_effective_placeholders()`.
2. Confirm the custom control renders with the UI returned by the placeholder's `build_ui()` hook.
3. Interact with the custom control and confirm the plot updates after `draw`.
4. Confirm `outputCode` contains the expression returned by the placeholder's `resolve_expr()` hook.
5. Launch a variant with custom `copy_rules` targeting the custom placeholder keyword and confirm the custom label or help text appears.
6. Export the custom-placeholder app and inspect the generated script.
7. Confirm the exported script defines `custom_placeholders <- ...`.
8. Confirm the exported script rebuilds `placeholders <- ggpaintr_effective_placeholders(custom_placeholders)`.
9. Confirm the exported server passes `placeholders = placeholders` into `ggpaintr_server(...)`.
10. Run the exported custom-placeholder app and confirm the custom control still works end to end.

## Missing-object checks

1. Launch a formula like `ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()`.
2. Click `draw`.
3. Confirm the app launches successfully before clicking `draw`.
4. Confirm the app shows an inline plot error mentioning the missing object.
5. Confirm the plot stays empty.
6. Confirm generated code remains visible when plot construction fails after completion succeeds.

## Recovery checks

1. Trigger a completion-stage error such as malformed `expr`.
2. Correct the input and click `draw` again.
3. Confirm the inline error disappears after the successful redraw.
4. Confirm the plot renders normally again.
5. Confirm `outputCode` updates to the corrected code.
