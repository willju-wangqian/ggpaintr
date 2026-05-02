# Manual Checklist for `ggpaintr`

For known unresolved boundaries and reproducible edge-case examples, see
`tests/manual/ggpaintr-edge-cases.Rmd`.

## Core Shiny checks

1. Launch a `ptr_app()` app with static data and confirm the control tabs render.
2. Select `var`, `text`, `num`, and `expr` inputs and click `draw`.
3. Confirm the plot updates and `outputCode` matches the chosen inputs.
4. Disable one or more layer checkboxes and confirm the plot and code both update.
5. Enter a malformed `expr` value and confirm the app shows an inline error instead of failing silently.
6. Enter a faceting expression with a missing variable such as `~ Speciesasdf` and confirm the inline error channel shows the faceting error instead of raw Shiny `[object Object]`.
7. Launch a formula such as `aes(x = var + 1, y = log(var))`, confirm the picker still offers only column names, and confirm the generated code shows the selected columns inside those formula-level transforms.

## `checkbox_defaults` initial-state checks

1. Launch:

   ```r
   ptr_app(
     "ggplot(data = mtcars, aes(x = var, y = var)) +
        geom_point() + geom_smooth(method = text) + geom_line()",
     checkbox_defaults = list(geom_smooth = FALSE, geom_line = FALSE)
   )
   ```

   Confirm `geom_point` is checked and `geom_smooth` + `geom_line` are unchecked at startup. Toggle each on/off and confirm the plot updates.

2. Launch a formula with two `geom_point()` calls and pass `checkbox_defaults = list(geom_point = c(TRUE, FALSE))`. Confirm the first `geom_point` checkbox is on, the second is off.

3. Launch the same duplicate-layer formula with ``checkbox_defaults = list(`geom_point-2` = FALSE)``. Confirm only the second instance starts unchecked.

4. Pass `checkbox_defaults = list(geom_typo = FALSE)` against any formula. Confirm a console warning lists `geom_typo` as unknown and the app launches with all layers checked.

5. Pass `checkbox_defaults = list(geom_point = c(TRUE, FALSE, TRUE))` against a formula with two `geom_point()` calls. Confirm a console warning about the extra value, and the resulting state is first on / second off.

## Local data normalization checks

1. Create a local data frame with spaced or punctuated column names and pass it through `ptr_normalize_column_names()`.
2. Launch a `ptr_app()` formula against the normalized object.
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
2. Confirm the shell text shows `ggpaintr Plot Builder` and `Update plot`.
3. Confirm the upload controls show `Choose a data file`, `Optional dataset name`, and the accepted-format help text.
4. After upload, confirm prompts such as `Choose the x-axis column`, `Choose the y-axis column`, `Point size`, `Transparency`, `Plot title`, and `Facet by` appear.
5. Confirm no default control label contains parser-style text such as `argument 1`.
6. Launch an app with a custom `ui_text` list.
7. Confirm the targeted labels and placeholders use the custom wording while untargeted controls keep the defaults.
8. Confirm the customized app still renders the plot and generated code normally.

## Shiny integration checks

1. Launch an app built from `ptr_server_state()` plus the `ptr_register_*()` helpers.
2. Confirm the embedded app still renders the control panel, plot, inline errors, and code.
3. Add one small custom output or observer that reads `ptr_state$runtime()` and confirm it updates after `draw`.
4. Launch a variant with custom top-level ids from `ptr_build_ids()` and confirm the app still behaves normally.
5. Launch a variant with a custom `renderPlot()` that uses `ptr_extract_plot()`.
6. Confirm the custom plot renderer can add styling while the default error and code binders still work.
7. Confirm the custom plot renderer clears the plot when `ptr_extract_plot()` returns `NULL`.

## Placeholder registry checks

1. Launch an app with a custom placeholder registry built by `ptr_merge_placeholders()`.
2. Confirm the custom control renders with the UI returned by the placeholder's `build_ui()` hook.
3. Interact with the custom control and confirm the plot updates after `draw`.
4. Confirm `outputCode` contains the expression returned by the placeholder's `resolve_expr()` hook.
5. Launch a variant with custom `ui_text` targeting the custom placeholder keyword and confirm the custom label or help text appears.

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

## Layer switcher checks

1. Launch:

   ```r
   ptr_app("
   mtcars |> head(num) |> ggplot(aes(x = var, y = var)) +
     geom_point(size = num) + geom_smooth(method = text) + labs(title = text)
   ")
   ```

2. Confirm the control panel shows a `pickerInput()` labeled `Layer` with all four layer names; only one panel body is visible at a time.
3. With `ggplot` selected, confirm two sub-tabs appear (`Data`, `Controls`); Data holds the pipeline `num` control and an `Update data` button, Controls holds the `var` pickers.
4. With `geom_point` / `geom_smooth` / `labs` selected, confirm controls render flat (no inner sub-tabs) since none of those layers have a data pipeline.
5. Confirm `Update plot` is rendered exactly once, sitting below the tabset.
6. Confirm only the non-`ggplot` layer panels show an `Include this layer in the plot` checkbox at the top.
7. Toggle a layer's checkbox off and confirm the panel content visually grays out (`opacity ~ 0.5`) while the inputs stay clickable; confirm the layer disappears from both the plot and `outputCode`. Re-check and confirm everything restores.
8. Re-launch the app with `ui_text = list(shell = list(layer_picker = list(label = "Choose layer"), data_subtab = list(label = "Pipeline"), controls_subtab = list(label = "Aesthetics")))` and confirm those three labels override defaults while everything else (Update plot, Update data, layer checkbox) keeps default copy.

## Data-pipeline placeholder checks

1. Launch:

   ```r
   ptr_app("
   mtcars |> filter(cyl == num) |> head(num) |>
     ggplot(aes(x = var, y = var)) + geom_point()
   ")
   ```

2. In the `ggplot` layer's Data sub-tab, confirm the two `num` controls are labeled `Enter a number for filter()` and `Enter a number for head()` (verb-aware, not generic).
3. Confirm `var` pickers in the Controls sub-tab are empty until `Update data` succeeds.
4. Set `filter(num) = 6`, `head(num) = 10`, click `Update data`, and confirm the `var` pickers populate with mtcars columns.
5. Pick `x = mpg`, `y = disp`, click `Update plot`, and confirm the plot renders.
6. Change `head(num)` to `3` without clicking `Update data`. Confirm a stale-data signal appears on the `Update data` button (extra CSS class) and that `outputCode` and the plot still reflect the previous cache (`head(10)`), not `head(3)`.
7. Click `Update data`. Confirm the stale signal clears, `outputCode` now shows `head(3)`, and the `var` pickers retain `mpg`/`disp` (selection preserved).
8. Click `Update plot` and confirm the new plot is rendered.
9. Re-launch with two pipeline-bearing layers:

   ```r
   ptr_app("
   mtcars |> head(num) |> ggplot(aes(x = var, y = var)) +
     geom_point(data = mtcars |> dplyr::filter(num > 0), color = 'red')
   ")
   ```

   Confirm each layer panel has its own Data sub-tab and its own `Update data` button. Edit each separately and confirm the buttons fire independently — clicking `Update data` for `geom_point` does not refresh the `ggplot` cache, and vice versa.
10. Re-launch with a nested `data = ...` chain:

    ```r
    ptr_app("
    ggplot(
      data = mtcars |> filter(cyl == num) |> head(num),
      mapping = aes(x = var, y = var)
    ) + geom_point()
    ")
    ```

    Confirm pipeline controls still surface in the Data sub-tab and that `outputCode` after `Update data` + `Update plot` includes the full piped expression.
11. Launch a pipeline with a `text` placeholder, e.g. `iris |> dplyr::filter(Species != text) |> ggplot(aes(x = var, y = var)) + geom_point()`. Type `"setosa"` (with quotes), click `Update data`, then `Update plot`. Confirm the cached filter and `outputCode` use a single set of quotes (`Species != "setosa"`, not double-escaped).
