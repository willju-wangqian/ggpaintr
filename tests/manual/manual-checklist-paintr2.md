# Manual Checklist for `paintr2`

## Core Shiny checks

1. Launch a `ggpaintr_basic2()` app with static data and confirm the control tabs render.
2. Select `var`, `text`, `num`, and `expr` inputs and click `draw`.
3. Confirm the plot updates and `outputCode` matches the chosen inputs.
4. Disable one or more layer checkboxes and confirm the plot and code both update.
5. Enter a malformed `expr` value and confirm the app shows an inline error instead of failing silently.
6. Enter a faceting expression with a missing variable such as `~ Speciesasdf` and confirm the inline error channel shows the faceting error instead of raw Shiny `[object Object]`.

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

## Layer-specific upload checks

1. Launch a formula where only one layer uses `data = upload`.
2. Confirm the uploaded dataset drives only the upload-backed `var` selectors.
3. Confirm the final plot still combines static-data and upload-data layers.

## Export checks

1. Export a generated app from a static-data formula and run it.
2. Export a generated app from an upload formula and run it.
3. In the exported upload app, repeat one CSV upload flow and one RDS upload flow.
4. Confirm the exported app still renders the plot and code correctly.
5. Confirm exported apps also show inline error messages for bad `expr` input and bad uploads.
6. Confirm exported apps clear the plot on failure and recover normally after a corrected redraw.

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
