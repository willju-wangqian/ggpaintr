# Manual Checklist for `paintr2`

## Core Shiny checks

1. Launch a `ggpaintr_basic2()` app with static data and confirm the control tabs render.
2. Select `var`, `text`, `num`, and `expr` inputs and click `draw`.
3. Confirm the plot updates and `outputCode` matches the chosen inputs.
4. Disable one or more layer checkboxes and confirm the plot and code both update.

## Upload checks

1. Launch a formula using `ggplot(data = upload, aes(x = var, y = var)) + geom_point()`.
2. Upload `simple_numeric.csv`.
3. Leave the dataset name blank and confirm the default name is derived from the filename.
4. Confirm `var` selectors appear only after the upload succeeds.
5. Draw the plot and confirm the code uses the resolved dataset object name.
6. Repeat with `simple_numeric.rds`.
7. Repeat with a custom dataset name and confirm the custom name is used.
8. Try `bad_extension.txt` and confirm the app shows a clear error without crashing.

## Layer-specific upload checks

1. Launch a formula where only one layer uses `data = upload`.
2. Confirm the uploaded dataset drives only the upload-backed `var` selectors.
3. Confirm the final plot still combines static-data and upload-data layers.

## Export checks

1. Export a generated app from a static-data formula and run it.
2. Export a generated app from an upload formula and run it.
3. In the exported upload app, repeat one CSV upload flow and one RDS upload flow.
4. Confirm the exported app still renders the plot and code correctly.
