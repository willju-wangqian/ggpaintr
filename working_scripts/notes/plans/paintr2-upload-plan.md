# Plan: Add Real `upload` Support to `paintr2`

## Summary

The idea is doable with the current `paintr2` architecture.

Current state after rereading the repo note and code:
- `upload` is already recognized as a placeholder token during parsing and replacement.
- The current UI for `upload` is only a dataset picker, not a real local file upload control.
- The server side assumes `input[[id]]` is the name of an existing object and calls `get(...)`, so it does not yet load files from the user’s machine.
- The package already contains a working upload pattern in the bundled app (`inst/shiny/ggpaintr_app`) that can be reused conceptually.
- I cannot literally clear memory or start a new chat from here, but I refreshed context by rereading [working_scripts/notes/paintr2-overview.md](/Users/willju/Research/ggpaintr/working_scripts/notes/paintr2-overview.md) and tracing the relevant source paths.

Defaults chosen for the implementation plan:
- Support file types: `csv` and `rds`
- Support scope: both global `ggplot(data = upload, ...)` and layer-specific `geom_*(data = upload, ...)`
- Exported apps should preserve the same upload-and-name behavior

## Current Placeholder Implementation Map

### Parsing and keyword detection
- Placeholder target list: [R/paintr2_func.R#L86](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L86)
- Keyword detection for `var`, `text`, `num`, `expr`, `upload`: [R/paintr2_func.R#L126](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L126)
- Formula parsing and UI metadata construction: [R/paintr2_func.R#L411](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L411)

### UI-side placeholder implementations
- `upload` UI generator: [R/ui_function.R#L1](/Users/willju/Research/ggpaintr/R/ui_function.R#L1)
- `text` UI generator: [R/ui_function.R#L22](/Users/willju/Research/ggpaintr/R/ui_function.R#L22)
- `num` UI generator: [R/ui_function.R#L34](/Users/willju/Research/ggpaintr/R/ui_function.R#L34)
- `expr` UI generator: [R/ui_function.R#L46](/Users/willju/Research/ggpaintr/R/ui_function.R#L46)
- `var` placeholder UI generator: [R/ui_function.R#L15](/Users/willju/Research/ggpaintr/R/ui_function.R#L15)
- `var` picker renderer: [R/ui_function.R#L58](/Users/willju/Research/ggpaintr/R/ui_function.R#L58)
- Placeholder-to-UI dispatch: [R/ui_function.R#L81](/Users/willju/Research/ggpaintr/R/ui_function.R#L81)

### Server-side placeholder implementations
- `var` substitution: [R/paintr2_func.R#L154](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L154)
- `num` substitution: [R/paintr2_func.R#L167](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L167)
- `text` substitution: [R/paintr2_func.R#L177](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L177)
- `expr` substitution: [R/paintr2_func.R#L188](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L188)
- `upload` substitution stub: [R/paintr2_func.R#L197](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L197)
- Main substitution loop: [R/paintr2_func.R#L512](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L512)

### Dynamic `var` UI behavior tied to uploaded data
- Global `data = upload` detection: [R/ui_function.R#L126](/Users/willju/Research/ggpaintr/R/ui_function.R#L126)
- Layer-level `data = upload` handling: [R/ui_function.R#L161](/Users/willju/Research/ggpaintr/R/ui_function.R#L161)
- Current lookup mechanism uses `get(input[[id]])`: [R/ui_function.R#L163](/Users/willju/Research/ggpaintr/R/ui_function.R#L163) and [R/ui_function.R#L179](/Users/willju/Research/ggpaintr/R/ui_function.R#L179)

### Exported app generation
- Exported app template builder: [R/paintr2_func.R#L598](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L598)
- Export writer: [R/paintr2_func.R#L647](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L647)

### Existing upload precedent elsewhere in package
- Bundled app file upload UI: [inst/shiny/ggpaintr_app/ui.R#L35](/Users/willju/Research/ggpaintr/inst/shiny/ggpaintr_app/ui.R#L35)
- Bundled app file parsing with `read.csv` / `readRDS`: [inst/shiny/ggpaintr_app/server.R#L17](/Users/willju/Research/ggpaintr/inst/shiny/ggpaintr_app/server.R#L17)

## Feasibility Assessment

`upload` is not complete today.

Why it is incomplete:
- `generate_ui_upload()` currently renders a `pickerInput()` of built-in dataset names, not `fileInput()`: [R/ui_function.R#L1](/Users/willju/Research/ggpaintr/R/ui_function.R#L1)
- `handle_upload()` only parses a string into an expression and is marked with a comment saying it needs to be fixed for real data upload: [R/paintr2_func.R#L197](/Users/willju/Research/ggpaintr/R/paintr2_func.R#L197)
- `output_embed_var()` expects `input[[upload_id]]` to be the name of an object retrievable with `get(...)`, not a Shiny file upload payload: [R/ui_function.R#L163](/Users/willju/Research/ggpaintr/R/ui_function.R#L163)
- The exported app template does not currently contain any upload-specific runtime code.

Despite that, the feature is a strong fit for the current design:
- `paintr_formula()` already discovers `upload` placeholders cleanly.
- `output_embed_var()` already branches on `data = upload`, so the architecture already knows where uploaded datasets should affect `var` pickers.
- `paintr_complete_expr()` already has a single substitution pass where upload expressions can be rewritten.
- The package already includes proven `csv`/`rds` loading logic in another app.

## Implementation Guideline

### 1. Define the v1 `upload` contract

Treat each `upload` placeholder as a composite input concept with two UI controls:
- a `fileInput()` for dataset upload
- a `textInput()` for the dataset object name

Behavior:
- If the name field is filled, use it as the dataset object name.
- If the name field is blank, derive the object name from the uploaded file name without extension.
- The uploaded dataset must become available to the running Shiny app as a server-side object reference usable by the `paintr2` expression pipeline.
- Supported file types are `csv` and `rds`.
- Invalid file types should produce a Shiny validation message and prevent plotting.
- Uploaded data must be usable both for populating `var` choices and for final `ggplot2` evaluation.

### 2. Introduce a stable internal shape for upload controls

For every upload placeholder ID, generate a small UI bundle rather than a single input:
- file control ID: the base upload ID
- name control ID: a deterministic derived ID such as `paste0(base_id, "+name")`

Implementation rule:
- Keep the base upload ID reserved for the actual uploaded file payload because other parts of the code already key off that placeholder ID.
- Add helper functions for deriving companion IDs so UI generation, server loading, and export code all use the same naming scheme.

### 3. Change the UI generator for `upload`

Replace the current built-in dataset picker in `generate_ui_upload()` with a composite UI that contains:
- `fileInput()` accepting `.csv` and `.rds`
- `textInput()` labeled as dataset/object name
- optional short help text explaining that blank name defaults to the file name

Important design choice:
- `generate_ui_individual()` should keep returning one UI object per placeholder. For `upload`, that object should be a container such as `tagList(...)` holding both controls.
- Preserve the existing `attr(ui, "ui_expr")` pattern so exported app generation still has a path to emit UI text later.

### 4. Add server-side upload loading and naming

Add a dedicated helper layer in `R/paintr2_func.R` or `R/ui_function.R` for:
- reading the uploaded file
- validating extension
- deriving the dataset object name
- making both dataset object and object name available to the rest of the app

Recommended runtime shape:
- maintain a reactive store keyed by upload placeholder ID
- each key stores:
  - `data`: the parsed data frame / tibble / object from `read.csv` or `readRDS`
  - `object_name`: resolved dataset name
  - `code_text`: code-like representation for generated code, such as `read.csv("file.csv")` or `readRDS("file.rds")`

Reason for this shape:
- `output_embed_var()` needs the in-memory data object for `names(data)`
- `paintr_complete_expr()` needs the resolved object name or inline expression for plot code generation
- exported app generation will need both the UI IDs and server loading logic

### 5. Replace the current `get(input[[id]])` upload lookup path

Refactor `output_embed_var()` so upload-backed data resolution no longer uses `get(input[[id]])`.

Instead:
- resolve the dataset via a shared upload-data accessor
- use that accessor in both places that currently branch on `data = upload`:
  - layer-level data lookup
  - global `ggplot(data = upload)` lookup

Required behavior:
- before upload, `var` UI for upload-backed expressions should stay absent or empty
- after upload succeeds, `var` UI should render with column names from the uploaded dataset
- if the uploaded object is not tabular or has no column names, show a validation error and do not render `var` selectors

### 6. Update `handle_upload()` and formula completion semantics

Change `handle_upload()` so it no longer assumes `input_item` is directly parsable user text.

New rule:
- for runtime plot evaluation, replace `upload` with the resolved dataset object name expression
- for code generation, replace `upload` with a reproducible code expression that matches the upload source contract

Recommended behavior:
- in the live app, assign the uploaded object into a controlled environment and substitute the symbol name into the expression
- in generated code, emit code that reconstructs the same upload handling path rather than trying to embed the current in-memory dataset

This keeps `ggplot(data = upload, ...)` working naturally after substitution.

### 7. Update export generation to preserve upload support

The exported app must include:
- the upload UI bundle
- upload parsing helpers
- runtime storage of uploaded datasets
- default object-name derivation
- the same dynamic `var` behavior after upload
- the same validation on file types

Implementation strategy:
- extend `get_shiny_template()` so the template contains explicit hooks for upload helper code, not just generic UI text replacement
- generate any required helper functions or observer blocks when the `paintr_obj` contains at least one `upload` placeholder
- keep the non-upload template path unchanged when no `upload` placeholder is present

### 8. Keep scope intentionally narrow for v1

Do not include these in the first implementation unless required by failing tests:
- multiple-file upload
- Excel or TSV parsing
- persistent upload storage across sessions
- automatic sanitization for invalid R object names beyond a minimal safe normalization
- support for non-tabular uploads when `var` placeholders are present

Minimal safe name normalization for v1:
- strip file extension when defaulting from filename
- convert spaces and punctuation to `_`
- if the result is not a syntactic name, normalize with `make.names()`

## Testing and Acceptance Criteria

### Functional scenarios
- `ggplot(data = upload, aes(x = var, y = var)) + geom_point()`:
  - upload a CSV
  - leave dataset name blank
  - `var` selectors appear with uploaded column names
  - plot renders
- same formula, but provide a custom dataset name:
  - the resulting expression uses that name
  - code output reflects the chosen name
- layer-specific upload:
  - `ggplot(data = iris, aes(...)) + geom_point(data = upload, aes(...))`
  - upload-backed `var` choices are resolved from the uploaded dataset for that layer
- upload an `.rds` tabular object:
  - selectors populate and plot renders
- unsupported extension:
  - user sees a validation message
  - no plot is attempted
- upload present but no file selected:
  - upload-backed `var` controls do not render
  - draw does not crash the app
- blank custom name:
  - fallback name is derived from the file name
- exported app:
  - same CSV and RDS scenarios work after download

### Regression checks
- Existing `var`, `text`, `num`, and `expr` flows without upload continue to work unchanged.
- Formulas with no `upload` placeholder still export and run normally.
- `data = upload` in either top-level `ggplot()` or a later layer does not break checkbox logic or code output assembly.

## Assumptions

- Uploaded datasets are expected to be tabular for the main `var` workflow.
- `csv` files are read with base `read.csv()` for consistency with the bundled app.
- `rds` files are read with `readRDS()`.
- The implementation should reuse existing patterns from the bundled `ggpaintr_app` where practical, but the `paintr2` path should keep its own formula-driven architecture rather than depending on that app’s server code.
- The stray `browser()` call in `generate_ui_individual()` should be removed or bypassed during implementation because it would break normal use when an `expr` placeholder is present.
