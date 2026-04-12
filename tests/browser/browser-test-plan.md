# ggpaintr Browser Test Plan

This file defines declarative browser-level E2E tests for ggpaintr Shiny apps.
It mirrors the manual test workbook at `tests/manual/manual-test-ggpaintr.Rmd`.

Executed by the `browser-tester` agent (`.claude/agents/browser-tester.md`).

## Conventions

- **Input IDs** are discovered at runtime via `Object.keys(Shiny.shinyapp.$inputValues)`.
  Human-readable hints below use the parameter name (e.g., `x var`, `y var`, `size num`)
  but the agent resolves to actual Shiny IDs like `ggplot+3+2`.
- **Actions**: `select` (pickerInput), `type` (text/num input), `upload` (JS-injected file),
  `click` (button), `uncheck`/`check` (checkbox).
- **Checks**: `dom` (element state), `r-side` (JS query into Shiny reactive values),
  `console` (no R errors in browser console).
- **Upload fixtures** live in `tests/testthat/fixtures/`.

---

## Canary: Formula 2 (Run First)

Simplest formula — validates the entire infrastructure before the full suite.

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = mtcars, aes(x = var, y = var)) +
      geom_point() +
      labs(title = text)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. `x var` → select `mpg`
2. `y var` → select `disp`
3. `title text` → type `var picker boundary`
4. Click `draw`

### Checks

- dom: Each `var` control renders as a pickerInput with mtcars column names as choices
- dom: No free-text expression entry offered in var pickers
- dom: `#outputPlot img` is present after draw
- dom: `#outputCode` contains `mpg` and `disp`
- r-side: `Shiny.shinyapp.$values.outputCode` is non-empty
- console: No R errors

---

## Formula 1: Static All-Placeholders Interaction

Exercises `var`, `text`, `num`, `expr`, and layer checkbox behavior.

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = iris, aes(x = var, y = var)) +
      geom_point(aes(color = var), size = num, alpha = num) +
      labs(title = text, x = text, y = text) +
      facet_wrap(expr) +
      theme(legend.position = text)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. `x var` → select `Sepal.Length`
2. `y var` → select `Sepal.Width`
3. `color var` → select `Species`
4. `size num` → type `2.5`
5. `alpha num` → type `0.7`
6. `title text` → type `Iris manual test`
7. `x text` → type `Sepal length`
8. `y text` → type `Sepal width`
9. `facet_wrap expr` → type `~ Species`
10. `legend.position text` → type `bottom`
11. Click `draw`

### Checks

- dom: Control tabs render for all layers
- dom: `#outputPlot img` present after draw
- dom: `#outputCode` contains `Sepal.Length`, `Sepal.Width`, `Species`
- r-side: `Shiny.shinyapp.$values.outputCode` contains `geom_point` and `facet_wrap`
- console: No R errors

### Layer Toggle Checks

1. Uncheck `geom_point` → click `draw`
   - dom: `#outputCode` does not contain `geom_point`
2. Uncheck `facet_wrap` → click `draw`
   - dom: `#outputCode` does not contain `facet_wrap`
3. Uncheck `labs` → click `draw`
   - dom: `#outputCode` does not contain `labs`
4. Re-check all layers → click `draw`
   - dom: `#outputCode` contains all layers again

### Blank Input Checks

1. Clear `title text` → click `draw`
   - dom: `#outputCode` does not contain `title =`
2. Clear `facet_wrap expr` → click `draw`
   - dom: `#outputCode` does not contain `facet_wrap`
3. Clear `legend.position text` → click `draw`
   - dom: `#outputCode` does not contain `legend.position`

---

## Formula 2B: Formula-Level var Transforms

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = mtcars, aes(x = var + 1, y = log(var))) +
      geom_point() +
      labs(title = text)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. `x var` → select `mpg`
2. `y var` → select `disp`
3. `title text` → type `transformed var mapping`
4. Click `draw`

### Checks

- dom: Pickers offer only column names (not expressions)
- dom: `#outputCode` contains `mpg + 1` and `log(disp)`
- dom: `#outputPlot img` present after draw
- console: No R errors

---

## Formula 2C: Local Column Normalization

### Setup Objects

```r
spaced_manual <- data.frame(left = 1:4, right = c(2, 4, 6, 8), check.names = FALSE)
names(spaced_manual) <- c("first column", "second column")
spaced_manual_clean <- ggpaintr::ptr_normalize_column_names(spaced_manual)
```

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = spaced_manual_clean, aes(x = var, y = var)) +
      geom_point() +
      labs(title = text)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. `x var` → select `first_column`
2. `y var` → select `second_column`
3. `title text` → type `normalized local data`
4. Click `draw`

### Checks

- dom: Var pickers show `first_column` and `second_column`
- dom: `#outputCode` contains `first_column` and `second_column`
- dom: `#outputPlot img` present after draw
- console: No R errors

---

## Formula 3: Global Upload

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = upload, aes(x = var, y = var)) +
      geom_point(size = num) +
      labs(title = 'good')
  "),
  port = 4321, launch.browser = FALSE
)
```

### Pre-Upload Checks

- dom: `var` selectors are not available (deferred uiOutput is empty or placeholder)

### Inputs — CSV Upload

1. `upload file` → upload `tests/testthat/fixtures/simple_numeric.csv`
2. Leave dataset name blank
3. Wait for `var` pickers to appear
4. `x var` → select `x`
5. `y var` → select `y`
6. `size num` → type `2`
7. Click `draw`

### Checks — CSV Upload

- dom: `var` selectors appeared after upload
- dom: `#outputPlot img` present after draw
- dom: `#outputCode` uses a filename-derived object name (not blank)
- r-side: Plot rendered successfully
- console: No R errors

### Inputs — RDS Upload (restart app)

1. `upload file` → upload `tests/testthat/fixtures/simple_numeric.rds`
2. `dataset name` → type `manual_upload_data`
3. Wait for `var` pickers to appear
4. `x var` → select `x`
5. `y var` → select `y`
6. `size num` → type `2`
7. Click `draw`

### Checks — RDS Upload

- dom: `#outputCode` contains `manual_upload_data`
- dom: `#outputPlot img` present
- console: No R errors

### Inputs — Bad Upload (restart app)

1. `upload file` → upload `tests/testthat/fixtures/bad_extension.txt`
2. Click `draw`

### Checks — Bad Upload

- dom: `#outputError` contains `Input error`
- dom: `var` selectors did not become usable
- dom: `#outputPlot img` is absent
- dom: `#outputCode` is blank
- console: No crash

---

## Formula 4: Layer-Specific Upload

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point(aes(color = var), size = 2) +
      geom_point(data = upload, aes(x = var, y = var, color = var), size = num) +
      labs(title = text)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Pre-Upload Checks

- dom: Static iris layer controls are available
- dom: Upload-backed layer `var` controls are deferred/empty

### Inputs

1. `iris color var` → select `Species`
2. `upload file` → upload `tests/testthat/fixtures/simple_numeric.csv`
3. Wait for upload-layer `var` pickers
4. `upload x var` → select `x`
5. `upload y var` → select `y`
6. `upload color var` → select `group`
7. `upload size num` → type `3`
8. `title text` → type `mixed static/upload plot`
9. Click `draw`

### Checks

- dom: `#outputPlot img` present
- dom: `#outputCode` contains both `iris` and uploaded data references
- console: No R errors

### Layer Toggle Check

1. Uncheck upload-backed `geom_point` (ID hint: `geom_point-2+checkbox` — duplicate layers use `-N` suffix) → click `draw`
   - dom: `#outputPlot img` still present (static layer only)
   - dom: `#outputCode` does not contain upload data reference

---

## Formula 6A+6D: Error Feedback and Recovery — Malformed expr

### Batch with: F6D (same app, single launch)

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point() +
      facet_wrap(expr)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Error Scenario: Bare Tilde

1. `facet_wrap expr` → type `~`
2. Click `draw`

### Checks — Bare Tilde

- dom: `#outputError` contains `Input error`
- dom: `#outputPlot` shows a blank canvas (img element is always present because `renderPlot` renders `plot.new()`)
- dom: `#outputCode` is blank
- r-side: Session is still responsive

### Error Scenario: Invalid Column

1. `facet_wrap expr` → type `~ Speciesasdf`
2. Click `draw`

### Checks — Invalid Column

- dom: `#outputError` contains `Plot error`
- dom: `#outputPlot` shows a blank canvas (img always present via `renderPlot`)
- dom: `#outputCode` is non-blank (completion succeeded)
- dom: No raw `Error: [object Object]` text visible

### Recovery Scenario (formerly F6D)

1. `facet_wrap expr` → type `~ Species`
2. Click `draw`

### Checks — Recovery

- dom: `#outputError` is empty or hidden (error cleared)
- dom: `#outputPlot img` present
- dom: `#outputCode` contains `facet_wrap(~ Species)`
- r-side: Session remains responsive

---

## Formula 6B: Error Feedback — Upload Error

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = upload, aes(x = var, y = var)) +
      geom_point()
  "),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. `upload file` → upload `tests/testthat/fixtures/bad_extension.txt`
2. Click `draw`

### Checks

- dom: `#outputError` contains `Input error`
- dom: `var` selectors not usable
- dom: `#outputPlot` shows a blank canvas (img always present via `renderPlot`)
- dom: `#outputCode` blank

---

## Formula 6C: Error Feedback — Missing Object

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = unknown_object, aes(x = mpg, y = disp)) +
      geom_point()
  "),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. Click `draw`

### Checks

- dom: App launched without crashing
- dom: `#outputError` contains `Plot error`
- dom: `#outputPlot` shows a blank canvas (img always present via `renderPlot`)
- dom: `#outputCode` is non-blank (code generation succeeded)

---

---

## Formula 7: Default Copy Rules

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app("
    ggplot(data = upload, aes(x = var, y = var)) +
      geom_point(size = num, alpha = num) +
      labs(title = text) +
      facet_grid(expr)
  "),
  port = 4321, launch.browser = FALSE
)
```

### Pre-Upload Checks (Copy Text)

- dom: Page title contains `ggpaintr Plot Builder`
- dom: Draw button text is `Update plot`
- dom: Export button text is `Export Shiny app`
- dom: Upload control label contains `Choose a data file`
- dom: Dataset name label contains `Optional dataset name`
- dom: Upload help text mentions `.csv` and `.rds`

### Inputs

1. `upload file` → upload `tests/testthat/fixtures/simple_numeric.csv`
2. Wait for `var` pickers

### Post-Upload Checks (Copy Text)

- dom: `x var` label contains `x-axis column`
- dom: `y var` label contains `y-axis column`
- dom: `size num` label contains `Point size`
- dom: `alpha num` label contains `Transparency`
- dom: Alpha help text mentions values between `0` and `1`
- dom: `title text` label contains `Plot title`
- dom: Faceting label contains `Facet by`
- dom: No control label contains `argument 1` or similar parser-style text

### Inputs (continued)

1. `x var` → select `x`
2. `y var` → select `y`
3. `size num` → type `2`
4. `alpha num` → type `0.8`
5. `title text` → type `Default copy test`
6. `facet_grid expr` → type `~ group`
7. Click `draw`

### Checks

- dom: `#outputPlot img` present
- dom: `#outputCode` reflects formula inputs, not UI wording
- console: No R errors

---

## Formula 8: Custom Copy Rules

### Launch

```r
custom_ui_text <- list(
  shell = list(
    title = list(label = "Exploratory Plot Builder"),
    draw_button = list(label = "Render plot")
  ),
  params = list(
    x = list(var = list(label = "Pick the field for the x-axis")),
    y = list(var = list(label = "Pick the field for the y-axis")),
    title = list(text = list(label = "Chart title"))
  ),
  layers = list(
    facet_wrap = list(
      expr = list(
        `__unnamed__` = list(
          label = "Split the plot by",
          placeholder = "~ Species"
        )
      )
    )
  )
)

shiny::runApp(
  ggpaintr::ptr_app(
    "
    ggplot(data = iris, aes(x = var, y = var)) +
      geom_point() +
      labs(title = text) +
      facet_wrap(expr)
    ",
    ui_text = custom_ui_text
  ),
  port = 4321, launch.browser = FALSE
)
```

### Copy Text Checks

- dom: Page title contains `Exploratory Plot Builder`
- dom: Draw button text is `Render plot`
- dom: `x var` label contains `Pick the field for the x-axis`
- dom: `y var` label contains `Pick the field for the y-axis`
- dom: `title text` label contains `Chart title`
- dom: Faceting label contains `Split the plot by`
- dom: Export button still reads `Export Shiny app` (untargeted default)
- dom: Layer checkboxes still read `Include this layer` (untargeted default)

### Inputs

1. `x var` → select `Sepal.Length`
2. `y var` → select `Sepal.Width`
3. `title text` → type `Custom copy test`
4. `facet_wrap expr` → type `~ Species`
5. Click `Render plot`

### Checks

- dom: `#outputPlot img` present
- dom: `#outputCode` unchanged by custom copy (reflects formula, not UI text)
- console: No R errors

---

## Formula 10: Embedded App with Custom IDs

### Launch

```r
manual_ids <- ggpaintr::ptr_build_ids(
  control_panel = "builder_controls",
  draw_button = "render_plot",
  plot_output = "main_plot",
  error_output = "main_error",
  code_output = "main_code"
)

ui <- shiny::fluidPage(
  shiny::titlePanel("Embedded ggpaintr manual test"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      ggpaintr::ptr_input_ui(ids = manual_ids),
      shiny::tags$hr(),
      shiny::textOutput("runtimeStatus")
    ),
    shiny::mainPanel(
      ggpaintr::ptr_output_ui(ids = manual_ids)
    )
  )
)

server <- function(input, output, session) {
  paintr_state <- ggpaintr::ptr_server_state(
    "
    ggplot(data = iris, aes(x = var, y = var)) +
      geom_point(aes(color = var), size = num) +
      labs(title = text)
    ",
    ids = manual_ids
  )
  ggpaintr::ptr_setup_controls(input, output, paintr_state, ids = manual_ids)
  ggpaintr::ptr_register_draw(input, paintr_state, ids = manual_ids)
  ggpaintr::ptr_register_plot(output, paintr_state, ids = manual_ids)
  ggpaintr::ptr_register_error(output, paintr_state, ids = manual_ids)
  ggpaintr::ptr_register_code(output, paintr_state, ids = manual_ids)

  output$runtimeStatus <- shiny::renderText({
    runtime_result <- paintr_state$runtime()
    if (is.null(runtime_result)) return("Waiting for draw")
    if (isTRUE(runtime_result$ok)) return(paste("Last draw succeeded:", runtime_result$code_text))
    runtime_result$message
  })
}

shiny::runApp(shiny::shinyApp(ui, server), port = 4321, launch.browser = FALSE)
```

### Pre-Draw Check

- dom: `#runtimeStatus` contains `Waiting for draw`

### Inputs

1. `x var` → select `Sepal.Length`
2. `y var` → select `Sepal.Width`
3. `color var` → select `Species`
4. `size num` → type `2`
5. `title text` → type `Embedded app test`
6. Click draw button (id: `render_plot`)

### Checks

- dom: `#main_plot img` present (custom output ID)
- dom: `#main_code` contains `Sepal.Length`
- dom: `#runtimeStatus` contains `Last draw succeeded`
- r-side: Draw button fired (verify via `Shiny.shinyapp.$values` containing updated plot/code, not via `$inputValues` key — action button input keys lack the `:shiny.action` suffix in the JS-side registry)
- console: No R errors

---

## Formula 11: Custom Plot Rendering

### Launch

```r
ui <- shiny::fluidPage(
  shiny::titlePanel("Custom plot rendering"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(ggpaintr::ptr_input_ui()),
    shiny::mainPanel(
      shiny::plotOutput("outputPlot"),
      shiny::uiOutput("outputError"),
      shiny::verbatimTextOutput("outputCode")
    )
  )
)

server <- function(input, output, session) {
  paintr_state <- ggpaintr::ptr_server_state("
    ggplot(data = iris, aes(x = var, y = var)) +
      geom_point() + labs(title = text)
  ")
  ggpaintr::ptr_setup_controls(input, output, paintr_state)
  ggpaintr::ptr_register_draw(input, paintr_state)
  ggpaintr::ptr_register_error(output, paintr_state)
  ggpaintr::ptr_register_code(output, paintr_state)

  output$outputPlot <- shiny::renderPlot({
    plot_obj <- ggpaintr::ptr_extract_plot(paintr_state$runtime())
    if (is.null(plot_obj)) { plot.new(); return(invisible(NULL)) }
    plot_obj + ggplot2::theme_minimal(base_size = 16)
  })
}

shiny::runApp(shiny::shinyApp(ui, server), port = 4321, launch.browser = FALSE)
```

### Inputs

1. `x var` → select `Sepal.Length`
2. `y var` → select `Sepal.Width`
3. `title text` → type `Custom theme test`
4. Click `draw`

### Checks

- dom: `#outputPlot img` present
- dom: `#outputCode` contains `Sepal.Length`
- r-side: Plot value is non-null after draw
- console: No R errors

---

## Formula 12: Custom date Placeholder

### Setup Objects

```r
sales_manual <- data.frame(
  day = as.Date("2024-01-01") + 0:5,
  value = c(10, 13, 12, 16, 18, 17),
  segment = c("A", "A", "B", "B", "C", "C")
)

date_placeholder <- ggpaintr::ptr_define_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(as.character(value), "")) {
      return(ggpaintr::ptr_missing_expr())
    }
    rlang::expr(as.Date(!!as.character(value)))
  },
  copy_defaults = list(label = "Choose a date for {param}")
)

date_placeholders <- ggpaintr::ptr_merge_placeholders(
  list(date = date_placeholder)
)
```

### Launch

```r
shiny::runApp(
  ggpaintr::ptr_app(
    "
    ggplot(data = sales_manual, aes(x = day, y = value)) +
      geom_line() +
      geom_point(size = num) +
      geom_vline(xintercept = date, color = text) +
      labs(title = text)
    ",
    placeholders = date_placeholders
  ),
  port = 4321, launch.browser = FALSE
)
```

### Inputs

1. `size num` → type `2`
2. `color text` → type `firebrick`
3. `title text` → type `Date placeholder test`
4. `xintercept date` → set to `2024-01-04`
5. Click `draw`

### Checks

- dom: Date control renders as a date picker (not a text input)
- dom: Date label contains `Choose a date for xintercept`
- dom: `#outputPlot img` present
- dom: `#outputCode` contains `as.Date("2024-01-04")`
- console: No R errors

### Update Check

1. Change date to `2024-01-02`
2. Click `draw`
   - dom: `#outputCode` contains `as.Date("2024-01-02")`

---

## Runtime Assembly Edge Cases

Tested as sub-checks within Formula 1 and Formula 3/4:

1. Uncheck all optional layers in Formula 1 → click `draw`
   - dom: `#outputPlot img` still present (valid blank base plot)
   - dom: `#outputCode` contains only `ggplot(...)` call
2. Re-check layers → click `draw`
   - dom: App remains responsive, plot updates normally
