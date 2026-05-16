# Contract tests for the L3 single-piece UI builders: each public piece of
# the ggpaintr UI has its own exported function, and the bundled apps stay
# byte-identical because the bundle and the pieces share one builder.

fml <- "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"

# ---- ptr_ui_plot ----

test_that("ptr_ui_plot emits the plot id and (default) the inline error slot", {
  rendered <- as.character(ptr_ui_plot("m"))
  expect_match(rendered, "m-ptr_plot")
  expect_match(rendered, "m-ptr_error")
  expect_match(rendered, "ptr-card--plot")
  # no toggle button by default (standalone)
  expect_no_match(rendered, "ptr-code-toggle")
  # not a control / code piece
  expect_no_match(rendered, "m-ptr_layer_select")
  expect_no_match(rendered, "m-ptr_code")
})

test_that("ptr_ui_plot(error = FALSE) drops the nested error slot", {
  rendered <- as.character(ptr_ui_plot("m", error = FALSE))
  expect_match(rendered, "m-ptr_plot")
  expect_no_match(rendered, "m-ptr_error")
})

test_that("ptr_ui_plot(code_toggle = TRUE) adds the show-code button", {
  rendered <- as.character(ptr_ui_plot("m", code_toggle = TRUE))
  expect_match(rendered, "ptr-code-toggle")
})

# ---- ptr_ui_error ----

test_that("ptr_ui_error emits only the error output slot", {
  rendered <- as.character(ptr_ui_error("m"))
  expect_match(rendered, "m-ptr_error")
  expect_no_match(rendered, "m-ptr_plot")
  expect_no_match(rendered, "m-ptr_code")
})

# ---- ptr_ui_code ----

test_that("ptr_ui_code default style is a plain always-visible panel", {
  rendered <- as.character(ptr_ui_code("m"))
  expect_match(rendered, "m-ptr_code")
  expect_match(rendered, "ptr-card--code")
  # the slide-out chrome (and its hidden-until-toggled window) is opt-in
  expect_no_match(rendered, "ptr-code-window")
})

test_that("ptr_ui_code(style = 'window') is the slide-out chrome", {
  rendered <- as.character(ptr_ui_code("m", style = "window"))
  expect_match(rendered, "m-ptr_code")
  expect_match(rendered, "ptr-code-window")
  expect_match(rendered, "ptr-copy-btn")
})

test_that("ptr_ui_code rejects an unknown style", {
  expect_error(ptr_ui_code("m", style = "nope"))
})

# ---- ptr_ui_code_toggle: byte-identical bundled regression guard ----

test_that("ptr_ui_code_toggle reproduces the bundled .ptr-output block exactly", {
  # This is the core guarantee: ptr_app() / ptr_module_ui() render
  # ptr_outputs_panel(); the public toggle helper must emit the identical
  # DOM so behaviour and performance are unchanged by the split.
  expect_identical(
    as.character(ptr_ui_code_toggle("p1")),
    as.character(ptr_outputs_panel(shiny::NS("p1")))
  )
  expect_identical(
    as.character(ptr_ui_code_toggle()),
    as.character(ptr_outputs_panel(shiny::NS(NULL)))
  )
})

test_that("ptr_outputs_ui is a thin composite of the pieces", {
  rendered <- as.character(ptr_outputs_ui("x"))
  expect_match(rendered, "x-ptr_plot")
  expect_match(rendered, "x-ptr_error")
  expect_match(rendered, "x-ptr_code")
  expect_match(rendered, "ptr-code-toggle")
  expect_match(rendered, "ptr-output")
})

# ---- ptr_ui_controls vs the ptr_controls_ui composite ----

test_that("ptr_ui_controls emits control ids with NO .ptr-app wrapper or assets", {
  rendered <- as.character(ptr_ui_controls("x", fml))
  expect_match(rendered, "x-ptr_update_plot")
  expect_match(rendered, "x-ptr_layer_select")
  expect_match(rendered, "x-ptr_layer_tabset")
  # the piece is bare: no self-wrap, no bundled assets
  expect_no_match(rendered, "ptr-layer-disabled")
  expect_no_match(rendered, 'class="ptr-app"')
  expect_no_match(rendered, "ggpaintr.css")
})

test_that("ptr_controls_ui = ptr_ui_assets + ptr_ui_controls (still self-wrapped)", {
  rendered <- render_with_deps(ptr_controls_ui("x", fml))
  expect_match(rendered, "x-ptr_update_plot", fixed = TRUE)
  expect_match(rendered, "ggpaintr-layer", fixed = TRUE)  # carries assets
  expect_match(rendered, "ptr-app", fixed = TRUE)         # self-wraps
})

# ---- ptr_ui_assets ----

test_that("ptr_ui_assets emits the full bundle, == internal ptr_assets()", {
  expect_identical(ptr_ui_assets(), ptr_assets())
  rendered <- render_with_deps(ptr_ui_assets())
  expect_match(rendered, "ggpaintr.css", fixed = TRUE)         # cosmetic dep
  expect_match(rendered, "ggpaintr-layer.css", fixed = TRUE)   # structural dep
})

# ---- ptr_ui_header ----

test_that("ptr_ui_header renders the branded header; default title is ggpaintr", {
  expect_match(as.character(ptr_ui_header("My App")), "My App")
  expect_match(as.character(ptr_ui_header("My App")), "ptr-app__header")
  expect_match(as.character(ptr_ui_header()), "ggpaintr")
})

# ---- ptr_server(shared_state = ...) ----

test_that("ptr_server accepts a ptr_shared_server() bundle via shared_state", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp))  + geom_line()'
  )
  expect_silent({
    server <- function(input, output, session) {
      shared <- ptr_shared_server(formulas, envir = globalenv())
      ptr_server(
        input, output, session,
        formula = formulas[[1]],
        envir = globalenv(),
        shared_state = shared
      )
    }
    shiny::testServer(server, {})
  })
})

test_that("ptr_server rejects a non-ptr_shared_state shared_state", {
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_server(
          input, output, session,
          formula = fml,
          shared_state = list(shared = list(), draw_trigger = NULL)
        )
      }
      shiny::testServer(server, {})
    },
    "ptr_shared_state"
  )
})
