.app_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- ptr_app ----

test_that("ptr_app returns a shinyApp object", {
  app <- ptr_app(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_components returns a UI tag and server function", {
  parts <- ptr_app_components(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  expect_true(inherits(parts$ui, "shiny.tag") || inherits(parts$ui, "shiny.tag.list"))
  expect_true(is.function(parts$server))
})

test_that("ptr_app_components UI emits expected output ids", {
  parts <- ptr_app_components(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  rendered <- as.character(parts$ui)
  expect_match(rendered, "ptr_plot")
  expect_match(rendered, "ptr_code")
  expect_match(rendered, "ptr_error")
  expect_match(rendered, "ptr_layer_select")
})

test_that("ptr_app emits an Update Plot trigger button", {
  # Spec L142 + BDD G11.12: standalone `ptr_app` carries an Update Plot
  # button so the plot is gated behind an explicit click rather than
  # re-rendering on every keystroke.
  parts <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = ppVar, y = ppVar)) + geom_point()",
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, 'id="ptr_update_plot"', fixed = TRUE)
})

# ---- module variants — namespacing isolation (E6) ----

test_that("ptr_server ids are namespaced by id", {
  ui <- ptr_ui(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    "m1"
  )
  rendered <- as.character(ui)
  expect_match(rendered, "m1-ptr_plot")
  expect_match(rendered, "m1-ptr_layer_select")
})

test_that("two ptr_server instances do not collide", {
  ui_a <- ptr_ui(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()", "a")
  ui_b <- ptr_ui(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()", "b")
  expect_no_match(as.character(ui_a), "b-ptr_plot")
  expect_no_match(as.character(ui_b), "a-ptr_plot")
})

# ---- Level-2 split UI: ptr_controls_ui / ptr_outputs_ui ----

test_that("ptr_ui_controls emits the control ids (no output ids)", {
  ui <- ptr_ui_controls(
    "ggplot(data = mtcars, aes(x = ppVar, y = ppVar)) + geom_point()",
    "x"
  )
  rendered <- as.character(ui)
  expect_match(rendered, "x-ptr_update_plot")
  expect_match(rendered, "x-ptr_layer_select")
  expect_match(rendered, "x-ptr_layer_tabset")
  # Orthogonality: a bare L3 piece carries only the widget deps it needs,
  # NOT the ggpaintr bundle (assets come from ptr_ui_assets / ptr_ui_page /
  # ptr_ui — asserted in test-asset-bundle / test-ui-pieces).
  dep_names <- vapply(htmltools::findDependencies(ui),
                      function(d) d$name, character(1))
  expect_false("ggpaintr-layer" %in% dep_names)
  # outputs live in the output combinators, not here
  expect_no_match(rendered, "x-ptr_plot")
  expect_no_match(rendered, "x-ptr_code")
})

test_that("output combinators emit the output ids (no control ids)", {
  ui <- ptr_ui_toggle_code(
    ptr_ui_inline_error(ptr_ui_plot("x"), ptr_ui_error("x")),
    ptr_ui_code("x")
  )
  rendered <- as.character(ui)
  expect_match(rendered, "x-ptr_plot")
  expect_match(rendered, "x-ptr_error")
  expect_match(rendered, "x-ptr_code")
  expect_no_match(rendered, "x-ptr_layer_select")
})

# ---- ptr_app_grid (BDD P12.16) ----

test_that("ptr_app_grid returns a shiny app object", {
  app <- ptr_app_grid(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = ppNum(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point(size = ppNum(shared = "sz"))'
    ),
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_grid_components UI contains shared widget id and per-plot module ids", {
  parts <- ptr_app_grid_components(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = ppNum(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point(size = ppNum(shared = "sz"))'
    ),
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  # PR-B (shared-multi-instance): shared widget ids use the canonical
  # `shared_<key>` form. `shared_ui` removed (see ?ptr_shared); the `sz`
  # value key now auto-renders from `ppNum`'s own `build_ui`, emitted as the
  # deferred `uiOutput` container `shared_sz_ui` (the static widget at
  # `shared_sz` is filled server-side), matching P06.f in test-rewrite-shared.
  expect_match(ui_html, "id=\"shared_sz_ui\"", fixed = TRUE)
  expect_match(ui_html, "plot_1-", fixed = TRUE)
  expect_match(ui_html, "plot_2-", fixed = TRUE)
})

test_that("ptr_app_grid works with no shared controls", {
  app <- ptr_app_grid(
    plots = list("ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point()"),
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_grid rejects empty plots list", {
  expect_error(
    ptr_app_grid(plots = list()),
    "length\\(plots\\)"
  )
})

test_that("ptr_app_grid rejects non-string plot entries", {
  expect_error(
    ptr_app_grid(plots = list(42)),
    "is_string"
  )
})

# Removed: the two tests below covered `shared_ui` argument validation
# (unique non-empty names; function entries), both gone with the argument
# (see ?ptr_shared "Removed `shared_ui`"). Retained commented for provenance:
#
# test_that("ptr_app_grid rejects shared_ui without unique non-empty names", {
#   expect_error(
#     ptr_app_grid(
#       plots = list(
#         'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), size = ppNum(shared = "sz"))'
#       ),
#       shared_ui = list(function(id) shiny::sliderInput(id, "x", 1, 10, 5)),
#       envir = .app_test_env()
#     ),
#     "unique non-empty names"
#   )
# })
#
# test_that("ptr_app_grid rejects non-function shared_ui entries", {
#   expect_error(
#     ptr_app_grid(
#       plots = list(
#         'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), size = ppNum(shared = "sz"))'
#       ),
#       shared_ui = list(sz = "not a function"),
#       envir = .app_test_env()
#     ),
#     "must be a function"
#   )
# })

test_that("ptr_app_grid_components UI includes the draw-all button", {
  # Post-PR-B (shared-multi-instance plan): the draw-all button is owned
  # by `ptr_shared_ui()` and gated on `length(plots) >= 2` together with
  # at least one shared placeholder somewhere across the formulas. Use a
  # two-plot grid sharing one `ppVar()` consumer to exercise it.
  parts <- ptr_app_grid_components(
    plots = list(
      'ggplot(mtcars) + geom_point(aes(x = ppVar(shared = "c"), y = mpg))',
      'ggplot(mtcars) + geom_line(aes(x = ppVar(shared = "c"), y = hp))'
    ),
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "ptr_shared_draw_all", fixed = TRUE)
})

test_that("ptr_app_grid_components draw-all button label is configurable", {
  parts <- ptr_app_grid_components(
    plots = list(
      'ggplot(mtcars) + geom_point(aes(x = ppVar(shared = "c"), y = mpg))',
      'ggplot(mtcars) + geom_line(aes(x = ppVar(shared = "c"), y = hp))'
    ),
    draw_all_label = "Refresh everything",
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "Refresh everything", fixed = TRUE)
})
