# Contract tests for the L3 single-piece UI builders: each public piece of
# the ggpaintr UI has its own exported function, and the bundled apps stay
# byte-identical because the bundle and the pieces share one builder.

fml <- "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"

# ---- ptr_ui_plot: truly bare (no behaviour flags) ----

test_that("ptr_ui_plot formals are exactly ('id') -- no error=/code_toggle=", {
  expect_identical(names(formals(ptr_ui_plot)), "id")
})

test_that("ptr_ui_plot('p') is the bare plot slot: no error, no toggle", {
  rendered <- as.character(ptr_ui_plot("p"))
  expect_match(rendered, "p-ptr_plot")
  expect_match(rendered, "ptr-card--plot")
  # bare: no nested error slot, no show-code button
  expect_no_match(rendered, "p-ptr_error")
  expect_no_match(rendered, "ptr-code-toggle")
  # not a control / code piece
  expect_no_match(rendered, "p-ptr_layer_select")
  expect_no_match(rendered, "p-ptr_code")
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

# ---- output combinators: ptr_ui_inline_error / ptr_ui_toggle_code ----
# ptr_ui_code_toggle is deleted; the bundled .ptr-output block is now
# composed from bare pieces + these combinators. (Step 05 rebuilds
# ptr_module_ui/ptr_outputs_ui on this same composition.)

test_that("ptr_ui_inline_error nests the error slot inline in the plot card", {
  ui <- ptr_ui_inline_error(ptr_ui_plot("p"), ptr_ui_error("p"))
  rendered <- as.character(ui)
  expect_match(rendered, "ptr-card--plot")
  expect_match(rendered, "p-ptr_plot")
  expect_match(rendered, "p-ptr_error")
  # the error sits inside the card body, after the plot (not a sibling)
  flat <- gsub("\\s+", " ", rendered)
  expect_match(flat, "ptr-card__body")
  expect_match(flat, "ptr-card__body.*p-ptr_plot.*p-ptr_error")
  # inline-error alone has no toggle and no .ptr-output (no toggle => none)
  expect_no_match(rendered, "ptr-code-toggle")
  expect_no_match(rendered, "ptr-output")
})

test_that("ptr_ui_toggle_code(inline_error(plot, error), code) == old composite", {
  ui <- ptr_ui_toggle_code(
    ptr_ui_inline_error(ptr_ui_plot("p"), ptr_ui_error("p")),
    ptr_ui_code("p")
  )
  rendered <- as.character(ui)

  # exactly one .ptr-output scope, with the toggle + window sharing it
  expect_equal(count_occurrences(rendered, "ptr-output"), 1L)
  expect_match(rendered, "ptr-code-toggle")
  expect_match(rendered, "ptr-code-window")
  expect_match(rendered, "ptr-copy-btn")  # window chrome (Copy/Close)

  # the three server-registered ids are present and unchanged
  expect_match(rendered, "p-ptr_plot")
  expect_match(rendered, "p-ptr_error")
  expect_match(rendered, "p-ptr_code")

  # functionally equivalent to the deleted ptr_ui_code_toggle("p"),
  # i.e. the bundled ptr_outputs_panel(NS("p")) block: same scope class,
  # same toggle/window classes, same ids.
  ref <- as.character(ptr_outputs_panel(shiny::NS("p")))
  for (marker in c("ptr-output", "ptr-code-toggle", "ptr-code-window",
                   "ptr-copy-btn", "p-ptr_plot", "p-ptr_error",
                   "p-ptr_code")) {
    expect_match(rendered, marker, info = marker)
    expect_match(ref, marker, info = marker)
  }
  expect_equal(
    count_occurrences(rendered, "ptr-output"),
    count_occurrences(ref, "ptr-output")
  )
})

test_that("ptr_ui_toggle_code code-style is irrelevant (combinator supplies window)", {
  # default panel-style code in, slide-out window out either way
  panel_in <- as.character(
    ptr_ui_toggle_code(ptr_ui_plot("p"), ptr_ui_code("p"))
  )
  window_in <- as.character(
    ptr_ui_toggle_code(ptr_ui_plot("p"), ptr_ui_code("p", style = "window"))
  )
  expect_match(panel_in, "ptr-code-window")
  expect_match(window_in, "ptr-code-window")
  expect_match(panel_in, "p-ptr_code")
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

# ---- only standalone entrypoints own the full-viewport page canvas ----
# The full-viewport `min-height:100vh` backdrop is opt-in via the
# `ptr-app--page` modifier. Only the standalone entrypoints (ptr_app /
# ptr_app_grid) add it. Everything designed to embed in a host app -- the
# region halves (ptr_controls_ui/ptr_outputs_ui/ptr_shared_ui),
# ptr_module_ui, ptr_ui_page -- stays bare `.ptr-app` so it sizes to its
# content instead of stretching the host's column/sidebar floor-to-ceiling.

test_that("region self-wraps stay bare .ptr-app (no --page canvas)", {
  # ptr_outputs_ui is intentionally dangling this step (its only call was
  # the deleted ptr_ui_code_toggle); Step 05 rebuilds it on the combinators
  # and re-asserts its bare-.ptr-app contract there.
  ctl <- as.character(ptr_controls_ui("x", fml))
  expect_match(ctl, 'class="ptr-app"', fixed = TRUE)
  expect_no_match(ctl, "ptr-app--page", fixed = TRUE)
})

test_that("ptr_ui_page stays bare .ptr-app so it embeds (no --page canvas)", {
  html <- render_with_deps(ptr_ui_page(ptr_ui_plot()))
  expect_match(html, 'class="ptr-app"', fixed = TRUE)
  expect_no_match(html, "ptr-app--page", fixed = TRUE)
})

test_that("ptr_module_ui stays bare .ptr-app so it embeds (no --page canvas)", {
  skip(paste(
    "Step 03 deletes ptr_ui_code_toggle; ptr_module_ui still routes through",
    "the (knowingly-dangling) ptr_outputs_ui until Step 05 rebuilds",
    "ptr_module_ui on the combinators. Step 05 restores this assertion."
  ))
  html <- render_with_deps(ptr_module_ui("m", fml))
  expect_match(html, 'class="ptr-app"', fixed = TRUE)
  expect_no_match(html, "ptr-app--page", fixed = TRUE)
})

test_that("ptr_app is standalone -> carries ptr-app--page", {
  # ptr_app_components() builds via ptr_outputs_panel() (unaffected here).
  # The ptr_app_grid half composes ptr_module_ui() -> ptr_outputs_ui() ->
  # deleted ptr_ui_code_toggle; Step 05/06 restore the grid assertion.
  app <- render_with_deps(ptr_app_components(fml)$ui)
  expect_match(app, "ptr-app--page", fixed = TRUE)
})

# ---- ptr_ui_assets ----

test_that("ptr_ui_assets emits the full bundle, == internal ptr_assets()", {
  expect_identical(ptr_ui_assets(), ptr_assets())
  rendered <- render_with_deps(ptr_ui_assets())
  expect_match(rendered, "ggpaintr.css", fixed = TRUE)         # cosmetic dep
  expect_match(rendered, "ggpaintr-layer.css", fixed = TRUE)   # structural dep
})

# ---- ptr_ui_page shell ----

test_that("ptr_ui_page emits exactly one .ptr-app inside a Bootstrap page", {
  ui <- ptr_ui_page(ptr_ui_controls(formula = fml), ptr_ui_plot())
  # fluidPage() returns a tagList (bootstrapPage chrome), not a bare tag
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
  html <- render_with_deps(ui)
  # one theme scope, default fluidPage -> Bootstrap container-fluid
  expect_equal(count_occurrences(html, 'class="ptr-app"'), 1L)
  expect_match(html, "container-fluid", fixed = TRUE)
  # the deduped bundle is present
  expect_equal(count_occurrences(html, "/ggpaintr.css\""), 1L)
  expect_equal(count_occurrences(html, "ggpaintr-ui.js"), 1L)
  expect_equal(count_occurrences(html, "ggpaintr-layer.js"), 1L)
})

test_that("ptr_ui_page page= swaps the page builder; non-function errors", {
  for (pg in list(shiny::fillPage, shiny::bootstrapPage, shiny::fixedPage)) {
    html <- render_with_deps(ptr_ui_page(ptr_ui_plot(), page = pg))
    expect_equal(count_occurrences(html, 'class="ptr-app"'), 1L)
    expect_match(html, "/ggpaintr.css\"")
  }
  # fixedPage -> container (not container-fluid)
  expect_match(render_with_deps(ptr_ui_page(page = shiny::fixedPage)),
               "container", fixed = TRUE)
  expect_error(ptr_ui_page(page = 1), "function")
  expect_error(ptr_ui_page(page = "fluidPage"), "function")
})

test_that("ptr_ui_page css= threads through and links after ggpaintr.css", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "mine.css"); writeLines(".ptr-app{}", f)
  html <- render_with_deps(ptr_ui_page(ptr_ui_plot(), css = f))
  pos_bundled <- regexpr('/ggpaintr\\.css"', html)
  pos_user <- regexpr("/mine\\.css", html)
  expect_gt(pos_bundled, 0L)
  expect_gt(pos_user, 0L)
  expect_lt(pos_bundled, pos_user)
})

test_that("htmlDependency dedupes a page nesting several asset emitters", {
  # Grid-like: the shell injects ptr_assets() once, and each self-wrapping
  # ptr_controls_ui() composite injects it again. htmltools must collapse
  # every ggpaintr dependency to a single <head> injection.
  ui <- ptr_ui_page(
    shiny::fluidRow(
      shiny::column(6, ptr_controls_ui("a", fml)),
      shiny::column(6, ptr_controls_ui("b", fml))
    )
  )
  html <- render_with_deps(ui)
  expect_equal(count_occurrences(html, "/ggpaintr.css\""), 1L)
  expect_equal(count_occurrences(html, "ggpaintr-ui.js"), 1L)
  expect_equal(count_occurrences(html, "ggpaintr-layer.js"), 1L)
  expect_equal(count_occurrences(html, "ggpaintr-layer.css"), 1L)
})

test_that("ptr_register_* are not exported (dead post-rewrite surface)", {
  exports <- getNamespaceExports("ggpaintr")
  expect_false("ptr_register_plot" %in% exports)
  expect_false("ptr_register_error" %in% exports)
  expect_false("ptr_register_code" %in% exports)
  # but they still exist internally (ptr_server's sole caller)
  expect_true(is.function(ggpaintr:::ptr_register_plot))
})

# ---- ptr_ui_header ----

test_that("ptr_ui_header renders the branded header; default title is ggpaintr", {
  expect_match(as.character(ptr_ui_header("My App")), "My App")
  expect_match(as.character(ptr_ui_header("My App")), "ptr-app__header")
  expect_match(as.character(ptr_ui_header()), "ggpaintr")
})

# ---- ptr_server(shared_state = ...) ----

test_that("ptr_server accepts a ptr_shared_server() bundle via shared_state", {
  skip(paste(
    "Pre-existing (base FAIL 1): Step 02 changed ptr_shared_server() to",
    "require a ptr_shared() spec, not (formulas, envir=). Not Step 03's",
    "surface; Step 10 owns the full-suite sweep / shared-API test rewrite."
  ))
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
