# Behaviour: every raw-Shiny entry point routes through the ptr_assets()
# bundle and exposes the same `css =` override portal in the same order
# (the structural-layer dependency, then ggpaintr.css, then user css).
# ggpaintr.css / the JS handlers ship as htmlDependency objects post
# refactor, so render dependencies + body together (see helper-assets.R).

render_ui <- function(ui) {
  render_with_deps(ui)
}

linked_csses <- function(html) {
  m <- regmatches(
    html,
    gregexpr('<link[^>]*href="([^"]+\\.css)"', html, perl = TRUE)
  )[[1L]]
  sub('.*href="([^"]+\\.css)".*', "\\1", m)
}

fixture_formula <- "ggplot(mtcars, aes(x = var, y = var)) + geom_point()"

write_temp_css <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  f <- file.path(dir, "user.css")
  writeLines(".ptr-app { background: hotpink; }", f)
  f
}

mount_calls <- function(css = NULL) {
  list(
    ptr_app          = ptr_app_components(fixture_formula, css = css)$ui,
    ptr_app_grid     = ptr_app_grid_components(list(fixture_formula), css = css)$ui,
    ptr_module_ui    = ptr_module_ui("m", fixture_formula, css = css),
    ptr_controls_ui  = ptr_controls_ui("p", fixture_formula, css = css),
    ptr_outputs_ui   = ptr_outputs_ui("p", css = css),
    ptr_shared_ui    = ptr_shared_ui(
      formulas = list(
        "ggplot(mtcars) + geom_point(aes(x = var(shared = \"x\")))",
        "ggplot(mtcars) + geom_point(aes(x = var(shared = \"x\")))"
      ),
      css = css
    )
  )
}

test_that("every raw-Shiny entry point self-wraps in .ptr-app", {
  uis <- mount_calls()
  for (nm in names(uis)) {
    html <- render_ui(uis[[nm]])
    expect_match(html, "class=\"ptr-app", fixed = TRUE,
                 info = paste("entry point:", nm))
  }
})

test_that("every raw-Shiny entry point accepts css = path", {
  fns <- list(
    ptr_app          = ptr_app,
    ptr_app_grid     = ptr_app_grid,
    ptr_module_ui    = ptr_module_ui,
    ptr_controls_ui  = ptr_controls_ui,
    ptr_outputs_ui   = ptr_outputs_ui,
    ptr_shared_ui    = ptr_shared_ui
  )
  for (nm in names(fns)) {
    expect_true("css" %in% names(formals(fns[[nm]])),
                info = paste("missing css arg in", nm))
  }

  f <- write_temp_css()
  uis <- mount_calls(css = f)
  base <- basename(f)
  for (nm in names(uis)) {
    html <- render_ui(uis[[nm]])
    expect_match(html, base, fixed = TRUE,
                 info = paste("user css link missing in", nm))
  }
})

test_that("user css links after ggpaintr.css", {
  f <- write_temp_css()
  uis <- mount_calls(css = f)
  base <- basename(f)
  for (nm in names(uis)) {
    html <- render_ui(uis[[nm]])
    pos_bundled <- regexpr('/ggpaintr\\.css"', html)
    pos_user <- regexpr(paste0("/", base), html)
    expect_gt(pos_bundled, 0L)
    expect_gt(pos_user, 0L)
    expect_lt(pos_bundled, pos_user)
  }
})

test_that("htmlDependency dedupes the bundle under nested injection", {
  # ptr_module_ui() nests controls + outputs, emitting ptr_assets() three
  # times; htmltools collapses each dependency to one <head> injection.
  ui <- ptr_module_ui("m", fixture_formula)
  html <- render_ui(ui)
  expect_equal(count_occurrences(html, "ggpaintr-layer.js"), 1L)
  expect_equal(count_occurrences(html, "ggpaintr-ui.js"), 1L)
  expect_equal(count_occurrences(html, "/ggpaintr.css\""), 1L)
})

test_that("ptr_shared_ui accepts css = path", {
  f <- write_temp_css()
  ui <- ptr_shared_ui(
    formulas = list(
      "ggplot(mtcars) + geom_point(aes(x = var(shared = \"x\")))",
      "ggplot(mtcars) + geom_point(aes(x = var(shared = \"x\")))"
    ),
    css = f
  )
  html <- render_ui(ui)
  expect_match(html, basename(f), fixed = TRUE)
  expect_match(html, "ggpaintr-layer", fixed = TRUE)
})

test_that("ptr_assets() emits assets in the documented order", {
  f <- write_temp_css()
  html <- render_ui(ptr_assets(css = f))
  pos_layer <- regexpr('/ggpaintr-layer\\.css"', html)
  pos_bundled <- regexpr('/ggpaintr\\.css"', html)
  pos_user <- regexpr(paste0("/", basename(f)), html)
  expect_gt(pos_layer, 0L)
  expect_gt(pos_bundled, 0L)
  expect_gt(pos_user, 0L)
  expect_lt(pos_layer, pos_bundled)
  expect_lt(pos_bundled, pos_user)
})

test_that("grid path emits user css exactly once (no double link via shared_ui)", {
  f <- write_temp_css()
  parts <- ptr_app_grid_components(
    list(
      "ggplot(mtcars) + geom_point(aes(x = var(shared = \"x\")))",
      "ggplot(mtcars) + geom_point(aes(x = var(shared = \"x\")))"
    ),
    css = f
  )
  html <- render_ui(parts$ui)
  hrefs <- linked_csses(html)
  user_hits <- sum(grepl(paste0("/", basename(f), "$"), hrefs))
  expect_equal(user_hits, 1L)
})

test_that("verb-badge rule owns its font-weight (order-independent vs Bootstrap)", {
  # Regression: the pipeline-verb <code> badge has no own font-weight, so it
  # inherited weight from its checkbox <label>. ggpaintr `.ptr-app label`
  # {550} and Bootstrap `.checkbox label` {400} tie at specificity (0,1,1),
  # so the winner is decided by stylesheet load order. The asset->
  # htmlDependency refactor flipped that order (ggpaintr.css now loads
  # *before* bootstrap.min.css), silently dropping the badge from 550 to
  # 400. The badge must declare its own weight on `.ptr-app
  # .ptr-stage-head code` (specificity 0,2,1) so it wins regardless of
  # load order.
  css <- readLines(system.file("www", "ggpaintr.css", package = "ggpaintr"))
  txt <- paste(css, collapse = "\n")
  block <- regmatches(
    txt,
    regexpr("\\.ptr-app \\.ptr-stage-head code\\s*\\{[^}]*\\}", txt)
  )
  expect_length(block, 1L)
  expect_match(block, "font-weight\\s*:\\s*550")
})

test_that("page canvas is opt-in: bare .ptr-app has no 100vh, --page does", {
  # The full-viewport backdrop is opt-in. Bare `.ptr-app` is just the
  # themed scope (so an embedded ptr_module_ui/ptr_ui_page or a region
  # half sizes to its content, not the host's whole column). Only the
  # standalone entrypoints add `ptr-app--page`, which carries the
  # `min-height:100vh` canvas. Regression: bare `.ptr-app` must NOT
  # declare `min-height:100vh`; `.ptr-app--page` must.
  css <- readLines(system.file("www", "ggpaintr.css", package = "ggpaintr"))
  txt <- paste(css, collapse = "\n")
  bare <- regmatches(
    txt, regexpr("\\.ptr-app\\s*\\{[^}]*\\}", txt)
  )
  expect_length(bare, 1L)
  expect_no_match(bare, "100vh")
  page <- regmatches(
    txt, regexpr("\\.ptr-app\\.ptr-app--page\\s*\\{[^}]*\\}", txt)
  )
  expect_length(page, 1L)
  expect_match(page, "min-height\\s*:\\s*100vh")
})

test_that("composing the bundle keeps the legacy ggpaintr/ resource prefix served", {
  # Regression: ptr_ui_header() references the logo by the absolute path
  # "ggpaintr/ggpaintr-logo.png". The old core_assets_tags() served the
  # whole inst/www dir under the "ggpaintr" resource prefix; the
  # htmlDependency rewrite must keep doing so or the logo 404s.
  ptr_assets()  # side-effect: registers the resource path
  www <- normalizePath(system.file("www", package = "ggpaintr"),
                       mustWork = FALSE)
  registered <- vapply(shiny::resourcePaths(), normalizePath,
                       character(1), mustWork = FALSE)
  expect_true(www %in% registered)
  expect_true(file.exists(file.path(www, "ggpaintr-logo.png")))
})
