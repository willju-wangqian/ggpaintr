# Behaviour: every raw-Shiny entry point routes through the ptr_assets()
# bundle and exposes the same `css =` override portal in the same order
# (ggpaintr.css before user css).

render_ui <- function(ui) {
  as.character(htmltools::renderTags(ui)$html)
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
    pos_bundled <- regexpr("ggpaintr/ggpaintr\\.css", html)
    pos_user <- regexpr(paste0("/", base), html)
    expect_gt(pos_bundled, 0L)
    expect_gt(pos_user, 0L)
    expect_lt(pos_bundled, pos_user)
  }
})

test_that("ptr_assets() is idempotent under nested injection", {
  ui <- ptr_module_ui("m", fixture_formula)
  html <- render_ui(ui)
  expect_match(html, "__ptr_set_class_registered", fixed = TRUE)
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
  expect_match(html, ".ptr-stage", fixed = TRUE)
})

test_that("ptr_assets() emits assets in the documented order", {
  f <- write_temp_css()
  html <- render_ui(ptr_assets(css = f))
  pos_set_class <- regexpr("ptr_set_class", html)
  pos_bundled <- regexpr("ggpaintr/ggpaintr\\.css", html)
  pos_user <- regexpr(paste0("/", basename(f)), html)
  expect_gt(pos_set_class, 0L)
  expect_gt(pos_bundled, 0L)
  expect_gt(pos_user, 0L)
  expect_lt(pos_set_class, pos_bundled)
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
