# Behavior PIN (2026-05-30), NOT a bug repro: a SHARED consumer
# (`var(shared = "grp")`) over a column DERIVED by a placeholder-carrying stage
# (`mutate(adj = ppExpr(...))`) is fed the SOURCE columns (`names(mtcars)`) and
# never offers `adj` -- by design. The shared resolver
# (`resolve_shared_consumer`, R/paintr-shared.R:65-127) truncates the upstream at
# the first placeholder-carrying stage and must NOT evaluate producer hooks to
# populate a dropdown (constraint (a): a custom resolve_expr may do I/O), and has
# no canonical occurrence to source producer values from (constraint (b)). This
# is the documented counterpart to the NON-shared Option I behaviour, where a
# single canonical occurrence DOES get live producer values
# (test-spec-seed-derived-column-stage-off-browser.R). Rule of thumb: to reflect
# a pipeline's own derived columns in a picker, do NOT declare it `shared`.
#
# This test pins the intentional limitation so a future "fix" that feeds producer
# values into the shared binder (which would violate constraints (a)/(b)) trips a
# RED here and prompts a design decision rather than landing silently.

app_dir <- function() {
  test_path("fixtures", "vignette-apps", "shared-spec-seed-derived-column")
}

test_that("a shared consumer over a ppExpr-derived column offers source columns, not the derived one (by design)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir(), name = "shared-spec-seed-derived-column", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  html <- app$get_html("#shared_grp")
  # The shared picker is fed the SOURCE columns (names(mtcars))...
  expect_true(
    grepl("mpg", html, fixed = TRUE),
    label = "shared picker offers the source columns (mpg)"
  )
  # ...and DELIBERATELY does NOT offer the placeholder-derived column.
  expect_false(
    grepl(">adj<", html, fixed = TRUE),
    label = "shared picker does NOT offer the ppExpr-derived column (by design)"
  )
  # The spec seed naming the non-offered derived column is dropped gracefully:
  # the picker boots empty rather than erroring (a non-source seed is simply not
  # a valid choice for a shared picker).
  v <- app$get_value(input = "shared_grp")
  expect_true(
    is.null(v) || length(v) == 0L,
    label = paste0("a derived-column seed is dropped, picker stays empty (got: ",
                   deparse(v), ")")
  )
})
