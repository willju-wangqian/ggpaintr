make_plots <- function(n) rep(list("y ~ x"), n)

# Each plot module renders its own inner class="row" div.
# Total class="row" count = n_grid_rows + n_plots (one inner row per module).
# Helper to count class="row" occurrences in rendered UI HTML.
count_rows <- function(html) length(gregexpr('class="row"', html, fixed = TRUE)[[1]])

test_that("ptr_app_grid_components: no ncol/nrow uses one row", {
  parts <- ptr_app_grid_components(
    make_plots(4), expr_check = FALSE,
    envir = new.env(parent = baseenv())
  )
  html <- paste(as.character(parts$ui), collapse = "")
  # 1 grid row + 4 inner module rows
  expect_equal(count_rows(html), 1L + 4L)
  expect_match(html, "col-sm-3", fixed = TRUE)
})

test_that("ptr_app_grid_components: ncol=2 with 6 plots gives 3 rows", {
  parts <- ptr_app_grid_components(
    make_plots(6), expr_check = FALSE,
    envir = new.env(parent = baseenv()),
    ncol = 2
  )
  html <- paste(as.character(parts$ui), collapse = "")
  # 3 grid rows + 6 inner module rows
  expect_equal(count_rows(html), 3L + 6L)
  expect_match(html, "col-sm-6", fixed = TRUE)
})

test_that("ptr_app_grid_components: nrow=2 with 6 plots gives 2 rows", {
  parts <- ptr_app_grid_components(
    make_plots(6), expr_check = FALSE,
    envir = new.env(parent = baseenv()),
    nrow = 2
  )
  html <- paste(as.character(parts$ui), collapse = "")
  # 2 grid rows + 6 inner module rows
  expect_equal(count_rows(html), 2L + 6L)
  expect_match(html, "col-sm-4", fixed = TRUE)
})

test_that("ptr_app_grid_components: ncol=2 nrow=3 with 6 plots exact fit", {
  parts <- ptr_app_grid_components(
    make_plots(6), expr_check = FALSE,
    envir = new.env(parent = baseenv()),
    ncol = 2, nrow = 3
  )
  html <- paste(as.character(parts$ui), collapse = "")
  # 3 grid rows + 6 inner module rows
  expect_equal(count_rows(html), 3L + 6L)
})

test_that("ptr_app_grid_components: ncol=3 nrow=2 with 5 plots — 2 rows no error", {
  expect_no_error({
    parts <- ptr_app_grid_components(
      make_plots(5), expr_check = FALSE,
      envir = new.env(parent = baseenv()),
      ncol = 3, nrow = 2
    )
  })
  html <- paste(as.character(parts$ui), collapse = "")
  # 2 grid rows + 5 inner module rows
  expect_equal(count_rows(html), 2L + 5L)
})

test_that("ptr_app_grid_components: ncol*nrow < n_plots errors", {
  expect_error(
    ptr_app_grid_components(
      make_plots(5), expr_check = FALSE,
      envir = new.env(parent = baseenv()),
      ncol = 2, nrow = 2
    ),
    regexp = "ncol \\* nrow"
  )
})

test_that("ptr_app_grid_components: ncol=0 errors", {
  expect_error(
    ptr_app_grid_components(
      make_plots(3), expr_check = FALSE,
      envir = new.env(parent = baseenv()),
      ncol = 0
    )
  )
})

test_that("ptr_app_grid_components: nrow=0 errors", {
  expect_error(
    ptr_app_grid_components(
      make_plots(3), expr_check = FALSE,
      envir = new.env(parent = baseenv()),
      nrow = 0
    )
  )
})
