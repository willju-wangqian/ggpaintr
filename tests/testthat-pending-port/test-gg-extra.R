build_extra_state <- function() {
  ptr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )
}

test_that("ptr_server_state exposes an empty extras reactiveVal", {
  ps <- build_extra_state()

  expect_true(is.function(ps$extras))
  expect_identical(shiny::isolate(ps$extras()), list())
})

test_that("ptr_gg_extra captures expressions and returns evaluated values", {
  ps <- build_extra_state()

  result <- ptr_gg_extra(ps, ggplot2::theme_minimal(base_size = 16))

  expect_type(result, "list")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "theme")

  captured <- shiny::isolate(ps$extras())
  expect_type(captured, "list")
  expect_length(captured, 1)
  expect_s3_class(captured[[1]], "quosure")
  expect_match(
    rlang::quo_text(captured[[1]]),
    "theme_minimal(base_size = 16)",
    fixed = TRUE
  )
})

test_that("ptr_gg_extra accepts multiple components in one call", {
  ps <- build_extra_state()

  result <- ptr_gg_extra(
    ps,
    ggplot2::theme_minimal(),
    ggplot2::scale_x_log10()
  )

  expect_length(result, 2)
  captured <- shiny::isolate(ps$extras())
  expect_length(captured, 2)
  expect_match(rlang::quo_text(captured[[1]]), "theme_minimal", fixed = TRUE)
  expect_match(rlang::quo_text(captured[[2]]), "scale_x_log10", fixed = TRUE)
})

test_that("ptr_gg_extra replaces prior extras on each call", {
  ps <- build_extra_state()

  ptr_gg_extra(ps, ggplot2::theme_minimal())
  ptr_gg_extra(ps, ggplot2::theme_bw())

  captured <- shiny::isolate(ps$extras())
  expect_length(captured, 1)
  expect_match(rlang::quo_text(captured[[1]]), "theme_bw", fixed = TRUE)
})

test_that("ptr_gg_extra with no extras writes an empty list", {
  ps <- build_extra_state()
  ptr_gg_extra(ps, ggplot2::theme_minimal())

  result <- ptr_gg_extra(ps)

  expect_identical(result, list())
  expect_length(shiny::isolate(ps$extras()), 0L)
})

test_that("ptr_gg_extra rejects missing or malformed ptr_state", {
  expect_error(
    ptr_gg_extra(),
    "requires `ptr_state` as the first argument"
  )
  expect_error(
    ptr_gg_extra(list()),
    "does not expose an `extras` reactiveVal"
  )
  expect_error(
    ptr_gg_extra(list(extras = "not a function")),
    "does not expose an `extras` reactiveVal"
  )
})

test_that("ptr_gg_extra does not write extras when evaluation fails", {
  ps <- build_extra_state()
  ptr_gg_extra(ps, ggplot2::theme_minimal())
  before <- shiny::isolate(ps$extras())

  expect_error(ptr_gg_extra(ps, stop("boom")))

  expect_identical(shiny::isolate(ps$extras()), before)
})

test_that("ptr_extract_code returns base code when extras are NULL or empty", {
  rt <- list(ok = TRUE, code_text = "ggplot(mtcars) + geom_point()")

  expect_identical(ptr_extract_code(rt), rt$code_text)
  expect_identical(ptr_extract_code(rt, extras = NULL), rt$code_text)
  expect_identical(ptr_extract_code(rt, extras = list()), rt$code_text)
})

test_that("ptr_extract_code appends extras to base code on success", {
  rt <- list(ok = TRUE, code_text = "ggplot(mtcars) + geom_point()")
  extras <- list(rlang::quo(ggplot2::theme_minimal(base_size = 16)))

  out <- ptr_extract_code(rt, extras = extras)

  expect_match(out, "ggplot(mtcars) + geom_point()", fixed = TRUE)
  expect_match(out, "ggplot2::theme_minimal(base_size = 16)", fixed = TRUE)
  expect_match(out, " +\n  ggplot2::theme_minimal", fixed = TRUE)
})

test_that("ptr_extract_code appends multiple extras joined with `+\\n  `", {
  rt <- list(ok = TRUE, code_text = "ggplot(mtcars) + geom_point()")
  extras <- list(
    rlang::quo(ggplot2::theme_minimal()),
    rlang::quo(ggplot2::scale_x_log10())
  )

  out <- ptr_extract_code(rt, extras = extras)

  expect_match(out, "theme_minimal()", fixed = TRUE)
  expect_match(out, "scale_x_log10()", fixed = TRUE)
  expect_match(out, "theme_minimal\\(\\) \\+\\n  ggplot2::scale_x_log10")
})

test_that("ptr_extract_code suppresses extras when runtime is not ok", {
  rt_plot_fail <- list(ok = FALSE, code_text = "ggplot(mtcars) + geom_point()")
  rt_complete_fail <- list(ok = FALSE, code_text = NULL)
  extras <- list(rlang::quo(ggplot2::theme_minimal()))

  expect_identical(
    ptr_extract_code(rt_plot_fail, extras = extras),
    rt_plot_fail$code_text
  )
  expect_null(ptr_extract_code(rt_complete_fail, extras = extras))
})

test_that("ptr_extract_code returns NULL for NULL runtime_result", {
  expect_null(ptr_extract_code(NULL))
  expect_null(ptr_extract_code(NULL, extras = list(rlang::quo(theme_minimal()))))
})

test_that("ptr_validate_state requires an extras reactiveVal", {
  ps <- build_extra_state()
  ps$extras <- NULL

  expect_error(ptr_validate_state(ps), "missing required entries")
})

test_that("ptr_validate_state rejects non-function extras", {
  ps <- build_extra_state()
  ps$extras <- "not a function"

  expect_error(ptr_validate_state(ps), "reactive accessors must be functions")
})
