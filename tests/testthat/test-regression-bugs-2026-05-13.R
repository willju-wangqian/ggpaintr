# Regression tests for the three bugs filed in
# `dev/audit/feature-coverage-2026-05-13.html` and the accompanying handoff
# `dev/plans/bug-fix-2026-05-13.md`.
#
# Loop convention: each bug starts with a focused failing reproduction, then
# the fix in R/paintr-*.R, then the test goes green.

# `shinyApp()` stores the UI tag in `environment(app$httpHandler)$ui` (no
# top-level `app$ui` slot). Pulling it out lets these tests inspect the
# rendered HTML without booting a server. Works for both `ptr_app()` and
# `ptr_app_bslib()` (both return a `shiny.appobj` with the same shape).
app_ui_html <- function(app) {
  e <- environment(app$httpHandler)
  paste(as.character(e$ui), collapse = "")
}

test_that("BUG-A1: aes `var` pickers refresh through pds -> diamonds -> colvars subset", {
  # Mirrors the App-1 scenario the audit flagged: a pipeline-head custom
  # source (`pds`, companion-less) followed by `select(colvars)` (custom
  # multi-column consumer), `mutate(var + log(var))`, `filter(var > num)`,
  # then `ggplot(aes(x = var, y = var, color = var))`. The audit's
  # observation was that switching `pds` from mpg to diamonds left the
  # three aes pickers (`ggplot_1_*_var_NA`) with empty options while the
  # mutate/filter pickers refreshed. This test pins the R-side invariant:
  # every var consumer downstream of the pipeline must receive the
  # upstream columns at each step of the pds -> diamonds -> colvars-subset
  # chain.
  src_kw <- paste0("bugA1src_", as.integer(Sys.time()))
  ptr_define_placeholder_source(
    keyword       = src_kw,
    build_ui      = function(node, label = NULL, ...) {
      shiny::selectInput(node$id, label %||% "Dataset",
                         choices = c("mpg", "diamonds"),
                         selected = "mpg")
    },
    resolve_data  = function(value, node, ...) {
      if (is.null(value) || !nzchar(value)) return(NULL)
      get(value, envir = asNamespace("ggplot2"))
    },
    resolve_expr  = function(value, node, ...) {
      if (is.null(value) || !nzchar(value)) return(NULL)
      rlang::sym(value)
    }
  )
  withr::defer(ptr_clear_placeholder(src_kw))

  cv_kw <- paste0("bugA1cv_", as.integer(Sys.time()))
  ptr_define_placeholder_consumer(
    keyword = cv_kw,
    build_ui = function(node, cols = character(), label = NULL,
                        selected = character(), ...) {
      shiny::selectInput(node$id, label, choices = cols,
                         selected = selected, multiple = TRUE)
    },
    resolve_expr = function(value, node, ...) {
      if (length(value) == 0L) return(NULL)
      do.call(call, c(list("c"), as.list(value)))
    }
  )
  withr::defer(ptr_clear_placeholder(cv_kw))

  captured <- new.env(parent = emptyenv())
  captured$cols_by_id <- list()
  orig_var_bu <- ggpaintr:::ptr_builtin_var_build_ui
  new_var_bu <- function(node, cols = character(), label = NULL,
                         copy = NULL, selected = character(0), ...) {
    captured$cols_by_id[[node$id]] <<- cols
    orig_var_bu(node, cols = cols, label = label, copy = copy,
                selected = selected, ...)
  }
  # local_mocked_bindings intercepts the package's INTERNAL call to
  # ptr_builtin_var_build_ui (via the builtin registry). assignInNamespace
  # cannot reach it under devtools: the package resolves the binding through
  # the attached package env, not the patched namespace, so the spy never
  # fires and captured$cols_by_id stays empty.
  testthat::local_mocked_bindings(
    ptr_builtin_var_build_ui = new_var_bu, .package = "ggpaintr"
  )
  suppressWarnings(ptr_register_builtins())
  withr::defer(suppressWarnings(ptr_register_builtins()))

  formula <- sprintf(paste(
    "%s |> head(num) |> dplyr::select(%s) |>",
    "dplyr::mutate(metric = var + log(var)) |>",
    "dplyr::filter(var > num) |>",
    "ggplot(aes(x = var, y = var, color = var))"
  ), src_kw, cv_kw)

  shiny::testServer(function(input, output, session) {
    st <- ptr_server(input, output, session, formula, expr_check = FALSE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    src <- find_nodes(st$tree(), is_ptr_ph_data_source)[[1L]]
    colvars_node <- find_nodes(st$tree(), function(n) {
      is_ptr_ph_data_consumer(n) && identical(n$keyword, cv_kw)
    })[[1L]]
    var_consumers <- Filter(
      function(n) is.null(n$shared),
      find_nodes(st$tree(), function(n) {
        is_ptr_ph_data_consumer(n) && identical(n$keyword, "var")
      })
    )
    aes_var_ids <- vapply(
      Filter(function(n) identical(n$layer_name, "ggplot") &&
                          !grepl("_4_|_5_", n$id),
             var_consumers),
      `[[`, character(1), "id")

    read_consumer_ui <- function() {
      for (cn in var_consumers) {
        tryCatch(output[[paste0(cn$id, "_ui")]], error = function(e) NULL)
      }
    }

    # (1) Initial: pds = mpg. Every var picker (mutate/filter and aes)
    # sees the full mpg column list.
    do.call(session$setInputs,
            stats::setNames(list("mpg"), src$id))
    session$flushReact()
    read_consumer_ui()
    for (cn in var_consumers) {
      expect_equal(captured$cols_by_id[[cn$id]], names(ggplot2::mpg),
                   info = sprintf("var %s at pds=mpg", cn$id))
    }

    # (2) Switch source: diamonds. Every var picker must refresh -- this
    # is the cascade BUG-A1's audit said failed for the aes pickers.
    do.call(session$setInputs,
            stats::setNames(list("diamonds"), src$id))
    session$flushReact()
    read_consumer_ui()
    for (cn in var_consumers) {
      expect_equal(captured$cols_by_id[[cn$id]], names(ggplot2::diamonds),
                   info = sprintf("var %s at pds=diamonds", cn$id))
    }

    # (3) Pick a colvars subset. Every var picker downstream of
    # `select(colvars)` (mutate/filter AND aes, all of which sit beyond
    # the select stage in the same pipeline) must now see only the
    # selected columns.
    subset_cols <- c("carat", "price", "cut")
    do.call(session$setInputs,
            stats::setNames(list(subset_cols), colvars_node$id))
    session$flushReact()
    read_consumer_ui()
    for (cn in var_consumers) {
      expect_equal(captured$cols_by_id[[cn$id]], subset_cols,
                   info = sprintf("var %s after colvars subset", cn$id))
    }
    expect_true(length(aes_var_ids) >= 1L,
                info = "aes var pickers must be present in the tree")
  })
})

test_that("BUG-A2: ui_text$defaults$num overrides reach the shared num widget", {
  # First a unit-level check on `ptr_resolve_ui_text` with `param = NULL` --
  # the resolver must fall through to `defaults$<keyword>` for help/empty_text
  # when the caller has no specific param to key off.
  ui_text <- ptr_ui_text(list(defaults = list(num = list(
    help = "Any number works here.",
    empty_text = "No number yet"
  ))))
  copy <- ptr_resolve_ui_text("control",
                              keyword = "num",
                              param = NULL,
                              layer_name = NULL,
                              ui_text = ui_text)
  expect_equal(copy$help, "Any number works here.")
  expect_equal(copy$empty_text, "No number yet")

  # End-to-end: a shared `num` widget across two geom_point args (alpha and
  # size) is rendered statically into the sidebar by `ptr_app()` /
  # `ptr_app_bslib()`. The widget's help / empty_text leaves must come from
  # `ui_text$defaults$num`, not from the alpha-specific built-in copy --
  # otherwise multi-param widgets inherit alpha's "0 and 1" hint.
  formula <- paste0(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + ",
    "geom_point(alpha = num(shared = 'lvl'), size = num(shared = 'lvl'))"
  )
  app <- ptr_app(formula, ui_text = ui_text, expr_check = FALSE)
  rendered <- app_ui_html(app)
  expect_true(grepl("Any number works here.", rendered, fixed = TRUE),
              info = "defaults$num$help must reach the shared num widget")
  expect_true(grepl("No number yet", rendered, fixed = TRUE),
              info = "defaults$num$empty_text must reach the shared num widget (as `placeholder` on the underlying <input>)")
  expect_false(grepl("Enter a value between 0 and 1.", rendered, fixed = TRUE),
               info = "alpha-specific built-in help must not bleed into shared widget")
})

test_that("BUG-B1: ptr_app_bslib reads its title from ui_text$shell$title$label", {
  skip_if_not_installed("bslib")
  formula <- "ggplot(mtcars) + geom_point()"
  ut <- ptr_ui_text(list(shell = list(title = list(label = "From ui_text"))))
  app <- ptr_app_bslib(formula, ui_text = ut, expr_check = FALSE)
  rendered <- app_ui_html(app)
  expect_true(grepl("From ui_text", rendered, fixed = TRUE),
              info = "ui_text$shell$title$label must reach the rendered bslib navbar brand")
})
