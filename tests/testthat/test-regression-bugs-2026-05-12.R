# Regression tests for the six bugs filed in
# `dev/browser-test-report-feature-coverage-2026-05-12.html`.
#
# Loop convention: each bug starts skipped. To work a bug,
#   1. remove its `skip()` line
#   2. flesh out a minimal failing reproduction (red)
#   3. fix the bug in R/paintr-*.R (green)
#   4. commit, then move to the next bug.
#
# Order (chosen 2026-05-13):
#   BUG-1 → BUG-3 → BUG-4 → BUG-2 → BUG-6 → BUG-5

test_that("BUG-1: ptr_define_placeholder_source() without companion_id_fn does not emit NA companion id", {
  kw <- paste0("bug1src_", as.integer(Sys.time()))
  ptr_define_placeholder_source(
    keyword      = kw,
    build_ui     = function(node, label = NULL, ...) NULL,
    resolve_data = function(value, node, ...) mtcars
    # NOTE: no companion_id_fn
  )
  withr::defer(ptr_clear_placeholder(kw))

  formula <- sprintf("%s |> head(num) |> ggplot(aes(x = var, y = var))", kw)
  tree <- ptr_translate(formula, expr_check = FALSE)
  spec <- ptr_runtime_input_spec(tree)

  bad <- spec$role == "source_companion" & is.na(spec$input_id)
  expect_false(any(bad),
               info = "no source_companion row should have an NA input_id")
})

test_that("BUG-3: ptr_app_grid() validates shared keys against the union across plots", {
  fa <- "iris |> ggplot(aes(x = var(shared = 'cv'), y = var(shared = 'cv'))) + geom_point()"
  fb <- "iris |> ggplot(aes(x = var(shared = 'cv'), y = var(shared = 'cv'))) + geom_point(size = num(shared = 'pt'))"
  shared <- list(cv = shiny::reactiveVal(NULL), pt = shiny::reactiveVal(NULL))

  # Cross-plot validation: 'pt' is used by fb, so passing both plots must be silent.
  expect_silent(ptr_validate_shared_bindings(shared, plots = list(fa, fb)))

  # Per-plot context: validating against just fa (which doesn't use 'pt') must
  # not abort when the embedder passes the full `plots` set — fa's module
  # legitimately receives bindings for keys used by other plots.
  ta <- ptr_translate(fa, expr_check = FALSE)
  expect_silent(
    ptr_validate_shared_bindings(shared, tree = ta, plots = list(fa, fb),
                                 strict_missing = FALSE)
  )
})

test_that("BUG-4: custom ptr_define_placeholder_consumer() receives upstream column vector", {
  # Repro: at the same upstream pipeline depth, a custom consumer's build_ui
  # must receive the same `cols` vector as a built-in `var` consumer.
  kw <- paste0("bug4cv_", as.integer(Sys.time()))
  captured <- new.env(parent = emptyenv())
  captured$bu_calls <- list()
  ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = function(node, label = NULL, cols = character(),
                        data = NULL, selected = character(), ...) {
      captured$bu_calls[[length(captured$bu_calls) + 1L]] <<-
        list(kw = "custom", id = node$id, cols = cols)
      shiny::selectInput(node$id, label, choices = cols, multiple = TRUE)
    },
    resolve_expr = function(value, node, ...) {
      if (length(value) == 0L) return(NULL)
      do.call(call, c(list("c"), as.list(value)))
    }
  )
  withr::defer(ptr_clear_placeholder(kw))

  # Spy on the built-in `var` build_ui to capture its cols too.
  orig_var_bu <- getFromNamespace("ptr_builtin_var_build_ui", "ggpaintr")
  new_var_bu <- function(node, cols = character(), label = NULL,
                        copy = NULL, selected = character(0), ...) {
    captured$bu_calls[[length(captured$bu_calls) + 1L]] <<-
      list(kw = "var", id = node$id, cols = cols)
    orig_var_bu(node, cols = cols, label = label, copy = copy,
                selected = selected, ...)
  }
  assignInNamespace("ptr_builtin_var_build_ui", new_var_bu, "ggpaintr")
  withr::defer({
    assignInNamespace("ptr_builtin_var_build_ui", orig_var_bu, "ggpaintr")
    suppressWarnings(ptr_register_builtins())
  })
  suppressWarnings(ptr_register_builtins())

  formula <- sprintf(
    "upload |> head(num) |> %s(%s) |> dplyr::mutate(new_var = var + var) |> ggplot(aes(x = var, y = var))",
    "dplyr::select", kw
  )

  shiny::testServer(function(input, output, session) {
    st <- ptr_server(input, output, session, formula, expr_check = FALSE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    upl <- find_nodes(st$tree(), is_ptr_ph_data_source)[[1L]]
    df_fake <- head(iris)
    assign("fakeup", df_fake, envir = st$eval_env)
    do.call(session$setInputs,
            stats::setNames(list("fakeup"), upl$companion_id))
    st$resolved_sources[[upl$id]](df_fake)
    # Force renderUI evaluation by reading every consumer output id.
    for (cnode in find_nodes(st$tree(), is_ptr_ph_data_consumer)) {
      if (!is.null(cnode$shared)) next
      tryCatch(output[[paste0(cnode$id, "_ui")]], error = function(e) NULL)
    }
  })

  # Last build_ui call per consumer is the post-upload one.
  last_by_id <- list()
  for (c in captured$bu_calls) last_by_id[[c$id]] <- c
  custom_calls <- Filter(function(c) c$kw == "custom", last_by_id)
  var_calls    <- Filter(function(c) c$kw == "var",    last_by_id)
  expect_true(length(custom_calls) >= 1L,
              info = "custom consumer build_ui must be invoked at least once")
  expect_true(length(var_calls) >= 1L,
              info = "built-in var build_ui must be invoked at least once")
  custom_cols <- custom_calls[[1L]]$cols
  expected <- names(df_fake <- head(iris))
  expect_equal(custom_cols, expected,
               info = "custom consumer must receive the uploaded frame's columns")
  for (vc in var_calls) {
    expect_equal(vc$cols, expected,
                 info = sprintf("var %s must receive the uploaded frame's columns", vc$id))
  }
})

test_that("BUG-2: ui_text$shell$draw_button$label propagates to the draw button", {
  skip("BUG-2 pending — reproduction TBD by diagnose pass")
  # Repro outline:
  #   - ui_text <- list(shell = list(draw_button = list(label = "Render plot")))
  #   - Build the shell via the same path ptr_app() uses; extract the button's label.
  #   - Expect "Render plot", not "Update plot".
  # Likely fix location: paintr-app.R:191 reads shell_copy$update_plot_label,
  # but layer_panel_default_shell_copy() in paintr-build-ui.R:404-411 stores the
  # override under shell_copy$draw_button$label.
})

test_that("BUG-6: pipeline-stage enable-checkbox labels show the pipeline verb", {
  skip("BUG-6 pending — reproduction TBD by diagnose pass")
  # Repro outline:
  #   - parsed <- ptr_parse_formula(ex1_formula)
  #   - Pull the stage labels generator (whatever feeds the Data sub-tab checkboxes).
  #   - Expect head() / mutate() / filter() / select() — NOT "+()", ">()", or "".
})

test_that("BUG-5: successful draw clears stale shared-picker error from #ptr_error", {
  skip("BUG-5 pending — likely needs testServer() or browser harness")
  # Repro outline:
  #   - testServer(ptr_server, args=list(parsed = ex3_formula), ...)
  #   - Simulate pre-upload state → error string set; then upload + draw.
  #   - Expect error reactive to clear after successful render.
  # If unreachable via testServer(), defer to the browser harness as acceptance test.
})
