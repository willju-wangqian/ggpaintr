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

test_that("BUG-1: ptr_define_placeholder_source() without shortcut does not emit NA companion id", {
  kw <- paste0("bug1src_", as.integer(Sys.time()))
  ptr_define_placeholder_source(
    keyword      = kw,
    build_ui     = function(node, label = NULL, ...) NULL,
    resolve_data = function(value, node, ...) mtcars
    # NOTE: shortcut defaults to FALSE
  )
  withr::defer(ptr_clear_placeholder(kw))

  formula <- sprintf("%s |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar))", kw)
  tree <- ptr_translate(formula, expr_check = FALSE)
  spec <- ptr_runtime_input_spec(tree)

  bad <- spec$role == "source_companion" & is.na(spec$input_id)
  expect_false(any(bad),
               info = "no source_companion row should have an NA input_id")
})

test_that("BUG-1 deep: shortcut-less source resolves through to downstream var consumers", {
  # Companion `11b6860` (2026-05-13) cleared the NA-input-id cascade; companion
  # `c0d5467` closed the deeper layer where, even with a NULL shortcut_id, the
  # pipeline-head observer / `substitute_walk.ptr_ph_data_source` /
  # consumer-snapshot construction were still upload-shaped. This regression
  # guards the end-to-end resolution: selecting a dataset on a no-shortcut
  # source must populate downstream `var` pickers with that dataset's columns.
  kw <- paste0("bug1deep_", as.integer(Sys.time()))
  ptr_define_placeholder_source(
    keyword      = kw,
    build_ui     = function(node, label = NULL, ...) {
      shiny::selectInput(node$id, label, choices = c("mtcars", "iris"))
    },
    resolve_data = function(value, node, ...) {
      if (is.null(value) || !nzchar(value)) return(NULL)
      get(value, envir = asNamespace("datasets"))
    }
    # NOTE: shortcut defaults to FALSE — the deep path under test
  )
  withr::defer(ptr_clear_placeholder(kw))

  # Spy on the built-in `var` build_ui to capture the cols vector the
  # framework feeds it post-resolution.
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

  formula <- sprintf("%s |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar))", kw)

  shiny::testServer(function(input, output, session) {
    st <- ptr_server_internal(input, output, session, formula, expr_check = FALSE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    src <- find_nodes(st$tree(), is_ptr_ph_data_source)[[1L]]
    expect_null(src$shortcut_id,
                info = "constructor default must remain NULL post `11b6860`")
    # Drive the source's own input id (no companion).
    do.call(session$setInputs,
            stats::setNames(list("mtcars"), src$id))
    # Read consumer outputs so their renderUI fires.
    for (cnode in find_nodes(st$tree(), is_ptr_ph_data_consumer)) {
      if (!is.null(cnode$shared)) next
      tryCatch(output[[paste0(cnode$id, "_ui")]], error = function(e) NULL)
    }
  })

  expect_true(length(captured$cols_by_id) >= 1L,
              info = "at least one downstream var build_ui must be invoked")
  for (id in names(captured$cols_by_id)) {
    expect_equal(captured$cols_by_id[[id]], names(mtcars),
                 info = sprintf("var %s must receive mtcars columns", id))
  }
})

test_that("BUG-3: ptr_app_grid() validates shared keys against the union across plots", {
  fa <- "iris |> ggplot(aes(x = ppVar(shared = 'cv'), y = ppVar(shared = 'cv'))) + geom_point()"
  fb <- "iris |> ggplot(aes(x = ppVar(shared = 'cv'), y = ppVar(shared = 'cv'))) + geom_point(size = ppNum(shared = 'pt'))"
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
  # NOTE (2026-05-13): the report's BUG-4 turned out to be a probe-level
  # false positive (selectize.js moves <option> children out of the <select>
  # element, so `select.options.length === 0` even when the widget is
  # populated). See dev/tasks/bug-4-browser-followup.md for the verification.
  #
  # This test still has value as a contract: a custom consumer's build_ui
  # must receive the same `cols` vector as the built-in `var` consumer at the
  # same upstream pipeline depth. If anyone keys `ptr_resolve_upstream` or
  # `invoke_build_ui`'s `extra` payload on the keyword in the future, this
  # guard rail fires.
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
  orig_var_bu <- ggpaintr:::ptr_builtin_var_build_ui
  new_var_bu <- function(node, cols = character(), label = NULL,
                        copy = NULL, selected = character(0), ...) {
    captured$bu_calls[[length(captured$bu_calls) + 1L]] <<-
      list(kw = "ppVar", id = node$id, cols = cols)
    orig_var_bu(node, cols = cols, label = label, copy = copy,
                selected = selected, ...)
  }
  # See the BUG-1-deep note: local_mocked_bindings reaches the package's
  # internal builtin-registry call; assignInNamespace does not under devtools.
  testthat::local_mocked_bindings(
    ptr_builtin_var_build_ui = new_var_bu, .package = "ggpaintr"
  )
  suppressWarnings(ptr_register_builtins())
  withr::defer(suppressWarnings(ptr_register_builtins()))

  formula <- sprintf(
    "ppUpload |> head(ppNum) |> %s(%s) |> dplyr::mutate(new_var = ppVar + ppVar) |> ggplot(aes(x = ppVar, y = ppVar))",
    "dplyr::select", kw
  )

  shiny::testServer(function(input, output, session) {
    st <- ptr_server_internal(input, output, session, formula, expr_check = FALSE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    upl <- find_nodes(st$tree(), is_ptr_ph_data_source)[[1L]]
    df_fake <- head(iris)
    assign("fakeup", df_fake, envir = st$eval_env)
    do.call(session$setInputs,
            stats::setNames(list("fakeup"), upl$shortcut_id))
    st$resolved_sources[[upl$id]](df_fake)
    # ADR 0015 PLAN-02 / Option E: the per-source `state$bound_names[[id]]`
    # reactiveVal is written by `bind_source_value()` AFTER `assign()` in
    # the real source observer. This testServer fixture bypasses
    # `ptr_setup_pipelines()` and does the assign/slot writes manually, so
    # the bound_names bump must also be done manually to satisfy
    # entry_reactive's new `req(state$bound_names[[id]]())` guard. Mirrors
    # the `req()` chain in `R/paintr-server.R` ptr_setup_consumer_uis.
    st$bound_names[[upl$id]]("fakeup")
    # ADR 0015: entry_reactive now `req()`s on the source-ready reactive.
    # A flushReact is required to propagate the resolved_sources update
    # into entry_reactive's dep graph before reading the consumer outputs;
    # without it, the read sees a pre-flush snapshot and silent-errors.
    session$flushReact()
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
  var_calls    <- Filter(function(c) c$kw == "ppVar",    last_by_id)
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
  ui_text <- ptr_ui_text(list(
    shell = list(draw_button = list(label = "Render plot"))
  ))
  shell_copy <- ggpaintr:::layer_panel_default_shell_copy(ui_text)
  expect_equal(shell_copy$update_plot_label, "Render plot")

  # End-to-end through the app components: the draw button tag's label leaf
  # must carry the override.
  parts <- ptr_app_components("ggplot()", ui_text = ui_text, expr_check = FALSE)
  ui_html <- as.character(parts$ui)
  expect_true(grepl('id="ptr_update_plot"', ui_html, fixed = TRUE),
              info = "draw button must be present in UI")
  expect_true(grepl(">Render plot<", ui_html, fixed = TRUE),
              info = "draw button label must read 'Render plot'")
})

test_that("BUG-6: pipeline-stage enable-checkbox labels show the pipeline verb", {
  formula <- paste0(
    "mtcars |> head(ppNum) |> dplyr::select(ppVar) |> ",
    "dplyr::mutate(new = ppVar + ppNum) |> dplyr::filter(ppVar > ppNum) |> ",
    "ggplot(aes(x = ppVar, y = ppVar))"
  )
  tree <- ptr_translate(formula, expr_check = FALSE)
  layer <- tree$layers[[1L]]
  entries <- ggpaintr:::find_layer_placeholders_with_stage(layer$data_arg)
  verbs <- vapply(entries, function(e) e$verb %||% NA_character_, character(1))
  # Drop the head-of-pipeline source entry (no stage); keep verbs once.
  unique_verbs <- unique(verbs[!is.na(verbs)])
  expect_true(all(c("head", "select", "mutate", "filter") %in% unique_verbs),
              info = sprintf("got verbs: %s",
                             paste(unique_verbs, collapse = ", ")))
  # And specifically: no inner-operator verbs.
  expect_false(any(c("+", ">", "==", "*") %in% unique_verbs),
               info = "inner-operator verbs leaked to stage labels")
})

test_that("BUG-5: successful draw clears stale shared-picker error from #ptr_error", {
  formula <- paste0(
    "mtcars |> ggplot(aes(x = ppVar, y = ppVar)) + ",
    "geom_point(size = ppVar(shared = 'v'))"
  )
  shiny::testServer(function(input, output, session) {
    st <- ptr_server_internal(input, output, session, formula, expr_check = FALSE,
                     auto_bind_shared = TRUE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    # Simulate the shared resolver having latched an error early (pre-draw)
    # and a subsequently-successful draw landing in state$runtime.
    st$shared_resolution_errors(
      "Shared `ppVar(shared = \"v\")` cannot be resolved."
    )
    st$runtime(list(
      ok = TRUE, plot = ggplot2::ggplot(),
      code_text = "ggplot(mtcars) + geom_point()"
    ))
    rendered <- tryCatch(output$ptr_error, error = function(e) e)
    if (inherits(rendered, "error")) rendered <- NULL
    txt <- if (is.null(rendered)) "" else as.character(rendered)
    expect_false(grepl("cannot be resolved", txt, fixed = TRUE),
                 info = paste0("error pane should not show stale shared-picker ",
                               "warning after a successful draw; got: ", txt))
  })
})

# Bugs reported by interactive testing 2026-05-13 (post-loop):

test_that("BUG-7: validate_input may return NULL for a valid input (idiomatic R)", {
  # The roxygen contract for `validate_input` previously said "TRUE or an
  # error message string" — and the framework strictly required `TRUE`. Users
  # following R's standard "NULL means no error to report" idiom hit a
  # confusing "validation failed." abort. Fix accepts both TRUE and NULL.
  kw <- paste0("bug7val_", as.integer(Sys.time()))
  validate_calls <- 0L
  ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = function(node, label = NULL, cols = character(), ...) NULL,
    resolve_expr = function(value, node, ...) {
      if (length(value) == 0L) return(NULL)
      rlang::sym(value)
    },
    validate_session_input = function(value, ctx) {
      validate_calls <<- validate_calls + 1L
      bad <- setdiff(value, ctx$upstream_cols)
      if (length(bad)) sprintf("unknown: %s", paste(bad, collapse = ", "))
      else NULL                                 # <- idiomatic "no error"
    }
  )
  withr::defer(ptr_clear_placeholder(kw))

  formula <- sprintf(
    "iris |> dplyr::select(%s) |> ggplot(aes(x = ppVar, y = ppVar))", kw
  )
  tree <- ptr_translate(formula, expr_check = FALSE)
  consumer_node <- find_nodes(tree, function(n) {
    is_ptr_ph_data_consumer(n) && identical(n$keyword, kw)
  })[[1L]]
  snapshot <- list()
  snapshot[[consumer_node$id]] <- "Sepal.Length"
  upstream_cols <- list()
  upstream_cols[[consumer_node$id]] <- names(iris)

  expect_silent(
    ptr_substitute(tree, input_snapshot = snapshot,
                   shared_bindings = list(),
                   eval_env = globalenv(),
                   upstream_cols = upstream_cols)
  )
  expect_true(validate_calls >= 1L,
              info = "validate_input must have run at least once during substitute")

  # An invalid value (column not in iris) must still abort with the custom
  # message, not the generic "validation failed".
  snapshot[[consumer_node$id]] <- "definitely_not_a_column"
  expect_error(
    ptr_substitute(tree, input_snapshot = snapshot,
                   shared_bindings = list(),
                   eval_env = globalenv(),
                   upstream_cols = upstream_cols),
    "unknown: definitely_not_a_column"
  )
})

test_that("BUG-8 advisory: pending data-source keywords surfaced for inline alert", {
  # When a shared picker's upstream contains an unresolved data-source
  # placeholder, the renderUI in `ptr_bind_shared_consumer_uis()` shows a
  # `alert-warning` naming those source keywords (rather than rendering a
  # silently-empty picker or pushing a hard error to #ptr_error).
  formula <- paste0(
    "ppUpload |> ggplot(aes(x = ppVar, y = ppVar)) + ",
    "geom_point(size = ppVar(shared = 'v'))"
  )
  tree <- ptr_translate(formula, expr_check = FALSE)
  shared_node <- find_nodes(tree, function(n) {
    is_ptr_ph_data_consumer(n) && !is.null(n$shared)
  })[[1L]]
  pending <- ggpaintr:::pending_data_source_keywords(shared_node$upstream)
  expect_equal(pending, "ppUpload",
               info = "shared widget pre-upload advisory should name `upload`")

  # Multiple distinct sources in a shared upstream should each appear.
  # Define a second data-source placeholder for the test, then build a
  # toy upstream tree by hand at the keyword level.
  kw <- paste0("bug8src_", as.integer(Sys.time()))
  ptr_define_placeholder_source(
    keyword = kw,
    build_ui = function(node, label = NULL, ...) NULL,
    resolve_data = function(value, node, ...) NULL
  )
  withr::defer(ptr_clear_placeholder(kw))
  formula2 <- sprintf(
    "%s |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point(size = ppVar(shared = 'w'))",
    kw
  )
  tree2 <- ptr_translate(formula2, expr_check = FALSE)
  shared_node2 <- find_nodes(tree2, function(n) {
    is_ptr_ph_data_consumer(n) && !is.null(n$shared)
  })[[1L]]
  pending2 <- ggpaintr:::pending_data_source_keywords(shared_node2$upstream)
  expect_equal(pending2, kw,
               info = "advisory should name any custom data-source keyword, not only `upload`")
})

test_that("BUG-8: shared `ppVar(shared='v')` resolves through an upload data source", {
  # The 2026-05-12 report missed this; it surfaced from interactive testing.
  # `ppVar(shared = 'v')` deep inside a geom on an `upload`-headed pipeline
  # was returning the static "no dataset to list columns from" error because
  # `truncate_upstream_at_placeholder()` refused any upstream whose source
  # was a placeholder. Data-source placeholders (which resolve at runtime
  # via their companion input) are now treated as a valid source.

  # Unit-level: truncate_upstream_at_placeholder keeps an upload-source.
  formula <- paste0(
    "ppUpload |> ggplot(aes(x = ppVar, y = ppVar)) + ",
    "geom_point(size = ppVar(shared = 'v'))"
  )
  tree <- ptr_translate(formula, expr_check = FALSE)
  shared_nodes <- find_nodes(tree, function(n) {
    is_ptr_ph_data_consumer(n) && !is.null(n$shared)
  })
  expect_true(length(shared_nodes) >= 1L)
  truncated <- ggpaintr:::truncate_upstream_at_placeholder(
    shared_nodes[[1L]]$upstream
  )
  expect_false(is.null(truncated),
               info = paste0("truncate_upstream_at_placeholder must accept an ",
                             "upstream whose source is a data-source placeholder ",
                             "(returning NULL is what fed the stale error)"))

  # Integration-level: resolve_shared_consumers returns kind == "upstream",
  # not kind == "error", for this tree.
  resolutions <- ptr_resolve_shared_consumers(tree)
  expect_true("v" %in% names(resolutions))
  expect_equal(resolutions$v$kind, "upstream",
               info = "shared resolver should yield a runtime upstream binding")
  expect_false(grepl("no dataset to list columns",
                     resolutions$v$error %||% "", fixed = TRUE))
})
