# A `ptr_ph_data_source` placeholder (e.g. `upload`) used at the head of a
# pipeline -- `upload |> head(num) |> ... |> ggplot(...)` -- rather than as a
# layer's bare `data_arg`. Unlike a bare-data-source layer (whose resolved
# frame is swapped into `data_arg` by `inject_resolved_data()`), a pipeline-head
# source survives substitution only as a symbol, so `ptr_setup_pipelines()`
# binds the resolved frame into `state$eval_env` under the dataset name and
# bumps `state$resolved_sources` so downstream consumer pickers refresh.

test_that("find_source_companion_ids_in_upstream finds upload companions", {
  r <- ptr_translate(
    "ppUpload |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()"
  )
  consumer <- find_nodes(r, is_ptr_ph_data_consumer)[[1L]]
  ids <- find_source_companion_ids_in_upstream(consumer$upstream)
  expect_length(ids, 1L)
  expect_match(ids, "_name$")

  # No source in the upstream -> nothing.
  r2 <- ptr_translate("mtcars |> head(ppNum) |> ggplot(aes(x = ppVar))")
  c2 <- find_nodes(r2, is_ptr_ph_data_consumer)[[1L]]
  expect_length(find_source_companion_ids_in_upstream(c2$upstream), 0L)
  expect_length(find_source_companion_ids_in_upstream(NULL), 0L)
})

test_that("ptr_init_state tracks resolved_sources for pipeline-head sources only", {
  s_pipe <- ptr_init_state(
    "ppUpload |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()",
    envir = globalenv()
  )
  expect_length(s_pipe$resolved_sources, 1L)
  expect_true(is.function(s_pipe$resolved_sources[[1L]]))  # a reactiveVal

  # Bare-data-source layer: handled via `state$resolved_data`, not sources.
  s_bare <- ptr_init_state(
    "ggplot(data = ppUpload, aes(x = ppVar, y = ppVar)) + geom_point()",
    envir = globalenv()
  )
  expect_length(s_bare$resolved_sources, 0L)
  expect_named(s_bare$resolved_data, "ggplot")

  # No data source at all.
  s_none <- ptr_init_state("ggplot(mtcars, aes(x = ppVar))", envir = globalenv())
  expect_length(s_none$resolved_sources, 0L)
})

test_that("ptr_init_state gives state its own child eval env", {
  e <- new.env()
  s <- ptr_init_state("ggplot(mtcars, aes(x = ppVar))", envir = e)
  expect_false(identical(s$eval_env, e))
  expect_identical(parent.env(s$eval_env), e)
})

test_that("pipeline-head `ppUpload` resolves downstream consumers and renders", {
  e <- new.env(parent = globalenv())
  formula <-
    "ppUpload |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()"
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(input, output, session, formula,
                                          envir = e)
  }
  shiny::testServer(server, {
    state <- session$userData$state
    tree <- shiny::isolate(state$tree())
    src <- find_nodes(tree, is_ptr_ph_data_source)[[1L]]
    expect_false(is.null(state$resolved_sources[[src$id]]))

    fp <- fixture_path("simple_numeric.csv")
    do.call(session$setInputs,
            stats::setNames(list(mock_upload_input(fp)), src$id))
    # `updateTextInput()` does not echo back inside `testServer`, so set the
    # dataset-name companion the way the browser auto-fill would.
    do.call(session$setInputs,
            stats::setNames(list("simple_numeric"), src$companion_id))
    session$flushReact()

    # The resolved frame is bound under its dataset name in the state's
    # (child) eval env, not the caller's env.
    expect_true(exists("simple_numeric", envir = state$eval_env,
                       inherits = FALSE))
    expect_s3_class(get("simple_numeric", envir = state$eval_env),
                    "data.frame")
    expect_false(exists("simple_numeric", envir = e, inherits = FALSE))

    # Downstream `var` pickers see the upstream columns.
    snap <- list()
    spec <- state$input_spec
    for (i in seq_len(nrow(spec))) {
      v <- shiny::isolate(input[[spec$input_id[i]]])
      if (!is.null(v)) snap[[spec$input_id[i]]] <- v
    }
    res <- runtime_upstream_data(state, snap)
    consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
    for (cons in consumers) {
      expect_true(cons$id %in% names(res))
      expect_setequal(res[[cons$id]]$cols, c("x", "y", "group"))
    }

    # Pick columns + draw -> a real plot comes out (the symbol resolves at
    # eval time too, not just for the pickers).
    for (cons in consumers) {
      do.call(session$setInputs, stats::setNames(list("x"), cons$id))
    }
    session$setInputs(ptr_update_plot = 1)
    session$flushReact()
    rt <- state$runtime()
    expect_true(isTRUE(rt$ok))
    expect_s3_class(rt$plot, "ggplot")
  })
})

test_that("pipeline-head `ppUpload` populates the consumer picker UI (renderUI path)", {
  # Exercises the full renderUI path, including a pipeline stage (`head(num)`,
  # `dplyr::filter(num > 0)`) that prunes away when the producer is unset --
  # this is where `prune_walk.ptr_call()` meets a `ptr_missing` arg.
  e <- new.env(parent = globalenv())
  formula <- paste(
    "ppUpload |> head(ppNum) |> dplyr::filter(ppNum > 0) |>",
    "ggplot(aes(x = ppVar, y = ppVar)) + geom_point()"
  )
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(input, output, session, formula,
                                          envir = e)
  }
  shiny::testServer(server, {
    tree <- shiny::isolate(session$userData$state$tree())
    src <- find_nodes(tree, is_ptr_ph_data_source)[[1L]]
    cons <- find_nodes(tree, is_ptr_ph_data_consumer)[[1L]]
    out_id <- consumer_output_id(cons$id)

    # ADR 0015: pre-upload, the consumer's `entry_reactive` `req()`s on the
    # upstream source-ready reactive. Reading the renderUI before any upload
    # therefore short-circuits with a `shiny.silent.error` — Shiny's standard
    # "not yet ready" signal that yields no DOM. The contract this test pins
    # is that AFTER the upload the picker populates; the pre-upload state is
    # whatever Shiny's req() semantics produce.
    expect_error(output[[out_id]], class = "shiny.silent.error")

    fp <- fixture_path("simple_numeric.csv")
    do.call(session$setInputs,
            stats::setNames(list(mock_upload_input(fp)), src$id))
    do.call(session$setInputs,
            stats::setNames(list("simple_numeric"), src$companion_id))
    session$flushReact()

    ui_html <- paste(as.character(output[[out_id]]), collapse = "")
    for (col in c("x", "y", "group")) {
      expect_true(grepl(paste0("value=\"", col, "\""), ui_html, fixed = TRUE))
    }
  })
})

test_that("pipeline-head source clears its slot when file AND companion are removed", {
  # ADR 0024 update: the companion is a data-loading entry point. A
  # successful upload assigns the df into `state$eval_env` under the
  # companion's typed name (bind_source_value, R/paintr-server.R:877).
  # That binding is sticky — clearing JUST the fileInput leaves the
  # name resolvable in eval_env, so the entry-point path
  # (try_bind_source_default_resolved) re-binds via env lookup. To
  # fully clear the slot, the user must clear BOTH the fileInput and
  # the companion textInput. Pre-ADR-0024 this test asserted clearing
  # only the file sufficed (because try_bind bailed on null default);
  # post-ADR-0024 it does not.
  e <- new.env(parent = globalenv())
  formula <-
    "ppUpload |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()"
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(input, output, session, formula,
                                          envir = e)
  }
  shiny::testServer(server, {
    state <- session$userData$state
    src <- find_nodes(shiny::isolate(state$tree()), is_ptr_ph_data_source)[[1L]]
    fp <- fixture_path("simple_numeric.csv")
    do.call(session$setInputs,
            stats::setNames(list(mock_upload_input(fp)), src$id))
    do.call(session$setInputs,
            stats::setNames(list("simple_numeric"), src$companion_id))
    session$flushReact()
    expect_s3_class(state$resolved_sources[[src$id]](), "data.frame")

    # Clear ONLY the file. ADR 0024 entry-point semantics: the slot stays
    # bound because the companion still resolves "simple_numeric" in eval_env
    # (from the prior bind). NOT the same as the pre-ADR-0024 assertion.
    do.call(session$setInputs, stats::setNames(list(NULL), src$id))
    session$flushReact()
    expect_s3_class(state$resolved_sources[[src$id]](), "data.frame")

    # Clear BOTH file AND companion. Now nothing resolves → slot becomes NULL.
    do.call(session$setInputs, stats::setNames(list(""), src$companion_id))
    session$flushReact()
    expect_null(state$resolved_sources[[src$id]]())
  })
})
