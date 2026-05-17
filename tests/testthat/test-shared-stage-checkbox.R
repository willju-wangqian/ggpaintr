# Stage(verb) checkbox for shared placeholders inside a data pipeline.
#
# When a shared placeholder (e.g. `var(shared = "a")`) lives inside a
# pipeline stage whose only placeholders are shared, the per-formula Data
# sub-tab renders no checkbox for that stage -- `find_layer_placeholders_*`
# skips shared placeholders. The shared section is responsible for showing
# the stage(verb) toggle so the user can prune the stage from the runtime
# pipeline. These tests pin that behavior.

.find_tags <- function(tag, has_class = NULL, has_id = NULL) {
  out <- list()
  visit <- function(x) {
    if (inherits(x, "shiny.tag")) {
      cls <- x$attribs$class %||% ""
      id <- x$attribs$id %||% ""
      class_match <- is.null(has_class) || grepl(has_class, cls, fixed = TRUE)
      id_match <- is.null(has_id) || identical(id, has_id)
      if (class_match && id_match) out[[length(out) + 1L]] <<- x
      for (child in x$children) visit(child)
    } else if (inherits(x, "shiny.tag.list") || is.list(x)) {
      for (el in x) visit(el)
    }
  }
  visit(tag)
  out
}

example_formula <- paste0(
  "mtcars |> dplyr::filter(mpg > 10) |> ",
  "dplyr::select(var(shared = \"a\"), gear) |> ",
  "ggplot(aes(x = var(shared = \"b\"), y = gear)) + geom_point()"
)

test_that("collect_orphan_shared_stages flags select() with only shared placeholder", {
  tree <- ptr_translate(example_formula, expr_check = FALSE)
  orphans <- collect_orphan_shared_stages(tree)
  expect_equal(length(orphans), 1L)
  expect_equal(orphans[[1L]]$verb, "select")
  expect_equal(orphans[[1L]]$shared_keys, "a")
  expect_true(nzchar(orphans[[1L]]$stage_id))
})

test_that("collect_orphan_shared_stages skips stages that mix shared and non-shared placeholders", {
  tree <- ptr_translate(
    "mtcars |> dplyr::select(var(shared = \"a\"), var) |> ggplot(aes(x = gear))",
    expr_check = FALSE
  )
  expect_equal(length(collect_orphan_shared_stages(tree)), 0L)
})

test_that("collect_orphan_shared_stages ignores shared placeholders outside any pipeline stage", {
  tree <- ptr_translate(
    "mtcars |> ggplot(aes(x = var(shared = \"b\"), y = gear)) + geom_point()",
    expr_check = FALSE
  )
  expect_equal(length(collect_orphan_shared_stages(tree)), 0L)
})

test_that("shared section renders a .ptr-stage block with stage_id input for orphan stages", {
  tree <- ptr_translate(example_formula, expr_check = FALSE)
  panel <- ptr_controls_panel(tree, render_shared_section = TRUE)
  stage_heads <- .find_tags(panel, has_class = "ptr-stage-head")
  expect_equal(length(stage_heads), 1L)

  orphan_sid <- collect_orphan_shared_stages(tree)[[1L]]$stage_id
  expect_true(length(.find_tags(panel, has_id = orphan_sid)) > 0L)
  expect_match(paste(as.character(panel), collapse = ""),
               "select()", fixed = TRUE)
})

test_that("shared widget for non-pipeline key is NOT wrapped in a stage block", {
  tree <- ptr_translate(example_formula, expr_check = FALSE)
  panel <- ptr_controls_panel(tree, render_shared_section = TRUE)
  # Only the orphan stage (key "a") should be wrapped; "b" is bare.
  expect_equal(length(.find_tags(panel, has_class = "ptr-stage-head")), 1L)
  # Both shared widgets must still be present (one wrapped, one bare).
  a_id <- consumer_output_id(canonical_shared_id("a"))
  b_id <- consumer_output_id(canonical_shared_id("b"))
  expect_true(length(.find_tags(panel, has_id = a_id)) > 0L)
  expect_true(length(.find_tags(panel, has_id = b_id)) > 0L)
})

test_that("the orphan stage's checkbox id matches what ptr_setup_stage_enabled observes", {
  # End-to-end contract: the checkbox in the shared section must use the
  # same `stage_id` that `collect_stage_ids(tree)` enumerates so the existing
  # observer wiring (paintr-server.R::ptr_setup_stage_enabled) picks it up
  # without any new server-side glue.
  tree <- ptr_translate(example_formula, expr_check = FALSE)
  all_sids <- collect_stage_ids(tree)
  orphan_sid <- collect_orphan_shared_stages(tree)[[1L]]$stage_id
  expect_true(orphan_sid %in% all_sids)
})

test_that("disable_walk with the orphan stage_id false prunes select() from the pipeline", {
  tree <- ptr_translate(example_formula, expr_check = FALSE)
  orphan_sid <- collect_orphan_shared_stages(tree)[[1L]]$stage_id
  pruned <- disable_walk(tree, stats::setNames(list(FALSE), orphan_sid))
  ggplot_layer <- NULL
  for (l in pruned$layers) {
    if (identical(l$name, "ggplot")) { ggplot_layer <- l; break }
  }
  expect_false(is.null(ggplot_layer))
  # The select stage is gone; the remaining stages should not include it.
  stages <- if (is_ptr_pipeline(ggplot_layer$data_arg)) {
    ggplot_layer$data_arg$stages
  } else list(ggplot_layer$data_arg)
  verbs <- vapply(stages, function(s) {
    if (is_ptr_call(s)) bare_call_name(s$fun) %||% "" else ""
  }, character(1))
  expect_false("select" %in% verbs)
})

# ---- multi-tree path: ptr_shared_ui / ptr_shared_server / ptr_app_grid ----

grid_plots <- list(
  paste0("mtcars |> dplyr::select(var(shared = \"a\"), mpg) |> ",
         "ggplot(aes(x = mpg, y = mpg)) + geom_point()"),
  paste0("mtcars |> dplyr::filter(var(shared = \"a\") > 0) |> ",
         "ggplot(aes(x = mpg, y = mpg)) + geom_point()")
)

test_that("collect_shared_stage_keys unions orphan stages across trees", {
  trees <- lapply(grid_plots, ptr_translate, expr_check = FALSE)
  info <- collect_shared_stage_keys(trees)
  expect_named(info, "a")
  expect_setequal(info$a$verbs, c("select", "filter"))
  # Two stages, one per tree.
  expect_equal(length(info$a$stages), 2L)
  expect_setequal(vapply(info$a$stages, `[[`, integer(1), "tree_idx"), c(1L, 2L))
})

test_that("ptr_ui_shared_panel emits one synthetic stage checkbox for the union of trees", {
  panel <- ptr_ui_shared_panel(ptr_shared(grid_plots, expr_check = FALSE))
  expect_equal(length(.find_tags(panel, has_id = "shared_a_stage_enabled")), 1L)
  expect_equal(length(.find_tags(panel, has_class = "ptr-stage-head")), 1L)
  # Verb label folds both verbs together.
  html <- paste(as.character(panel), collapse = "")
  expect_match(html, "select/filter()", fixed = TRUE)
})

test_that("ptr_app_grid_components UI carries the synthetic shared-stage checkbox", {
  skip(paste(
    "Deferred to L2/L3 redesign step 06 (ptr_app_grid -> coordinator):",
    "ptr_app_grid_components still calls the removed ptr_shared_ui."
  ))
  parts <- ptr_app_grid_components(plots = grid_plots, expr_check = FALSE)
  ui_html <- paste(as.character(parts$ui), collapse = "")
  expect_match(ui_html, "id=\"shared_a_stage_enabled\"", fixed = TRUE)
})

test_that("ptr_shared_server returns a shared_stage_enabled reactive per orphan key", {
  ctx <- new.env(parent = emptyenv())
  shiny::testServer(
    app = function(input, output, session) {
      ctx$state <- ptr_shared_server(
        ptr_shared(grid_plots, expr_check = FALSE)
      )
    },
    expr = {
      expect_named(ctx$state$shared_stage_enabled, "a")
      expect_true(shiny::is.reactive(ctx$state$shared_stage_enabled$a))
      # Default is TRUE when the checkbox has not been touched.
      expect_true(shiny::isolate(ctx$state$shared_stage_enabled$a()))
      session$setInputs(`shared_a_stage_enabled` = FALSE)
      expect_false(shiny::isolate(ctx$state$shared_stage_enabled$a()))
    }
  )
})

test_that("ptr_setup_shared_stage_enabled mirrors a FALSE reactive into state$stage_enabled", {
  # Unit-test the mirroring helper directly: feed a synthetic reactive,
  # confirm the matching orphan stage_id flips to FALSE in state.
  ctx <- new.env(parent = emptyenv())
  shiny::testServer(
    app = function(input, output, session) {
      ctx$state <- ptr_init_state(
        grid_plots[[1L]], expr_check = FALSE,
        shared = list(a = shiny::reactive("mpg"))
      )
      ctx$flag <- shiny::reactiveVal(TRUE)
      sse <- list(a = shiny::reactive(ctx$flag()))
      ptr_setup_shared_stage_enabled(ctx$state, sse)
    },
    expr = {
      session$flushReact()
      orphan_sid <- collect_orphan_shared_stages(
        shiny::isolate(ctx$state$tree())
      )[[1L]]$stage_id
      expect_true(isTRUE(
        shiny::isolate(ctx$state$stage_enabled())[[orphan_sid]]
      ))
      ctx$flag(FALSE)
      session$flushReact()
      expect_false(isTRUE(
        shiny::isolate(ctx$state$stage_enabled())[[orphan_sid]]
      ))
    }
  )
})

test_that("grid app: flipping the shared-stage checkbox disables stages in every module", {
  skip(paste(
    "Deferred to L2/L3 redesign step 06 (ptr_app_grid -> coordinator):",
    "ptr_app_grid_components still calls the removed ptr_shared_ui."
  ))
  # End-to-end: build the grid server, run testServer, flip the
  # synthetic input, and confirm BOTH plot modules' `state$stage_enabled`
  # has its orphan stage_id set to FALSE. This is the user-visible
  # promise: one checkbox prunes the same logical stage in every plot.
  parts <- ptr_app_grid_components(
    plots = grid_plots, expr_check = FALSE,
    envir = new.env(parent = baseenv())
  )
  # Inject a per-module state capture: wrap ptr_module_server to record
  # the state object it returns, keyed by module id.
  module_states <- new.env(parent = emptyenv())
  orig_ms <- getFromNamespace("ptr_module_server", "ggpaintr")
  capt_ms <- function(id, formula, ...) {
    res <- orig_ms(id, formula, ...)
    # `ptr_module_server` returns the moduleServer result, which IS the
    # state list from `ptr_server`. Cache by id for later inspection.
    module_states[[id]] <- res
    res
  }
  assignInNamespace("ptr_module_server", capt_ms, "ggpaintr")
  withr::defer(
    assignInNamespace("ptr_module_server", orig_ms, "ggpaintr")
  )
  shiny::testServer(parts$server, {
    session$flushReact()
    expected_sids <- vapply(c("plot_1", "plot_2"), function(id) {
      s <- module_states[[id]]
      collect_orphan_shared_stages(shiny::isolate(s$tree()))[[1L]]$stage_id
    }, character(1))
    # Pre: enabled.
    for (id in c("plot_1", "plot_2")) {
      sid <- expected_sids[[id]]
      expect_true(isTRUE(shiny::isolate(module_states[[id]]$stage_enabled())[[sid]]))
    }
    session$setInputs(`shared_a_stage_enabled` = FALSE)
    session$flushReact()
    # Post: disabled in BOTH modules.
    for (id in c("plot_1", "plot_2")) {
      sid <- expected_sids[[id]]
      expect_false(isTRUE(shiny::isolate(module_states[[id]]$stage_enabled())[[sid]]))
    }
  })
})

test_that("ptr_app builds without error on a formula that has an orphan shared stage", {
  # Smoke test: full ptr_app construction path must accept the example
  # formula. UI extraction mirrors test-regression-bugs-2026-05-13.R.
  app <- ptr_app(example_formula, expr_check = FALSE)
  ui_html <- paste(as.character(environment(app$httpHandler)$ui),
                   collapse = "")
  expect_match(ui_html, "ptr-stage-head", fixed = TRUE)
  expect_match(ui_html, "select()", fixed = TRUE)
})
