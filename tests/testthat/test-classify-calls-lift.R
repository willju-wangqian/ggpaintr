# PLAN-02 (ADR 0012) — end-to-end tree-shape assertions for the
# canonical-pipeline-lift pass `ptr_classify_calls`. Tests verify the BDD
# scenarios for canonical-tree identity across surface forms, the `+` fence,
# terminal grounding, id stability, and the retirement of the old
# `%>%`-only translation symbols.

collect_all_placeholder_ids <- function(tree) {
  ids <- character()
  ggpaintr:::ptr_walk(tree, function(n) {
    if (is_ptr_placeholder(n) && !is.null(n$id)) {
      ids[[length(ids) + 1L]] <<- n$id
    }
  })
  sort(unique(ids))
}

# ---- canonical-tree identity ---------------------------------------------

test_that("canonical-tree identity: native pipe vs magrittr pipe", {
  f_native   <- "ppUpload |> filter(ppVar > ppNum) |> mutate(new_var = ppVar + 1) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  f_magrittr <- "ppUpload %>% filter(ppVar > ppNum) %>% mutate(new_var = ppVar + 1) %>% ggplot(aes(ppVar, ppVar)) + geom_point()"

  t_n <- ptr_translate(f_native)
  t_m <- ptr_translate(f_magrittr)

  da_n <- t_n$layers[[1L]]$data_arg
  da_m <- t_m$layers[[1L]]$data_arg

  expect_s3_class(da_n, "ptr_pipeline")
  expect_s3_class(da_m, "ptr_pipeline")
  expect_equal(da_n$op, "|>")
  expect_equal(da_m$op, "|>")
  expect_equal(length(da_n$stages), length(da_m$stages))
  expect_true(ggpaintr:::ptr_tree_structural_equal(da_n, da_m))
})

test_that("canonical-tree identity: native pipe vs nested-call", {
  f_native <- "ppUpload |> filter(ppVar > ppNum) |> mutate(new_var = ppVar + 1) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  f_nested <- "ggplot(mutate(filter(ppUpload, ppVar > ppNum), new_var = ppVar + 1), aes(ppVar, ppVar)) + geom_point()"

  t_n <- ptr_translate(f_native)
  t_s <- ptr_translate(f_nested)

  da_n <- t_n$layers[[1L]]$data_arg
  da_s <- t_s$layers[[1L]]$data_arg

  expect_s3_class(da_n, "ptr_pipeline")
  expect_s3_class(da_s, "ptr_pipeline")
  expect_equal(da_n$op, "|>")
  expect_equal(da_s$op, "|>")
  expect_true(ggpaintr:::ptr_tree_structural_equal(da_n, da_s))
})

# ---- terminal grounding --------------------------------------------------

test_that("bare-data-name terminal grounds the lift in nested-call form", {
  f <- "ggplot(filter(penguins, bill_length_mm > 40), aes(bill_length_mm)) + geom_point()"
  tree <- ptr_translate(f, expr_check = FALSE)
  # `filter(penguins, ...)` is single-stage at the data-arg level and is
  # therefore left as a `ptr_call` rather than being lifted into a pipeline:
  # GATE 0 (single-stage) is the documented rejection reason for a chain
  # that has no manipulation above the source. The grounded shape is the
  # ptr_call whose deepest non-call (after split) is the bare symbol
  # `penguins`.
  da <- tree$layers[[1L]]$data_arg
  expect_s3_class(da, "ptr_call")
  res <- ggpaintr:::try_lift_to_pipeline(da$expr)
  expect_false(isTRUE(res$success))
  expect_equal(res$reason, "single-stage")
  # Cross-check the grounding via direct resugar: the source slot is the
  # bare `penguins` symbol regardless of GATE 0 firing.
  parts <- ggpaintr:::resugar_pipeline_stages(da$expr)
  expect_identical(parts$source, quote(penguins))
})

test_that("bare-data terminal lifts into pipeline when the chain has depth >= 2", {
  f <- "ggplot(mutate(filter(penguins, x > 1), y = z), aes(y)) + geom_point()"
  tree <- ptr_translate(f, expr_check = FALSE)
  da <- tree$layers[[1L]]$data_arg
  expect_s3_class(da, "ptr_pipeline")
  expect_gte(length(da$stages), 2L)
  expect_identical(da$stages[[1L]]$expr, quote(penguins))
})

test_that("opaque-call terminal does NOT lift (native pipe, depth >= 2)", {
  f <- "make_data() |> filter(ppVar > 1) |> mutate(y = z) |> ggplot(aes(x)) + geom_point()"
  tree <- ptr_translate(f, expr_check = FALSE)
  da <- tree$layers[[1L]]$data_arg
  expect_false(inherits(da, "ptr_pipeline"))
  expect_s3_class(da, "ptr_call")
})

test_that("opaque-call terminal does NOT lift (nested-call form, depth >= 2)", {
  f <- "ggplot(mutate(filter(read_csv(\"foo.csv\"), ppVar > 1), y = z), aes(x)) + geom_point()"
  tree <- ptr_translate(f, expr_check = FALSE)
  da <- tree$layers[[1L]]$data_arg
  expect_false(inherits(da, "ptr_pipeline"))
  expect_s3_class(da, "ptr_call")
})

# ---- + fence -------------------------------------------------------------

test_that("the + fence holds — lift never walks into $children", {
  # `aes(x = filter(z, y > 1))` puts a nested call inside the layer's
  # children (the aes() arg). The classify_calls pass must process
  # `layer$data_arg` only, never `$children`, so no ptr_pipeline can
  # appear anywhere inside the layer's children subtree.
  f <- "ggplot(penguins, aes(x = filter(z, y > 1))) + geom_point()"
  tree <- ptr_translate(f, expr_check = FALSE)
  found_pipeline_in_children <- FALSE
  for (layer in tree$layers) {
    if (!is_ptr_layer(layer)) next
    ggpaintr:::ptr_walk(layer$children, function(n) {
      if (is_ptr_pipeline(n)) found_pipeline_in_children <<- TRUE
    })
  }
  expect_false(found_pipeline_in_children)
})

# ---- id stability across surface forms ----------------------------------

test_that("placeholder ids are identical across surface forms (post-lift)", {
  f_native   <- "penguins |> filter(ppVar > 1) |> arrange(ppVar) |> ggplot(aes(ppVar)) + geom_point()"
  f_magrittr <- "penguins %>% filter(ppVar > 1) %>% arrange(ppVar) %>% ggplot(aes(ppVar)) + geom_point()"
  f_nested   <- "ggplot(arrange(filter(penguins, ppVar > 1), ppVar), aes(ppVar)) + geom_point()"

  t_n <- ptr_translate(f_native,   expr_check = FALSE)
  t_m <- ptr_translate(f_magrittr, expr_check = FALSE)
  t_s <- ptr_translate(f_nested,   expr_check = FALSE)

  ids_n <- collect_all_placeholder_ids(t_n)
  ids_m <- collect_all_placeholder_ids(t_m)
  ids_s <- collect_all_placeholder_ids(t_s)
  expect_equal(ids_n, ids_m)
  expect_equal(ids_m, ids_s)
})

test_that("stage_ids are stamped on multi-stage lifted pipelines", {
  f_native <- "penguins |> filter(ppVar > 1) |> arrange(ppVar) |> ggplot(aes(ppVar)) + geom_point()"
  tree <- ptr_translate(f_native, expr_check = FALSE)
  da <- tree$layers[[1L]]$data_arg
  expect_s3_class(da, "ptr_pipeline")
  # Stage 1 = source (literal `penguins`), no stage_id.
  # Stages >= 2 carry placeholders, so each gets a stage_id.
  expect_null(da$stages[[1L]]$stage_id)
  for (i in seq(2L, length(da$stages))) {
    expect_true(!is.null(da$stages[[i]]$stage_id))
  }
})

# ---- retired %>%-only translation symbols --------------------------------

test_that("old pipeline-translation symbols are deleted", {
  ns <- asNamespace("ggpaintr")
  expect_false(exists("translate_pipeline",   envir = ns, inherits = FALSE))
  expect_false(exists("collect_pipe_stages",  envir = ns, inherits = FALSE))
  expect_false(exists("translate_piped_layer", envir = ns, inherits = FALSE))
})

# ---- named-first-arg eval round trip -------------------------------------

test_that("pipeline_to_eval_expr round-trips first_arg_name when the source slot was named", {
  # Build a typed pipeline by hand with `first_arg_name = "data"` on stage 2
  # so the inserted-source slot is restored as `data = ...` at eval time.
  src <- ptr_literal(quote(penguins))
  stage_call <- ptr_call(
    fun  = quote(ggplot),
    args = list(quote(aes(x = bill_length))),
    expr = quote(ggplot(aes(x = bill_length)))
  )
  stage_call$first_arg_name <- "data"
  pipe <- ptr_pipeline(
    stages = list(src, stage_call),
    op = "|>",
    expr = quote(ggplot(data = penguins, aes(x = bill_length)))
  )
  expect_identical(
    ggpaintr:::pipeline_to_eval_expr(pipe),
    quote(ggplot(data = penguins, aes(x = bill_length)))
  )
})

test_that("pipeline_to_eval_expr emits positional source when first_arg_name is empty", {
  src <- ptr_literal(quote(penguins))
  stage_call <- ptr_call(
    fun  = quote(filter),
    args = list(quote(x > 1)),
    expr = quote(filter(x > 1))
  )
  stage_call$first_arg_name <- ""
  pipe <- ptr_pipeline(
    stages = list(src, stage_call),
    op = "|>",
    expr = quote(filter(penguins, x > 1))
  )
  expect_identical(
    ggpaintr:::pipeline_to_eval_expr(pipe),
    quote(filter(penguins, x > 1))
  )
})
