# ADR 0020 / PLAN-01 SC2-SC9 — translator wiring for ppLayerOff / ppVerbOff.
#
# The translator recognises the two ADR-0020 structural keywords via the
# placeholder registry (`placeholder_keyword()` returns their names) but
# never builds `ptr_ph_*` nodes for them — instead, special-unwrap branches
# rewrite the typed tree at translate time. Layer-position `ppLayerOff`
# becomes a `ptr_layer` with `default_active = !hide`; pipeline-stage
# `ppVerbOff` becomes a `ptr_call` with `default_stage_enabled = !hide`.
# Wrong-position uses and non-literal `hide` slots abort with class
# `ptr_translate_error`.

# All ADR-0020 unwrap logic runs inside `ptr_classify_calls` /
# `build_pipeline_from_lift`, which `ptr_translate` only invokes when
# `annotate = TRUE` (the default). Use the annotated path for every
# structural / validation assertion below.
ptr_translate_annot <- function(expr) {
  ptr_translate(rlang::expr_text(expr))
}

# ---- SC2: registry recognition --------------------------------------------

test_that("ppLayerOff is registered in the placeholder registry", {
  expect_true("ppLayerOff" %in% ptr_registry_keywords())
})

test_that("ppVerbOff is registered in the placeholder registry", {
  expect_true("ppVerbOff" %in% ptr_registry_keywords())
})

test_that("placeholder_keyword recognises ppLayerOff in a call expression", {
  kw <- ggpaintr:::placeholder_keyword(
    quote(ppLayerOff(geom_point(), FALSE))
  )
  expect_equal(kw, "ppLayerOff")
})

test_that("placeholder_keyword recognises ppVerbOff in a call expression", {
  kw <- ggpaintr:::placeholder_keyword(
    quote(ppVerbOff(.data, mutate(x = 1), FALSE))
  )
  expect_equal(kw, "ppVerbOff")
})

# ---- SC9: ptr_call default_stage_enabled slot ------------------------------

test_that("ptr_call has default_stage_enabled = TRUE by default", {
  n <- ggpaintr:::ptr_call(
    fun = quote(mutate),
    args = list(quote(x), quote(1)),
    expr = quote(mutate(x, 1))
  )
  expect_true("default_stage_enabled" %in% names(n))
  expect_true(n$default_stage_enabled)
})

# ---- SC3: layer-position unwrap with hide = TRUE ---------------------------

test_that("ppLayerOff(geom_point(), TRUE) at layer position is off-by-default", {
  tree <- ptr_translate_annot(quote(ggplot() + ppLayerOff(geom_point(), TRUE)))
  expect_length(tree$layers, 2L)
  l2 <- tree$layers[[2L]]
  expect_s3_class(l2, "ptr_layer")
  expect_equal(l2$name, "geom_point")
  # active_input_id is stamped in `ptr_translate`'s layer-name pass after
  # `translate_layer` returns — it is built from the resolved layer name.
  expect_equal(l2$active_input_id, "geom_point_checkbox")
  expect_false(l2$default_active)
  # The wrapper call MUST NOT appear anywhere in the tree.
  saw_wrapper <- FALSE
  ggpaintr:::ptr_walk(tree, function(n) {
    if (ggpaintr:::is_ptr_call(n) && is.symbol(n$fun) &&
        identical(as.character(n$fun), "ppLayerOff")) {
      saw_wrapper <<- TRUE
    }
  })
  expect_false(saw_wrapper)
})

# ---- SC4: layer-position unwrap with hide = FALSE produces structurally-
# ----    equal tree to the bare layer
test_that("ppLayerOff(geom_point(), FALSE) is structurally equal to bare layer", {
  t_off  <- ptr_translate_annot(quote(ggplot() + ppLayerOff(geom_point(), FALSE)))
  t_bare <- ptr_translate_annot(quote(ggplot() + geom_point()))
  expect_true(ggpaintr:::ptr_tree_structural_equal(t_off, t_bare))
  expect_true(t_off$layers[[2L]]$default_active)
})

# ---- SC5: pipeline-stage unwrap with hide = TRUE ---------------------------

test_that("ppVerbOff at pipeline-stage position with hide=TRUE is off-by-default", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbOff(mutate(mpg = mpg + 100), TRUE))
  ))
  # The ggplot layer's data_arg should be a ptr_pipeline.
  ggplot_layer <- tree$layers[[1L]]
  expect_s3_class(ggplot_layer, "ptr_layer")
  da <- ggplot_layer$data_arg
  expect_s3_class(da, "ptr_pipeline")
  # The first downstream stage (index 2, since stage 1 is the source) is
  # the unwrapped mutate ptr_call.
  expect_gte(length(da$stages), 2L)
  stage2 <- da$stages[[2L]]
  expect_s3_class(stage2, "ptr_call")
  expect_true(is.symbol(stage2$fun))
  expect_equal(as.character(stage2$fun), "mutate")
  expect_false(stage2$default_stage_enabled)
  # stage_id is the standard ggplot-rooted pattern.
  expect_match(stage2$stage_id, "^ggplot_.*_stage_enabled$")
  # No surviving ppVerbOff wrapper anywhere.
  saw_wrapper <- FALSE
  ggpaintr:::ptr_walk(tree, function(n) {
    if (ggpaintr:::is_ptr_call(n) && is.symbol(n$fun) &&
        identical(as.character(n$fun), "ppVerbOff")) {
      saw_wrapper <<- TRUE
    }
  })
  expect_false(saw_wrapper)
})

# ---- SC6: pipeline-stage unwrap with hide = FALSE produces structurally
# ----    equal tree to the bare stage
test_that("ppVerbOff with hide=FALSE is structurally equal to the bare stage", {
  t_off  <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbOff(mutate(x = 1), FALSE))
  ))
  t_bare <- ptr_translate_annot(quote(ggplot(mtcars |> mutate(x = 1))))
  expect_true(ggpaintr:::ptr_tree_structural_equal(t_off, t_bare))
})

# ---- SC7: translate-time validation, non-literal hide ----------------------

test_that("ppLayerOff with non-literal hide aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(ggplot() + ppLayerOff(geom_point(), some_var))),
    class = "ptr_translate_error",
    regexp = "ppLayerOff.*literal|literal.*ppLayerOff"
  )
})

test_that("ppVerbOff with non-literal hide aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot(mtcars |> ppVerbOff(mutate(x = 1), some_var))
    )),
    class = "ptr_translate_error",
    regexp = "ppVerbOff.*literal|literal.*ppVerbOff"
  )
})

# ---- SC8: translate-time validation, wrong-position keyword ----------------

test_that("ppLayerOff outside layer position aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot(aes(x = ppLayerOff(geom_point(), FALSE)))
    )),
    class = "ptr_translate_error",
    regexp = "ppLayerOff.*position|position.*ppLayerOff"
  )
})

test_that("ppVerbOff outside pipeline-stage position aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot() + ppVerbOff(mtcars, mutate(x = 1), FALSE)
    )),
    class = "ptr_translate_error",
    regexp = "ppVerbOff.*position|position.*ppVerbOff"
  )
})
