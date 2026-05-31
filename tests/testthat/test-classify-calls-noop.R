# PLAN-02 (ADR 0012 §2): the classification-pass slot reserved by PLAN-01 is
# now filled in. `ptr_classify_calls` lifts nested-call data-arg chains into
# canonical `ptr_pipeline` nodes whenever the lift's three gates pass; it is
# no longer the identity. The PLAN-01 baseline-fixture tests are retired here
# (the fixtures captured the pre-lift tree shape that PLAN-02 supersedes);
# see `test-classify-calls-lift.R` for the new tree-shape contract. The
# remaining test pins the input-validation contract.

test_that("ptr_classify_calls rejects non-ptr_root input", {
  expect_error(ptr_classify_calls(list()), class = "rlang_error")
})

test_that("ptr_classify_calls is a no-op on a tree without a liftable chain", {
  # A bare-data ggplot has no nested data-arg chain to lift; the pass must
  # leave the tree's layers identical to the pre-pass shape.
  root <- ptr_translate(
    "ggplot(penguins, aes(bill_length_mm)) + geom_point()",
    expr_check = FALSE
  )
  expect_true(identical(ptr_classify_calls(root)$layers, root$layers))
})
