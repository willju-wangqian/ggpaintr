# ADR 0021 PLAN-01 — `ptr_call` carries two new sibling UI-metadata slots
# (`has_user_control`, `stage_label`) and the structural comparator excludes
# them — along with the pre-existing `stage_id` UI-routing field — from the
# walk. These tests pin the constructor defaults, the explicit-arg round-trip,
# and the comparator-exclusion contract for all three UI-metadata names.

test_that("ptr_call default args produce has_user_control = FALSE and stage_label = NULL", {
  node <- ggpaintr:::ptr_call("foo", list())
  expect_true(identical(node$has_user_control, FALSE))
  expect_true(is.null(node$stage_label))
})

test_that("ptr_call explicit args round-trip has_user_control and stage_label", {
  node <- ggpaintr:::ptr_call(
    "foo", list(),
    has_user_control = TRUE, stage_label = "X"
  )
  expect_true(identical(node$has_user_control, TRUE))
  expect_true(identical(node$stage_label, "X"))
})

test_that("ptr_tree_structural_equal ignores stage_id, has_user_control, and stage_label", {
  # SC-3a: differ only on stage_id
  a_sid <- ggpaintr:::ptr_call("foo", list())
  b_sid <- ggpaintr:::ptr_call("foo", list())
  a_sid$stage_id <- "foo_1_stage_enabled"
  b_sid$stage_id <- NULL
  expect_true(identical(
    ggpaintr:::ptr_tree_structural_equal(a_sid, b_sid),
    TRUE
  ))

  # SC-3b: differ only on has_user_control
  a_huc <- ggpaintr:::ptr_call("foo", list(), has_user_control = TRUE)
  b_huc <- ggpaintr:::ptr_call("foo", list(), has_user_control = FALSE)
  expect_true(identical(
    ggpaintr:::ptr_tree_structural_equal(a_huc, b_huc),
    TRUE
  ))

  # SC-3c: differ only on stage_label
  a_lbl <- ggpaintr:::ptr_call("foo", list(), stage_label = "Transform x")
  b_lbl <- ggpaintr:::ptr_call("foo", list(), stage_label = NULL)
  expect_true(identical(
    ggpaintr:::ptr_tree_structural_equal(a_lbl, b_lbl),
    TRUE
  ))
})
