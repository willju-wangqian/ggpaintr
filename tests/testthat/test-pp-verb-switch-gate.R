# ADR 0021 / PLAN-04 SC1-SC7 — gate disjunction + stamp_default_stage_enabled_ids removal.
#
# `is_data_chain_call(node)` becomes the single source of truth for "should
# this stage have a UI checkbox?": `is_ptr_call(node) && (walk_has_placeholder(node)
# || isTRUE(node$has_user_control))`. The second-pass helper
# `stamp_default_stage_enabled_ids` is deleted; its prior rescue of
# `ppVerbOff(hide=TRUE)` stages with no placeholders is now covered by
# `ppVerbOff` unwrap stamping `has_user_control = TRUE` (Option A).

# Helper: translate from a quoted expression (mirrors the calling
# convention used by test-pp-verb-switch-translate.R and test-pp-off-translate.R).
ptr_translate_annot <- function(expr) {
  ptr_translate(rlang::expr_text(expr))
}

# Helper: walk a tree and return the first ptr_call whose head is named
# `nm` (a chr scalar). Used to grab the carrier ptr_call after unwrap.
find_first_call_named <- function(root, nm) {
  out <- NULL
  ggpaintr:::ptr_walk(root, function(n) {
    if (!is.null(out)) return()
    if (ggpaintr:::is_ptr_call(n) && is.symbol(n$fun) &&
        identical(as.character(n$fun), nm)) {
      out <<- n
    }
  })
  out
}

# ---- SC-1: ppVerbSwitch(..., TRUE) carrier gets a stage_id ----------------
test_that("SC-1: ppVerbSwitch with switch_on=TRUE carrier has a non-NULL stage_id", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(mpg = mpg + 100), TRUE))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_type(carrier$stage_id, "character")
  expect_true(nzchar(carrier$stage_id))
  expect_match(carrier$stage_id, "_stage_enabled$")
})

# ---- SC-2: ppVerbSwitch(..., FALSE) carrier ALSO gets a stage_id ---------
test_that("SC-2: ppVerbSwitch with switch_on=FALSE carrier has a non-NULL stage_id", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(mpg = mpg + 100), FALSE))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_type(carrier$stage_id, "character")
  expect_true(nzchar(carrier$stage_id))
})

# ---- SC-3: bare verb stage does NOT get a stage_id ------------------------
test_that("SC-3: bare verb stage (no wrapper, no placeholder) carrier stage_id is NULL", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> mutate(mpg = mpg + 100))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_identical(is.null(carrier$stage_id), TRUE)
})

# ---- SC-4: placeholder-bearing stage still gets a stage_id ---------------
test_that("SC-4: placeholder-bearing stage carrier has a non-NULL stage_id", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> mutate(x = ppVar(col)))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_type(carrier$stage_id, "character")
  expect_true(nzchar(carrier$stage_id))
})

# ---- SC-5: stamp_default_stage_enabled_ids is absent from the namespace ---
test_that("SC-5: stamp_default_stage_enabled_ids is removed from the package namespace", {
  expect_false(exists("stamp_default_stage_enabled_ids",
                      envir = asNamespace("ggpaintr"),
                      inherits = FALSE))
})

# ---- SC-6: ppVerbOff(hide=TRUE) still boots OFF after stamp helper removal
test_that("SC-6: ppVerbOff(verb, TRUE) carrier has default_stage_enabled=FALSE AND a non-NULL stage_id", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbOff(mutate(mpg = mpg + 100), TRUE))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  # Tree-side: default_stage_enabled is the load-bearing flag downstream
  # readers (snapshot, UI, server spec-apply) consume to boot the checkbox OFF.
  expect_identical(carrier$default_stage_enabled, FALSE)
  # And the gate disjunction (via Option A's has_user_control stamp) gives
  # the carrier a stage_id, restoring what stamp_default_stage_enabled_ids
  # used to rescue.
  expect_type(carrier$stage_id, "character")
  expect_true(nzchar(carrier$stage_id))
})

# ---- SC-7: structural equality holds across the gate extension ----------
test_that("SC-7: ppVerbSwitch tree compares structurally equal to the bare-verb tree", {
  tree_A <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(x = 1), FALSE))
  ))
  tree_B <- ptr_translate_annot(quote(
    ggplot(mtcars |> mutate(x = 1))
  ))
  expect_true(ggpaintr:::ptr_tree_structural_equal(tree_A, tree_B))
})
