# ADR 0021 / PLAN-03 SC1-SC9 — translator wiring for ppVerbSwitch.
#
# `ppVerbSwitch(.data, verb_expr, switch_on, label)` at a pipeline-stage
# position is intercepted by `build_pipeline_from_lift`'s unwrap branch.
# The wrapper never appears in the typed tree; instead the inner verb
# call's `ptr_call` is stamped with `has_user_control = TRUE`,
# `default_stage_enabled = switch_on`, and `stage_label = label`.
# Non-literal `switch_on` / `label`, non-call `verb_expr`, and
# wrong-position uses abort with class `"ptr_translate_error"`.

# Annotation flag governs the unwrap path; mirror the helper from
# `test-pp-toggles-translate.R` so this file uses the same calling convention.
ptr_translate_annot <- function(expr) {
  ptr_translate(rlang::expr_text(expr))
}

# Helper: walk a tree and return the first ptr_call whose head is named `nm`
# (a chr scalar). Used by SC-1..SC-3 / SC-9 to grab the carrier node after
# unwrap.
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

# ---- SC-1: switch_on=TRUE stamps has_user_control + default_stage_enabled=TRUE
test_that("ppVerbSwitch with switch_on=TRUE stamps user-control TRUE + default-enabled TRUE", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(mpg = mpg + 100), TRUE))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_identical(carrier$has_user_control, TRUE)
  expect_identical(carrier$default_stage_enabled, TRUE)
  expect_null(carrier$stage_label)
  # The wrapper call MUST NOT appear anywhere in the tree.
  saw_wrapper <- FALSE
  ggpaintr:::ptr_walk(tree, function(n) {
    if (ggpaintr:::is_ptr_call(n) && is.symbol(n$fun) &&
        identical(as.character(n$fun), "ppVerbSwitch")) {
      saw_wrapper <<- TRUE
    }
  })
  expect_false(saw_wrapper)
})

# ---- SC-2: switch_on=FALSE stamps default_stage_enabled=FALSE
test_that("ppVerbSwitch with switch_on=FALSE stamps default_stage_enabled=FALSE", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(mpg = mpg + 100), FALSE))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_identical(carrier$has_user_control, TRUE)
  expect_identical(carrier$default_stage_enabled, FALSE)
})

# ---- SC-3: label is stamped on the carrier
test_that("ppVerbSwitch stamps stage_label on the carrier ptr_call", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(mpg = mpg + 100), TRUE, "Transform mpg"))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_identical(carrier$stage_label, "Transform mpg")
})

# ---- SC-4: structural equality with bare-verb tree (switch_on=FALSE)
test_that("ppVerbSwitch(switch_on=FALSE) is structurally equal to the bare verb stage", {
  tree_A <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(mpg = mpg + 100), FALSE))
  ))
  tree_B <- ptr_translate_annot(quote(
    ggplot(mtcars |> mutate(mpg = mpg + 100))
  ))
  expect_true(ggpaintr:::ptr_tree_structural_equal(tree_A, tree_B))
})

# ---- SC-5: switch_on must be a literal logical
test_that("ppVerbSwitch with non-literal switch_on aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot(mtcars |> ppVerbSwitch(mutate(x = 1), some_var))
    )),
    class = "ptr_translate_error",
    regexp = "`switch_on`"
  )
})

# ---- SC-6: label must be a literal character or NULL
test_that("ppVerbSwitch with non-literal label aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot(mtcars |> ppVerbSwitch(mutate(x = 1), TRUE, my_label_var))
    )),
    class = "ptr_translate_error",
    regexp = "`label`"
  )
})

# ---- SC-7: verb_expr must be a call
test_that("ppVerbSwitch with non-call verb_expr aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot(mtcars |> ppVerbSwitch(42, TRUE))
    )),
    class = "ptr_translate_error",
    regexp = "verb call.*ppVerbSwitch|ppVerbSwitch.*verb call"
  )
})

# ---- SC-8: wrong-position aborts via the structural-wrapper assertion
test_that("ppVerbSwitch outside pipeline-stage position aborts (ptr_translate_error)", {
  expect_error(
    ptr_translate_annot(quote(
      ggplot(mtcars, aes(x = ppVerbSwitch(mutate(x = 1), TRUE)))
    )),
    class = "ptr_translate_error",
    regexp = "ppVerbSwitch"
  )
})

# ---- SC-9: composes with inner placeholder
test_that("ppVerbSwitch composes with an inner ppVar placeholder", {
  tree <- ptr_translate_annot(quote(
    ggplot(mtcars |> ppVerbSwitch(mutate(x = ppVar(col)), TRUE, "Transform x"))
  ))
  carrier <- find_first_call_named(tree, "mutate")
  expect_s3_class(carrier, "ptr_call")
  expect_identical(carrier$has_user_control, TRUE)
  expect_identical(carrier$stage_label, "Transform x")
  # Walking the carrier finds at least one placeholder node with
  # keyword == "ppVar". (`ppVar` produces a `ptr_ph_data_consumer`, not a
  # `ptr_ph_value` — the plan's `ptr_ph_value` wording is a slip; we use
  # the union predicate `is_ptr_placeholder` which honors the SC's intent
  # of "the inner placeholder is present unchanged".)
  saw_pp_var <- FALSE
  ggpaintr:::ptr_walk(carrier, function(n) {
    if (ggpaintr:::is_ptr_placeholder(n) &&
        identical(n$keyword, "ppVar")) {
      saw_pp_var <<- TRUE
    }
  })
  expect_true(saw_pp_var)
})
