# PLAN-01 (ADR 0012 §2): reserve the classification-pass slot in ptr_translate.
# This pass must be a true no-op (identity on the tree). Plan 02 fills in the
# lift / role-tagging body. These tests pin the no-op contract so that any
# accidental tree rewriting in this pass is caught immediately.
#
# Scenarios 2 and 3 (translate output unchanged for %>% and |> inputs) compare
# against pre-edit baseline trees captured BEFORE this plan added the slot to
# ptr_translate. The baselines are stored as .rds fixtures under
# tests/testthat/fixtures/ so the comparison survives across runs.

test_that("ptr_classify_calls is identity on a translated tree", {
  root <- ptr_translate(
    "ggplot(penguins, aes(bill_length_mm)) + geom_point()"
  )
  expect_true(identical(ptr_classify_calls(root), root))
})

test_that("ptr_translate output unchanged for %>% input (vs pre-slot baseline)", {
  baseline_path <- test_path("fixtures", "classify-calls-noop-baseline-pct.rds")
  skip_if_not(file.exists(baseline_path), "baseline fixture missing")
  baseline <- readRDS(baseline_path)
  root <- ptr_translate(
    "penguins %>% filter(bill_length_mm > 40) %>% ggplot(aes(bill_length_mm)) + geom_point()"
  )
  expect_true(identical(root$layers, baseline$layers))
  # And: no new node classes introduced anywhere in the tree.
  collect_classes <- function(x) {
    out <- list(class(x))
    if (is.list(x)) {
      for (child in x) out <- c(out, collect_classes(child))
    }
    out
  }
  new_classes      <- unique(unlist(collect_classes(root$layers)))
  baseline_classes <- unique(unlist(collect_classes(baseline$layers)))
  expect_true(all(new_classes %in% baseline_classes))
})

test_that("ptr_translate output unchanged for |> input (vs pre-slot baseline)", {
  baseline_path <- test_path("fixtures", "classify-calls-noop-baseline-native.rds")
  skip_if_not(file.exists(baseline_path), "baseline fixture missing")
  baseline <- readRDS(baseline_path)
  root <- ptr_translate(
    "penguins |> filter(bill_length_mm > 40) |> ggplot(aes(bill_length_mm)) + geom_point()"
  )
  expect_true(identical(root$layers, baseline$layers))
})

test_that("ptr_classify_calls rejects non-ptr_root input", {
  expect_error(ptr_classify_calls(list()), class = "rlang_error")
})
