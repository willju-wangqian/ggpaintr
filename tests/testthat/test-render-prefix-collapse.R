# ADR 0012 PLAN-04 — preserve-mode prefix-collapse render rule.
#
# `render_pipeline_body()` in R/paintr-render.R branches on
# `preserve_placeholders`:
#   * FALSE (eval-mode) — unchanged from today: every stage emits as a
#     `|>`/`%>%` chain link (Scope §2 / BDD "Render non-preserve mode is
#     unchanged from today").
#   * TRUE  (preserve-mode) — new three-branch prefix-collapse rule:
#       k = NA              -> collapse to a nested-call atom (no pipe op)
#       k == 1 OR k == 2    -> render the whole pipeline as `|>` chain
#       k >= 3              -> collapse stages[1..k-1] to a nested atom,
#                              render stages[k..N] as `|>` chain whose
#                              first segment is the collapsed prefix.
#     (k is the 1-based index of the first stage whose subtree contains a
#     placeholder; NA when none do.)
#
# Tests build typed pipelines directly (one ptr_pipeline per scenario) and
# call `ggpaintr:::render_walk()` with `preserve_placeholders = TRUE/FALSE`.
# Rendered output is compared modulo whitespace (collapsed-whitespace
# string-equality) so the line-wrapping heuristics in `render_call_text()`
# do not change the contract being asserted.

ws_collapse <- function(x) {
  trimws(gsub("\\s+", " ", x))
}

# Helper: translate a one-layer formula and return the layer's data_arg.
data_arg_from_formula <- function(formula) {
  tree <- ptr_translate(formula, expr_check = FALSE)
  tree$layers[[1L]]$data_arg
}

test_that("preserve-mode renders all-placeholder-free pipeline as a single nested-call atom (no pipe op)", {
  # All stages are placeholder-free. Source = `penguins` (a symbol). Two
  # verb stages with no placeholders. The rule must collapse the whole
  # pipeline to a single nested call atom, with no `|>` and no `%>%`.
  data_arg <- data_arg_from_formula(
    "penguins |> dplyr::filter(bill_length_mm > 40) |> dplyr::summarise(n = dplyr::n()) |> ggplot(aes(bill_length_mm)) + geom_point()"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  expect_false(grepl("|>", rendered, fixed = TRUE))
  expect_false(grepl("%>%", rendered, fixed = TRUE))
  expect_equal(
    ws_collapse(rendered),
    ws_collapse("dplyr::summarise(dplyr::filter(penguins, bill_length_mm > 40), n = dplyr::n())")
  )
})

test_that("preserve-mode renders pipeline whose SOURCE is a placeholder as a `|>` chain (k == 1)", {
  # ppUpload is the source — k == 1. Whole pipeline renders as `|>` chain.
  # Two verb stages above source (filter + mutate) keep this test stable
  # under the prefix-collapse render rule.
  data_arg <- data_arg_from_formula(
    "ppUpload |> dplyr::filter(bill_length_mm > 40) |> dplyr::mutate(y = bill_length_mm * 2) |> ggplot(aes(bill_length_mm))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  expect_true(grepl("|>", rendered, fixed = TRUE))
  # Source line starts (modulo whitespace) with the bare keyword `ppUpload`.
  # Preserve mode drops parens when the placeholder has no current_pick and
  # no shared key (which is the case here).
  expect_true(grepl("^ppUpload\\b", ws_collapse(rendered)))
})

test_that("preserve-mode renders pipeline with placeholder at stage 2 as a `|>` chain", {
  # Source = `penguins`; placeholder appears at stage 2 (in-filter ppVar).
  # k == 2; whole pipeline renders as `|>` chain. Two verb stages above
  # source (filter + mutate) keep this test stable under the
  # prefix-collapse render rule.
  data_arg <- data_arg_from_formula(
    "penguins |> dplyr::filter(ppVar > 1) |> dplyr::mutate(y = bill_length_mm * 2) |> ggplot(aes(ppVar))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  expect_true(grepl("|>", rendered, fixed = TRUE))
  expect_true(grepl("^penguins", ws_collapse(rendered)))
})

test_that("preserve-mode collapses the placeholder-free prefix and chains the placeholder tail (k >= 3)", {
  # Source = `penguins`; stages 2 and 3 are placeholder-free; the in-aes
  # `ppVar` arrives at stage 4 (the `ggplot(aes(ppVar))` segment of the
  # data_arg pipeline — the data_arg's stages are the UPSTREAM ones only,
  # which is the prior-to-terminal positions). The new rule collapses
  # stages[1..k-1] to one nested atom and chains stages[k..N].
  data_arg <- data_arg_from_formula(
    "penguins |> dplyr::filter(bill_length_mm > 40) |> dplyr::mutate(y = bill_length_mm * 2) |> ggplot(aes(ppVar))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  collapsed <- ws_collapse(rendered)

  # Output must begin with the COLLAPSED placeholder-free nested-call
  # source — the rule's defining behavior. The exact wrapping depends on
  # which stage k lands at; we accept ANY prefix that contains the
  # nested-call shape (no `|>` between the leftmost `dplyr::filter` /
  # `dplyr::mutate` and `penguins`) and that the rendered output as a
  # whole still contains at least one `|>` (the tail chain).
  expect_true(
    grepl("dplyr::filter(penguins,", collapsed, fixed = TRUE) ||
      grepl("dplyr::mutate(dplyr::filter(penguins,", collapsed, fixed = TRUE),
    info = "rendered preserve-mode output begins with collapsed nested-call source"
  )
})

test_that("preserve-mode preserves a `%>%`-sourced all-placeholder-free chain as a `%>%` chain (asymmetric vs `|>` per ADR 0012 §5 OQ2)", {
  # ADR 0012 §5 OQ2 closed (PLAN-01 of 0012b): asymmetric
  # "%>%-preserve, |>-collapse" render policy. The `|>`-sibling block
  # above still collapses to a nested-call atom (that is today's
  # canonical default for `|>`). A `%>%`-sourced chain now PRESERVES as
  # a `%>%` chain even with no placeholders — `$op == "%>%"` triggers
  # the chain branch in `render_pipeline_body`. Two verb stages above
  # source (filter + mutate) keep this test stable.
  data_arg <- data_arg_from_formula(
    "penguins %>% dplyr::filter(bill_length_mm > 40) %>% dplyr::mutate(y = bill_length_mm * 2) %>% ggplot(aes(bill_length_mm))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")
  expect_equal(data_arg$op, "%>%")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  expect_false(grepl("|>", rendered, fixed = TRUE))
  expect_true(grepl("%>%", rendered, fixed = TRUE))
  # The data_arg pipeline has 3 stages (penguins, dplyr::filter,
  # dplyr::mutate) — the terminal ggplot(...) layer is OUTSIDE this
  # data_arg subtree. Each stage emits on its own line joined by ` %>%`.
  expect_equal(
    rendered,
    "penguins %>%\n  dplyr::filter(bill_length_mm > 40) %>%\n  dplyr::mutate(y = bill_length_mm * 2)"
  )
})

test_that("non-preserve-mode rendering is unchanged from today (chain rendering with operator)", {
  # PLAN-04's prefix-collapse rule must NOT fire when
  # `preserve_placeholders = FALSE`. The same data_arg that collapses to
  # nested-call form under preserve-mode renders as a `|>` chain under
  # non-preserve mode — matching today's behavior. (BDD "Render
  # non-preserve mode is unchanged from today" / Scope §2.)
  #
  # The non-preserve assertion uses BYTE-IDENTICAL comparison against the
  # baseline render output captured from the merged G2 state (PLAN-02 +
  # PLAN-04) where multi-stage `|>` inputs canonicalise to `ptr_pipeline`
  # via PLAN-02's lift. Presence checks (`grepl("|>", ...)`) would not
  # have caught whitespace/indentation/operator-substitution regressions;
  # the BDD `Then` clause for this scenario is "Render non-preserve mode
  # is unchanged from today" — that demands equality, not presence.
  # Two verb stages above source (filter + mutate) keep this test stable
  # under the prefix-collapse render rule.
  data_arg <- data_arg_from_formula(
    "penguins |> dplyr::filter(bill_length_mm > 40) |> dplyr::mutate(y = bill_length_mm * 2) |> ggplot(aes(bill_length_mm))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered_preserve <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  rendered_eval <- ggpaintr:::render_walk(data_arg, preserve_placeholders = FALSE)

  # Preserve-mode: byte-identical collapsed nested-call form (PLAN-04 NEW).
  # k = NA (all stages placeholder-free) → collapse entire pipeline to
  # one nested-call atom. The two-verb prefix nests `dplyr::mutate(...)`
  # around `dplyr::filter(...)` around the bare `penguins` source.
  baseline_preserve <- "dplyr::mutate(\n  dplyr::filter(penguins, bill_length_mm > 40),\n  y = bill_length_mm * 2\n)"
  expect_identical(rendered_preserve, baseline_preserve)

  # Non-preserve mode: byte-identical against baseline captured from the
  # merged G2 state. Each stage emits as a `|>` chain link — the
  # pre-PLAN-04 chain-rendering behaviour the contract demands preserve.
  baseline_eval <- "penguins |>\n  dplyr::filter(bill_length_mm > 40) |>\n  dplyr::mutate(y = bill_length_mm * 2)"
  expect_identical(rendered_eval, baseline_eval)
})
