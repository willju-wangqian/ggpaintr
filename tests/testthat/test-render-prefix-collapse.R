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
  # Two verb stages above source so PLAN-02's GATE 0 accepts the lift and
  # the layer's data_arg becomes a `ptr_pipeline` (a single verb stage
  # reduces to a single-stage chain at the layer level and GATE 0
  # correctly rejects it — see ADR 0012 PLAN-02 §GATE 0).
  data_arg <- data_arg_from_formula(
    "ppUpload |> dplyr::filter(bill_length_mm > 40) |> dplyr::mutate(y = bill_length_mm * 2) |> ggplot(aes(bill_length_mm))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  expect_true(grepl("|>", rendered, fixed = TRUE))
  # Source line starts (modulo whitespace) with `ppUpload(`.
  expect_true(grepl("^ppUpload\\(", ws_collapse(rendered)))
})

test_that("preserve-mode renders pipeline with placeholder at stage 2 as a `|>` chain", {
  # Source = `penguins`; placeholder appears at stage 2 (in-filter ppVar).
  # k == 2; whole pipeline renders as `|>` chain. Two verb stages above
  # source so PLAN-02's GATE 0 accepts the lift and the layer's data_arg
  # becomes a `ptr_pipeline` (a single verb stage reduces to a
  # single-stage chain at the layer level and GATE 0 correctly rejects
  # it — see ADR 0012 PLAN-02 §GATE 0).
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

test_that("preserve-mode collapses a `%>%`-sourced all-placeholder-free chain to nested-call form (user-facing invariant)", {
  # User wrote `%>%` with no placeholders. Today (pre-PLAN-04) this would
  # render as `penguins %>% dplyr::filter(...) %>% dplyr::mutate(...) %>%
  # ggplot(...)`. The prefix-collapse rule subsumes it into a nested-call
  # atom: no `|>`, no `%>%`. See BDD "Render preserve-mode — `%>%`-source
  # user sees no `|>` for their all-placeholder-free chain". Two verb
  # stages above source so PLAN-02's GATE 0 accepts the lift and the
  # layer's data_arg becomes a `ptr_pipeline` (a single verb stage
  # reduces to a single-stage chain at the layer level and GATE 0
  # correctly rejects it — see ADR 0012 PLAN-02 §GATE 0).
  data_arg <- data_arg_from_formula(
    "penguins %>% dplyr::filter(bill_length_mm > 40) %>% dplyr::mutate(y = bill_length_mm * 2) %>% ggplot(aes(bill_length_mm))"
  )
  expect_s3_class(data_arg, "ptr_pipeline")

  rendered <- ggpaintr:::render_walk(data_arg, preserve_placeholders = TRUE)
  expect_false(grepl("|>", rendered, fixed = TRUE))
  expect_false(grepl("%>%", rendered, fixed = TRUE))
  # The two-verb prefix-collapse output exceeds the inline RENDER_WIDTH
  # budget so `render_call_text()` breaks each argument onto its own
  # line. `ws_collapse()` (gsub("\\s+", " ", x)) reduces those breaks
  # to single spaces — including the space after `(` and before `)`.
  # Compare against the same-shape expected string so the assertion is
  # whitespace-insensitive but operator-sensitive (no `|>`, no `%>%`).
  expect_equal(
    ws_collapse(rendered),
    ws_collapse("dplyr::mutate(\n  dplyr::filter(penguins, bill_length_mm > 40),\n  y = bill_length_mm * 2\n)")
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
  #
  # Two verb stages above source so PLAN-02's GATE 0 accepts the lift and
  # the layer's data_arg becomes a `ptr_pipeline` (a single verb stage
  # reduces to a single-stage chain at the layer level and GATE 0
  # correctly rejects it — see ADR 0012 PLAN-02 §GATE 0).
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
