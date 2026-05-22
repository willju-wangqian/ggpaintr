# End-to-end tree-shape assertions for ptr_classify_calls + the pipeline
# lift (ADR 0012 §3). Exercises ptr_translate output — the surface contract
# downstream code depends on.

test_that("native-pipe input produces a ptr_pipeline data_arg with the same shape as %>% input", {
  f_native <- "ppUpload |> filter(ppVar > ppNum) |> mutate(new_var = ppVar + 1) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  f_magrittr <- "ppUpload %>% filter(ppVar > ppNum) %>% mutate(new_var = ppVar + 1) %>% ggplot(aes(ppVar, ppVar)) + geom_point()"

  t_native <- ptr_translate(f_native)
  t_magrittr <- ptr_translate(f_magrittr)

  d_native <- t_native$layers[[1]]$data_arg
  d_magrittr <- t_magrittr$layers[[1]]$data_arg

  expect_s3_class(d_native, "ptr_pipeline")
  expect_s3_class(d_magrittr, "ptr_pipeline")
  expect_equal(length(d_native$stages), length(d_magrittr$stages))

  # subbed-1 ≡ subbed-2: the per-stage call heads (in order) match across
  # the two surface forms.
  head_of_stage <- function(stage) {
    # Each stage is a ptr_call (or the source: ptr_ph_data_source / literal).
    # Use the stage's $expr if it's a call; else its class.
    e <- stage$expr
    if (is.call(e)) as.character(e[[1]]) else NA_character_
  }
  expect_equal(
    vapply(d_native$stages, head_of_stage, character(1)),
    vapply(d_magrittr$stages, head_of_stage, character(1))
  )

  # The first stage in both forms is the data source (ppUpload).
  expect_s3_class(d_native$stages[[1]], "ptr_ph_data_source")
  expect_s3_class(d_magrittr$stages[[1]], "ptr_ph_data_source")
})

test_that("plain non-pipe formula is bit-identical to baseline (no lift fires)", {
  # No nested chain — ggplot(penguins, aes(...)) — first arg is a bare
  # symbol, not a call, so the loop terminates immediately and no lift
  # happens. data_arg stays the same shape it had after translate_node
  # (a ptr_literal in this case).
  tree <- ptr_translate("ggplot(penguins, aes(bill_length_mm)) + geom_point()")
  d <- tree$layers[[1]]$data_arg
  expect_false(inherits(d, "ptr_pipeline"))
  expect_s3_class(d, "ptr_literal")
})

test_that("placeholder-source pipe formula already produces a ptr_pipeline (pipe-syntax path)", {
  # Pipe-syntax formulas with a placeholder source bypass the lift entirely:
  # `translate_piped_layer` produces a `ptr_pipeline` directly. This test
  # documents the boundary — the lift only matters when a `ptr_call` ever
  # reaches `ptr_classify_calls`, which the existing pipe-syntax path
  # never produces. The shape downstream sees is the same.
  tree <- ptr_translate(
    "ppUpload |> filter(ppVar > ppNum) |> ggplot(aes(ppVar)) + geom_point()"
  )
  d <- tree$layers[[1]]$data_arg
  expect_s3_class(d, "ptr_pipeline")
  expect_s3_class(d$stages[[1]], "ptr_ph_data_source")
  expect_equal(d$stages[[1]]$keyword, "ppUpload")
})

test_that("quiet failure when terminal is an opaque call (nested-call form, make_data())", {
  # Nested-call form (the post-desugar shape of expression-input `|>`) —
  # this is what hits the lift path. The lift's terminal-grounding check
  # vetoes when the deepest first-arg is itself a call.
  expect_silent(
    tree <- ptr_translate(
      "ggplot(filter(make_data(), x > 1), aes(x)) + geom_point()"
    )
  )
  d <- tree$layers[[1]]$data_arg
  expect_s3_class(d, "ptr_call")
  expect_false(inherits(d, "ptr_pipeline"))
})

test_that("quiet failure when terminal is read_csv(\"foo.csv\") (nested-call form)", {
  expect_silent(
    tree <- ptr_translate(
      'ggplot(filter(read_csv("foo.csv"), x > 1), aes(x)) + geom_point()'
    )
  )
  d <- tree$layers[[1]]$data_arg
  expect_s3_class(d, "ptr_call")
  expect_false(inherits(d, "ptr_pipeline"))
})

test_that("nested-call form with a placeholder-source terminal lifts (lift path coverage)", {
  # Directly exercise the lift on nested-call input that the preprocessor
  # would never see (no `|>` tokens). Mirrors the post-desugar shape of
  # expression-input `|>` when the source is a placeholder (`ppUpload`).
  # Per PLAN-02's terminal-grounding narrowing, only role="source"
  # placeholder symbols ground the lift in this plan; bare data names
  # (`penguins`) stay as `ptr_call` until plan 04 retires the per-layer
  # fast path. See `terminal_grounds_for_lift`'s comment for the full
  # rationale.
  tree <- ptr_translate(
    "ggplot(filter(ppUpload, ppVar > ppNum), aes(ppVar)) + geom_point()"
  )
  d <- tree$layers[[1]]$data_arg
  expect_s3_class(d, "ptr_pipeline")
  expect_equal(length(d$stages), 2L)
  expect_s3_class(d$stages[[1]], "ptr_ph_data_source")
  expect_equal(d$stages[[1]]$keyword, "ppUpload")
})

test_that("nested-call form with a bare data-name terminal does NOT lift (plan-02 narrowing)", {
  # `penguins` is a bare data-name symbol, not a registered
  # placeholder-source. Per PLAN-02 the lift declines so downstream
  # paintr-disable / paintr-prune keep the `ptr_call` shape they were
  # written against. Plan 04 retires that constraint.
  tree <- ptr_translate(
    "ggplot(filter(penguins, bill_length_mm > 40), aes(bill_length_mm)) + geom_point()"
  )
  d <- tree$layers[[1]]$data_arg
  expect_false(inherits(d, "ptr_pipeline"))
  expect_s3_class(d, "ptr_call")
})

test_that("+ fence holds — nested calls inside aes() are not lifted", {
  # filter() is nested inside aes(), which lives under layer$children, not
  # under layer$data_arg. The lift pass must not descend into children.
  tree <- ptr_translate(
    "ggplot(penguins, aes(x = filter(penguins, y > 1))) + geom_point()"
  )
  # data_arg is the bare `penguins` symbol — no lift candidate.
  d <- tree$layers[[1]]$data_arg
  expect_s3_class(d, "ptr_literal")
  # aes() child is untouched — still a ptr_call (or whatever translate_node
  # produced for it), never a ptr_pipeline.
  children <- tree$layers[[1]]$children
  has_pipeline_child <- any(vapply(children, function(ch) inherits(ch, "ptr_pipeline"), logical(1)))
  expect_false(has_pipeline_child)
})

test_that("lifted pipeline op slot is \"|>\" (native pipe is the default for lifts)", {
  tree <- ptr_translate(
    "ppUpload |> filter(ppVar > ppNum) |> ggplot(aes(ppVar)) + geom_point()"
  )
  d <- tree$layers[[1]]$data_arg
  expect_s3_class(d, "ptr_pipeline")
  expect_equal(d$op, "|>")
})
