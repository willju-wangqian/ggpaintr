# ADR 0012 PLAN-03 — Bug-3b regression net.
#
# Source: ADR 0012 §1.3 row "ADR-0011 Bug 3b" — "empty `filter()` survives
# prune under `|>` input ... `prune_walk.ptr_pipeline` elides stages whose
# args all prune to missing. With no `ptr_pipeline` node, that elision never
# runs." PLAN-02's canonical-pipeline lift restores the elision path; this
# file is the regression net that asserts the bug stays fixed across all
# three surface forms (`|>` / `%>%` / nested-call) — the BDD `Then` clauses
# state the SAME canonical pruned tree must arise regardless of surface
# syntax (ADR 0012 §1 "tree is semantic, not syntactic").
#
# Public + already-existing internal API only (per the plan's Constraints):
#   ptr_translate, ggpaintr:::ptr_substitute, ggpaintr:::substitute_walk,
#   ggpaintr:::ptr_prune, ggpaintr:::ptr_render,
#   ggpaintr:::default_drop_when_empty,
#   ggpaintr:::ptr_tree_structural_equal.
#
# Note on dplyr attachment. `default_drop_when_empty()` carries qualified
# names (e.g. `dplyr::filter`). For bare `filter()` etc. in a formula to
# match the remove_set, `qualified_call_name()` must resolve the bare
# symbol to its namespaced form via the attached search path — so each
# bare-verb test scopes `withr::local_package("dplyr")` (test-file local,
# not a top-level `library()`).

# ---- helpers --------------------------------------------------------------

# Translate → substitute (empty snapshot) → prune → render. The four-step
# pipeline the BDD `When` clause specifies, condensed.
render_pruned <- function(formula) {
  tree <- ptr_translate(formula, expr_check = FALSE)
  ctx <- list(snapshot = list(), upstream_cols = list())
  sub <- ggpaintr:::substitute_walk(tree, ctx)
  pruned <- ggpaintr:::ptr_prune(sub)
  ggpaintr:::ptr_render(pruned)
}

# Pruned data_arg from layer 1 — the load-bearing position for the
# canonical-pipeline lift. NULL when prune dropped it entirely (the
# elision case).
pruned_data_arg <- function(formula) {
  tree <- ptr_translate(formula, expr_check = FALSE)
  ctx <- list(snapshot = list(), upstream_cols = list())
  sub <- ggpaintr:::substitute_walk(tree, ctx)
  pruned <- ggpaintr:::ptr_prune(sub)
  pruned$layers[[1L]]$data_arg
}

# ---- bug-3b scenarios (empty filter, three surface forms) ----------------

test_that("PLAN-03 / bug-3b: empty filter() elides under `|>`", {
  withr::local_package("dplyr")
  txt <- render_pruned(
    "ppUpload |> filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  expect_false(grepl("filter", txt, fixed = TRUE))
})

test_that("PLAN-03 / bug-3b: empty filter() elides under `%>%`", {
  withr::local_package("dplyr")
  txt <- render_pruned(
    "ppUpload %>% filter(ppVar > ppNum) %>% ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  expect_false(grepl("filter", txt, fixed = TRUE))
})

test_that("PLAN-03 / bug-3b: empty filter() elides when written as nested calls", {
  withr::local_package("dplyr")
  txt <- render_pruned(
    "ggplot(filter(ppUpload, ppVar > ppNum), aes(ppVar, ppVar)) + geom_point()"
  )
  expect_false(grepl("filter", txt, fixed = TRUE))
})

# ---- cross-form uniformity (tree is semantic, not syntactic) -------------

test_that("PLAN-03 / bug-3b: all three surface forms produce identical pruned data_arg (empty filter elision)", {
  # ADR 0012 §1: "|>", "%>%", and nested-call surface forms MUST produce the
  # same canonical pruned tree. After elision, the data_arg position is
  # empty (NULL) for all three — `ptr_tree_structural_equal(NULL, NULL)` is
  # TRUE, and the trio render to byte-identical text.
  withr::local_package("dplyr")
  d_native <- pruned_data_arg(
    "ppUpload |> filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  d_magri <- pruned_data_arg(
    "ppUpload %>% filter(ppVar > ppNum) %>% ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  d_nested <- pruned_data_arg(
    "ggplot(filter(ppUpload, ppVar > ppNum), aes(ppVar, ppVar)) + geom_point()"
  )
  expect_true(ggpaintr:::ptr_tree_structural_equal(d_native, d_magri))
  expect_true(ggpaintr:::ptr_tree_structural_equal(d_magri, d_nested))
})

test_that("PLAN-03 / bug-3b: all three surface forms drop filter() and keep the source symbol (post-ADR-0025 §5)", {
  # Pre-ADR-0025: the substitute walker emitted `ptr_missing()` for an
  # empty-snapshot upload, so all three surface forms rendered to the
  # data-less stub `ggplot() + geom_point()`. ADR 0025 §5 / PLAN-02
  # replaces that stub with `as.name(node$auto_name)` (the deterministic
  # binding symbol). Per ADR 0025 §3 that auto-name is the system
  # `df_<hash(node$id)>` (empty textbox -> auto_name fallback), so the
  # source position now renders explicitly, e.g. with `df_c21b33` =
  # df_<hash("ggplot_1_ppUpload_NA")>:
  #   - `|>` / nested: `ggplot(data = df_c21b33) + geom_point()`
  #   - `%>%`: `df_c21b33 %>% ggplot() + geom_point()`
  # The byte-identical-across-forms invariant no longer holds at the
  # rendered-text layer because the `%>%` surface preserves the pipe
  # call while `|>` desugars to nested. The PRUNE invariant (empty
  # `filter()` is elided) survives intact across all three forms and is
  # the load-bearing claim of this regression net.
  withr::local_package("dplyr")
  # Computed in-test the same way paintr-ids.R::ptr_hash() does.
  src_auto <- paste0("df_", substr(rlang::hash("ggplot_1_ppUpload_NA"), 1L, 6L))
  t_native <- render_pruned(
    "ppUpload |> filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  t_magri <- render_pruned(
    "ppUpload %>% filter(ppVar > ppNum) %>% ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  t_nested <- render_pruned(
    "ggplot(filter(ppUpload, ppVar > ppNum), aes(ppVar, ppVar)) + geom_point()"
  )
  for (t in list(t_native, t_magri, t_nested)) {
    expect_false(grepl("filter", t, fixed = TRUE))
    expect_true(grepl(src_auto, t, fixed = TRUE))
  }
  # `|>` and nested forms collapse to the same nested-call render shape;
  # `%>%` preserves the surface pipe call (R semantics: magrittr is a
  # real function call, native pipe is parser sugar).
  expect_identical(t_native, t_nested)
})

# ---- non-empty filter is PRESERVED (don't over-elide) --------------------

test_that("PLAN-03 / bug-3b: non-empty filter is preserved across all three surface forms", {
  # Without `withr::local_package("dplyr")` the bare `filter` symbol does
  # not resolve to `dplyr::filter` for `qualified_call_name`. With it
  # attached, the qualified name DOES match the remove_set — but the call
  # has a real argument (`bill_length_mm > 40`) so its arg list is
  # non-empty after prune, and the call survives. This test guards
  # against over-elision (dropping calls that only LOOK empty from a
  # placeholder-substitution standpoint when in fact they hold real
  # expressions).
  withr::local_package("dplyr")
  for (formula in c(
    "penguins |> filter(bill_length_mm > 40) |> ggplot(aes(bill_length_mm)) + geom_point()",
    "penguins %>% filter(bill_length_mm > 40) %>% ggplot(aes(bill_length_mm)) + geom_point()",
    "ggplot(filter(penguins, bill_length_mm > 40), aes(bill_length_mm)) + geom_point()"
  )) {
    txt <- render_pruned(formula)
    expect_true(grepl("filter", txt, fixed = TRUE),
                info = paste("filter missing in:", formula))
    expect_true(grepl("bill_length_mm > 40", txt, fixed = TRUE),
                info = paste("filter body missing in:", formula))
  }
})

# ---- across-verb elision sweep -------------------------------------------

test_that("PLAN-03 / bug-3b: empty form of every elidable dplyr verb in default_drop_when_empty() elides under `|>`", {
  # BDD scenario "Across-verb elision sweep": for each verb in
  # filter/mutate/arrange/group_by — empty form (placeholder-only args
  # that prune to ptr_missing) drops under `|>`. The four verbs are all
  # in `default_drop_when_empty()` under the dplyr namespace.
  withr::local_package("dplyr")
  empty_cases <- list(
    filter   = "ppUpload |> filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) + geom_point()",
    mutate   = "ppUpload |> mutate(new_var = ppVar) |> ggplot(aes(ppVar, ppVar)) + geom_point()",
    arrange  = "ppUpload |> arrange(ppVar) |> ggplot(aes(ppVar, ppVar)) + geom_point()",
    group_by = "ppUpload |> group_by(ppVar) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
  )
  purrr::iwalk(empty_cases, function(formula, verb) {
    txt <- render_pruned(formula)
    expect_false(grepl(verb, txt, fixed = TRUE),
                 info = paste0("verb `", verb, "` survived empty under `|>`: ", txt))
  })
})

test_that("PLAN-03 / bug-3b: non-empty form of every elidable verb survives under `|>`", {
  # The mirror of the elision sweep: each verb's non-empty form survives.
  # Without this, an over-eager prune that drops everything would pass
  # the elision tests vacuously.
  withr::local_package("dplyr")
  nonempty_cases <- list(
    filter   = "penguins |> filter(bill_length_mm > 40) |> ggplot(aes(bill_length_mm)) + geom_point()",
    mutate   = "penguins |> mutate(z = bill_length_mm + 1) |> ggplot(aes(bill_length_mm)) + geom_point()",
    arrange  = "penguins |> arrange(bill_length_mm) |> ggplot(aes(bill_length_mm)) + geom_point()",
    group_by = "penguins |> group_by(bill_length_mm) |> ggplot(aes(bill_length_mm)) + geom_point()"
  )
  purrr::iwalk(nonempty_cases, function(formula, verb) {
    txt <- render_pruned(formula)
    expect_true(grepl(verb, txt, fixed = TRUE),
                info = paste0("verb `", verb, "` lost from non-empty form under `|>`: ", txt))
  })
})

# ---- partial-fill regression: no syntactically empty verb call survives ---
#
# 2026-05-27 user report: with formula
#   mtcars |> mutate(adj = ppVar + ppVar) |> ggplot(aes(x = ppVar)) +
#     geom_histogram()
# only the aes-side `x = ppVar` widget was filled (e.g. "mpg"); the two
# mutate-side `ppVar` widgets were left blank. Expected: either the full
# mutate body substitutes through, or the empty mutate stage elides cleanly
# from the rendered code. Observed in installed ggpaintr 0.9.2: the rendered
# code panel showed `mtcars |> mutate()` — a syntactically empty verb call,
# meaningless to a copy-paste reader. Fixed in `post-add-expr`; this test
# locks the symptom-level invariant: the substituted+pruned rendered text
# MUST NOT contain any zero-arg verb call (`verb()`). It catches the
# observed regression regardless of which upstream path produced it.

# Helper: substitute with a *partial* snapshot + populated upstream_cols
# (mirrors the live runtime feed when the user has picked exactly one
# column-picker widget). Returns the rendered code string.
render_pruned_partial <- function(formula, snapshot, upstream_cols) {
  tree <- ptr_translate(formula, expr_check = FALSE)
  tree <- ggpaintr:::ptr_assign_ids(tree)
  res <- ggpaintr:::ptr_complete_expr_safe(
    tree,
    snapshot      = snapshot,
    upstream_cols = upstream_cols
  )
  stopifnot(isTRUE(res$ok))
  res$code_text
}

test_that("partial-fill: rendered code never contains a zero-arg verb call", {
  withr::local_package("dplyr")

  formula <- paste(
    "mtcars |> mutate(adj = ppVar + ppVar) |>",
    "ggplot(aes(x = ppVar)) + geom_histogram()"
  )
  cols <- names(mtcars)
  # Snapshot: only the aes-side ppVar is filled; both mutate-side ppVar
  # widgets are absent (i.e. NULL in the live runtime feed).
  snap <- list(ggplot_1_1_ppVar_NA = "mpg")
  ucols <- list(
    ggplot_1_1_ppVar_NA    = cols,
    ggplot_2_1_1_ppVar_NA  = cols,
    ggplot_2_1_2_ppVar_NA  = cols
  )

  txt <- render_pruned_partial(formula, snap, ucols)

  # Symptom-level invariant: no `name()` zero-arg verb call survives. The
  # regex captures any identifier followed immediately by `()` — `aes()` and
  # `geom_histogram()` are intentionally excluded (an empty aes / standalone
  # geom is the legitimate zero-arg form they take in pruned output).
  # Targets the data-pipeline-verb shape `mutate()`, `filter()`, etc.
  empty_verb_re <- "\\b(mutate|filter|arrange|group_by|summari[sz]e|select|transmute|rename|reframe)\\(\\s*\\)"
  expect_false(grepl(empty_verb_re, txt),
               info = paste0("zero-arg verb call survived render: ", txt))

  # Sibling positive assertion: the aes-side substitution must have
  # succeeded (otherwise the test would vacuously pass when sub bails
  # before prune even gets a chance).
  expect_true(grepl("aes(x = mpg)", txt, fixed = TRUE),
              info = paste0("aes substitution missing: ", txt))
})
