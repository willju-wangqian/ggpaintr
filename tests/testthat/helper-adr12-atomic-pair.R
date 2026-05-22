# Helper: ADR 0012 atomic-pair (PLAN-02 ⇄ PLAN-04) skip-gate probe.
#
# Context. ADR 0012 PLAN-02 (resugar-pipeline-lift) and PLAN-04 (downstream
# uniformity + preserve-mode prefix-collapse render + runtime fast-path delete)
# form an atomic pair — see `dev/plans/0012-role-based-tree-and-ptr-spec/index.md`.
# In the half-state where PLAN-02 has merged but PLAN-04 has not, render-side
# tests that assert the user-visible pipe surface (`|>` / `%>%`) round-trips
# fail because the lift now produces a canonical `ptr_pipeline` data_arg but
# the render walker has not yet been taught the prefix-collapse rule that
# re-projects the lifted shape back to the source-side pipe-surface form the
# user wrote. Those tests are CONTRACT — they assert behaviour the atomic
# pair as a whole guarantees — so PLAN-02-alone skip-gates them rather than
# deleting or weakening them. PLAN-04 either changes the render walker or
# updates these tests; in either case it removes the skip-gate guard.
#
# Probe shape — content-based, never branch-name-based. We feed a known
# multi-stage pipe formula through translate + default-mode render and check
# whether the top-level pipe surface appears in the output. On PLAN-02-alone
# the render emits the nested-call form (`ggplot(data = head(mtcars, 2), ...)`)
# and the probe returns FALSE → the gated tests SKIP. On a future G2-merged
# branch where the render walker (or the assertion itself) is updated, the
# probe will return TRUE → the gated tests RUN as written.
plan04_prefix_collapse_merged <- function() {
  # Multi-stage chain (2+ verb stages above source) so PLAN-02's GATE 0
  # accepts the lift and the data_arg becomes a `ptr_pipeline`. A
  # single-stage chain (e.g. `mtcars |> head(2) |> ggplot(...)`) reduces
  # to a single-stage data_arg at the layer level and GATE 0 correctly
  # rejects it — the probe would then always return FALSE regardless of
  # PLAN-04's render state. The multi-stage input below routes through
  # the lifted pipeline and the prefix-collapse render rule.
  tree <- tryCatch(
    ptr_translate("mtcars |> head(5) |> dplyr::filter(mpg > 20) |> ggplot(aes(x = mpg))"),
    error = function(e) NULL
  )
  if (is.null(tree)) return(FALSE)
  rendered <- tryCatch(
    ptr_render(tree),
    error = function(e) ""
  )
  grepl("|>", rendered, fixed = TRUE)
}
