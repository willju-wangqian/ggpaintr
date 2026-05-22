# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> EXCEPT: the vignette mirror at
#     `vignettes/ggpaintr-customization.Rmd` (chunks `consumer-colvars` +
#     `consumer-colvars-app`) uses the single-verb-stage formula
#       mtcars |> dplyr::select(colvars) |>
#         ggplot(aes(x = ppVar, y = ppVar)) + geom_point()
#     Post-ADR-0012 G2 (PLAN-02 lift + PLAN-04 prefix-collapse render +
#     consumer-uniformity), PLAN-02 GATE 0 rejects single-stage chains
#     (one verb above source) — the layer's data_arg stays a `ptr_call`,
#     the consumer-uniformity upstream-resolution path that populates the
#     consumer picker's `cols` does not fire for that ptr_call shape, and
#     the picker remains empty. This fixture diverges from the vignette
#     to demonstrate the post-G2 consumer-uniformity contract: a multi-
#     stage chain (>= 2 verbs above source) lifts the data_arg to a
#     `ptr_pipeline`, which the uniformity path resolves into populated
#     upstream cols. `head(20)` is added as a no-op-for-demo first stage.
#     Follow-up: a successor plan should either update the vignette to
#     match this fixture, OR keep the vignette pedagogical (single-stage)
#     while pointing readers to this fixture as the working example. Do
#     NOT silently edit the vignette here — that is a separate plan's
#     territory. <<<
# >>> verbatim: vignettes/ggpaintr-customization.Rmd chunk `consumer-colvars`
#     (registration) + chunk `consumer-colvars-app` (the app, formula
#     amended per the EXCEPT block above) >>>
ptr_define_placeholder_consumer(
  keyword       = "colvars",
  build_ui      = function(node, cols = character(), label = NULL,
                           selected = character(0), ...) {
    selectInput(node$id, label = label %||% "Columns",
                choices = cols, selected = intersect(selected, cols),
                multiple = TRUE)
  },
  resolve_expr  = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)

ptr_app("mtcars |> head(20) |> dplyr::select(colvars) |>
        ggplot(aes(x = ppVar, y = ppVar)) + geom_point()")
# <<<
