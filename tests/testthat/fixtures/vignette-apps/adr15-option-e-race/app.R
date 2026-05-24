# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror. It is the ADR 0015 PLAN-02
# / Option E race-provable regression test. The CSV in this directory is
# named penguins.csv deliberately so that ptr_bind_source_autoname() will
# updateTextInput(comp_id, "penguins") on upload. The CSV's columns
# (col_a, col_b) are disjoint from datasets::penguins's columns
# (species, island, bill_length_mm, ...) so that any leak of
# datasets::penguins through R's parent-chain scoping fallback would
# show up as a downstream picker offering bill_length_mm / body_mass_g
# / species / etc. Formula mirrors test-adr12-bug-3a.R — pipeline-head
# ppUpload with downstream var pickers in both filter() and aes().
# Placeholder ids under the formula:
#   - upload source: ggplot_1_ppUpload_NA (companion: _name)
#   - in-aes ppVars: ggplot_1_1_ppVar_NA, ggplot_1_2_ppVar_NA
#   - in-filter ppVar: ggplot_2_1_1_ppVar_NA
ptr_app(
  "ppUpload |> dplyr::filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
)
