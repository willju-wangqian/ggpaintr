# Boot scaffolding (NOT vignette code): regression fixture for the
# "deselecting a ppVar picker (multiple = TRUE) and clicking Update Plot
# snaps the widget back to its formula default" bug. Exercises both
# call sites the fix touched:
#   - non-shared consumer renderUI   (ptr_setup_consumer_uis)
#   - shared    consumer renderUI    (ptr_bind_shared_consumer_uis)
#
# Unit-layer coverage for the same defect lives in
# tests/testthat/test-invoke-build-ui-selected-contract.R (U3). This
# fixture exercises the end-to-end reactive path: a real pickerInput +
# Update Plot click invalidating entry_reactive's `ptr_update_plot` dep,
# causing the renderUI to re-fire on the same boot session.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  'ggplot(mtcars,
          aes(x = ppVar(mpg),
              color = ppVar(cyl, shared = "grp"))) +
     geom_point() +
     facet_wrap(vars(ppVar(cyl, shared = "grp")))'
)
