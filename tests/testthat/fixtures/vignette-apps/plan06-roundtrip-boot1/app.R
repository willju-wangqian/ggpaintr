# Boot scaffolding (NOT vignette code). PLAN-06 / ADR 0025 §6 -- the
# boot-1 side of the spec round-trip end-to-end test. Boots a simple
# `ppUpload |> ggplot(...)` app so the test can:
#   1. Upload `penguins.csv` to the ppUpload's fileInput.
#   2. Leave the shortcut textbox at "" (the case the fallback covers).
#   3. Switch the code panel to spec mode and read out the spec dump.
# The test then writes the dumped spec to disk so boot-2 can read it.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  "ppUpload |> ggplot(aes(x = ppVar('flipper_length_mm'), y = ppVar('body_mass_g'))) + geom_point()"
)
