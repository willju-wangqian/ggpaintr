# MRE app for the ADR 0025 §3b consumer-seeding bug.
# Boots the in-development package (GGP_PKG, else this worktree) so it
# exercises dev source, mirroring the shinytest2 fixture pattern.
.pkg <- Sys.getenv("GGP_PKG", unset = "/Users/willju/Research/ggpaintr-ppUpload-bug")
pkgload::load_all(.pkg, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# No data at boot: `ppUpload()` has no name, so there is no env-shortcut seed
# and the source is empty until the user uploads a file. The two consumers
# carry formula-side defaults: ppVar(mpg) at x, ppVar(wt) at y.
#
# ADR 0025 §3b (locked 2026-05-28): "a consumer seeds its formula-side default
# only at boot, and only against the data that is present at boot. Once the app
# is running, any new data source ... clears the consumer picker. The picker
# comes up populated ... but with no selection."
#
# So after the FIRST upload (the app was running, no data at boot), the x
# picker should be POPULATED with the uploaded columns but have NO selection.
ptr_app(
  ggplot() + geom_point(data = ppUpload(), aes(x = ppVar(mpg), y = ppVar(wt)))
)
