# Boot scaffolding (NOT vignette code): load the in-development package so the
# e2e test exercises dev source, not a stale system install.
#
# ADR 0025 contract (ii) regression fixture. The data source `ppUpload(mtcars)`
# resolves the env frame `mtcars` AT BOOT via the shortcut (textbox defaults to
# "mtcars"), so the consumer `ppVar(mpg)` MUST seed its formula default `mpg` at
# boot (author-known data). A subsequent file upload is NEW data, so the default
# must NOT carry — the picker comes up populated but unselected.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
mtcars <- datasets::mtcars
ptr_app(
  ggplot() + geom_point(data = ppUpload(mtcars), aes(x = ppVar(mpg)))
)
