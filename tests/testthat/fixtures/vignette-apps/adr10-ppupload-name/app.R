# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. This fixture is NOT vignette-paired — it exists to e2e-cover
# ADR 0010 (ppUpload(name) default-arg + identity-outside-app + auto-resolve
# via the existing eval_env parent chain). The fixture binds a small inline
# `penguins` data.frame in the app process and passes the bareword `penguins`
# to ppUpload(); the seeded companion textInput plus eval_env auto-resolve
# should render the plot without any actual file upload (Clarification C3).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
penguins <- data.frame(
  species         = c("Adelie", "Gentoo", "Chinstrap"),
  bill_length_mm  = c(39.1, 50.0, 49.5),
  bill_depth_mm   = c(18.7, 15.5, 17.5),
  stringsAsFactors = FALSE
)
ptr_app(
  "ppUpload(penguins) |> dplyr::filter(species == \"Adelie\") |> ggplot(aes(x = ppVar(bill_length_mm), y = ppVar(bill_depth_mm))) + geom_point()"
)
