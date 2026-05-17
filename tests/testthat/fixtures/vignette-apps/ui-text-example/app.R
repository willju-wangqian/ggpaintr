# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-customization.Rmd chunk `ui-text-example` >>>
custom_text <- ptr_ui_text(list(
  shell = list(
    title       = list(label = "Iris explorer"),
    draw_button = list(label = "Render")
  ),
  params = list(
    x     = list(var  = list(label = "X variable")),
    title = list(text = list(label = "Plot heading"))
  )
))

ptr_app(
  "ggplot(iris, aes(x = var, y = var, color = var)) +
     geom_point() +
     labs(title = text)",
  ui_text = custom_text
)
# <<<
