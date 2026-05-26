# DIAGNOSE: user's EXACT unquoted form.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

df_main <- mtcars

ptr_app(
  ggplot(ppUpload(df_main), aes(x = ppVar(hp), y = mpg)) + geom_point()
)
