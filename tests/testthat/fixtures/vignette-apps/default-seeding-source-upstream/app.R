# DIAGNOSE fixture: user's example A reproduction.
# ptr_app( ggplot(ppUpload(df_main), aes(x = ppVar(hp), y = mpg)) + geom_point() )
# Reported: ppVar picker is empty at app boot. removing ppUpload works.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

df_main <- mtcars

ptr_app(
  "ggplot(ppUpload(df_main), aes(x = ppVar('hp'), y = mpg)) + geom_point()"
)
