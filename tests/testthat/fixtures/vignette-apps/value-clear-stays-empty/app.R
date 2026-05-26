# Boot scaffolding (NOT vignette code): regression fixture for the
# value-path analogue of the deselect-stays-empty bug. ppText/ppNum
# typically emit "" / NA_real_ (length 1) on user clear -- those don't
# hit the bug branch in the legacy code (it required length-0). But
# the closure-flag fix at R/paintr-server.R:1475 (non-shared value loop)
# applies symmetrically here, and we pin the contract so any future
# regression at that call site surfaces. Includes a shared ppNum (lw)
# to exercise the parallel call site at R/paintr-server.R:1561.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  'ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(cyl))) +
     geom_smooth(method = ppText("lm"),
                 linewidth = ppNum(2, shared = "lw")) +
     geom_line(linewidth = ppNum(3, shared = "lw"))'
)
