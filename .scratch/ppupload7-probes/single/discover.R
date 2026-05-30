Sys.setenv(NOT_CRAN = "true")
library(shinytest2)
pkg <- "/Users/willju/Research/ggpaintr-ppUpload-bug"
Sys.setenv(GGP_PKG = pkg)
app <- suppressWarnings(shinytest2::AppDriver$new(
  "/Users/willju/Research/ggpaintr-ppUpload-bug/.scratch/ppupload7-probes/single", name = "ppupload7-single",
  load_timeout = 60 * 1000, timeout = 30 * 1000))
on.exit(app$stop(), add = TRUE)
app$wait_for_idle(timeout = 25 * 1000)
inames <- tryCatch(names(app$get_values()$input), error = function(e) paste("ERR:", conditionMessage(e)))
cat("INPUT IDS:\n"); print(inames)
