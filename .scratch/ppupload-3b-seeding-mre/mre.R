#!/usr/bin/env Rscript
# Headless reproduction of the ADR 0025 §3b consumer-seeding bug.
#
#   Run from the repo root:
#     Rscript .scratch/ppupload-3b-seeding-mre/mre.R
#
# Needs: shinytest2 + chromote + Chrome (same stack as the e2e suite).
# Prints the x-picker selection after the first upload and a PASS/BUG verdict.
pkg <- normalizePath(".")
Sys.setenv(GGP_PKG = pkg, NOT_CRAN = "true")
suppressMessages(library(shinytest2))

app_dir <- file.path(pkg, ".scratch", "ppupload-3b-seeding-mre")
app <- AppDriver$new(app_dir, name = "ppupload-3b-mre",
                     load_timeout = 60 * 1000, timeout = 30 * 1000)
on.exit(app$stop(), add = TRUE)
app$wait_for_idle(timeout = 25 * 1000)

# First upload while the app is running (no data at boot).
app$upload_file(geom_point_0_ppUpload_NA = file.path(app_dir, "data.csv"))
app$wait_for_idle(timeout = 15 * 1000)

picker_html <- app$get_html("#geom_point_1_1_ppVar_NA") %||% ""
sel <- app$get_value(input = "geom_point_1_1_ppVar_NA")

cat("\n================ ADR 0025 §3b MRE ================\n")
cat("Formula : geom_point(data = ppUpload(), aes(x = ppVar(mpg), y = ppVar(wt)))\n")
cat("Boot    : no data (ppUpload() has no env-shortcut seed)\n")
cat("Action  : upload data.csv (columns: mpg, wt, hp)\n")
cat("Spec    : ADR 0025 §3b -> picker POPULATED but UNSELECTED (upload = new work)\n")
cat("----------------------------------------------------\n")
cat("x-picker populated with 'mpg'? ", grepl("mpg", picker_html, fixed = TRUE), "\n", sep = "")
cat("x-picker selection after upload: ", deparse(sel), "\n", sep = "")
if (!is.null(sel) && nzchar(sel)) {
  cat("VERDICT : BUG REPRODUCED -- the formula default '", sel,
      "' was auto-selected.\n", sep = "")
  cat("          ADR 0025 §3b requires NO selection after a post-boot upload.\n")
} else {
  cat("VERDICT : no bug -- picker came up unselected, per ADR 0025 §3b.\n")
}
cat("=================================================\n")
