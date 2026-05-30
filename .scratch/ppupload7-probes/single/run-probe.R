Sys.setenv(NOT_CRAN = "true")
library(shinytest2)
pkg <- "/Users/willju/Research/ggpaintr-ppUpload-bug"
Sys.setenv(GGP_PKG = pkg)
csv <- file.path(pkg, "tests/testthat/fixtures/penguins.csv")
stopifnot(file.exists(csv))
say <- function(...) cat("\n====", ..., "====\n")

app <- suppressWarnings(shinytest2::AppDriver$new(
  "/Users/willju/Research/ggpaintr-ppUpload-bug/.scratch/ppupload7-probes/single", name = "ppupload7-single",
  load_timeout = 60 * 1000, timeout = 30 * 1000))
on.exit(app$stop(), add = TRUE)
app$wait_for_idle(timeout = 25 * 1000)
short_id <- "ggplot_0_ppUpload_NA_shortcut"

find_cid <- function() {
  inames <- tryCatch(names(app$get_values()$input), error = function(e) character())
  cid <- setdiff(grep("ppVar", inames, value = TRUE),
                 grep("shortcut", inames, value = TRUE))
  list(all = inames, cid = cid)
}

# SOURCE FIRST (Mechanism B: consumer suspended until source ready).
say("STEP 1: type 'typed_ds' into", short_id, "(provide source)")
app$set_inputs(!!short_id := "typed_ds", wait_ = FALSE)
app$wait_for_idle(timeout = 15 * 1000)

# Now open the Controls subtab so the (now un-gated) ppVar picker binds.
for (val in c("Controls", "Data")) {
  app$set_inputs(ggplot_subtab = val, wait_ = FALSE)
  app$wait_for_idle(timeout = 12 * 1000)
  f <- find_cid()
  cat("subtab=", val, " inputs: ", paste(f$all, collapse=", "), "\n", sep="")
  if (length(f$cid) > 0) break
}
f <- find_cid()
cat("consumer id candidate(s):", paste(f$cid, collapse=", "), "\n")
if (length(f$cid) == 0) { cat("NO CONSUMER PICKER BOUND — stopping.\n"); quit(save="no") }
cid <- f$cid[[1]]
cat("consumer html:\n"); print(app$get_html(paste0("#", cid)))

say("STEP 2: select", cid, "= body_mass_g")
app$set_inputs(!!cid := "body_mass_g", wait_ = FALSE)
app$wait_for_idle(timeout = 10 * 1000)
before <- tryCatch(app$get_value(input = cid), error = function(e) paste("ERR:", conditionMessage(e)))
cat("selected BEFORE upload:", format(before), "\n")

say("STEP 3: upload penguins.csv")
app$upload_file(ggplot_0_ppUpload_NA = csv)
app$wait_for_idle(timeout = 20 * 1000)
after <- tryCatch(app$get_value(input = cid), error = function(e) paste("ERR:", conditionMessage(e)))
short_after <- tryCatch(app$get_value(input = short_id), error = function(e) "ERR")
cat("shortcut AFTER upload (mutex should clear):", format(short_after), "\n")
cat("selected AFTER upload:", format(after), "\n")

say("VERDICT (single-instance): persist => clear failed; empty/reset => clear stuck")
say("CHILD PROBE LOGS")
df <- as.data.frame(app$get_logs())
cat(paste(df$message[grepl("PROBE_", df$message)], collapse = "\n"), "\n")
