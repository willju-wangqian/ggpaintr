# Drive the instrumented probe app and report whether the host-scope shared
# consumer clears its selection across a typed_ds -> penguins.csv source switch.
Sys.setenv(NOT_CRAN = "true")
library(shinytest2)

pkg <- "/Users/willju/Research/ggpaintr-ppUpload-bug"
Sys.setenv(GGP_PKG = pkg)
csv <- file.path(pkg, "tests/testthat/fixtures/penguins.csv")
stopifnot(file.exists(csv))

app <- suppressWarnings(shinytest2::AppDriver$new(
  "/Users/willju/Research/ggpaintr-ppUpload-bug/.scratch/ppupload7-probes/shared",
  name = "ppupload7-probe",
  load_timeout = 60 * 1000,
  timeout = 30 * 1000
))
on.exit(app$stop(), add = TRUE)
app$wait_for_idle(timeout = 25 * 1000)

say <- function(...) cat("\n====", ..., "====\n")

# 1. Type the env dataset name into the host shortcut textbox.
say("STEP 1: type 'typed_ds' into #shared_ds_shortcut")
app$set_inputs(shared_ds_shortcut = "typed_ds", wait_ = FALSE)
app$wait_for_idle(timeout = 15 * 1000)
cat("shared_col choices/html after typing:\n")
print(app$get_html("#shared_col"))

# 2. Select a column that exists in BOTH typed_ds and penguins (body_mass_g).
say("STEP 2: select #shared_col = 'body_mass_g'")
app$set_inputs(shared_col = "body_mass_g", wait_ = FALSE)
app$wait_for_idle(timeout = 10 * 1000)
sel_before <- tryCatch(app$get_value(input = "shared_col"), error = function(e) paste("ERR:", conditionMessage(e)))
cat("shared_col selected BEFORE upload:", format(sel_before), "\n")

# 3. Upload penguins.csv -> source switches; mutex clears the shortcut.
say("STEP 3: upload penguins.csv to #shared_ds")
app$upload_file(shared_ds = csv)
app$wait_for_idle(timeout = 20 * 1000)
sel_after <- tryCatch(app$get_value(input = "shared_col"), error = function(e) paste("ERR:", conditionMessage(e)))
shortcut_after <- tryCatch(app$get_value(input = "shared_ds_shortcut"), error = function(e) "ERR")
cat("shortcut AFTER upload (expect cleared by mutex):", format(shortcut_after), "\n")
cat("shared_col selected AFTER upload:", format(sel_after), "\n")

say("VERDICT")
cat("body_mass_g exists in BOTH datasets.\n")
cat("If sel_after is still 'body_mass_g' -> host-scope new-source CLEAR did NOT fire (stale pick rides).\n")
cat("If sel_after is NULL/'' or reset -> clear fired.\n")

say("CHILD PROBE LOGS (PROBE_CUS / PROBE_CLEAR)")
logs <- app$get_logs()
df <- as.data.frame(logs)
msgs <- df$message[grepl("PROBE_", df$message %||% "")]
if (length(msgs) == 0) {
  cat("(no PROBE_ lines captured; dumping all stderr lines)\n")
  print(utils::tail(df[grepl("stderr", df$location %||% ""), "message"], 40))
} else {
  cat(paste(msgs, collapse = "\n"), "\n")
}
