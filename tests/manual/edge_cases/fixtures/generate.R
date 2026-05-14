# Generate every tabular fixture used by tests/manual/edge_cases/launchers/.
# Run from the repo root: Rscript tests/manual/edge_cases/fixtures/generate.R
#
# Output (under this directory):
#   simple_numeric.csv  — 5 rows × 3 numeric columns
#   simple_numeric.tsv  — same, tab-delimited
#   simple_numeric.rds  — same, R native
#   simple_numeric.xlsx — same, one sheet "data"
#   simple_numeric.json — same, lite array-of-records
#   bad_extension.txt   — same data but the wrong extension (for the upload-reject test)
#
# Deterministic; rerunning produces byte-identical files (except xlsx, which is
# a zip and embeds a timestamp). The launchers do not depend on byte-equality.

here <- "tests/manual/edge_cases/fixtures"
if (!dir.exists(here)) {
  stop("Run from the repo root. Current wd: ", getwd())
}

df <- data.frame(
  a = c(1.1, 2.2, 3.3, 4.4, 5.5),
  b = c(10L, 20L, 30L, 40L, 50L),
  c = c(0.5, 0.6, 0.7, 0.8, 0.9)
)

write.csv(df, file.path(here, "simple_numeric.csv"), row.names = FALSE)
write.table(df, file.path(here, "simple_numeric.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
saveRDS(df, file.path(here, "simple_numeric.rds"))
writexl::write_xlsx(list(data = df), file.path(here, "simple_numeric.xlsx"))
writeLines(jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
           file.path(here, "simple_numeric.json"))
# A "wrong extension" payload — uses the same CSV bytes so the reject is
# extension-based, not content-based.
file.copy(file.path(here, "simple_numeric.csv"),
          file.path(here, "bad_extension.txt"), overwrite = TRUE)

cat("Wrote fixtures to:", here, "\n")
list.files(here, full.names = FALSE) |> writeLines()
