# Boot scaffolding (NOT vignette code). PLAN-06 / ADR 0025 §6 -- the
# boot-2 side of the spec round-trip end-to-end test. Reads the spec
# dumped by boot-1 from `Sys.getenv("PLAN06_SPEC_RDS")`, and the
# `auto-name <- read.csv("...")` binding from
# `Sys.getenv("PLAN06_AUTO_NAME")` + `Sys.getenv("PLAN06_CSV_PATH")`.
#
# This mimics the human-side "spec round-trip" workflow: paste the
# emitted `ptr_spec <- list(...)` into your R session, run
# `<auto-name> <- read.csv("...")`, then `ptr_app(formula, spec = ...)`.
# The env-shortcut path then resolves the auto-name in the caller env
# and the rendered plot reproduces boot-1.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

spec_rds  <- Sys.getenv("PLAN06_SPEC_RDS",  unset = "")
auto_name <- Sys.getenv("PLAN06_AUTO_NAME", unset = "")
csv_path  <- Sys.getenv("PLAN06_CSV_PATH",  unset = "")
if (!nzchar(spec_rds) || !file.exists(spec_rds)) {
  stop("PLAN06_SPEC_RDS not set or missing")
}
if (!nzchar(auto_name) || !nzchar(csv_path)) {
  stop("PLAN06_AUTO_NAME / PLAN06_CSV_PATH unset")
}
spec <- readRDS(spec_rds)
# Bind the uploaded df under the auto-name into the caller env (this
# script's globalenv() is the envir for ptr_app()). Matches the
# `<auto-name> <- read.csv("...")` prologue Plan 04 emits for the
# human workflow.
assign(auto_name, utils::read.csv(csv_path), envir = globalenv())

ptr_app(
  "ppUpload |> ggplot(aes(x = ppVar('flipper_length_mm'), y = ppVar('body_mass_g'))) + geom_point()",
  spec = spec
)
