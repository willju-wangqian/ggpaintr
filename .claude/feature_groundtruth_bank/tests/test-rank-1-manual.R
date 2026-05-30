# =============================================================================
# MANUAL verification of sections/rank-1.feature — the 17 scenarios that need a
# LIVE Shiny session (reactive flush, widget rendering, upstream-data resolution,
# spec round-trip). They CANNOT be asserted from loaded source alone, so they are
# runnable examples instead of testthat tests. The other 79 scenarios ARE
# auto-verified in test-rank-1.R.
#
# These are "what I believe works but could not verify" — YOU run them and judge.
#
# HOW TO RUN
#   1. Load the package first:  pkgload::load_all(".")   (or library(ggpaintr))
#   2. Run ONE block at a time. Each builds an app object `appNN`; launch it by
#      evaluating the variable (or shiny::runApp(appNN)). An app blocks the
#      console — stop it (Esc) before running the next block.
#   3. Read the "# EXPECTED:" comment above each app and confirm by eye / by the
#      R console messages some examples print.
#
# Only setup assumed beyond a loaded package (%||% is ggpaintr-internal):
`%||%` <- function(a, b) if (is.null(a)) b else a
library(ggplot2)
# library(dplyr)   # only needed if you extend the pipeline examples
# =============================================================================


# -----------------------------------------------------------------------------
# Rule: ptr_app(...)
# -----------------------------------------------------------------------------

# [envir] is the lookup scope for a shortcut/spec-named data frame — EXPECTED
# EXPECTED: type  my_df  into the "Optional dataset name" box -> the column
# pickers populate with mtcars's columns. ggpaintr does get("my_df", envir).
my_df <- mtcars
app01 <- ptr_app(
  "ppUpload() |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()",
  envir = environment()
)

# [safe_to_remove] an opted-in name is dropped when it ends up empty — EXPECTED
# coord_fixed is NOT in the curated cleanup list. Run BOTH apps; in each, clear
# the numeric "ratio" box and click Update, then read the generated-code panel.
# EXPECTED app02a: code still shows  + coord_fixed()   (empty call kept)
# EXPECTED app02b: code drops coord_fixed() entirely    (opted into removal)
app02a <- ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point() + coord_fixed(ratio = ppNum)")
app02b <- ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point() + coord_fixed(ratio = ppNum)",
                  safe_to_remove = c("coord_fixed"))

# [spec] a named list overrides widget values at session boot — EXPECTED
# Discover the exact input id first:  ptr_id_table("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()")
# then put <that id> = "cyl" in spec. EXPECTED: the x column-picker boots showing
# "cyl" already selected (no click needed), and the first plot uses cyl on x.
.f03 <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
# ids <- ptr_id_table(.f03); print(ids)   # <- run this to read the real id
app03 <- ptr_app(.f03, spec = list("ggplot_1_1_ppVar_NA" = "cyl"))  # adjust id to match ptr_id_table()

# [x] inside ptr_app the keyword binds to a typed widget — EXPECTED
# EXPECTED: ppVar -> two "Pick a column" selectInputs; ppNum -> a numericInput;
# ppText -> a textInput; ppExpr -> a code editor box.
app04 <- ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(size = ppNum) + labs(title = ppText) + facet_wrap(ppExpr)")

# [x] ppUpload renders a file picker + optional name box — EXPECTED
# EXPECTED: a fileInput (accepts .csv/.tsv/.rds/.xlsx/.xls/.json) AND an
# "Optional dataset name" textbox beside it.
app06 <- ptr_app("ppUpload() |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()")


# -----------------------------------------------------------------------------
# Rule: pp* placeholder arg `...`  (shared)
# -----------------------------------------------------------------------------

# [...] shared = '<id>' lifts the widget into a top-level shared section — EXPECTED
# Shown via ptr_app_grid (its natural home). Both formulas declare shared='xcol'.
# EXPECTED: ONE "xcol" column-picker at the TOP drives the x of BOTH plots
# (not one picker per plot).
app05 <- ptr_app_grid(list(
  "ggplot(mtcars, aes(x = ppVar(shared = 'xcol'), y = ppVar)) + geom_point()",
  "ggplot(mtcars, aes(x = ppVar(shared = 'xcol'), y = ppVar)) + geom_line()"
))


# -----------------------------------------------------------------------------
# Rule: ptr_define_placeholder_value(...) — callback runtime behaviour
# -----------------------------------------------------------------------------

# [build_ui.selected] declaring selected = NULL receives the seeded value — EXPECTED
# A percent placeholder seeded by the positional default ppPct(75).
# EXPECTED: the numeric input boots showing 75 (selected delivered the default).
ppPct <- ptr_define_placeholder_value("ppPct",
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    n <- suppressWarnings(as.numeric(selected))
    shiny::numericInput(node$id, label %||% "Percent",
                        value = if (length(n) == 1 && is.finite(n)) n else NA_real_,
                        min = 0, max = 100, step = 1)
  },
  resolve_expr = function(value, node, ...) if (length(value) == 1 && is.finite(value)) value / 100 else NULL,
  default_arg = ptr_default_numeric(),
  copy_defaults = list(label = "Percent for {param}"))
app07 <- ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(alpha = ppPct(75))")
# (cleanup when done: ptr_clear_placeholder("ppPct"))

# [resolve_expr.return] returning NULL prunes the placeholder's argument — EXPECTED
# EXPECTED: clear the size numericInput, click Update -> generated code shows
# geom_point() with NO size = argument (NULL from resolve_expr pruned it).
app08 <- ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(size = ppNum)")

# [validate_input] returning TRUE/NULL accepts, a string rejects — EXPECTED
# A consumer that only accepts numeric columns. iris has a factor (Species).
# EXPECTED: pick Species on x -> inline error "Pick a numeric column."; pick a
# numeric column (Sepal.Length) -> plot renders.
ppNumVar <- ptr_define_placeholder_consumer("ppNumVar",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...)
    shiny::selectInput(node$id, label %||% "Numeric column", choices = cols,
                       selected = intersect(selected %||% character(0), cols)),
  resolve_expr = function(value, node, ...) if (length(value) == 1 && nzchar(value)) rlang::sym(value) else NULL,
  validate_input = function(value, ctx) {
    if (length(value) == 1 && value %in% ctx$upstream_cols && is.numeric(ctx$data[[value]])) TRUE
    else "Pick a numeric column."
  })
app09 <- ptr_app("ggplot(iris, aes(x = ppNumVar, y = ppVar)) + geom_point()")
# (cleanup: ptr_clear_placeholder("ppNumVar"))

# [validate_input.ctx] for a VALUE placeholder ctx$upstream_cols and ctx$data are NULL — EXPECTED
# EXPECTED (R console, on each Update): "upstream_cols NULL? TRUE  data NULL? TRUE"
ppShow <- ptr_define_placeholder_value("ppShow",
  build_ui = function(node, label = NULL, ...) shiny::textInput(node$id, label %||% "Title"),
  resolve_expr = function(value, node, ...) value,
  validate_input = function(value, ctx) {
    message("[ppShow] upstream_cols NULL? ", is.null(ctx$upstream_cols),
            "  data NULL? ", is.null(ctx$data)); TRUE
  })
app10 <- ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point() + labs(title = ppShow)")
# (cleanup: ptr_clear_placeholder("ppShow"))


# -----------------------------------------------------------------------------
# Rule: ptr_define_placeholder_consumer(...) — callback runtime behaviour
# -----------------------------------------------------------------------------

# [build_ui.cols] cols is the upstream column-name vector, character(0) before resolve — EXPECTED
# EXPECTED (R console): "[ppEcho] cols: mpg,cyl,disp,..." once upstream resolves;
# an early "[ppEcho] cols: " (character(0)) is possible on the first pre-data render.
ppEcho <- ptr_define_placeholder_consumer("ppEcho",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) {
    message("[ppEcho] cols: ", paste(cols, collapse = ","),
            " | data null? ", is.null(data), " | nrow: ", if (is.null(data)) NA else nrow(data))
    shiny::selectInput(node$id, label %||% "Column", choices = cols,
                       selected = intersect(selected %||% character(0), cols))
  },
  resolve_expr = function(value, node, ...) if (length(value) == 1 && nzchar(value)) rlang::sym(value) else NULL)
app11 <- ptr_app("ggplot(mtcars, aes(x = ppEcho, y = ppVar)) + geom_point()")
# [build_ui.data] data is the upstream data frame, NULL while pending — EXPECTED
# Same app11 — its message also prints  "data null? FALSE | nrow: 32"  once mtcars resolves
# (and "data null? TRUE | nrow: NA" if it fires before upstream data is ready).
app12 <- app11
# (cleanup: ptr_clear_placeholder("ppEcho"))

# [build_ui.selected] must be filtered through intersect(selected, cols) — EXPECTED
# Needs a data swap. app11 already uses intersect(). Procedure: with a ppUpload
# source, pick a column, then upload a DIFFERENT dataset lacking that column.
# EXPECTED (with intersect, as written): the stale pick clears cleanly. WITHOUT
# intersect (delete it from build_ui) selectInput silently falls back to its
# first choice — that's the bug intersect() prevents.
ppEcho2 <- ptr_define_placeholder_consumer("ppEcho2",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...)
    shiny::selectInput(node$id, label %||% "Column", choices = cols,
                       selected = intersect(selected %||% character(0), cols)),  # <- the rule under test
  resolve_expr = function(value, node, ...) if (length(value) == 1 && nzchar(value)) rlang::sym(value) else NULL)
app13 <- ptr_app("ppUpload() |> ggplot(aes(x = ppEcho2, y = ppVar)) + geom_point()")
# (cleanup: ptr_clear_placeholder("ppEcho2"))

# [validate_input.ctx] consumer ctx$upstream_cols and ctx$data ARE populated — EXPECTED
# EXPECTED (R console): "[ppChk] upstream_cols: mpg,cyl,... | data class: data.frame"
# i.e. NOT NULL (contrast with app10's value-role placeholder, where both are NULL).
ppChk <- ptr_define_placeholder_consumer("ppChk",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...)
    shiny::selectInput(node$id, label %||% "Column", choices = cols,
                       selected = intersect(selected %||% character(0), cols)),
  resolve_expr = function(value, node, ...) if (length(value) == 1 && nzchar(value)) rlang::sym(value) else NULL,
  validate_input = function(value, ctx) {
    message("[ppChk] upstream_cols: ", paste(ctx$upstream_cols, collapse = ","),
            " | data class: ", paste(class(ctx$data), collapse = "/")); TRUE
  })
app14 <- ptr_app("ggplot(mtcars, aes(x = ppChk, y = ppVar)) + geom_point()")
# (cleanup: ptr_clear_placeholder("ppChk"))

# [validate_input] is NOT invoked while upstream resolution is pending — EXPECTED
# Pair ppChk2 with a ppUpload source so upstream is genuinely pending at boot.
# EXPECTED (R console): NO "[ppChk2] VALIDATE CALLED" line appears until AFTER you
# upload a file (upstream resolves). Before that, the hook is skipped.
ppChk2 <- ptr_define_placeholder_consumer("ppChk2",
  build_ui = function(node, cols, data, label = NULL, selected = NULL, ...)
    shiny::selectInput(node$id, label %||% "Column", choices = cols,
                       selected = intersect(selected %||% character(0), cols)),
  resolve_expr = function(value, node, ...) if (length(value) == 1 && nzchar(value)) rlang::sym(value) else NULL,
  validate_input = function(value, ctx) { message("[ppChk2] VALIDATE CALLED"); TRUE })
app15 <- ptr_app("ppUpload() |> ggplot(aes(x = ppChk2, y = ppVar)) + geom_point()")
# (cleanup: ptr_clear_placeholder("ppChk2"))


# -----------------------------------------------------------------------------
# Rule: ptr_define_placeholder_source(...) — spec round-trip
# -----------------------------------------------------------------------------

# [spec] shortcut sources round-trip via the shortcut name, dropping the payload — EXPECTED
# ppUpload is a shortcut source. Procedure: launch, type a name in "Optional
# dataset name" (e.g. my_df with my_df in scope), interact, then inspect the spec
# the preserve/reproduce panel emits.
# EXPECTED: the captured spec carries the typed NAME (shortcut id), NOT the
# fileInput data.frame payload (that node$id value is dropped — a tempfile path
# that cannot survive the session).
my_df2 <- mtcars
app16 <- ptr_app("ppUpload() |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()",
                 envir = environment())

# [spec] a complex-valued source WITHOUT shortcut cannot round-trip — NOT EXPECTED
# A custom source whose value is a raw data.frame and shortcut = FALSE (default).
# EXPECTED: its node$id value is NOT included in the spec snapshot — only
# deparse-able literals round-trip; a complex object silently can't, which is why
# the built-in ppUpload opts into shortcut = TRUE instead.
ppRawSrc <- ptr_define_placeholder_source("ppRawSrc",
  build_ui = function(node, label = NULL, ...) shiny::selectInput(node$id, label %||% "Dataset", choices = c("mtcars", "iris")),
  resolve_data = function(value, node, ...) if (length(value) == 1 && nzchar(value)) get(value, "package:datasets") else NULL,
  shortcut = FALSE)
app17 <- ptr_app("ppRawSrc() |> ggplot(aes(x = ppVar, y = ppVar)) + geom_point()")
# (cleanup: ptr_clear_placeholder("ppRawSrc"))

# -----------------------------------------------------------------------------
# Launch any one with, e.g.:   app01   (or shiny::runApp(app01))
# Remember to ptr_clear_placeholder(...) the custom keywords when finished.
# -----------------------------------------------------------------------------
