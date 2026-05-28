#!/usr/bin/env Rscript
# Build a symbol → line-range manifest for the ggpaintr source tree.
#
# Output: dev/agents/ggpaintr-source-expert/symbol-lines.json
#
# Usage (from project root):
#   Rscript dev/agents/ggpaintr-source-expert/build-symbol-manifest.R
#
# Why this exists: the ggpaintr-source-expert curriculum stores file:line
# citations as fast-path hints. They drift on every edit. This script is
# the agent's ground-truth lookup — re-run it whenever the curriculum is
# about to be consulted on a question whose answer depends on exact line
# numbers, or as a pre-commit hook on R/.
#
# The manifest captures TOP-LEVEL function definitions only:
#   <name> <- function(...) { ... }
# It handles S3 methods (build_ui_for.ptr_layer), one-liners, and any
# top-level `<-` or `=` assignment whose RHS is a function() call.
# Nested defs inside local() / inside other functions are not indexed —
# they aren't load-bearing API surface.

if (!file.exists("DESCRIPTION") || !dir.exists("R")) {
  stop("Run from the ggpaintr project root (the directory containing DESCRIPTION and R/).")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite').")
}

extract_top_level_functions <- function(file) {
  exprs <- parse(file, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")
  out <- list()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (!is.call(e)) next
    op <- e[[1L]]
    is_assign <- identical(op, quote(`<-`)) || identical(op, quote(`=`))
    if (!is_assign) next
    rhs <- e[[3L]]
    if (!is.call(rhs)) next
    if (!identical(rhs[[1L]], quote(`function`))) next
    lhs <- e[[2L]]
    if (!is.name(lhs)) next                          # skip `pkg::name <- function(...)`
    name <- as.character(lhs)
    src <- as.integer(srcrefs[[i]])
    out[[name]] <- list(
      file = file,
      start_line = src[1L],
      end_line = src[3L]
    )
  }
  out
}

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
all_symbols <- list()
duplicates <- character()

for (f in files) {
  res <- tryCatch(
    extract_top_level_functions(f),
    error = function(err) {
      message(sprintf("WARN: failed to parse %s: %s", f, conditionMessage(err)))
      list()
    }
  )
  for (name in names(res)) {
    if (!is.null(all_symbols[[name]])) {
      duplicates <- c(duplicates, sprintf("%s (in %s and %s)",
                                          name, all_symbols[[name]]$file, f))
    }
    all_symbols[[name]] <- res[[name]]
  }
}

if (length(duplicates) > 0L) {
  message("WARN: duplicate symbol names across files:")
  for (d in duplicates) message("  - ", d)
}

git_head <- tryCatch(
  trimws(system2("git", c("rev-parse", "--short", "HEAD"),
                 stdout = TRUE, stderr = FALSE)),
  error = function(e) NA_character_,
  warning = function(w) NA_character_
)

manifest <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  git_head = git_head,
  n_files = length(files),
  n_symbols = length(all_symbols),
  symbols = all_symbols
)

out_path <- "dev/agents/ggpaintr-source-expert/symbol-lines.json"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(manifest, out_path, pretty = TRUE, auto_unbox = TRUE)

cat(sprintf("Wrote %d symbols across %d files to %s\n",
            length(all_symbols), length(files), out_path))
cat(sprintf("git HEAD: %s   generated_at: %s\n",
            manifest$git_head, manifest$generated_at))
