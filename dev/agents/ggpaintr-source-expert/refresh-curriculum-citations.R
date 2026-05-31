#!/usr/bin/env Rscript
# Refresh function-level line citations in knowledge markdown files from the
# current symbol-lines.json manifest.
#
# Usage (from project root):
#   Rscript dev/agents/ggpaintr-source-expert/refresh-curriculum-citations.R          # dry-run
#   Rscript dev/agents/ggpaintr-source-expert/refresh-curriculum-citations.R --apply  # write
#
# What it does:
#   For every `R/paintr-X.R:N` or `R/paintr-X.R:N-M` citation in
#   knowledge/*.md, look ~100 chars before the citation for a known
#   function name (a key in symbol-lines.json). If found AND the cited
#   file matches the symbol's file AND the cited range is "function-level"
#   (start within ±5 of the JSON's start_line; end within ±5 of end_line),
#   rewrite the line numbers from the JSON.
#
# What it does NOT touch:
#   - Inner-block citations (e.g. lines 1754-1764 of a function spanning
#     1742-1851). Those describe code regions inside a function body and
#     can't be safely auto-refreshed.
#   - Citations whose adjacent symbol the JSON doesn't know about.
#   - Citations to non-paintr files.
#
# Requires symbol-lines.json. Build first with build-symbol-manifest.R.

args <- commandArgs(trailingOnly = TRUE)
apply_changes <- "--apply" %in% args

manifest_path <- "dev/agents/ggpaintr-source-expert/symbol-lines.json"
if (!file.exists(manifest_path)) {
  stop("symbol-lines.json not found. Build first:\n",
       "  Rscript dev/agents/ggpaintr-source-expert/build-symbol-manifest.R")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite').")
}

manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
symbols <- manifest$symbols

# Token regex used to extract identifier-like words from the lookbehind.
# Matches R identifiers including S3 method names with dots
# (e.g. build_ui_for.ptr_layer).
token_re <- "[A-Za-z][A-Za-z0-9_.]*"

# Citation regex — captures file, start line, optional end line.
citation_re <- "(R/paintr-[A-Za-z0-9_-]+\\.R):(\\d+)(?:-(\\d+))?"

md_files <- list.files("dev/agents/ggpaintr-source-expert/knowledge",
                       pattern = "\\.md$", full.names = TRUE)

LOOKBEHIND_CHARS <- 120L
RANGE_TOLERANCE  <- 5L

total_rewrites <- 0L
total_no_match  <- 0L
total_wrong_file <- 0L
total_inner_block <- 0L
total_already_current <- 0L

for (md in md_files) {
  full_text <- paste(readLines(md, warn = FALSE), collapse = "\n")

  m <- gregexpr(citation_re, full_text, perl = TRUE)[[1L]]
  if (m[1L] == -1L) next
  match_lens <- attr(m, "match.length")

  changes <- list()

  for (i in seq_along(m)) {
    start_pos <- m[i]
    end_pos <- start_pos + match_lens[i] - 1L
    citation <- substr(full_text, start_pos, end_pos)

    parts <- regmatches(citation, regexec(citation_re, citation, perl = TRUE))[[1L]]
    cited_file  <- parts[2L]
    cited_start <- as.integer(parts[3L])
    cited_end   <- if (nchar(parts[4L]) > 0L) as.integer(parts[4L]) else cited_start

    # Lookbehind for the adjacent symbol mention. Extract every identifier-
    # like token; walk from the closest-to-citation backwards; the first
    # token that's a key in `symbols` is the citation's referent.
    look_start <- max(1L, start_pos - LOOKBEHIND_CHARS)
    lookbehind <- substr(full_text, look_start, start_pos - 1L)

    tokens <- regmatches(lookbehind, gregexpr(token_re, lookbehind, perl = TRUE))[[1L]]
    matched_sym <- NULL
    if (length(tokens) > 0L) {
      for (tok in rev(tokens)) {                      # closest-to-citation first
        if (!is.null(symbols[[tok]])) {
          matched_sym <- tok
          break
        }
      }
    }

    if (is.null(matched_sym)) {
      total_no_match <- total_no_match + 1L
      next
    }
    sym_info <- symbols[[matched_sym]]
    if (!identical(sym_info$file, cited_file)) {
      total_wrong_file <- total_wrong_file + 1L
      next
    }

    json_start <- sym_info$start_line
    json_end   <- sym_info$end_line
    if (abs(cited_start - json_start) > RANGE_TOLERANCE ||
        abs(cited_end   - json_end)   > RANGE_TOLERANCE) {
      # Inner-block citation — citing a region inside the function body,
      # not the function definition itself. Skip.
      total_inner_block <- total_inner_block + 1L
      next
    }

    new_citation <- if (json_start == json_end) {
      sprintf("%s:%d", cited_file, json_start)
    } else {
      sprintf("%s:%d-%d", cited_file, json_start, json_end)
    }

    if (identical(citation, new_citation)) {
      total_already_current <- total_already_current + 1L
      next
    }

    changes[[length(changes) + 1L]] <- list(
      start = start_pos, end = end_pos,
      old = citation, new = new_citation,
      symbol = matched_sym
    )
  }

  if (length(changes) == 0L) next

  # Apply right-to-left so earlier positions stay valid.
  ord <- order(-vapply(changes, function(x) x$start, integer(1L)))
  new_text <- full_text
  for (j in ord) {
    ch <- changes[[j]]
    new_text <- paste0(
      substr(new_text, 1L, ch$start - 1L),
      ch$new,
      substr(new_text, ch$end + 1L, nchar(new_text))
    )
  }

  cat(sprintf("\n=== %s — %d rewrite%s ===\n",
              basename(md), length(changes), if (length(changes) == 1L) "" else "s"))
  # Print in document order for readability.
  doc_order <- order(vapply(changes, function(x) x$start, integer(1L)))
  for (j in doc_order) {
    ch <- changes[[j]]
    cat(sprintf("  [%s]  %s  ->  %s\n", ch$symbol, ch$old, ch$new))
  }

  total_rewrites <- total_rewrites + length(changes)

  if (apply_changes) {
    writeLines(strsplit(new_text, "\n", fixed = TRUE)[[1L]], md)
  }
}

cat(sprintf(
  "\nSummary: %d rewritten | %d already current | %d inner-block skipped | %d no-symbol-nearby | %d wrong-file\n",
  total_rewrites, total_already_current, total_inner_block, total_no_match, total_wrong_file
))

if (!apply_changes && total_rewrites > 0L) {
  cat("\nDry-run mode. Rerun with --apply to write the changes above.\n")
}
