# dev/build-llm.R — build-time generation + sync gate for the LLM-assist corpus.
#
# The content under inst/llm/ teaches an LLM the ggpaintr API. Because it is a
# second, hand-synced copy of the API contract, it drifts (see the ptr_module_*
# and bare-placeholder-token drift, 2026-05-30). This script makes the *facts*
# in that corpus source-derived and un-driftable:
#
#   lint     — assert every documented ptr_*() call names a real export and no
#              code example uses a pre-pp* bare placeholder token. The worklist
#              producer; exit 1 on any HARD violation.
#   generate — regenerate the sentinel-delimited managed blocks (signatures,
#              token table, topic index) in place from live introspection.
#   verify   — generate into a scratch copy and assert no diff vs the committed
#              corpus (the document()-style sync gate). exit 1 on drift.
#
# Usage: Rscript dev/build-llm.R <lint|generate|verify>
# Run from the package root. Loads the *source* package via pkgload so the
# introspected exports/formals/registry are the in-development truth, not an
# installed build.

suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE,
                                   attach_testthat = FALSE))

LLM_DIR    <- "inst/llm"
PRIMER     <- file.path(LLM_DIR, "primer.md")
TOPICS_DIR <- file.path(LLM_DIR, "topics")

# ---- authoritative sources -------------------------------------------------

ggp_export_set <- function() {
  getNamespaceExports("ggpaintr")
}

# S3 generics that have registered methods but are NOT exported — i.e. internal
# generics a user cannot call without `:::`. Derived from NAMESPACE so the rule
# stays source-grounded (catches build_ui_for et al). Documenting one of these
# as public API is the class-3 drift.
internal_generics <- function() {
  ns <- readLines("NAMESPACE", warn = FALSE)
  s3 <- regmatches(ns, regexec("^S3method\\(([^,]+),", ns))
  generics <- unique(vapply(s3[lengths(s3) == 2], `[`, character(1), 2))
  setdiff(generics, ggp_export_set())
}

# Names that look like ptr_*() calls in the docs but are NOT package functions:
# reserved input/output ids documented in call-ish prose, and the like. Kept
# explicit so the linter stays precise rather than silently broad.
ID_ALLOWLIST <- c(
  "ptr_plot", "ptr_error", "ptr_code", "ptr_update_plot",
  "ptr_shared_draw_all", "ptr_shared_errors",
  "ptr_layer_select", "ptr_layer_tabset"
)

# Names referenced ONLY to state they do not exist ("there is no `x()` helper").
# Legitimate prose, not a drifted call — excluded from the HARD scan.
ABSENT_ALLOWLIST <- c("ptr_build_ids")

# The 5 user-facing placeholder formula tokens, post-ADR-0009. The structural
# registry keywords (ppLayerOff, ppVerbSwitch) are intentionally excluded — they
# are not formula tokens a user writes. Asserted against the live registry below.
# Argument names documented in the corpus that no longer exist on ANY exported
# function (verified absent from R/ / from the relevant formals(), 2026-05-30):
# removed features and renamed constructor args. Flagged in `arg =` position.
DEAD_ARGS <- c("checkbox_defaults", "shared_ui", "copy_defaults",
               "companion_id_fn",
               # ADR 0027 renamed the placeholder-constructor args. Old public
               # names below are now "unused argument" errors. NOTE: `named_args`
               # is intentionally EXCLUDED here — it is still a live INJECTED
               # `build_ui` formal (function(node, ..., named_args = list(), ...)),
               # so flagging it would false-positive on legitimate examples; the
               # constructor arg is now `parse_named_args`.
               "positional_arg", "validate_input", "runtime")

USER_TOKENS <- c("ppVar", "ppText", "ppNum", "ppExpr", "ppUpload")
# Pre-pp* bare forms that must never appear in a code example (they no longer
# parse). Used for the placeholder-drift WARN scan.
STALE_BARE_TOKENS <- c("var", "text", "num", "expr", "upload")

llm_files <- function() {
  c(PRIMER, list.files(TOPICS_DIR, pattern = "\\.md$", full.names = TRUE))
}

# ---- lint ------------------------------------------------------------------

# Every `ptr_xxx(` appearing in call position.
extract_ptr_calls <- function(lines) {
  m <- gregexpr("\\bptr_[A-Za-z0-9_]+(?=\\s*\\()", lines, perl = TRUE)
  toks <- regmatches(lines, m)
  data.frame(
    line = rep(seq_along(lines), lengths(toks)),
    name = unlist(toks),
    stringsAsFactors = FALSE
  )
}

# Dead argument names in `arg =` position (named-argument usage).
extract_dead_args <- function(lines) {
  pat <- sprintf("(?<![A-Za-z0-9_.])(%s)(?=\\s*=[^=])",
                 paste(DEAD_ARGS, collapse = "|"))
  m <- gregexpr(pat, lines, perl = TRUE)
  toks <- regmatches(lines, m)
  data.frame(
    line = rep(seq_along(lines), lengths(toks)),
    name = unlist(toks),
    stringsAsFactors = FALSE
  )
}

# Any of `names` appearing in call position (e.g. internal generics).
extract_named_calls <- function(lines, names) {
  if (!length(names)) return(data.frame(line = integer(), name = character()))
  pat <- sprintf("(?<![A-Za-z0-9_.:])(%s)(?=\\s*\\()",
                 paste(names, collapse = "|"))
  m <- gregexpr(pat, lines, perl = TRUE)
  toks <- regmatches(lines, m)
  data.frame(
    line = rep(seq_along(lines), lengths(toks)),
    name = unlist(toks),
    stringsAsFactors = FALSE
  )
}

# Bare placeholder tokens in call position not prefixed by `pp` — i.e. the
# pre-rename forms `var(`, `text(`, ... Excludes ppVar( etc. (capital after pp).
extract_bare_tokens <- function(lines) {
  # Exclude a leading word char, `.`, or `:` so namespaced calls (rlang::expr)
  # and dotted/identifier suffixes don't false-positive — only truly bare tokens.
  pat <- sprintf("(?<![A-Za-z0-9_.:])(%s)(?=\\s*\\()",
                 paste(STALE_BARE_TOKENS, collapse = "|"))
  m <- gregexpr(pat, lines, perl = TRUE)
  toks <- regmatches(lines, m)
  data.frame(
    line = rep(seq_along(lines), lengths(toks)),
    name = unlist(toks),
    stringsAsFactors = FALSE
  )
}

lint <- function() {
  exports <- ggp_export_set()
  internal_gen <- internal_generics()
  hard <- list(); warn <- list(); intl <- list(); dead <- list()

  for (f in llm_files()) {
    lines <- readLines(f, warn = FALSE)

    calls <- extract_ptr_calls(lines)
    bad <- calls[!(calls$name %in% exports) &
                 !(calls$name %in% ID_ALLOWLIST) &
                 !(calls$name %in% ABSENT_ALLOWLIST), ]
    if (nrow(bad)) hard[[f]] <- bad

    bare <- extract_bare_tokens(lines)
    if (nrow(bare)) warn[[f]] <- bare

    ig <- extract_named_calls(lines, internal_gen)
    if (nrow(ig)) intl[[f]] <- ig

    da <- extract_dead_args(lines)
    if (nrow(da)) dead[[f]] <- da
  }

  cat("== ggpaintr LLM corpus lint ==\n")
  cat(sprintf("exports introspected: %d ptr_* of %d total\n",
              sum(grepl("^ptr_", exports)), length(exports)))

  cat("\n-- HARD: ptr_*() calls naming a non-export --\n")
  if (!length(hard)) {
    cat("  none\n")
  } else {
    for (f in names(hard)) {
      b <- hard[[f]]
      for (i in seq_len(nrow(b)))
        cat(sprintf("  %s:%d  %s()\n", f, b$line[i], b$name[i]))
    }
  }

  cat("\n-- WARN: pre-pp* bare placeholder token in call position --\n")
  if (!length(warn)) {
    cat("  none\n")
  } else {
    for (f in names(warn)) {
      b <- warn[[f]]
      for (i in seq_len(nrow(b)))
        cat(sprintf("  %s:%d  %s(\n", f, b$line[i], b$name[i]))
    }
  }

  cat("\n-- HARD: internal (non-exported) generic documented as a call --\n")
  if (!length(intl)) {
    cat("  none\n")
  } else {
    for (f in names(intl)) {
      b <- intl[[f]]
      for (i in seq_len(nrow(b)))
        cat(sprintf("  %s:%d  %s()\n", f, b$line[i], b$name[i]))
    }
  }

  cat("\n-- HARD: removed/renamed argument documented in `arg =` position --\n")
  if (!length(dead)) {
    cat("  none\n")
  } else {
    for (f in names(dead)) {
      b <- dead[[f]]
      for (i in seq_len(nrow(b)))
        cat(sprintf("  %s:%d  %s =\n", f, b$line[i], b$name[i]))
    }
  }

  # registry sanity: the 5 documented user tokens must all be registered.
  kws <- ptr_registry_keywords()
  missing_tok <- setdiff(USER_TOKENS, kws)
  cat("\n-- registry: USER_TOKENS present in ptr_registry_keywords() --\n")
  cat(if (length(missing_tok))
        paste0("  MISSING: ", paste(missing_tok, collapse = ", "), "\n")
      else "  all present\n")

  n_hard <- sum(vapply(hard, nrow, integer(1))) +
            sum(vapply(intl, nrow, integer(1))) +
            sum(vapply(dead, nrow, integer(1)))
  cat(sprintf("\nHARD violations: %d   WARN: %d\n",
              n_hard, sum(vapply(warn, nrow, integer(1)))))
  invisible(n_hard)
}

# ---- generate: sentinel-delimited managed blocks ---------------------------
#
# A managed block is:
#   <!-- @ptr-gen:<type> -->
#   ...regenerated content...
#   <!-- @ptr-gen:end -->
# `generate` replaces each block's interior with render_block(<type>) computed
# from live introspection; `verify` asserts the committed files already match.

BLOCK_OPEN  <- "^<!-- @ptr-gen:([a-z-]+) -->\\s*$"
BLOCK_CLOSE <- "^<!-- @ptr-gen:end -->\\s*$"

# Token table — keyword column asserted against the live registry; widget/value
# prose is editorial (kept here so adding a builtin forces a deliberate edit).
TOKEN_ROWS <- list(
  ppVar    = c("column picker (`shinyWidgets::pickerInput`)", "column symbol"),
  ppText   = c("`textInput`", "string"),
  ppNum    = c("`numericInput`", "numeric"),
  ppExpr   = c("code input (denylist-guarded)", "parsed R expression"),
  ppUpload = c("`fileInput` (.csv/.tsv/.rds/.xlsx/.xls/.json)", "data frame")
)

render_token_table <- function() {
  kws <- ptr_registry_keywords()
  miss <- setdiff(names(TOKEN_ROWS), kws)
  if (length(miss))
    stop("TOKEN_ROWS names not registered: ", paste(miss, collapse = ", "))
  c(
    "| Keyword | Widget | Runtime value |",
    "|---------|--------|---------------|",
    vapply(names(TOKEN_ROWS), function(k)
      sprintf("| `%s` | %s | %s |", k, TOKEN_ROWS[[k]][1], TOKEN_ROWS[[k]][2]),
      character(1))
  )
}

# Topic index — names from files on disk; one-line descriptions are editorial
# (keys asserted == the topic files, so a new topic forces a description).
TOPIC_DESC <- c(
  overview            = "the 3-level L1/L2/L3 integration model",
  formula_syntax      = "the 5 `pp*` placeholder keywords, pipelines, `shared = \"...\"` + the partition rule",
  level1_ptr_app      = "minimal turn-key app",
  level1_ptr_options  = "session-wide settings via `ptr_options()`",
  level2_module       = "embed in your own Shiny app with the default layout (`ptr_ui` / `ptr_server`), single vs. multi instance",
  level2_shared       = "multiple linked instances + the shared coordinator trio (`ptr_shared` -> `ptr_shared_panel` / `ptr_shared_server`)",
  level2_custom_ids   = "id collisions, the reserved `ptr_` prefix, the generated input-id grammar",
  level2_ui_text      = "copy overrides via `ui_text`, the cascade rules, a worked example",
  level3_layout       = "bare `ptr_ui_*` pieces + combinators + the optional `ptr_ui_page` shell + the navbar/bslib escape hatch",
  level3_custom_render = "your own `renderPlot()` / `renderPlotly()` off `state$runtime()` via the `moduleServer(id)` pattern",
  level3_gg_extra     = "round-trip host ggplot layers into the plot AND code pane via `ptr_gg_extra(state, ...)`",
  custom_placeholder  = "the value / consumer / source constructors, end-to-end examples"
)

render_topic_index <- function() {
  files <- sort(sub("\\.md$", "",
                    list.files(TOPICS_DIR, pattern = "\\.md$")))
  miss <- setdiff(files, names(TOPIC_DESC))
  extra <- setdiff(names(TOPIC_DESC), files)
  if (length(miss))  stop("topic files lacking a TOPIC_DESC entry: ",
                          paste(miss, collapse = ", "))
  if (length(extra)) stop("TOPIC_DESC entries with no topic file: ",
                          paste(extra, collapse = ", "))
  vapply(files, function(f) sprintf("- `%s` — %s", f, TOPIC_DESC[[f]]),
         character(1))
}

render_block <- function(type) {
  # unname(): the renderers vapply over named inputs, yielding named vectors;
  # readLines() returns unnamed, so the names would defeat the identical()
  # idempotency / verify check even though writeLines() drops them.
  unname(switch(type,
    "token-table" = render_token_table(),
    "topic-index" = render_topic_index(),
    stop(sprintf("unknown @ptr-gen block type: %s", type))
  ))
}

# Rewrite every managed block in `lines`, returning the new lines.
regen_lines <- function(lines) {
  out <- character(0); i <- 1L
  while (i <= length(lines)) {
    m <- regmatches(lines[i], regexec(BLOCK_OPEN, lines[i]))[[1]]
    if (length(m) == 2L) {
      type <- m[2]
      close_rel <- which(grepl(BLOCK_CLOSE, lines[(i + 1L):length(lines)]))
      if (!length(close_rel))
        stop(sprintf("unterminated @ptr-gen:%s block", type))
      close_i <- i + close_rel[1]
      out <- c(out, lines[i], render_block(type), lines[close_i])
      i <- close_i + 1L
    } else {
      out <- c(out, lines[i]); i <- i + 1L
    }
  }
  out
}

generate <- function(write = TRUE) {
  changed <- character(0)
  for (f in llm_files()) {
    lines <- readLines(f, warn = FALSE)
    if (!any(grepl(BLOCK_OPEN, lines))) next
    new <- regen_lines(lines)
    if (!identical(new, lines)) {
      changed <- c(changed, f)
      if (write) writeLines(new, f)
    }
  }
  changed
}

verify <- function() {
  drift <- character(0)
  for (f in llm_files()) {
    lines <- readLines(f, warn = FALSE)
    if (!any(grepl(BLOCK_OPEN, lines))) next
    if (!identical(regen_lines(lines), lines)) drift <- c(drift, f)
  }
  if (length(drift)) {
    cat("@ptr-gen managed blocks are STALE — run `Rscript dev/build-llm.R generate`:\n")
    for (f in drift) cat("  ", f, "\n", sep = "")
    quit(status = 1, save = "no")
  }
  cat("@ptr-gen managed blocks: in sync\n")
}

# ---- dispatch --------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
cmd <- if (length(args)) args[[1]] else "lint"
switch(cmd,
  lint     = { n <- lint(); if (n > 0) quit(status = 1, save = "no") },
  generate = {
    ch <- generate(write = TRUE)
    if (length(ch)) { cat("regenerated @ptr-gen blocks in:\n");
                      for (f in ch) cat("  ", f, "\n", sep = "") }
    else cat("@ptr-gen blocks already current\n")
  },
  verify   = verify(),
  stop(sprintf("unknown command: %s (expected lint|generate|verify)", cmd))
)
