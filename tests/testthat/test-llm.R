# Tests for the LLM-assist surface (R/paintr-llm.R + inst/llm/ corpus).
#
# Two layers:
#  - behavioural contracts of the four exported file-reader functions;
#  - corpus-integrity invariants that gate the kind of drift fixed on
#    2026-05-30 (ghost ptr_module_* names, removed args, pre-pp* tokens).
#    These re-derive their expectations from NAMESPACE / the registry, so a
#    future API change that the corpus doesn't track turns them red. The
#    richer dev-time linter lives in dev/build-llm.R; this enshrines the core.

# ---- helpers ---------------------------------------------------------------

llm_dir <- function() {
  d <- system.file("llm", package = "ggpaintr")
  testthat::skip_if(!nzchar(d) || !dir.exists(d), "inst/llm not found")
  d
}

llm_corpus_files <- function() {
  d <- llm_dir()
  c(file.path(d, "primer.md"),
    list.files(file.path(d, "topics"), pattern = "\\.md$", full.names = TRUE))
}

# Every `ptr_xxx(` appearing in call position across `lines`.
ptr_calls_in <- function(lines) {
  m <- gregexpr("\\bptr_[A-Za-z0-9_]+(?=\\s*\\()", lines, perl = TRUE)
  unlist(regmatches(lines, m))
}

# Reserved ids / known-absent names the corpus may mention in call-ish prose.
ID_ALLOWLIST <- c("ptr_plot", "ptr_error", "ptr_code", "ptr_update_plot",
                  "ptr_shared_draw_all", "ptr_shared_errors",
                  "ptr_layer_select", "ptr_layer_tabset", "ptr_build_ids")

# ---- behavioural contract: ptr_llm_primer ----------------------------------

test_that("ptr_llm_primer returns a single non-empty string", {
  p <- ptr_llm_primer()
  expect_type(p, "character")
  expect_length(p, 1L)
  expect_true(nzchar(p))
  expect_match(p, "ggpaintr", fixed = TRUE)
})

# ---- behavioural contract: ptr_llm_topics ----------------------------------

test_that("ptr_llm_topics matches the topic files on disk, sorted", {
  topics <- ptr_llm_topics()
  expect_type(topics, "character")
  files <- sort(sub("\\.md$", "",
                    list.files(file.path(llm_dir(), "topics"),
                               pattern = "\\.md$")))
  expect_identical(topics, files)
  expect_false(is.unsorted(topics))
})

# ---- behavioural contract: ptr_llm_topic -----------------------------------

test_that("ptr_llm_topic returns the named topic's content", {
  txt <- ptr_llm_topic("overview")
  expect_type(txt, "character")
  expect_length(txt, 1L)
  expect_true(nzchar(txt))
})

test_that("ptr_llm_topic rejects an unknown topic with a typed error", {
  expect_error(ptr_llm_topic("does_not_exist"),
               class = "ggpaintr_llm_topic_unknown")
})

test_that("ptr_llm_topic validates its argument", {
  expect_error(ptr_llm_topic(c("a", "b")))   # length != 1
  expect_error(ptr_llm_topic(""))            # empty
  expect_error(ptr_llm_topic(42))            # not character
})

# ---- behavioural contract: ptr_llm_register --------------------------------

test_that("ptr_llm_register registers a tool and returns the chat", {
  skip_if_not_installed("ellmer")
  captured <- NULL
  fake_chat <- new.env()
  fake_chat$register_tool <- function(tool) captured <<- tool
  res <- ptr_llm_register(fake_chat)
  expect_identical(res, fake_chat)        # returns the chat (invisibly)
  expect_false(is.null(captured))         # a tool object was registered
})

test_that("ptr_llm_register honours a custom tool_name", {
  skip_if_not_installed("ellmer")
  captured <- NULL
  fake_chat <- new.env()
  fake_chat$register_tool <- function(tool) captured <<- tool
  ptr_llm_register(fake_chat, tool_name = "my_docs")
  # the registered ellmer ToolDef (S7) carries the requested name
  expect_identical(captured@name, "my_docs")
})

test_that("ptr_llm_register's tool schema argument is named `topic`", {
  # Contract: inst/llm/primer.md instructs the model to call
  # `ggpaintr_docs(topic)`, so the registered schema must accept `topic`.
  skip_if_not_installed("ellmer")
  captured <- NULL
  fake_chat <- new.env()
  fake_chat$register_tool <- function(tool) captured <<- tool
  ptr_llm_register(fake_chat)
  expect_identical(names(captured@arguments@properties), "topic")
})

test_that("ptr_llm_register rejects a non-chat object", {
  skip_if_not_installed("ellmer")
  expect_error(ptr_llm_register(42), class = "ggpaintr_llm_bad_chat")
})

# ---- corpus integrity: no ghost function names -----------------------------

test_that("every ptr_*() call in the corpus names an exported function", {
  exports <- getNamespaceExports("ggpaintr")
  offenders <- character(0)
  for (f in llm_corpus_files()) {
    calls <- ptr_calls_in(readLines(f, warn = FALSE))
    bad <- setdiff(unique(calls), c(exports, ID_ALLOWLIST))
    if (length(bad))
      offenders <- c(offenders, sprintf("%s: %s", basename(f),
                                         paste(bad, collapse = ", ")))
  }
  expect_identical(offenders, character(0))
})

# ---- corpus integrity: no removed/renamed arguments ------------------------

test_that("the corpus documents no removed/renamed arguments", {
  dead <- c("checkbox_defaults", "shared_ui", "copy_defaults", "companion_id_fn")
  pat <- sprintf("(?<![A-Za-z0-9_.])(%s)(?=\\s*=[^=])",
                 paste(dead, collapse = "|"))
  offenders <- character(0)
  for (f in llm_corpus_files()) {
    lines <- readLines(f, warn = FALSE)
    hits <- unlist(regmatches(lines, gregexpr(pat, lines, perl = TRUE)))
    if (length(hits))
      offenders <- c(offenders, sprintf("%s: %s", basename(f),
                                         paste(unique(hits), collapse = ", ")))
  }
  expect_identical(offenders, character(0))
})

# ---- corpus integrity: no pre-pp* bare placeholder tokens ------------------

test_that("the corpus uses no pre-pp* bare placeholder tokens in call position", {
  stale <- c("var", "text", "num", "expr", "upload")
  pat <- sprintf("(?<![A-Za-z0-9_.:])(%s)(?=\\s*\\()",
                 paste(stale, collapse = "|"))
  offenders <- character(0)
  for (f in llm_corpus_files()) {
    lines <- readLines(f, warn = FALSE)
    hits <- unlist(regmatches(lines, gregexpr(pat, lines, perl = TRUE)))
    if (length(hits))
      offenders <- c(offenders, sprintf("%s: %s", basename(f),
                                         paste(unique(hits), collapse = ", ")))
  }
  expect_identical(offenders, character(0))
})

# ---- corpus integrity: token table tracks the live registry ----------------

test_that("the five documented user tokens are all registered", {
  user_tokens <- c("ppVar", "ppText", "ppNum", "ppExpr", "ppUpload")
  expect_true(all(user_tokens %in% ptr_registry_keywords()))
})
