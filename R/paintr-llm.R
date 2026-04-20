#' Get the ggpaintr LLM system-prompt primer
#'
#' Returns the text of `inst/llm/primer.md` as a single string. Intended
#' for use as the `system_prompt =` (or equivalent) argument when wiring
#' ggpaintr into an LLM client such as `ellmer`, so the model knows when
#' to reach for ggpaintr and at which of the three integration levels.
#'
#' The primer is short by design — it establishes the extensibility
#' model and points the model at [ptr_llm_topic()] for runnable
#' examples. A companion tool that exposes `ptr_llm_topic()` to the
#' model lets it pull only the example it needs, instead of loading
#' every topic into the system prompt.
#'
#' @return A single character string.
#' @seealso [ptr_llm_topic()], [ptr_llm_topics()]
#' @export
#' @examples
#' primer <- ptr_llm_primer()
#' cat(substr(primer, 1, 200))
ptr_llm_primer <- function() {
  path <- system.file("llm", "primer.md", package = "ggpaintr")
  if (!nzchar(path)) {
    rlang::abort(
      "ggpaintr LLM primer not found.",
      class = "ggpaintr_llm_primer_missing"
    )
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

#' List available ggpaintr LLM topic names
#'
#' Returns the character vector of topic names accepted by
#' [ptr_llm_topic()]. Matches the files in `inst/llm/topics/` (stripped
#' of the `.md` extension), sorted alphabetically.
#'
#' @return A character vector.
#' @seealso [ptr_llm_topic()], [ptr_llm_primer()]
#' @export
#' @examples
#' ptr_llm_topics()
ptr_llm_topics <- function() {
  dir <- system.file("llm", "topics", package = "ggpaintr")
  if (!nzchar(dir)) {
    rlang::abort(
      "ggpaintr LLM topic directory not found.",
      class = "ggpaintr_llm_topics_missing"
    )
  }
  files <- list.files(dir, pattern = "\\.md$", full.names = FALSE)
  sort(sub("\\.md$", "", files))
}

#' Fetch a ggpaintr LLM topic by name
#'
#' Returns the runnable example + commentary for one topic as a single
#' character string. Designed to back an LLM tool such as
#' `ggpaintr_docs(topic)`: the model calls it when the user asks for
#' help with an interactive ggplot task, and receives exactly one
#' focused example instead of the entire manual.
#'
#' Each topic is derived from (and kept in sync with) either
#' `README.Rmd` or one of the two vignettes
#' (`ggpaintr-extensibility`, `ggpaintr-placeholder-registry`).
#'
#' @param name Topic name. Must be one of [ptr_llm_topics()].
#' @return A single character string.
#' @seealso [ptr_llm_topics()], [ptr_llm_primer()]
#' @export
#' @examples
#' cat(ptr_llm_topic("level1_ptr_app"))
ptr_llm_topic <- function(name) {
  assertthat::assert_that(
    is.character(name),
    length(name) == 1L,
    nzchar(name)
  )

  available <- ptr_llm_topics()
  if (!name %in% available) {
    rlang::abort(
      c(
        sprintf("Unknown ggpaintr LLM topic: %s", name),
        i = paste0("Available topics: ",
                   paste(available, collapse = ", "))
      ),
      class = "ggpaintr_llm_topic_unknown"
    )
  }

  path <- system.file("llm", "topics", paste0(name, ".md"),
                      package = "ggpaintr")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

#' Register ggpaintr with an ellmer chat session
#'
#' One-line registration of the ggpaintr docs tool on an existing
#' [ellmer][ellmer::ellmer-package] `Chat` object. The tool wraps
#' [ptr_llm_topic()] so the LLM can pull focused, runnable examples
#' on demand instead of loading every topic into the system prompt.
#'
#' The set of valid topic names is snapshotted at registration time
#' using [ptr_llm_topics()], so the LLM cannot request a topic that
#' does not exist. If you upgrade ggpaintr in the same session and new
#' topics are added, call this again on a fresh chat.
#'
#' This function does *not* set the chat's system prompt. Pass
#' [ptr_llm_primer()] to the `system_prompt =` argument of your
#' `chat_*()` constructor so the model knows when to reach for the tool.
#'
#' @param chat An ellmer `Chat` object (from `ellmer::chat_anthropic()`,
#'   `ellmer::chat_openai()`, etc.).
#' @param tool_name String. The name the LLM will call the tool under.
#'   Defaults to `"ggpaintr_docs"`; override if it would collide with
#'   another tool you already registered.
#' @return The `chat` object (invisibly), for piping.
#' @seealso [ptr_llm_primer()], [ptr_llm_topic()], [ptr_llm_topics()]
#' @export
#' @examples
#' \dontrun{
#' library(ellmer)
#' chat <- chat_anthropic(system_prompt = ptr_llm_primer())
#' ptr_llm_register(chat)
#' chat$chat("Build a Shiny app where the user picks X and Y columns from mtcars.")
#' }
ptr_llm_register <- function(chat, tool_name = "ggpaintr_docs") {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    rlang::abort(
      c(
        "ellmer is not installed.",
        i = 'Install it with install.packages("ellmer").'
      ),
      class = "ggpaintr_llm_ellmer_missing"
    )
  }

  assertthat::assert_that(
    is.character(tool_name),
    length(tool_name) == 1L,
    nzchar(tool_name)
  )
  if (!inherits(chat, "Chat") && !inherits(chat, "R6") &&
      !is.environment(chat)) {
    rlang::abort(
      "`chat` must be an ellmer Chat object.",
      class = "ggpaintr_llm_bad_chat"
    )
  }

  topic_names <- ptr_llm_topics()

  tool_def <- ellmer::tool(
    ptr_llm_topic,
    name = tool_name,
    description = paste(
      "Fetch a runnable ggpaintr example + commentary for one",
      "integration topic. Call BEFORE writing any R code for",
      "interactive ggplot tasks (Shiny apps showing ggplots with",
      "user-adjustable inputs). Returns markdown with a standalone",
      "runnable R snippet."
    ),
    arguments = list(
      name = ellmer::type_enum(
        topic_names,
        paste0(
          "Which ggpaintr topic to fetch. ",
          "Pick based on the user's task: overview for the 3-level ",
          "model; level1_ptr_app for a turn-key app; level2_* for ",
          "embedding in the user's own Shiny app; level3_* for ",
          "headless or custom renderPlot() use; custom_placeholder ",
          "for widget types beyond var/text/num/expr/upload."
        )
      )
    )
  )

  chat$register_tool(tool_def)
  invisible(chat)
}
