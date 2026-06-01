# RStudio addin: wrap a highlighted token in a ggpaintr placeholder.
#
# UX: highlight `mpg` in `aes(x = mpg)`, trigger the addin (Addins menu or a
# bound keyboard shortcut), pick a placeholder from a command-palette gadget,
# and the selection is rewritten to `ppVar(mpg)`. With nothing highlighted the
# same palette opens and inserts `ppVar()` with the caret between the parens.
#
# The palette's choices come from `ptr_registry_keywords()` at the moment it
# opens, so any placeholder a user registered this session
# (`ptr_define_placeholder_value()` etc.) appears automatically. Structural
# keywords (`ppLayerOff`, `ppVerbSwitch`) are included too — they wrap a call
# (a layer or a dplyr verb), so they are useful when a call is highlighted.
#
# The list is *ordered by the kind of the highlighted text* so the likely
# choice sits at the top: a string promotes `ppText`, a number `ppNum`, a bare
# symbol the column consumers then `ppExpr`, a call the structural keywords, and
# a name that resolves to a data frame promotes the data sources (`ppUpload`).
#
# Layering: `ptr_addin_build_replacement()` is the pure, RStudio-free text
# transform; `ptr_addin_classify()` + `ptr_addin_order()` are the pure
# type-aware ordering. `ptr_addin_pickable()` reads the registry. The gadget and
# `ptr_wrap_placeholder_addin()` are the interactive shell that only runs inside
# RStudio.

# Pure text transform: given a chosen keyword and the (possibly empty) selected
# text, return the replacement string plus the caret offset (characters from
# the start of the replacement where the cursor should land afterwards).
#
# Contract:
#   - Non-empty selection -> "<keyword>(<trimmed-selection>)", caret at the end.
#   - Empty/whitespace-only selection -> "<keyword>()", caret between the parens.
ptr_addin_build_replacement <- function(keyword, selected) {
  if (is.null(selected)) selected <- ""
  arg <- trimws(selected)
  if (nzchar(arg)) {
    text <- paste0(keyword, "(", arg, ")")
    list(text = text, caret = nchar(text))
  } else {
    text <- paste0(keyword, "()")
    list(text = text, caret = nchar(keyword) + 1L)
  }
}

# Human-readable one-liners for the built-ins; custom placeholders fall back to
# a role-derived description.
ptr_addin_describe <- function(keyword, role) {
  builtin <- c(
    ppVar        = "Pick a data column",
    ppNum        = "Numeric input (slider / box)",
    ppText       = "Free-text input",
    ppExpr       = "R expression input",
    ppUpload     = "Upload / choose a dataset",
    ppLayerOff   = "Toggle a ggplot layer on/off",
    ppVerbSwitch = "Toggle a data-prep verb on/off"
  )
  if (keyword %in% names(builtin)) return(unname(builtin[[keyword]]))
  switch(role,
    value      = "Custom value placeholder",
    consumer   = "Custom column placeholder",
    source     = "Custom data-source placeholder",
    structural = "Custom structural placeholder",
    paste0("Custom ", role, " placeholder")
  )
}

# Classify the highlighted text into a kind that drives the picker ordering.
# Returns one of: "empty", "string", "number", "dataframe", "symbol", "call",
# "other". `env` is consulted only to tell whether a bare symbol names a data
# frame (so a data-source placeholder can be promoted). Resolution follows the
# search path (`inherits = TRUE`), so both a workspace object
# (`df <- read_csv(...)`) and a package dataset (`mtcars`) are detected. This
# can misfire on a bare column name that shadows a dataset (`mpg` -> ggplot2's
# `mpg`), which only promotes the data sources to the top; every placeholder
# stays in the list, so the cost of a wrong guess is one keystroke.
ptr_addin_classify <- function(selected, env = globalenv()) {
  txt <- trimws(selected %||% "")
  if (!nzchar(txt)) return("empty")
  parsed <- tryCatch(rlang::parse_expr(txt), error = function(e) NULL)
  if (is.null(parsed)) return("other")
  if (is.character(parsed)) return("string")
  if (is.numeric(parsed)) return("number")
  # Unary +/- on a literal number parses as a call; treat as a number.
  if (is.call(parsed) && length(parsed) == 2L &&
      as.character(parsed[[1]]) %in% c("-", "+") && is.numeric(parsed[[2]])) {
    return("number")
  }
  if (is.symbol(parsed)) {
    nm <- as.character(parsed)
    val <- tryCatch(
      if (exists(nm, envir = env, inherits = TRUE)) {
        get(nm, envir = env, inherits = TRUE)
      } else NULL,
      error = function(e) NULL
    )
    if (is.data.frame(val)) return("dataframe")
    return("symbol")
  }
  if (is.call(parsed)) return("call")
  "other"
}

# Reorder pickable placeholders so the kind-appropriate ones sit on top. Pure
# function of (pickable, kind). Within a tier the secondary ordering is
# *by role* (consumer, value, source, structural), then built-ins before any
# custom keyword of that role, then alphabetical — so a custom placeholder joins
# its role group rather than being dumped at the bottom of the list.
ptr_addin_order <- function(pickable, kind) {
  if (length(pickable) == 0L) return(pickable)
  kws   <- vapply(pickable, function(p) p$keyword, character(1))
  roles <- vapply(pickable, function(p) p$role, character(1))

  # Tier 1: promotion by the highlighted kind.
  promoted <- switch(kind,
    string    = kws == "ppText",
    number    = kws == "ppNum",
    symbol    = roles == "consumer" | kws == "ppExpr",
    dataframe = roles == "source",
    call      = roles == "structural",
    rep(FALSE, length(kws))
  )
  prio <- ifelse(promoted, 1L, 2L)

  # Tier 2: by role. Consumers, then values, then sources, then structural —
  # this reproduces the built-in canonical order and lets customs interleave.
  role_rank <- c(consumer = 1L, value = 2L, source = 3L, structural = 4L)
  rr <- unname(role_rank[roles])
  rr[is.na(rr)] <- max(role_rank) + 1L

  # Tier 3: built-ins keep their canonical sub-order; customs fall after them
  # (within the same role) and then sort alphabetically (Tier 4 = kws).
  builtin_order <- c("ppVar", "ppNum", "ppText", "ppExpr", "ppUpload",
                     "ppLayerOff", "ppVerbSwitch")
  bi <- match(kws, builtin_order, nomatch = length(builtin_order) + 1L)

  pickable[order(prio, rr, bi, kws)]
}

# Pickable placeholders, registry-backed. Built-ins first in canonical order
# (including the two structural keywords), then any custom keywords
# alphabetically. Returns a list of list(keyword, role, description). The
# kind-aware promotion is applied later by `ptr_addin_order()`.
ptr_addin_pickable <- function() {
  kws <- ptr_registry_keywords()
  canonical <- c("ppVar", "ppNum", "ppText", "ppExpr", "ppUpload",
                 "ppLayerOff", "ppVerbSwitch")
  ordered <- c(intersect(canonical, kws), sort(setdiff(kws, canonical)))
  out <- list()
  for (kw in ordered) {
    entry <- ptr_registry_lookup(kw)
    out[[kw]] <- list(
      keyword = kw,
      role = entry$role,
      description = ptr_addin_describe(kw, entry$role)
    )
  }
  out
}

# Build the selectize `options` list — one object per placeholder, carrying the
# fields the custom renderer needs (keyword / role / description). Unnamed so it
# serialises to a JSON array; order is whatever `pickable` already is.
ptr_addin_items <- function(pickable) {
  unname(lapply(pickable, function(p) {
    list(kw = p$keyword, role = p$role, desc = p$description)
  }))
}

# Dark "command palette" stylesheet for the gadget — themes the miniUI chrome
# and the selectize control to match a VS Code-style palette.
ptr_addin_palette_css <- function() {
  shiny::HTML(r"(
    body, .gadget-tags, .gadget-title { background:#1e1e1e !important; }
    .gadget-title { color:#fff !important; border-bottom:1px solid #3c3c3c !important; }
    .gadget-title h1 { color:#fff !important; font-size:14px; font-weight:600; }
    .gadget-title .btn { background:#3a3d41; color:#ddd; border:1px solid #4a4a4a; }
    .gadget-title .btn-primary { background:#0e639c; border-color:#0e639c; color:#fff; }
    .gadget-content { background:#1e1e1e !important; }
    .control-label { color:#bbb !important; font-weight:600; margin-bottom:6px; }
    .selectize-control.single .selectize-input,
    .selectize-control.single .selectize-input.input-active {
      background:#2b2b2c !important; border:1px solid #094771 !important;
      box-shadow:none !important; color:#d4d4d4 !important; }
    .selectize-input > input { color:#d4d4d4 !important; }
    .selectize-input.focus { box-shadow:0 0 0 1px #0e639c !important; }
    .selectize-dropdown { background:#2b2b2c !important; border:1px solid #094771 !important;
      color:#d4d4d4 !important; }
    .selectize-dropdown .option { color:#d4d4d4; padding:7px 10px; }
    .selectize-dropdown .active { background:#04395e !important; color:#fff !important; }
    .pp-kw { font-family:Menlo,Consolas,monospace; color:#dcdcaa; }
    .pp-meta { color:#9a9a9a; margin-left:10px; font-size:12px; }
    .pp-role { font-style:italic; color:#4ec9b0; }
    .pp-role.pp-source { color:#e8a33d; }
    .pp-role.pp-structural { color:#c586c0; }
  )")
}

# selectize render functions: a monospace keyword + coloured role/description in
# the dropdown, and just the keyword in the chosen-item box. Raw JS (R >= 4.0).
ptr_addin_render_js <- function() {
  I(r"({
    option: function(item, escape) {
      return '<div class="pp-opt"><span class="pp-kw">' + escape(item.kw) + '</span>' +
        '<span class="pp-meta"><span class="pp-role pp-' + escape(item.role) + '">' +
        escape(item.role) + '</span> &middot; ' + escape(item.desc) + '</span></div>';
    },
    item: function(item, escape) {
      return '<div><span class="pp-kw">' + escape(item.kw) + '</span></div>';
    }
  })")
}

# The command-palette gadget. Returns the chosen keyword (string) or NULL if
# the user cancelled. Split out so the entry point stays thin.
ptr_addin_run_palette <- function(selected, env = globalenv()) {
  kind <- ptr_addin_classify(selected, env = env)
  items <- ptr_addin_items(ptr_addin_order(ptr_addin_pickable(), kind))
  has_selection <- nzchar(trimws(selected %||% ""))
  prompt <- if (has_selection) {
    sprintf("Wrap “%s” in…", trimws(selected))
  } else {
    "Insert placeholder…"
  }

  ui <- miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(ptr_addin_palette_css())),
    miniUI::gadgetTitleBar(
      "ggpaintr placeholder",
      right = shiny::tagList(
        # left of Insert: wrap the whole selection in { ... } |> ptr_app()
        miniUI::miniTitleBarButton("app", "Wrap in app"),
        miniUI::miniTitleBarButton("done", "Insert", primary = TRUE)
      )
    ),
    miniUI::miniContentPanel(
      shiny::selectizeInput(
        "kw", label = prompt, width = "100%",
        choices = NULL, selected = character(0),
        options = list(
          options = items,
          valueField = "kw", labelField = "kw",
          searchField = c("kw", "role", "desc"),
          render = ptr_addin_render_js(),
          placeholder = "type to filter, ↑↓ to move, Enter to pick",
          openOnFocus = TRUE, maxItems = 1L, maxOptions = 50L
        )
      ),
      # Autofocus + open the dropdown so it behaves like a command palette.
      shiny::tags$script(shiny::HTML(paste0(
        "$(function(){setTimeout(function(){",
        "var s=$('#kw')[0];",
        "if(s&&s.selectize){s.selectize.focus();s.selectize.open();}",
        "},200);});"
      )))
    )
  )

  server <- function(input, output, session) {
    pick <- function() {
      if (nzchar(input$kw %||% "")) {
        list(action = "placeholder", keyword = input$kw)
      } else {
        NULL
      }
    }
    # selectize sets input$kw only on an actual pick (Enter/click), not while
    # typing, so finishing here gives the one-keystroke palette feel.
    shiny::observeEvent(input$kw, {
      if (nzchar(input$kw)) shiny::stopApp(list(action = "placeholder", keyword = input$kw))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$done, shiny::stopApp(pick()))
    shiny::observeEvent(input$app,  shiny::stopApp(list(action = "app")))
    shiny::observeEvent(input$cancel, shiny::stopApp(NULL))
  }

  # suppressMessages hides shiny's "Listening on http://127.0.0.1:<port>" line
  # (the gadget is a tiny local Shiny app; the message is emitted via message()).
  suppressMessages(shiny::runGadget(
    ui, server,
    viewer = shiny::dialogViewer("ggpaintr placeholder", width = 460, height = 230)
  ))
}

#' RStudio addin: wrap a selection in a ggpaintr placeholder
#'
#' Interactive RStudio addin. Highlight a token in your ggplot expression
#' (e.g. `mpg` in `aes(x = mpg)`), run the addin, and pick a placeholder from
#' a command-palette gadget; the selection is rewritten to `ppVar(mpg)`. With
#' nothing highlighted the same palette opens and inserts `ppVar()` with the
#' caret between the parens. The placeholder list is read live from the
#' registry, so custom placeholders registered this session
#' (via [ptr_define_placeholder_value()] and friends) appear automatically.
#'
#' The gadget's *Wrap in app* button (left of *Insert*) takes a different
#' action: instead of inserting a placeholder it wraps the whole selection in a
#' braced block piped into [ptr_app()] —
#' `{` / `  <selection>` / `} |> ` / `  ptr_app()` — turning a ggplot
#' expression into a runnable ggpaintr app skeleton.
#'
#' @section Keyboard shortcut:
#' For a highlight-then-keystroke flow, bind the addin once (RStudio reads
#' shortcuts only from your own keybindings, so packages cannot ship one). The
#' addin must be **installed** (not merely `load_all()`-ed) to appear in the
#' shortcut dialog:
#'
#' 1. *Tools > Addins > Browse Addins…*, then the *Keyboard Shortcuts…*
#'    button. (Or *Tools > Modify Keyboard Shortcuts…* and type "ggpaintr"
#'    in the search box.)
#' 2. Find the *ggpaintr placeholder* row and click its *Shortcut* cell.
#' 3. Press the recommended combination: **Cmd+Shift+G** on macOS,
#'    **Ctrl+Shift+G** on Windows/Linux. (Any free combination works; pick
#'    another if that one is already bound.)
#' 4. *Apply*.
#'
#' Requires \pkg{rstudioapi} and \pkg{miniUI}; it errors with a clear message
#' when run outside RStudio.
#'
#' @return Invisibly `NULL`. Called for its side effect of editing the active
#'   RStudio document.
#'
#' @export
ptr_wrap_placeholder_addin <- function() {
  ptr_addin_require_rstudio()
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    rlang::abort("Install the 'miniUI' package to use the ggpaintr placeholder addin.")
  }
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  selected <- sel$text
  ptr_addin_apply_result(ctx, sel, selected, ptr_addin_run_palette(selected))
}

# Pure text transform behind the gadget's "Wrap in app" button. Wraps the
# selected content in a braced block piped into ptr_app():
#
#   {
#     <content>
#   } |>
#     ptr_app()
#
# Each content line is indented two spaces. An empty selection produces the
# skeleton with the caret on the blank inner line. Returns the replacement text
# plus the caret as (row offset below the selection start, 1-based column).
ptr_addin_build_app_wrap <- function(selected) {
  if (is.null(selected)) selected <- ""
  content <- sub("\\n+$", "", selected)  # drop trailing newlines
  if (!nzchar(trimws(content))) {
    return(list(text = "{\n  \n} |> \n  ptr_app()", caret_row = 1L, caret_col = 3L))
  }
  lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
  body <- paste0("  ", lines, collapse = "\n")
  text <- paste0("{\n", body, "\n} |> \n  ptr_app()")
  n_rows <- length(strsplit(text, "\n", fixed = TRUE)[[1]])
  list(text = text, caret_row = n_rows - 1L, caret_col = nchar("  ptr_app()") + 1L)
}

# ---------------------------------------------------------------------------
# Shared addin plumbing: the RStudio guard and applying a picker result to the
# document (a placeholder pick or the "wrap in ptr_app()" action).
# ---------------------------------------------------------------------------

# Abort unless we are running inside RStudio (the addin edits the document via
# rstudioapi).
ptr_addin_require_rstudio <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    rlang::abort("This ggpaintr addin must be run inside RStudio.")
  }
}

# Apply a picker result to the active document. `result` is NULL (cancelled),
# list(action = "app"), or list(action = "placeholder", keyword = ...).
ptr_addin_apply_result <- function(ctx, sel, selected, result) {
  if (is.null(result)) return(invisible(NULL))
  start <- sel$range$start
  if (identical(result$action, "app")) {
    # braced block piped into ptr_app(); caret placed by (row offset, column)
    wrap <- ptr_addin_build_app_wrap(selected)
    rstudioapi::modifyRange(sel$range, wrap$text, id = ctx$id)
    pos <- rstudioapi::document_position(start[[1]] + wrap$caret_row, wrap$caret_col)
  } else {
    # single-line replacement; caret column = start column + offset in the text
    repl <- ptr_addin_build_replacement(result$keyword, selected)
    rstudioapi::modifyRange(sel$range, repl$text, id = ctx$id)
    pos <- rstudioapi::document_position(start[[1]], start[[2]] + repl$caret)
  }
  rstudioapi::setCursorPosition(pos, id = ctx$id)
  invisible(NULL)
}
