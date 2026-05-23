# ============================================================================
# build-feature-bank.R
#
# Generates dev/feature_bank/feature-bank.html -- the single coherent record
# of every ggpaintr feature and where it is covered.
#
# Sources of truth:
#   * NAMESPACE                                  -- exports (Public API)
#   * dev/scripts/feature-coverage-examples.R    -- canonical feature map +
#                                                   one-shot demo / headless run
#   * R/paintr-*.R                                -- defining locations
#   * tests/testthat/test-*.R                     -- automated coverage
#   * dev/audit/2026-05-15-headless/driver-headless.R -- supplemental headless
#                                                       audit driver
#
# Coverage detection: simple substring match (`grepl(fixed = TRUE)`) of every
# `search_term` against every coverage source. If a source contains at least
# one term, it is linked as a coverage artefact for that row.
#
# Re-run after adding tests / changing the API:
#   Rscript dev/feature_bank/build-feature-bank.R
# ============================================================================

stopifnot(
  file.exists("DESCRIPTION"),
  any(grepl("^Package: ggpaintr\\s*$", readLines("DESCRIPTION")))
)

# ----------------------------------------------------------------------------
# 1. Feature inventory.
# Each row: id (slug), name (display), category, defined_at, description,
# search_terms (character vector). Tests / examples are searched for any of
# these terms.
# ----------------------------------------------------------------------------

mk <- function(id, name, category, defined_at, description, search_terms) {
  list(id = id, name = name, category = category, defined_at = defined_at,
       description = description, search_terms = search_terms)
}

features <- list(

  # ---- Public API: launchers ---------------------------------------------
  mk("ptr_app", "ptr_app()", "Public API",
     "R/paintr-app.R:29",
     "Turn-key Shiny app from a ggplot-like formula.",
     c("ptr_app(", "ptr_app <-")),
  mk("ptr_app_bslib", "ptr_app_bslib()", "Public API",
     "R/paintr-app-bslib.R:41",
     "bslib-styled launcher (theme + title precedence).",
     c("ptr_app_bslib")),
  mk("ptr_app_grid", "ptr_app_grid()", "Public API",
     "R/paintr-app.R:462",
     "Grid of apps with optional shared widgets + draw-all button.",
     c("ptr_app_grid")),

  # ---- Public API: settings ----------------------------------------------
  mk("ptr_options", "ptr_options()", "Public API",
     "R/paintr-options.R:64",
     "Read / set the process-global options; round-trip via do.call().",
     c("ptr_options")),

  # ---- Public API: granular wiring ---------------------------------------
  mk("ptr_init_state", "ptr_init_state()", "Public API",
     "R/paintr-server.R:67",
     "Build the long-lived ptr_state list explicitly (instead of ptr_server()).",
     c("ptr_init_state")),
  mk("ptr_server", "ptr_server()", "Public API",
     "R/paintr-server.R:272",
     "Module-server convenience: init_state + setup_* + register_* in one call.",
     c("ptr_server(", "ptr_server <-")),

  # ---- Public API: Shiny module ------------------------------------------
  mk("ptr_module_ui", "ptr_module_ui()", "Public API",
     "R/paintr-app.R:328",
     "Embeddable Shiny module UI; takes id + formula + ui_text + checkbox_defaults + expr_check + css.",
     c("ptr_module_ui")),
  mk("ptr_module_server", "ptr_module_server()", "Public API",
     "R/paintr-app.R:423",
     "Embeddable Shiny module server; returns the state list (for ptr_extract_*).",
     c("ptr_module_server")),

  # ---- Public API: split-UI ----------------------------------------------
  mk("ptr_controls_ui", "ptr_controls_ui()", "Public API",
     "R/paintr-app.R:363",
     "Controls side of a split-UI embed (pair with ptr_outputs_ui()).",
     c("ptr_controls_ui")),
  mk("ptr_outputs_ui", "ptr_outputs_ui()", "Public API",
     "R/paintr-app.R:399",
     "Outputs side of a split-UI embed (plot / code / error panes).",
     c("ptr_outputs_ui")),

  # ---- Public API: register / extract ------------------------------------
  mk("ptr_register_plot", "ptr_register_plot()", "Public API",
     "R/paintr-server.R:1152",
     "Wire the runtime plot output (called from ptr_server / ptr_module_server).",
     c("ptr_register_plot")),
  mk("ptr_register_error", "ptr_register_error()", "Public API",
     "R/paintr-server.R:1169",
     "Wire the runtime error pane.",
     c("ptr_register_error")),
  mk("ptr_register_code", "ptr_register_code()", "Public API",
     "R/paintr-server.R:1215",
     "Wire the runtime generated-code pane.",
     c("ptr_register_code")),
  mk("ptr_extract_plot", "ptr_extract_plot()", "Public API",
     "R/paintr-server.R:1241",
     "Isolate-read the current runtime ggplot object (headless extraction).",
     c("ptr_extract_plot")),
  mk("ptr_extract_code", "ptr_extract_code()", "Public API",
     "R/paintr-server.R:1249",
     "Isolate-read the current generated code text.",
     c("ptr_extract_code")),
  mk("ptr_extract_error", "ptr_extract_error()", "Public API",
     "R/paintr-server.R:1245",
     "Isolate-read the current runtime error (NULL on happy path).",
     c("ptr_extract_error")),
  mk("ptr_gg_extra", "ptr_gg_extra()", "Public API",
     "R/paintr-server.R:1271",
     "Append a user-owned ggplot object to the runtime composition.",
     c("ptr_gg_extra")),

  # ---- Public API: placeholder registry ----------------------------------
  mk("ptr_define_placeholder_value", "ptr_define_placeholder_value()", "Public API",
     "R/paintr-registry.R:204",
     "Register a non-data-aware placeholder keyword (custom widget).",
     c("ptr_define_placeholder_value")),
  mk("ptr_define_placeholder_consumer", "ptr_define_placeholder_consumer()", "Public API",
     "R/paintr-registry.R:231",
     "Register a data-aware (column-fed) placeholder keyword.",
     c("ptr_define_placeholder_consumer")),
  mk("ptr_define_placeholder_source", "ptr_define_placeholder_source()", "Public API",
     "R/paintr-registry.R:261",
     "Register a data-source placeholder (produces a data frame).",
     c("ptr_define_placeholder_source")),
  mk("ptr_clear_placeholder", "ptr_clear_placeholder()", "Public API",
     "R/paintr-registry.R:46",
     "Unregister a user-defined placeholder; refuses built-ins.",
     c("ptr_clear_placeholder")),

  # ---- Public API: copy / ui_text ----------------------------------------
  mk("ptr_ui_text", "ptr_ui_text()", "Public API",
     "R/paintr-copy.R:590",
     "Validate + canonicalize a ui_text rules object (shell / upload / layer_checkbox / defaults / params / layers).",
     c("ptr_ui_text(", "ptr_ui_text <-")),
  mk("ptr_resolve_ui_text", "ptr_resolve_ui_text()", "Public API",
     "R/paintr-copy.R:706",
     "Look up resolved copy for a control or app element across the merge chain.",
     c("ptr_resolve_ui_text")),

  # ---- Public API: data helper -------------------------------------------
  mk("ptr_normalize_column_names", "ptr_normalize_column_names()", "Public API",
     "R/paintr-data.R:22",
     "Coerce non-syntactic column names to make.names()-clean spellings.",
     c("ptr_normalize_column_names")),

  # ---- Public API: LLM tooling -------------------------------------------
  mk("ptr_llm_primer", "ptr_llm_primer()", "Public API",
     "R/paintr-llm.R:20",
     "Return the overview prompt used to teach an LLM about ggpaintr.",
     c("ptr_llm_primer")),
  mk("ptr_llm_topics", "ptr_llm_topics()", "Public API",
     "R/paintr-llm.R:42",
     "List the named documentation topics available to LLMs.",
     c("ptr_llm_topics")),
  mk("ptr_llm_topic", "ptr_llm_topic()", "Public API",
     "R/paintr-llm.R:72",
     "Fetch the markdown body of one documentation topic.",
     c("ptr_llm_topic")),
  mk("ptr_llm_register", "ptr_llm_register()", "Public API",
     "R/paintr-llm.R:127",
     "Register a ggpaintr_docs tool with an ellmer Chat session.",
     c("ptr_llm_register")),

  # ---- Public API: S3 extension generic ----------------------------------
  mk("build_ui_for", "build_ui_for()", "Public API",
     "R/paintr-build-ui.R:33",
     "S3 generic dispatched per placeholder class to emit the widget; the extension hook for custom keywords.",
     c("build_ui_for")),

  # ---- Extension points: internal but documented -------------------------
  mk("ptr_translate", "ptr_translate()", "Extension point",
     "R/paintr-translate.R:12",
     "Parse a formula string into a ptr_root tree; entry point for headless flows.",
     c("ptr_translate")),
  mk("ptr_runtime_input_spec", "ptr_runtime_input_spec()", "Extension point",
     "R/paintr-input-spec.R:23",
     "Derive the runtime input spec (input_id / role / keyword / param_key / shared) from a tree.",
     c("ptr_runtime_input_spec")),
  mk("ptr_run_formula", "ptr_run_formula()", "Extension point",
     "R/paintr-headless.R:117",
     "One-shot headless eval of a formula with explicit input values; returns code + plot + ok flag.",
     c("ptr_run_formula")),
  mk("ptr_default_snapshot", "ptr_default_snapshot()", "Extension point",
     "R/paintr-headless.R:84",
     "Build an inputs-snapshot from a parsed tree + its input spec.",
     c("ptr_default_snapshot")),
  mk("ptr_exec_headless", "ptr_exec_headless()", "Extension point",
     "R/paintr-headless.R:22",
     "Evaluate a tree against a pre-built snapshot (no Shiny session).",
     c("ptr_exec_headless")),
  mk("ptr_setup_producer_inputs", "ptr_setup_producer_inputs()", "Extension point",
     "R/paintr-server.R",
     "Wire producer-side reactives (formula leaves whose values come from input$).",
     c("ptr_setup_producer_inputs")),
  mk("ptr_setup_pipelines", "ptr_setup_pipelines()", "Extension point",
     "R/paintr-server.R",
     "Wire pipeline-head data-source observers (e.g. upload mid-pipeline).",
     c("ptr_setup_pipelines")),
  mk("ptr_setup_consumer_uis", "ptr_setup_consumer_uis()", "Extension point",
     "R/paintr-server.R",
     "Wire data-aware consumer pickers (cols-fed widgets like var, custom consumers).",
     c("ptr_setup_consumer_uis")),

  # ---- Built-in placeholder keywords -------------------------------------
  mk("kw-var", "var", "Placeholder keyword",
     "R/paintr-builtins.R",
     "Column picker (data-aware consumer; cols-driven choices).",
     c("var(", "keyword = \"var\"", "var consumer", "var placeholder")),
  mk("kw-text", "text", "Placeholder keyword",
     "R/paintr-builtins.R",
     "Free-text input (value-placeholder; auto-quoted in code).",
     c("text(", "keyword = \"text\"", "text placeholder")),
  mk("kw-num", "num", "Placeholder keyword",
     "R/paintr-builtins.R",
     "Numeric input (value-placeholder).",
     c("num(", "keyword = \"num\"", "num placeholder")),
  mk("kw-expr", "expr", "Placeholder keyword",
     "R/paintr-builtins.R",
     "R-expression input gated by expr_check (denylist / allowlist).",
     c("expr(", "keyword = \"expr\"", "expr placeholder")),
  mk("kw-upload", "upload", "Placeholder keyword",
     "R/paintr-upload.R",
     "File-upload data source; usable as pipeline head or layer data argument.",
     c("upload(", "keyword = \"upload\"", "upload placeholder", "ptr_setup_pipelines")),

  # ---- ptr_app() arguments -----------------------------------------------
  mk("arg-envir", "envir =", "ptr_app() arg",
     "R/paintr-app.R:29",
     "Where to look up bare data symbols / user functions referenced in the formula.",
     c("envir = ", "envir =")),
  mk("arg-ui_text", "ui_text =", "ptr_app() arg",
     "R/paintr-app.R:29",
     "Override copy across shell / upload / layer_checkbox / defaults / params / layers.",
     c("ui_text = ", "ptr_ui_text(", "ptr_resolve_ui_text")),
  mk("arg-checkbox_defaults", "checkbox_defaults =", "ptr_app() arg",
     "R/paintr-app.R:29",
     "Per-layer initial enable / disable state for the include-this-layer checkbox.",
     c("checkbox_defaults")),
  mk("arg-expr_check", "expr_check =", "ptr_app() arg",
     "R/paintr-app.R:29",
     "Toggle the expression-input safety guard: TRUE / FALSE / list(deny_list=) / list(allow_list=).",
     c("expr_check")),
  mk("arg-safe_to_remove", "safe_to_remove =", "ptr_app() arg",
     "R/paintr-app.R:29",
     "Opt extra function names into the empty-placeholder cleanup pass.",
     c("safe_to_remove")),
  mk("arg-css", "css =", "ptr_app() arg",
     "R/paintr-app.R:29",
     "Path to a user CSS file injected as <link> via the head tag.",
     c("css = ", "css =")),

  # ---- ptr_app_bslib() extra args ----------------------------------------
  mk("arg-theme", "theme = (bslib)", "ptr_app_bslib() arg",
     "R/paintr-app-bslib.R:41",
     "bslib::bs_theme object applied to the app shell.",
     c("theme = ", "bs_theme")),
  mk("arg-title-bslib", "title = (bslib)", "ptr_app_bslib() arg",
     "R/paintr-app-bslib.R:41",
     "Top-bar title (overrides ui_text$shell$title for the bslib shell).",
     c("title = ", "bslib title")),

  # ---- ptr_app_grid() args -----------------------------------------------
  mk("arg-plots", "plots =", "ptr_app_grid() arg",
     "R/paintr-app.R:462",
     "List of formulas to lay out in the grid.",
     c("plots = ", "ptr_app_grid")),
  mk("arg-shared_ui", "shared_ui =", "ptr_app_grid() arg",
     "R/paintr-app.R:462",
     "Shared widget(s) above the grid; bound via shared= ids on the formulas.",
     c("shared_ui")),
  mk("arg-draw_all_label", "draw_all_label =", "ptr_app_grid() arg",
     "R/paintr-app.R:462",
     "Label for the draw-all button that fires every cell at once.",
     c("draw_all_label", "draw_all_button")),
  mk("arg-title-grid", "title = (grid)", "ptr_app_grid() arg",
     "R/paintr-app.R:462",
     "Grid-level title displayed above the cell row.",
     c("title = ", "ptr_app_grid")),

  # ---- Behaviors / contracts ---------------------------------------------
  mk("b-shared", "shared = '<id>' annotation", "Behavior",
     "R/paintr-shared.R",
     "Bind multiple placeholders (any kind: var / num / text / expr / custom) to one synced widget.",
     c("shared = ", "shared_bindings", "shared_resolutions", "ptr_validate_shared_bindings")),
  mk("b-formula-transforms", "Formula-level transforms wrapping var", "Behavior",
     "R/paintr-substitute.R",
     "var inside an enclosing expression (var + 1, log(var)) still resolves as a column consumer.",
     c("var + 1", "log(var)", "log\\(var\\)", "formula-level transforms")),
  mk("b-empty-cleanup", "Empty-placeholder cleanup of curated calls", "Behavior",
     "R/paintr-prune.R",
     "Calls in the curated removable set (labs, facet_wrap, ...) drop when all placeholder args are empty.",
     c("prune", "default_drop_when_empty", "default_safe_to_remove")),
  mk("b-safe-to-remove", "safe_to_remove= drop semantics", "Behavior",
     "R/paintr-utils.R:161",
     "User-supplied function names are appended to the curated removable set.",
     c("safe_to_remove")),
  mk("b-expr-deny", "expr_check denylist (default)", "Behavior",
     "R/paintr-safety.R",
     "Built-in denylist (~151 entries) blocks risky symbols at translate time.",
     c("denylist", "deny_list", "is not allowed")),
  mk("b-expr-false", "expr_check = FALSE bypass", "Behavior",
     "R/paintr-translate.R:12",
     "Bypass the denylist entirely (only for trusted input).",
     c("expr_check = FALSE", "expr_check=FALSE")),
  mk("b-expr-deny-list", "expr_check = list(deny_list = ...)", "Behavior",
     "R/paintr-safety.R",
     "Swap in a user-supplied denylist for the default.",
     c("deny_list", "list(deny_list")),
  mk("b-expr-allow-list", "expr_check = list(allow_list = ...)", "Behavior",
     "R/paintr-safety.R",
     "Translate-time allowlist (only listed symbols accepted).",
     c("allow_list", "list(allow_list")),
  mk("b-copy-interpolate", "copy_defaults {param} interpolation", "Behavior",
     "R/paintr-copy.R",
     "Tokens like {param} in copy_defaults expand to the bound aes() argument name.",
     c("copy_defaults", "{param}", "ptr_interpolate_ui_text")),
  mk("b-copy-precedence", "copy precedence (defaults < params < layers)", "Behavior",
     "R/paintr-copy.R:706",
     "Resolved copy merges defaults, then param overrides, then layer overrides.",
     c("ptr_deep_merge_ui_text", "ptr_resolve_ui_text", "copy precedence")),
  mk("b-copy-leaves", "ui_text leaf fields (label / help / placeholder / empty_text)", "Behavior",
     "R/paintr-copy.R:154",
     "Each component resolves to a list of these four optional leaf strings.",
     c("ptr_ui_text_leaf_fields", "empty_text", "placeholder")),
  mk("b-copy-unnamed", "ui_text __unnamed__ positional key", "Behavior",
     "R/paintr-copy.R",
     "Layers with positional placeholder args (e.g. facet_wrap(~ Species)) register copy under __unnamed__.",
     c("__unnamed__")),
  mk("b-layer-data-literal", "Layer-level data = <local frame> literal", "Behavior",
     "R/paintr-resolve.R",
     "data = some_df on a layer resolves the literal against envir at runtime.",
     c("data_arg", "is_bare_data_source_layer", "inject_resolved_data")),
  mk("b-pipeline-head", "Pipeline-head data source (upload |> ... |> ggplot)", "Behavior",
     "R/paintr-server.R",
     "An upload or custom source at the head of a pipeline binds into eval_env so downstream stages see it.",
     c("pipeline_source", "ptr_setup_pipelines", "resolved_sources")),
  mk("b-stage-enable", "Per-stage Data sub-tab + enable / disable", "Behavior",
     "R/paintr-server.R",
     "Each pipeline stage carrying placeholders gets its own Data sub-tab + an enable toggle.",
     c("stage_enabled", "data_subtab", "stage_id")),
  mk("b-layer-enable", "Per-geom-layer enable / disable checkbox", "Behavior",
     "R/paintr-disable.R",
     "Each layer renders a checkbox; unchecked layers are pruned with a visual disabled-state class.",
     c("layer_checkbox", ".ptr-layer-disabled", "ptr-layer-disabled")),
  mk("b-validate-input", "Custom-placeholder validate_input hook", "Behavior",
     "R/paintr-registry.R:231",
     "Consumers + sources can supply a validate_input(value, upstream_cols, ...) hook for runtime feedback.",
     c("validate_input", "ptr_validate_input")),
  mk("b-registry-warn", "Process-global registry duplicate-keyword warning", "Behavior",
     "R/paintr-registry.R",
     "Re-registering an existing keyword warns and overwrites.",
     c("duplicate", "Overwriting placeholder")),
  mk("b-custom-ids", "Custom-ids / module ns prefix", "Behavior",
     "R/paintr-app.R:328",
     "ptr_module_ui(id = ...) prefixes every input id with the supplied namespace.",
     c("ns(", "shiny::NS", "module ns", "custom_id")),
  mk("b-css-cascade", "User CSS cascade via css= path", "Behavior",
     "R/paintr-app.R",
     "css = path injects a <link> tag; bad path / non-css extension surfaces an inline error.",
     c("css = ", "ex_css", "user-css")),
  mk("b-runtime-error", "Inline error pane (missing object / facet-missing-var / bad upload)", "Behavior",
     "R/paintr-eval.R",
     "Runtime evaluation errors surface in the error pane with the verbatim message (e.g. unsupported-extension wording).",
     c("ptr_register_error", "object not found", "unsupported-extension", "bad-extension"))
)

# ----------------------------------------------------------------------------
# 2. Coverage sources -- where we look for evidence.
# Each entry: id, label (display + sort-key), file pattern (glob).
# ----------------------------------------------------------------------------

source_groups <- list(
  list(id = "testthat",
       label = "tests/testthat",
       glob  = "tests/testthat/test-*.R",
       link_base = "../../"),
  list(id = "examples",
       label = "feature-coverage-examples.R",
       glob  = "dev/scripts/feature-coverage-examples.R",
       link_base = "../../"),
  list(id = "driver",
       label = "driver-headless.R",
       glob  = "dev/audit/2026-05-15-headless/driver-headless.R",
       link_base = "../../")
)

# ----------------------------------------------------------------------------
# Coverage detection: PARSE the source, deparse code-only (comments dropped).
# For testthat files, further restrict to text inside `test_that(...)` bodies
# so that mentioning a feature in a top-level setup helper or comment does
# NOT count as coverage. A feature counts as covered by a file only if at
# least one of its search_terms appears in that file's in-test code.
# ----------------------------------------------------------------------------

is_testthat_file <- function(path) {
  grepl("/tests/testthat/test-[^/]+\\.R$", path)
}

# Deparse every top-level expression. base::parse() strips comments, so the
# resulting string contains code only.
parse_code_only <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE),
                    error = function(e) NULL)
  if (is.null(exprs)) {
    # Fall back to a comment-stripped line scan so non-R sources (e.g. files
    # we may later add) still match. For R sources this branch is unused.
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) "")
    lines <- sub("(^|[^\\\\])#.*$", "\\1", lines)   # drop trailing comments
    return(paste(lines, collapse = "\n"))
  }
  paste(vapply(seq_along(exprs),
               function(i) paste(deparse(exprs[[i]], width.cutoff = 500L),
                                 collapse = "\n"),
               character(1)),
        collapse = "\n")
}

# Collect every `test_that(desc, { body })` body text from a parsed file.
# A feature only counts as covered by a testthat file when at least one term
# appears inside such a body.
extract_test_bodies <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE),
                    error = function(e) NULL)
  if (is.null(exprs)) return("")
  bodies <- character()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (is.call(e) && length(e) >= 3L &&
        identical(e[[1]], as.name("test_that"))) {
      bodies <- c(bodies, paste(deparse(e[[3]], width.cutoff = 500L),
                                collapse = "\n"))
    }
  }
  paste(bodies, collapse = "\n")
}

# Cache the *search text* per file (parsed + restricted to test_that for
# testthat sources, full code body otherwise). Built once, reused for every
# feature row.
read_search_text <- local({
  cache <- new.env(parent = emptyenv())
  function(path) {
    if (!is.null(cache[[path]])) return(cache[[path]])
    txt <- if (is_testthat_file(path)) extract_test_bodies(path)
           else                         parse_code_only(path)
    cache[[path]] <- txt
    txt
  }
})

source_files <- lapply(source_groups, function(g) Sys.glob(g$glob))

# ----------------------------------------------------------------------------
# 3. For each feature, find files in each source group that mention any term.
# ----------------------------------------------------------------------------

match_files <- function(files, terms) {
  hits <- character()
  for (f in files) {
    txt <- read_search_text(f)
    if (!nzchar(txt)) next
    if (any(vapply(terms, function(t) grepl(t, txt, fixed = TRUE),
                   logical(1)))) {
      hits <- c(hits, f)
    }
  }
  hits
}

rows <- lapply(features, function(feat) {
  cov <- vector("list", length(source_groups))
  names(cov) <- vapply(source_groups, function(g) g$id, character(1))
  for (i in seq_along(source_groups)) {
    cov[[i]] <- match_files(source_files[[i]], feat$search_terms)
  }
  feat$coverage <- cov
  feat$has_testthat <- length(cov$testthat) > 0L
  feat$has_demo     <- length(cov$examples) > 0L || length(cov$driver) > 0L
  # Three-level status:
  #   "tested"  - at least one tests/testthat/test-*.R hit
  #   "demo"    - canonical example or headless driver only (weaker; not unit-tested)
  #   "none"    - no source contains any search term
  feat$status <- if (feat$has_testthat) "tested"
                 else if (feat$has_demo) "demo"
                 else                     "none"
  feat
})

# Sanity counters
tested_n <- sum(vapply(rows, function(r) r$status == "tested", logical(1)))
demo_n   <- sum(vapply(rows, function(r) r$status == "demo",   logical(1)))
none_n   <- sum(vapply(rows, function(r) r$status == "none",   logical(1)))
covered_n <- tested_n + demo_n
cat(sprintf("Feature bank: %d rows -- %d tested, %d demo-only, %d uncovered.\n",
            length(rows), tested_n, demo_n, none_n))

# ----------------------------------------------------------------------------
# 4. Render HTML.
# ----------------------------------------------------------------------------

esc <- function(x) {
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

link_html <- function(path, link_base = "../../") {
  href <- paste0(link_base, path)
  label <- basename(path)
  sprintf('<a href="%s">%s</a>', esc(href), esc(label))
}

render_coverage_cell <- function(feat) {
  parts <- character()
  for (g in source_groups) {
    files <- feat$coverage[[g$id]]
    if (!length(files)) next
    links <- vapply(files, link_html, character(1),
                    link_base = g$link_base)
    parts <- c(parts, sprintf(
      '<div class="src-grp"><span class="src-label">%s</span>%s</div>',
      esc(g$label), paste(links, collapse = ", ")
    ))
  }
  if (!length(parts)) {
    return('<span class="badge b-warn">no automated / canonical coverage</span>')
  }
  paste(parts, collapse = "\n")
}

category_order <- c("Public API", "Extension point", "Placeholder keyword",
                    "ptr_app() arg", "ptr_app_bslib() arg",
                    "ptr_app_grid() arg", "Behavior")

# Stable sort by category then by name.
ord <- order(
  match(vapply(rows, `[[`, character(1), "category"), category_order),
  vapply(rows, `[[`, character(1), "name")
)
rows <- rows[ord]

status_badge <- function(s) {
  switch(s,
    tested = '<span class="badge b-ok">tested</span>',
    demo   = '<span class="badge b-warn">demo only</span>',
    none   = '<span class="badge b-danger">none</span>'
  )
}

tbody_rows <- vapply(rows, function(feat) {
  sprintf(
    paste0(
      '<tr data-category="%s" data-status="%s">',
      '<td class="col-name"><code>%s</code></td>',
      '<td class="col-cat">%s</td>',
      '<td class="col-where"><code>%s</code></td>',
      '<td class="col-desc">%s</td>',
      '<td class="col-status">%s</td>',
      '<td class="col-cov">%s</td>',
      '</tr>'
    ),
    esc(feat$category),
    esc(feat$status),
    esc(feat$name),
    esc(feat$category),
    esc(feat$defined_at),
    esc(feat$description),
    status_badge(feat$status),
    render_coverage_cell(feat)
  )
}, character(1))

today <- format(Sys.Date())
head_sha <- tryCatch(
  system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
  error = function(e) "(unknown)"
)

head_tpl <- '<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>ggpaintr feature bank with coverage status</title>
<link rel="stylesheet" href="../assets/doc.css">
</head>
<body>
<main>

<h1>ggpaintr feature bank with coverage status</h1>
<p class="meta">
  Single source of truth for every documented ggpaintr feature and where it is exercised.
  Generated: <code>__TODAY__</code>. Branch HEAD: <code>__SHA__</code>.<br>
  Re-generate with <code>Rscript dev/feature_bank/build-feature-bank.R</code>.
</p>

<div class="summary-counts">
  <div><strong>__TOTAL__</strong><span>total features</span></div>
  <div><strong>__TESTED__</strong><span>tested (testthat)</span></div>
  <div><strong>__DEMO__</strong><span>demo only</span></div>
  <div><strong>__UNCOVERED__</strong><span>uncovered</span></div>
</div>

<h2>How coverage is detected</h2>
<p>
  Each row carries a set of search terms (function names, argument names, behavior keywords).
  Each source file is parsed with <code>base::parse()</code> and the resulting expressions
  are <code>deparse</code>d back to text -- this drops every comment, so a feature mentioned
  only in a <code>#</code>-comment does <em>not</em> count. For <code>tests/testthat/test-*.R</code>
  files the search is further restricted to text inside <code>test_that(...)</code> bodies,
  so a name dropped in a top-level helper or setup block does not by itself satisfy a row.
  A source counts as evidence when at least one term appears literally in that restricted text.
  Sources searched: <code>tests/testthat/test-*.R</code> (automated; in-test bodies only),
  <code>dev/scripts/feature-coverage-examples.R</code> (canonical demo + headless source-eval; full code),
  and <code>dev/audit/2026-05-15-headless/driver-headless.R</code> (supplemental headless audit; full code).
  Status badges are three-level: <span class="badge b-ok">tested</span> when at least one
  <code>tests/testthat/test-*.R</code> file hits, <span class="badge b-warn">demo only</span>
  when only the canonical example or the headless driver hits (the row is exercised but has no
  dedicated unit test), <span class="badge b-danger">none</span> when no source contains any
  search term -- either the feature genuinely lacks coverage, or its search terms need refinement in
  <code>dev/feature_bank/build-feature-bank.R</code>.
</p>

<div class="controls">
  <label>Filter:</label>
  <input id="qfilter" type="text" placeholder="text search (name / description)" size="32" />
  <label>Category:</label>
  <select id="catfilter">
    <option value="">(all)</option>
__OPTIONS__
  </select>
  <label>Status:</label>
  <select id="covfilter">
    <option value="">(all)</option>
    <option value="tested">tested (testthat)</option>
    <option value="demo">demo only</option>
    <option value="none">uncovered</option>
  </select>
  <span id="visible-count" class="badge b-info"></span>
</div>

<table id="ftable">
<thead>
<tr>
  <th class="sortable" data-key="name">Feature</th>
  <th class="sortable" data-key="cat">Category</th>
  <th class="sortable" data-key="where">Where defined</th>
  <th>Description</th>
  <th class="sortable" data-key="status">Status</th>
  <th>Coverage</th>
</tr>
</thead>
<tbody>
__TBODY__
</tbody>
</table>

<h2>Companion artefacts</h2>
<ul>
  <li><a href="../scripts/feature-coverage-examples.R"><code>dev/scripts/feature-coverage-examples.R</code></a> &mdash; canonical demo that, between Example 1 / 2 / 3, exercises every documented feature; the headless half source-evaluates on <code>Rscript</code>.</li>
  <li><a href="../audit/2026-05-15-headless/driver-headless.R"><code>dev/audit/2026-05-15-headless/driver-headless.R</code></a> &mdash; supplemental driver closing the 15 headless / API-surface gap items on commit <code>3152f99</code>. Run logs alongside: <code>source-{stdout,stderr}.log</code>, <code>driver-{stdout,stderr}.log</code>.</li>
  <li><a href="../../tests/manual/edge_cases/"><code>tests/manual/edge_cases/</code></a> &mdash; manual edge-case launchers + fixtures (not counted as automated coverage; useful for spot-checks).</li>
</ul>

<script>
(function () {
  var table = document.getElementById("ftable");
  var tbody = table.tBodies[0];
  var rows  = Array.prototype.slice.call(tbody.rows);
  var qIn   = document.getElementById("qfilter");
  var catIn = document.getElementById("catfilter");
  var covIn = document.getElementById("covfilter");
  var cnt   = document.getElementById("visible-count");

  function apply() {
    var q   = qIn.value.toLowerCase().trim();
    var cat = catIn.value;
    var cov = covIn.value;
    var n   = 0;
    rows.forEach(function (tr) {
      var matchQ   = !q   || tr.textContent.toLowerCase().indexOf(q) !== -1;
      var matchCat = !cat || tr.dataset.category === cat;
      var matchCov = !cov || tr.dataset.status === cov;
      var show = matchQ && matchCat && matchCov;
      tr.classList.toggle("row-hide", !show);
      if (show) n++;
    });
    cnt.textContent = n + " of " + rows.length + " visible";
  }
  qIn.addEventListener("input",  apply);
  catIn.addEventListener("change", apply);
  covIn.addEventListener("change", apply);

  // Click-to-sort
  Array.prototype.forEach.call(table.querySelectorAll("th.sortable"), function (th, idx) {
    var dir = 1;
    var col = Array.prototype.indexOf.call(th.parentNode.children, th);
    th.addEventListener("click", function () {
      dir = -dir;
      rows.sort(function (a, b) {
        var av = a.children[col].textContent.trim().toLowerCase();
        var bv = b.children[col].textContent.trim().toLowerCase();
        return av < bv ? -dir : av > bv ? dir : 0;
      });
      rows.forEach(function (r) { tbody.appendChild(r); });
    });
  });

  apply();
})();
</script>

</main>
</body>
</html>
'

# Token replacement: avoids R sprintf's 8192-char format-length limit and
# any back-reference interpretation issues in the replacement text.
literal_replace <- function(text, needle, replacement) {
  pieces <- strsplit(text, needle, fixed = TRUE)[[1]]
  if (length(pieces) <= 1L) return(text)
  paste(pieces, collapse = replacement)
}

options_str <- paste(
  sprintf('    <option value="%s">%s</option>',
          esc(category_order), esc(category_order)),
  collapse = "\n"
)

html <- head_tpl
html <- literal_replace(html, "__TODAY__",     esc(today))
html <- literal_replace(html, "__SHA__",       esc(head_sha))
html <- literal_replace(html, "__TOTAL__",     as.character(length(rows)))
html <- literal_replace(html, "__TESTED__",    as.character(tested_n))
html <- literal_replace(html, "__DEMO__",      as.character(demo_n))
html <- literal_replace(html, "__UNCOVERED__", as.character(none_n))
html <- literal_replace(html, "__OPTIONS__",   options_str)
html <- literal_replace(html, "__TBODY__",     paste(tbody_rows, collapse = "\n"))

out_path <- "dev/feature_bank/feature-bank.html"
writeLines(html, out_path)
cat("Wrote", out_path, "\n")
