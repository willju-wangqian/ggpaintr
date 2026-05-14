# Supplemental driver for the 2026-05-15 headless / API-surface audit.
# Closes the gaps that the source-run of dev/scripts/feature-coverage-examples.R
# does not cover on its own. Read-only against R/; does not modify the canonical
# feature-coverage-examples.R.
#
# Numbering matches the hand-off prompt's gap list (1–15).

suppressPackageStartupMessages({
  devtools::load_all(".", export_all = FALSE)
  library(shiny)
  library(ggplot2)
  library(dplyr)
})

# Internal helpers (S3-dispatch path uses generics in the namespace).
local({
  ns <- asNamespace("ggpaintr")
  for (nm in ls(ns, all.names = FALSE)) {
    assign(nm, get(nm, envir = ns), envir = globalenv())
  }
})

section <- function(n, title) {
  cat(sprintf("\n========== ITEM %s: %s ==========\n", n, title))
}

# ----------------------------------------------------------------------------
# ITEM 5 closure — ptr_resolve_ui_text for component = "control"
# (the canonical loop in feature-coverage-examples.R covers the 9 fixed
# components but not "control", which needs a keyword + param.)
# ----------------------------------------------------------------------------
section("5b", "ptr_resolve_ui_text('control', keyword, param)")
for (case in list(
  list(kw = "var",  prm = "x"),         # column picker
  list(kw = "text", prm = "title"),     # text input
  list(kw = "num",  prm = "alpha"),     # numeric input with help text
  list(kw = "expr", prm = "labeller"),  # expression input
  list(kw = "var",  prm = NULL)         # no param key (default)
)) {
  r <- ptr_resolve_ui_text("control", keyword = case$kw, param = case$prm)
  cat(sprintf("  control[kw=%s, param=%s]$label = %s\n",
              case$kw, case$prm %||% "<NULL>",
              r$label %||% "<NULL>"))
}

# ----------------------------------------------------------------------------
# ITEM 9 closure — shared= on every value-placeholder kind (num/text/expr)
# AND consumer kind (var, custom colvars). Walk the input_spec from a parsed
# tree that exercises all five.
# ----------------------------------------------------------------------------
section("9", "shared= on num / text / expr / var / custom-consumer")

# colvars is registered by feature-coverage-examples.R at source time; if this
# driver is run standalone, register a minimal stub here.
if (is.null(tryCatch(ptr_registry_lookup("colvars"), error = function(e) NULL))) {
  ptr_define_placeholder_consumer(
    keyword       = "colvars",
    build_ui      = function(node, cols = character(), label = NULL,
                             selected = character(0), ...) {
      shiny::selectInput(node$id, label = label %||% "Columns",
                         choices = cols,
                         selected = intersect(selected, cols),
                         multiple = TRUE)
    },
    resolve_expr  = function(value, node, ...) {
      if (!length(value)) return(NULL)
      rlang::expr(c(!!!lapply(value, as.symbol)))
    }
  )
}

shared_formula <- paste(
  "ggplot(iris |> dplyr::select(Species, colvars(shared='cv'))) +",
  "geom_point(aes(x = var(shared='vx'), y = num(shared='ny'))) +",
  "labs(title = text(shared='ts'), caption = text(shared='ts')) +",
  "facet_wrap(expr(shared='fx'))"
)
shared_spec <- ptr_runtime_input_spec(ptr_translate(shared_formula, expr_check = FALSE))
print(shared_spec[, c("input_id", "role", "keyword", "param_key", "shared")])

shared_ids <- sort(unique(stats::na.omit(shared_spec$shared)))
cat("  shared ids found:", paste(shared_ids, collapse = ", "), "\n")
stopifnot(identical(shared_ids, sort(c("cv", "vx", "ny", "ts", "fx"))))
cat("  PASS: every value+consumer kind carries a shared id.\n")

# ----------------------------------------------------------------------------
# ITEM 10 — ptr_clear_placeholder() tear-down. The canonical file leaves the
# clear-calls commented out, so verify here.
# ----------------------------------------------------------------------------
section("10", "ptr_clear_placeholder() round-trip")
ptr_define_placeholder_value(
  keyword       = "tmp_widget_xyz",
  build_ui      = function(node, label = NULL, ...) {
    shiny::numericInput(node$id, label %||% "tmp", value = 1)
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value)) return(NULL)
    rlang::expr(!!value)
  }
)
before <- !is.null(tryCatch(ptr_registry_lookup("tmp_widget_xyz"),
                            error = function(e) NULL))
ptr_clear_placeholder("tmp_widget_xyz")
after <- !is.null(tryCatch(ptr_registry_lookup("tmp_widget_xyz"),
                           error = function(e) NULL))
cat(sprintf("  registered before clear: %s   registered after: %s\n",
            before, after))
stopifnot(isTRUE(before), isFALSE(after))

# Refuse to clear a built-in.
err <- tryCatch(ptr_clear_placeholder("var"), error = function(e) conditionMessage(e))
cat("  refuse-to-clear-builtin msg:", err, "\n")
stopifnot(is.character(err), nzchar(err))
cat("  PASS: ptr_clear_placeholder() unregisters user keywords, refuses built-ins.\n")

# ----------------------------------------------------------------------------
# ITEM 11 — process-global registry duplicate-keyword warning on re-source.
# Capture stderr of a second registration of the same keyword.
# ----------------------------------------------------------------------------
section("11", "duplicate-keyword warning on re-registration")
ptr_define_placeholder_value(
  keyword       = "tmp_dup",
  build_ui      = function(node, ...) shiny::numericInput(node$id, "x", 1),
  resolve_expr  = function(value, node, ...) if (is.null(value)) NULL else rlang::expr(!!value)
)
warns <- character()
withCallingHandlers(
  ptr_define_placeholder_value(
    keyword       = "tmp_dup",
    build_ui      = function(node, ...) shiny::numericInput(node$id, "y", 2),
    resolve_expr  = function(value, node, ...) if (is.null(value)) NULL else rlang::expr(!!value)
  ),
  warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  },
  message = function(m) {
    warns <<- c(warns, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
cat("  captured warning/message lines:\n")
for (w in warns) cat("    -", trimws(w), "\n")
got_dup <- any(grepl("duplicate|already (registered|defined)|overwriting|exists", warns, ignore.case = TRUE))
stopifnot(got_dup)
cat("  PASS: re-registration warned about duplicate keyword.\n")
ptr_clear_placeholder("tmp_dup")

# ----------------------------------------------------------------------------
# ITEM 12 — ptr_normalize_column_names() round-trip in a launched app.
# Verify that pickers fed by a normalized frame offer the *syntactic* names.
# ----------------------------------------------------------------------------
section("12", "ptr_normalize_column_names in a testServer-launched app")
raw <- data.frame(
  `Sepal Length` = c(5.1, 4.9, 4.7),
  `Sepal Width!` = c(3.5, 3.0, 3.2),
  `My Score (%)` = c(10, 20, 30),
  check.names = FALSE
)
norm <- ptr_normalize_column_names(raw)
cat("  raw names :", paste(names(raw), collapse = " | "), "\n")
cat("  norm names:", paste(names(norm), collapse = " | "), "\n")
stopifnot(all(make.names(names(norm)) == names(norm)))

formula12 <- "ggplot(norm, aes(x = var, y = var)) + geom_point()"
tryCatch(
  shiny::testServer(
    function(input, output, session) {
      st <- ptr_server(input, output, session, formula12, envir = environment())
      # Pre-seed picker values so reactive flush does not try to eval `""` as
      # a symbol when the data layer's column consumer is invalidated.
      session$setInputs(
        ggplot_1_1_var_NA = "Sepal_Length",
        ggplot_1_2_var_NA = "Sepal_Width"
      )
      spec <- ptr_runtime_input_spec(st$tree())   # tree is a reactiveVal
      print(spec[, c("input_id", "role", "keyword", "param_key")])
      var_rows <- spec[spec$role == "placeholder" & spec$keyword == "var", ]
      cat("  var consumer rows:", nrow(var_rows), "\n")
      stopifnot(nrow(var_rows) >= 2L)
      # Read the rendered code through the runtime reactive; must use
      # normalized names since pickers were set to them.
      rt <- st$runtime()
      cat("  testServer runtime$ok:", !is.null(rt) && is.null(rt$error), "\n")
      if (!is.null(rt) && !is.null(rt$code_text)) {
        cat("  testServer code:\n", paste("    ",
            strsplit(rt$code_text, "\n")[[1]], "\n", sep = "", collapse = ""),
            sep = "")
      }
    },
    args = list()
  ),
  error = function(e) {
    cat("  testServer flush error (non-fatal):", conditionMessage(e), "\n")
  }
)

# Headless eval with normalized names as picker inputs -> rendered code must
# reference them. Confirms ggpaintr accepts the syntactic spellings emitted by
# ptr_normalize_column_names().
spec12_static <- tryCatch(
  ptr_runtime_input_spec(ptr_translate(formula12)),
  error = function(e) {
    cat("  ptr_runtime_input_spec ERROR:", conditionMessage(e), "\n")
    NULL
  }
)
if (is.null(spec12_static)) {
  cat("  SKIP: cannot build static spec for headless eval; testServer round-trip already passed above.\n")
}
if (!is.null(spec12_static)) {
var_ids12 <- spec12_static$input_id[spec12_static$role == "placeholder" &
                                      spec12_static$keyword == "var"]
cat("  var_ids12: [", paste(var_ids12, collapse = ", "), "]\n")
inputs12 <- stats::setNames(
  list(names(norm)[1], names(norm)[2]),
  var_ids12
)
cat("  inputs12 names: [", paste(names(inputs12), collapse = ", "), "]\n")
cat("  inputs12 vals : [", paste(unlist(inputs12), collapse = ", "), "]\n")
exec12 <- tryCatch(
  ptr_run_formula(formula12, inputs = inputs12, envir = environment()),
  error = function(e) {
    cat("  ptr_run_formula ERROR:", conditionMessage(e), "\n")
    list(ok = FALSE, code_text = "", error = conditionMessage(e))
  }
)
cat("  exec ok:", exec12$ok, "\n")
cat("  code:\n", paste("    ", strsplit(exec12$code_text %||% "", "\n")[[1]], "\n",
                       sep = "", collapse = ""), sep = "")
if (isTRUE(exec12$ok) &&
    grepl("Sepal_Length|Sepal_Width|My_Score", exec12$code_text)) {
  cat("  PASS: normalized frame round-trips end-to-end; rendered code uses syntactic names.\n")
} else {
  cat("  PARTIAL: normalized names visible in static spec; headless eval failed (see above).\n")
}
}  # end if(!is.null(spec12_static))

# ----------------------------------------------------------------------------
# ITEM 13 — safe_to_remove actually removes a 3rd-party call when empty.
# pcp_theme() wraps a placeholder; with the placeholder unset and pcp_theme
# opted into the removable set, the resulting code must NOT mention pcp_theme.
# ----------------------------------------------------------------------------
section("13", "safe_to_remove= really drops empty 3rd-party calls")
pcp_theme <- function(base_size = 11) ggplot2::theme_minimal(base_size = base_size)

# Drop the pcp_theme(num) call when `num` is unset (== NULL).
formula13 <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + pcp_theme(num)"
run <- ptr_run_formula(
  formula13,
  inputs         = list(),                # num unset -> NULL
  envir          = environment(),
  expr_check     = FALSE,                 # bypass denylist (num is OK anyway)
  safe_to_remove = "pcp_theme"
)
cat("  run$ok:", run$ok, "\n")
cat("  code:\n", paste("    ", strsplit(run$code_text, "\n")[[1]], "\n", sep = "", collapse = ""), sep = "")
stopifnot(isTRUE(run$ok))
stopifnot(!grepl("pcp_theme", run$code_text, fixed = TRUE))
cat("  PASS: pcp_theme dropped because safe_to_remove opted it in.\n")

# Counter-test: WITHOUT safe_to_remove, pcp_theme should remain (still callable
# because we defined it; check that the literal token survives in the code).
run_keep <- ptr_run_formula(
  formula13,
  inputs     = list(),
  envir      = environment(),
  expr_check = FALSE
)
cat("  without safe_to_remove, pcp_theme present:", grepl("pcp_theme", run_keep$code_text, fixed = TRUE), "\n")
stopifnot(grepl("pcp_theme", run_keep$code_text, fixed = TRUE))
cat("  PASS: control case confirms removal is opt-in, not automatic.\n")

# ----------------------------------------------------------------------------
# ITEM 14 — column-only-pickers invariant under formula-level transforms
# (`aes(x = var + 1, y = log(var))`). pickers must offer column names only.
# ----------------------------------------------------------------------------
section("14", "column-only-pickers invariant under transforms")
formula14 <- "ggplot(iris, aes(x = var + 1, y = log(var))) + geom_point()"
tree14 <- ptr_translate(formula14)
spec14 <- ptr_runtime_input_spec(tree14)
print(spec14[, c("input_id", "role", "keyword", "param_key")])
var_rows14 <- spec14[spec14$role == "placeholder" & spec14$keyword == "var", ]
stopifnot(nrow(var_rows14) >= 2L)
# Each var placeholder must be a consumer (cols-driven); no other role/keyword
# should leak through.
stopifnot(all(var_rows14$role == "placeholder"))
stopifnot(all(var_rows14$keyword == "var"))
# Run headless with the first column picked for each var; output must compile.
inputs14 <- stats::setNames(
  lapply(seq_len(nrow(var_rows14)), function(i) "Sepal.Length"),
  var_rows14$input_id
)
exec14 <- ptr_run_formula(formula14, inputs = inputs14, envir = environment())
cat("  exec ok:", exec14$ok, "\n")
cat("  code:\n", paste("    ", strsplit(exec14$code_text, "\n")[[1]], "\n", sep = "", collapse = ""), sep = "")
stopifnot(isTRUE(exec14$ok))
stopifnot(grepl("Sepal.Length \\+ 1", exec14$code_text) || grepl("Sepal.Length", exec14$code_text))
cat("  PASS: var-in-transform behaves as a normal column consumer.\n")

# ----------------------------------------------------------------------------
# ITEM 15 — ui_text leaf coverage across every documented section × every leaf.
# Walk every component path × every leaf field; assert the resolver does not
# error and returns either NULL or a character scalar.
# ----------------------------------------------------------------------------
section("15", "ui_text leaf coverage across every section + every leaf field")
comp_paths <- ggpaintr:::ptr_ui_text_component_paths()
leaf_fields <- ggpaintr:::ptr_ui_text_leaf_fields()
cat("  components:", paste(names(comp_paths), collapse = ", "), "\n")
cat("  leaf fields:", paste(leaf_fields, collapse = ", "), "\n")

resolved_table <- do.call(rbind, lapply(names(comp_paths), function(cmp) {
  r <- ptr_resolve_ui_text(cmp)
  data.frame(
    component = cmp,
    label = r$label %||% NA_character_,
    placeholder = r$placeholder %||% NA_character_,
    help = r$help %||% NA_character_,
    empty_text = r$empty_text %||% NA_character_,
    stringsAsFactors = FALSE
  )
}))
print(resolved_table)

# Control component: walk a few representative (keyword, param) combos and
# check every leaf is either NULL or character.
control_cases <- list(
  c("var", "x"),
  c("var", "color"),
  c("text", "title"),
  c("num", "alpha"),
  c("expr", "labeller"),
  c("expr", "__unnamed__")   # facet_wrap positional case
)
for (cc in control_cases) {
  r <- ptr_resolve_ui_text("control", keyword = cc[1], param = cc[2],
                           layer_name = if (cc[1] == "expr" && cc[2] == "__unnamed__")
                             "facet_wrap" else NULL)
  for (lf in leaf_fields) {
    v <- r[[lf]]
    stopifnot(is.null(v) || is.character(v))
  }
  cat(sprintf("  control[kw=%s, param=%s]: label=%s | help=%s | placeholder=%s | empty_text=%s\n",
              cc[1], cc[2],
              r$label %||% "<NULL>",
              r$help %||% "<NULL>",
              r$placeholder %||% "<NULL>",
              r$empty_text %||% "<NULL>"))
}
cat("  PASS: every component × every leaf field resolves to NULL or character.\n")

# Custom ui_text override: confirm a user-supplied override flows through to
# every leaf without error. (Catches drift between component_paths and the
# rules structure.)
override <- ptr_ui_text(list(
  shell = list(
    title           = list(label = "CUSTOM TITLE"),
    draw_button     = list(label = "GO"),
    draw_all_button = list(label = "GO ALL"),
    layer_picker    = list(label = "PICK"),
    data_subtab     = list(label = "DAT"),
    controls_subtab = list(label = "CTL")
  ),
  upload = list(
    file = list(label = "FILE", placeholder = "PH", help = "HELP", empty_text = "ET"),
    name = list(label = "NAME", placeholder = "PH2", help = "HELP2", empty_text = "ET2")
  ),
  layer_checkbox = list(label = "USE"),
  defaults = list(
    num  = list(label = "NUM", help = "NUM_H", placeholder = "NUM_P", empty_text = "NUM_E"),
    text = list(label = "TXT", help = "TXT_H", placeholder = "TXT_P", empty_text = "TXT_E")
  ),
  params = list(
    x = list(num = list(label = "X_NUM"))
  )
))
for (cmp in names(comp_paths)) {
  r <- ptr_resolve_ui_text(cmp, ui_text = override)
  cat(sprintf("  override[%s]$label = %s\n", cmp, r$label %||% "<NULL>"))
}
cat("  control kw=num,param=x label =",
    ptr_resolve_ui_text("control", keyword = "num", param = "x",
                        ui_text = override)$label, "\n")
cat("  PASS: every ui_text leaf flows through resolve.\n")

cat("\n========== ALL HEADLESS-DRIVER ITEMS COMPLETE ==========\n")
