suppressPackageStartupMessages({
  library(ggpcp)
  data(flea, package = "GGally")
  devtools::load_all(".", quiet = TRUE)
})

cols_ph <- ptr_define_placeholder(
  keyword = "cols",
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },
  bind_ui = function(input, output, metas, context) NULL,
  resolve_expr = function(value, meta, context) {
    if (length(value) == 0) return(ptr_missing_expr())
    rlang::expr(dplyr::all_of(!!value))
  },
  copy_defaults = list(label = "Choose columns for {param}")
)

range_ph <- ptr_define_placeholder(
  keyword = "range",
  build_ui = function(id, copy, meta, context) {
    shiny::sliderInput(id, copy$label, min = 0, max = 20, value = c(0, 60))
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value)) return(ptr_missing_expr())
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Zoom range for {param}")
)

registry <- ptr_merge_placeholders(list(cols = cols_ph, range = range_ph))

formula <- 'ggplot(data = flea |>
  pcp_select(cols) |>
  pcp_scale(method="uniminmax") |>
  pcp_arrange() , mapping = aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species)) +
  coord_cartesian(xlim = range, ylim = range)'

obj <- ptr_parse_formula(formula, placeholders = registry)
cat("Layer names: ", paste(names(obj$expr_list), collapse = ", "), "\n\n")

# Find input IDs for the placeholders
spec <- ptr_runtime_input_spec(obj)
print(spec[, c("input_id", "layer_name", "param_key", "role", "keyword")])
cat("\n")

run_case <- function(label, inputs) {
  cat("==== ", label, " ====\n", sep = "")
  res <- tryCatch(
    ptr_complete_expr(obj, inputs),
    error = function(e) list(code_text = paste("ERROR:", conditionMessage(e)))
  )
  cat(res$code_text, "\n\n", sep = "")
}

# Build a "checkbox-on" base
base_inputs <- setNames(
  rep(list(TRUE), sum(spec$role == "layer_checkbox")),
  spec$input_id[spec$role == "layer_checkbox"]
)

# Case 1: nothing supplied (all placeholders missing)
run_case("Case 1: all inputs missing", base_inputs)

cols_id  <- as.character(spec$input_id[spec$keyword == "cols"])
range_ids <- as.character(spec$input_id[spec$keyword == "range"])

with_inputs <- function(...) {
  out <- base_inputs
  pairs <- list(...)
  for (i in seq_along(pairs)) out[[names(pairs)[i]]] <- pairs[[i]]
  out
}

run_case("Case 2: cols + range provided",
         `[[<-`(`[[<-`(`[[<-`(base_inputs,
                              cols_id, c("tars1", "tars2", "head")),
                       range_ids[1], c(0, 60)),
                range_ids[2], c(0, 60)))

run_case("Case 3: cols missing, range provided",
         `[[<-`(`[[<-`(base_inputs, range_ids[1], c(0, 60)),
                range_ids[2], c(0, 60)))

run_case("Case 4: cols provided, range missing",
         `[[<-`(base_inputs, cols_id, c("tars1", "tars2", "head")))
suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
})

# Helper: parse, complete, and report which named substring is in the result.
run <- function(label, formula, inputs, expect_present = NULL,
                expect_absent = NULL, safe_to_remove = character()) {
  obj <- tryCatch(ptr_parse_formula(formula), error = function(e) e)
  if (inherits(obj, "error")) {
    cat(sprintf("[%-3s] PARSE-ERR  %s\n", "?", label))
    return(invisible())
  }
  spec <- ptr_runtime_input_spec(obj)
  base <- setNames(
    rep(list(TRUE), sum(spec$role == "layer_checkbox")),
    spec$input_id[spec$role == "layer_checkbox"]
  )
  for (k in names(inputs)) base[[k]] <- inputs[[k]]
  res <- tryCatch(
    ptr_complete_expr(obj, base, safe_to_remove = safe_to_remove),
    error = function(e) list(code_text = paste("ERROR:", conditionMessage(e)))
  )
  ok <- TRUE
  for (s in expect_present) if (!grepl(s, res$code_text, fixed = TRUE)) ok <- FALSE
  for (s in expect_absent)  if ( grepl(s, res$code_text, fixed = TRUE)) ok <- FALSE
  cat(sprintf("[%-3s] %s\n      %s\n",
              if (ok) "OK" else "FAIL",
              label,
              gsub("\n", " ", res$code_text)))
}

# === Quadrant A — should survive AND does survive ============================
cat("\n## A) should survive AND does survive\n\n")

run(
  "A1: user-authored aes_pcp() inside mapping arg (no input)",
  "ggplot(data = mtcars, mapping = aes_pcp()) + geom_point(aes(x = mpg))",
  list(),
  expect_present = "aes_pcp()"
)

run(
  "A2: user-authored pcp_arrange() at end of pipe (no input)",
  "ggplot(data = flea |> pcp_arrange(), aes(x = mpg)) + geom_point()",
  list(),
  expect_present = "pcp_arrange("
)

run(
  "A3: top-level + pcp_theme() user-authored (no input)",
  "ggplot(data = mtcars, aes(x = var)) + geom_histogram() + pcp_theme()",
  list(),
  expect_present = "pcp_theme()"
)

run(
  "A4: top-level + labs() user-authored (curated name, but diff guard preserves)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + labs()",
  list(),
  expect_present = "labs()"
)

run(
  "A5: geom_point(colour = var) with var missing → kept as geom_point() (standalone)",
  "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point(colour = var)",
  list(),
  expect_present = "geom_point()"
)

run(
  "A6: theme(plot.title = element_text(size = num)) with num missing → element_text() kept",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + labs(title = text) + theme(plot.title = element_text(size = num))",
  list(),
  expect_present = "element_text()"
)

run(
  "A7: coord_cartesian(xlim = num, ylim = num) all missing → coord_cartesian() kept (not in curated)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + coord_cartesian(xlim = num, ylim = num)",
  list(),
  expect_present = "coord_cartesian()"
)

run(
  "A8: literal empty aes() in mapping survives (aes deliberately not in remove_set)",
  "ggplot(data = mtcars, mapping = aes()) + geom_point(aes(x = mpg, y = disp))",
  list(),
  expect_present = "mapping = aes()"
)

# === Quadrant B — should die AND does die ====================================
cat("\n## B) should die AND does die\n\n")

run(
  "B1: theme(plot.title = text) with text missing → theme dropped",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + theme(plot.title = text)",
  list(),
  expect_absent = "theme("
)

run(
  "B2: labs(title = text) with text missing → labs dropped",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + labs(title = text)",
  list(),
  expect_absent = "labs("
)

run(
  "B3: pcp_theme(title = text) with safe_to_remove = 'pcp_theme' → dropped",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + pcp_theme(title = text)",
  list(),
  expect_absent = "pcp_theme",
  safe_to_remove = "pcp_theme"
)

run(
  "B4: guides(colour = var) with var missing → guides dropped",
  "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point(aes(colour = mpg)) + guides(colour = var)",
  list(),
  expect_absent = "guides("
)

run(
  "B5: facet_wrap(facets = var) with var missing → facet_wrap dropped",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + facet_wrap(facets = var)",
  list(),
  expect_absent = "facet_wrap("
)

# === Quadrant C — should-survive regressions (would be a NEW bug) ============
cat("\n## C) should-survive regression hunters (any FAIL = bug)\n\n")

run(
  "C1: user-typed + theme() empty literal (curated name; diff guard must preserve)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + theme()",
  list(),
  expect_present = "theme()"
)

run(
  "C2: literal + labs() with safe_to_remove='labs' (still preserved, diff guard wins)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + labs()",
  list(),
  expect_present = "labs()",
  safe_to_remove = "labs"
)

run(
  "C3: nested user-authored vars() inside facet_wrap(vars(cyl))",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + facet_wrap(vars(cyl))",
  list(),
  expect_present = "vars(cyl)"
)

run(
  "C4: deeply nested user-authored aes_pcp() inside an unrelated wrapper",
  "ggplot(data = wrap(aes_pcp()), aes(x = mpg)) + geom_point()",
  list(),
  expect_present = "aes_pcp()"
)

# === Quadrant D — should-die scenarios that survive (documented under-pruning)
cat("\n## D) under-pruning by design (FAIL means we over-pruned)\n\n")

run(
  "D1: + unknown_helper(arg = num) with num missing → kept (third-party not in default set)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + unknown_helper(arg = num)",
  list(),
  expect_present = "unknown_helper("
)

run(
  "D2: user-authored + guides() (curated; diff guard preserves; documented behavior change)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + guides()",
  list(),
  expect_present = "guides()"
)

run(
  "D3: scale_x_continuous(limits = num) with num missing → kept (scale_* mixed-bag, not in curated)",
  "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram() + scale_x_continuous(limits = num)",
  list(),
  expect_present = "scale_x_continuous()"
)
