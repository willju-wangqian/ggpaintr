# Coverage for the diff-guard + curated remove-list + user override
# pruning rule that replaced the old name-based heuristic. See
# dev/empty-call-pruning-fix.md for the truth table.

sentinel <- rlang::sym("_NULL_PLACEHOLDER")
default_set <- default_safe_to_remove()

# --- default_safe_to_remove() shape ----------------------------------------

test_that("default_safe_to_remove returns the documented curated set", {
  expect_type(default_set, "character")
  expect_true(all(c(
    "theme", "labs", "xlab", "ylab", "ggtitle",
    "facet_wrap", "facet_grid", "facet_null",
    "xlim", "ylim", "lims", "expand_limits",
    "guides", "annotate"
  ) %in% default_set))
  expect_false("aes" %in% default_set)
  expect_false("vars" %in% default_set)
  expect_false("element_text" %in% default_set)
  expect_false(any(grepl("^geom_", default_set)))
  expect_false(any(grepl("^stat_", default_set)))
})

# --- validate_safe_to_remove() ---------------------------------------------

test_that("validate_safe_to_remove accepts character vectors and NULL", {
  expect_equal(validate_safe_to_remove(NULL), character())
  expect_equal(validate_safe_to_remove(character()), character())
  expect_equal(validate_safe_to_remove("pcp_theme"), "pcp_theme")
  expect_equal(
    validate_safe_to_remove(c("a", "b")),
    c("a", "b")
  )
})

test_that("validate_safe_to_remove rejects non-character input", {
  expect_error(validate_safe_to_remove(list("a")), "character vector")
  expect_error(validate_safe_to_remove(factor("a")), "character vector")
  expect_error(validate_safe_to_remove(1L), "character vector")
})

test_that("validate_safe_to_remove rejects NA, empty strings, ::, invalid names", {
  expect_error(validate_safe_to_remove(NA_character_), "NA")
  expect_error(validate_safe_to_remove(c("a", NA_character_)), "NA")
  expect_error(validate_safe_to_remove(""), "empty")
  expect_error(validate_safe_to_remove(c("ok", "")), "empty")
  expect_error(validate_safe_to_remove("patchwork::plot_layout"), "bare function names")
  expect_error(validate_safe_to_remove("plot layout"), "valid R names")
})

# --- prune_empty_substitution_artifacts: depth-0 contract -------------------
# The walker never drops at depth 0 — top-level layer removal is the job of
# ptr_remove_empty_nonstandalone_layers. These cases pin that contract.

test_that("walker returns depth-0 zero-arg call unchanged regardless of name", {
  for (nm in c("aes_pcp", "pcp_theme", "pcp_arrange", "labs", "theme")) {
    expr <- rlang::call2(nm)
    result <- prune_empty_substitution_artifacts(expr, expr, default_set)
    expect_equal(result, expr,
      info = paste0("name=", nm, " — depth-0 short-circuit"))
  }
})

test_that("nested user-authored zero-arg call is kept (name not in remove_set)", {
  # user writes: ggplot(data = flea, mapping = aes_pcp())
  expr <- rlang::expr(ggplot(data = flea, mapping = aes_pcp()))
  result <- prune_empty_substitution_artifacts(expr, expr, default_set)
  expect_equal(result, expr)
})

test_that("nested user-authored empty call WITH name in remove_set still drops", {
  # ggplot(mapping = theme()) — `theme` in default set, depth 1 — drops.
  remove_set <- default_set
  orig <- rlang::expr(ggplot(mapping = theme()))
  result <- prune_empty_substitution_artifacts(orig, orig, remove_set)
  expect_false(grepl("theme", rlang::expr_text(result), fixed = TRUE))
})

# --- prune_empty_substitution_artifacts: curated cleanup ------------------

test_that("theme(plot.title = text) with text missing gets pruned at top level", {
  orig <- rlang::expr(theme(plot.title = text))
  post <- rlang::expr(theme(plot.title = !!sentinel))
  result <- prune_empty_substitution_artifacts(post, orig, default_set)
  # post-walk it should be `theme()` with the sentinel slot swept.
  expect_true(rlang::is_call(result, "theme"))
  expect_equal(length(result), 1L)
})

test_that("nested theme(plot.title = element_text(size = num)) with num missing keeps element_text()", {
  orig <- rlang::expr(theme(plot.title = element_text(size = num)))
  post <- rlang::expr(theme(plot.title = element_text(size = !!sentinel)))
  result <- prune_empty_substitution_artifacts(post, orig, default_set)
  # element_text not in default set → kept as element_text()
  expect_equal(rlang::expr_text(result),
               rlang::expr_text(rlang::expr(theme(plot.title = element_text()))))
})

test_that("nested labs(title = text) with text missing collapses (labs in curated set)", {
  # labs is in curated set; nested case is flagged → swept from parent.
  orig <- rlang::expr(plot + labs(title = text))
  post <- rlang::expr(plot + labs(title = !!sentinel))
  result <- prune_empty_substitution_artifacts(post, orig, default_set)
  expect_false(grepl("labs", rlang::expr_text(result), fixed = TRUE))
})

# --- prune_empty_substitution_artifacts: user override --------------------

test_that("user safe_to_remove deletes nested third-party empty call", {
  remove_set <- unique(c(default_set, "pcp_theme"))
  orig <- rlang::expr(plot + pcp_theme(title = text))
  post <- rlang::expr(plot + pcp_theme(title = !!sentinel))
  result <- prune_empty_substitution_artifacts(post, orig, remove_set)
  expect_false(grepl("pcp_theme", rlang::expr_text(result), fixed = TRUE))
})

test_that("walker leaves user-supplied-set call alone at depth 0 (drop happens at layer-list pass)", {
  remove_set <- unique(c(default_set, "pcp_theme"))
  expr <- rlang::call2("pcp_theme")
  result <- prune_empty_substitution_artifacts(expr, expr, remove_set)
  expect_equal(result, expr)
})

# --- prune_empty_substitution_artifacts: expr placeholder is honored ------

test_that("expr-substituted subtree is kept verbatim, even when name is in remove_set", {
  # original layer was a bare symbol `expr`, substituted into `theme()`
  orig <- rlang::sym("expr")
  post <- rlang::call2("theme")
  result <- prune_empty_substitution_artifacts(post, orig, default_set)
  expect_equal(result, post)
})

test_that("expr-substituted nested zero-arg call survives", {
  orig <- rlang::sym("expr")
  post <- rlang::expr(theme(plot.title = element_text()))
  result <- prune_empty_substitution_artifacts(post, orig, default_set)
  expect_equal(result, post)
})

# --- prune_empty_substitution_artifacts: anonymous head -------------------

test_that("anonymous-head call is kept (call_name returns NULL)", {
  # (function() x)() — was non-empty, now empty: but no name match → kept
  fn <- function() x
  post <- as.call(list(rlang::expr((function() 1))))
  # Construct an "originally non-empty" version by hand
  orig <- as.call(list(rlang::expr((function() 1)), rlang::sym("ignored")))
  result <- prune_empty_substitution_artifacts(post, orig, default_set)
  expect_true(is.call(result))
  expect_null(rlang::call_name(result))
})

# --- prune_empty_substitution_artifacts: namespaced calls -----------------

test_that("namespaced call matches bare name in remove_set", {
  remove_set <- unique(c(default_set, "plot_layout"))
  # Build `pkg::plot_layout(...)` programmatically so static parsers don't
  # treat this as a package import.
  ns_head <- rlang::call2(
    rlang::sym("::"),
    rlang::sym("patchwork"),
    rlang::sym("plot_layout")
  )
  orig <- rlang::call2(
    "+",
    rlang::sym("plot"),
    rlang::call2(ns_head, ncol = rlang::sym("num"))
  )
  post <- rlang::call2(
    "+",
    rlang::sym("plot"),
    rlang::call2(ns_head, ncol = sentinel)
  )
  result <- prune_empty_substitution_artifacts(post, orig, remove_set)
  expect_false(grepl("plot_layout", rlang::expr_text(result), fixed = TRUE))
})

# --- ptr_remove_empty_nonstandalone_layers --------------------------------

test_that("top-level theme(plot.title = text) collapses to theme() then deleted", {
  orig_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    theme = rlang::expr(theme(plot.title = text))
  )
  expr_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    theme = rlang::call2("theme")
  )
  result <- ptr_remove_empty_nonstandalone_layers(
    expr_list, orig_list, default_set
  )
  expect_null(result$theme)
  expect_equal(result$ggplot, expr_list$ggplot)
})

test_that("top-level user-authored labs() literal drops (curated, no diff guard)", {
  # New rule: empty `labs()` is in default_set, removal is safe → drop, even
  # though the user typed it literally. (Symmetric for + theme(), + guides().)
  orig_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    labs = rlang::call2("labs")
  )
  expr_list <- orig_list
  result <- ptr_remove_empty_nonstandalone_layers(
    expr_list, orig_list, default_set
  )
  expect_null(result$labs)
})

test_that("top-level pcp_theme() survives without user safe_to_remove", {
  orig_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    pcp_theme = rlang::expr(pcp_theme(title = text))
  )
  expr_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    pcp_theme = rlang::call2("pcp_theme")
  )
  result <- ptr_remove_empty_nonstandalone_layers(
    expr_list, orig_list, default_set
  )
  expect_equal(result$pcp_theme, rlang::call2("pcp_theme"))
})

test_that("top-level pcp_theme() collapses with user safe_to_remove", {
  remove_set <- unique(c(default_set, "pcp_theme"))
  orig_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    pcp_theme = rlang::expr(pcp_theme(title = text))
  )
  expr_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    pcp_theme = rlang::call2("pcp_theme")
  )
  result <- ptr_remove_empty_nonstandalone_layers(
    expr_list, orig_list, remove_set
  )
  expect_null(result$pcp_theme)
})

test_that("top-level standalone geom_point() survives even when in remove_set", {
  remove_set <- unique(c(default_set, "geom_point"))
  orig_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    geom_point = rlang::expr(geom_point(colour = var))
  )
  expr_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    geom_point = rlang::call2("geom_point")
  )
  result <- ptr_remove_empty_nonstandalone_layers(
    expr_list, orig_list, remove_set
  )
  expect_equal(result$geom_point, rlang::call2("geom_point"))
})

test_that("top-level layer with bare-symbol original is honoured verbatim", {
  orig_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    expr = rlang::sym("expr")
  )
  expr_list <- list(
    ggplot = rlang::expr(ggplot(data = mtcars)),
    expr = rlang::call2("theme")  # substituted via expr placeholder
  )
  result <- ptr_remove_empty_nonstandalone_layers(
    expr_list, orig_list, default_set
  )
  expect_equal(result$expr, rlang::call2("theme"))
})

# --- end-to-end via ptr_complete_expr -------------------------------------

test_that("ptr_complete_expr keeps zero-arg user call from a third-party package", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_histogram() + pcp_theme()"
  )
  input <- list(
    "ggplot_3_2" = "mpg",
    "geom_histogram_checkbox" = TRUE,
    "pcp_theme_checkbox" = TRUE
  )
  result <- ptr_complete_expr(obj, input)
  expect_match(result$code_text, "pcp_theme\\(\\)", fixed = FALSE)
})

test_that("ptr_complete_expr drops theme() when its only arg goes missing", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_histogram() + theme(plot.title = text)"
  )
  input <- list(
    "ggplot_3_2" = "mpg",
    "geom_histogram_checkbox" = TRUE,
    "theme_checkbox" = TRUE
  )
  result <- ptr_complete_expr(obj, input)
  expect_false(grepl("theme\\(", result$code_text))
})

test_that("ptr_complete_expr respects user safe_to_remove", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_histogram() + pcp_theme(title = text)"
  )
  input <- list(
    "ggplot_3_2" = "mpg",
    "geom_histogram_checkbox" = TRUE,
    "pcp_theme_checkbox" = TRUE
  )
  result <- ptr_complete_expr(obj, input, safe_to_remove = "pcp_theme")
  expect_false(grepl("pcp_theme\\(", result$code_text))
})

test_that("ptr_complete_expr keeps element_text() when nested num goes missing", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_histogram() + theme(plot.title = element_text(size = num))"
  )
  input <- list(
    "ggplot_3_2" = "mpg",
    "geom_histogram_checkbox" = TRUE,
    "theme_checkbox" = TRUE
  )
  result <- ptr_complete_expr(obj, input)
  expect_match(result$code_text, "theme\\(plot\\.title = element_text\\(\\)\\)")
})

test_that("ptr_complete_expr keeps geom_point when its sole aesthetic is missing", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point(colour = var)"
  )
  input <- list(
    "geom_point_checkbox" = TRUE
  )
  result <- ptr_complete_expr(obj, input)
  expect_match(result$code_text, "geom_point\\(\\)")
})

test_that("ptr_complete_expr rejects invalid safe_to_remove", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = mpg)) + geom_histogram()"
  )
  input <- list("geom_histogram_checkbox" = TRUE)
  expect_error(
    ptr_complete_expr(obj, input, safe_to_remove = NA_character_),
    "NA"
  )
  expect_error(
    ptr_complete_expr(obj, input, safe_to_remove = ""),
    "empty"
  )
  expect_error(
    ptr_complete_expr(obj, input, safe_to_remove = "patchwork::plot_layout"),
    "bare function names"
  )
})
