# test-ppupload-name-e2e.R — R-level integration tests for ADR 0010
# (ppUpload(name) default-arg + auto-resolve via caller env).
#
# Covers PLAN-03 owned BDD scenarios at the R level (no browser):
#   (a) ptr_app_components UI carries the seeded textInput value="penguins"
#       at the upload node's companion_id (cross-checks PLAN-01).
#   (b) ptr_substitute(input_snapshot = list(<companion_id> = "penguins"))
#       emits the bare symbol `penguins` at the data-source slot (NOT a
#       string, NOT the internal upload id) -- the PLAN-02 round-trip seen
#       below the renderer.
#   (c) live-eval of the substituted AST resolves the bare symbol through
#       the existing eval_env parent chain (Clarification C2 / C3): the
#       caller-env `penguins` data.frame backs the ggplot output without
#       any actual file upload.

# Build a small inline data.frame so the test does not need palmerpenguins
# (Clarification C5: no new Suggests; inline is the default path).
make_penguins <- function() {
  data.frame(
    species         = c("Adelie", "Gentoo", "Chinstrap"),
    bill_length_mm  = c(39.1, 50.0, 49.5),
    bill_depth_mm   = c(18.7, 15.5, 17.5),
    stringsAsFactors = FALSE
  )
}

# Discover the upload node in a translated tree (returns the first match).
find_upload_node <- function(tree) {
  found <- list()
  ggpaintr:::ptr_walk(tree, visit = function(n) {
    if (ggpaintr:::is_ptr_ph_data_source(n) &&
        identical(n$keyword, "ppUpload")) {
      found[[length(found) + 1L]] <<- n
    }
  })
  found[[1L]]
}

formula_str <- paste0(
  "ppUpload(penguins) |> ",
  "dplyr::filter(species == \"Adelie\") |> ",
  "ggplot(aes(x = ppVar(bill_length_mm), y = ppVar(bill_depth_mm))) + ",
  "geom_point()"
)

test_that("ptr_app_components surfaces the seeded companion textInput in its UI (PLAN-01 end-to-end)", {
  penguins <- make_penguins()

  parts <- ggpaintr:::ptr_app_components(formula_str)
  ui_html <- as.character(parts$ui)

  # Discover the companion id from the translated tree (don't hardcode).
  tree <- ggpaintr:::ptr_translate(formula_str)
  upload <- find_upload_node(tree)
  expect_false(is.null(upload),
               label = "tree contains a ppUpload data-source node")
  expect_equal(upload$companion_id,
               paste0(upload$id, "_name"),
               label = "companion_id matches ptr_upload_name_id convention")

  # The rendered UI must contain a textInput at the companion id with
  # value="penguins" (the user-written symbol from the formula, seeded by
  # PLAN-01's default_arg = ptr_default_symbol_or_string() path).
  expect_true(grepl(upload$companion_id, ui_html, fixed = TRUE),
              label = "UI contains the companion input id")
  expect_match(ui_html, "value=\"penguins\"", fixed = TRUE)
})

test_that("ptr_substitute emits the bare symbol when companion snapshot is seeded (PLAN-02 contract)", {
  tree <- ggpaintr:::ptr_translate(formula_str)
  upload <- find_upload_node(tree)

  snapshot <- list()
  snapshot[[upload$companion_id]] <- "penguins"
  # Also seed the ppVar pickers so the rest of the substitution is well-formed.
  snapshot[["ggplot_1_1_ppVar_NA"]] <- "bill_length_mm"
  snapshot[["ggplot_1_2_ppVar_NA"]] <- "bill_depth_mm"

  subst <- ggpaintr:::ptr_substitute(tree, input_snapshot = snapshot)
  eval_expr <- ggpaintr:::layer_to_eval_expr(subst$layers[[1L]])
  expr_text <- deparse1(eval_expr)

  # The data-source slot must be the bare symbol `penguins`, NOT a string
  # ("penguins") and NOT the internal upload id (ggplot_1_ppUpload_NA).
  expect_match(expr_text, "filter(penguins,", fixed = TRUE)
  expect_false(grepl("\"penguins\"", expr_text, fixed = TRUE),
               label = "bare symbol, not a string literal")
  expect_false(grepl(upload$id, expr_text, fixed = TRUE),
               label = "no internal upload id leaks into the eval expression")
})

test_that("live-eval resolves the bare symbol via eval_env's parent chain (Clarification C2/C3)", {
  penguins <- make_penguins()  # bound in the caller frame of ptr_eval

  tree <- ggpaintr:::ptr_translate(formula_str)
  upload <- find_upload_node(tree)
  snapshot <- list()
  snapshot[[upload$companion_id]] <- "penguins"
  snapshot[["ggplot_1_1_ppVar_NA"]] <- "bill_length_mm"
  snapshot[["ggplot_1_2_ppVar_NA"]] <- "bill_depth_mm"

  subst <- ggpaintr:::ptr_substitute(tree, input_snapshot = snapshot)

  # ptr_eval's default eval_env = new.env(parent = parent.frame()), so
  # `penguins` here in the test frame is reachable via the parent chain --
  # the same mechanism documented at R/paintr-server.R:207 for the live app.
  plot <- ggpaintr:::ptr_eval(subst)
  expect_s3_class(plot, "ggplot")
  # The built layer must be backed by the caller-env penguins data, with
  # only the Adelie row remaining after the dplyr::filter() stage.
  layer_data <- plot$data
  expect_s3_class(layer_data, "data.frame")
  expect_equal(nrow(layer_data), 1L,
               label = "filter(species == \"Adelie\") narrowed to one row")
  expect_equal(layer_data$species, "Adelie")
})
