# PLAN-01 (ADR 0010) coverage: `ppUpload(name)` registry default_arg,
# companion-textInput seeding from node$default, and the
# identity-outside-app top-level `ppUpload(x)` semantics. Maps 1:1 to the
# 10 BDD scenarios in
# dev/plans/0010-ppupload-name-default-arg/01-registry-and-ui-seeding.html.

# ---- helpers ---------------------------------------------------------------

# Find every ptr_ph_data_source node with keyword == "ppUpload" in a tree.
find_upload_nodes <- function(tree) {
  ptr_collect(tree, function(n) {
    is_ptr_ph_data_source(n) && identical(n$keyword, "ppUpload")
  })
}

# Walk a shiny.tag tree and collect every shiny.tag whose `$name == "input"`.
collect_input_tags <- function(x) {
  acc <- list()
  recur <- function(node) {
    if (inherits(node, "shiny.tag")) {
      if (identical(node$name, "input")) {
        acc[[length(acc) + 1L]] <<- node
      }
      for (child in node$children) recur(child)
    } else if (is.list(node)) {
      for (child in node) recur(child)
    }
  }
  recur(x)
  acc
}

# Build a synthetic upload node with the minimum slots build_ui touches.
make_upload_node <- function(default = NULL,
                             id = "up1",
                             companion_id = "up1_name") {
  structure(
    list(
      keyword = "ppUpload",
      id = id,
      companion_id = companion_id,
      default = default
    ),
    class = c("ptr_ph_data_source", "ptr_placeholder", "ptr_call",
              "ptr_node")
  )
}

# ---- Scenario 1: bareword symbol arg stamps node$default -------------------

test_that("ppUpload(penguins) stamps node$default = 'penguins'", {
  tree <- ptr_translate(
    "ppUpload(penguins) |> ggplot(aes(x = ppVar)) + geom_point()"
  )
  uploads <- find_upload_nodes(tree)
  expect_equal(length(uploads), 1L)
  node <- uploads[[1L]]
  expect_identical(node$default, "penguins")
  expect_identical(node$keyword, "ppUpload")
})

# ---- Scenario 2: string arg stamps node$default identically ----------------

test_that('ppUpload("penguins") stamps node$default = "penguins"', {
  tree <- ptr_translate(
    'ppUpload("penguins") |> ggplot(aes(x = ppVar)) + geom_point()'
  )
  uploads <- find_upload_nodes(tree)
  expect_equal(length(uploads), 1L)
  expect_identical(uploads[[1L]]$default, "penguins")
})

# ---- Scenario 3: legacy ppUpload() leaves node$default NULL ----------------

test_that("legacy ppUpload() leaves node$default NULL", {
  tree <- ptr_translate(
    "ppUpload() |> ggplot(aes(x = ppVar)) + geom_point()"
  )
  uploads <- find_upload_nodes(tree)
  expect_equal(length(uploads), 1L)
  expect_null(uploads[[1L]]$default)
})

# ---- Scenario 4: legacy bareword ppUpload leaves node$default NULL ---------

test_that("legacy bareword ppUpload leaves node$default NULL", {
  tree <- ptr_translate(
    "ppUpload |> ggplot(aes(x = ppVar)) + geom_point()"
  )
  uploads <- find_upload_nodes(tree)
  expect_equal(length(uploads), 1L)
  expect_null(uploads[[1L]]$default)
})

# ---- Scenario 5: non-symbol-or-string positional arg aborts ----------------

test_that("ppUpload(1 + 2) aborts with rlang_error class", {
  expect_error(
    ptr_translate(
      "ppUpload(1 + 2) |> ggplot(aes(x = ppVar)) + geom_point()"
    ),
    class = "rlang_error"
  )
})

# ---- Scenario 6: build_ui seeds companion textInput value ------------------

test_that("ptr_builtin_upload_build_ui seeds companion value from default", {
  node <- make_upload_node(default = "penguins")
  ui <- ptr_builtin_upload_build_ui(node)
  inputs <- collect_input_tags(ui)
  text_inputs <- Filter(
    function(t) identical(t$attribs$type, "text") &&
      identical(t$attribs$id, "up1_name"),
    inputs
  )
  expect_equal(length(text_inputs), 1L)
  expect_identical(text_inputs[[1L]]$attribs$value, "penguins")
})

# ---- Scenario 7: build_ui leaves companion empty when default = NULL -------

test_that("ptr_builtin_upload_build_ui leaves companion value empty for NULL default", {
  node <- make_upload_node(default = NULL)
  ui <- ptr_builtin_upload_build_ui(node)
  inputs <- collect_input_tags(ui)
  text_inputs <- Filter(
    function(t) identical(t$attribs$type, "text") &&
      identical(t$attribs$id, "up1_name"),
    inputs
  )
  expect_equal(length(text_inputs), 1L)
  expect_identical(text_inputs[[1L]]$attribs$value, "")
})

# ---- Scenario 8: ppUpload(x) outside app is identity (data.frame) ----------

test_that("ppUpload(x) outside app returns x unchanged", {
  penguins <- data.frame(a = 1L)
  expect_identical(ggpaintr::ppUpload(penguins), penguins)
})

# ---- Scenario 9: ppUpload(\"string\") outside app returns string verbatim --

test_that('ppUpload("penguins") outside app returns the character scalar', {
  expect_identical(ggpaintr::ppUpload("penguins"), "penguins")
})

# ---- Scenario 10: ppUpload() (no arg) outside app still aborts -------------

test_that("ppUpload() with no arg outside app aborts with guard message", {
  expect_error(ggpaintr::ppUpload(), "only meaningful inside")
})
