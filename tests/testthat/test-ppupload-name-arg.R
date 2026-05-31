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
                             shortcut_id = "up1_shortcut") {
  structure(
    list(
      keyword = "ppUpload",
      id = id,
      shortcut_id = shortcut_id,
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

# ---- Scenario 6: static shortcut textInput seeds value from default --------
# ADR 0025 item #7: the shortcut textInput moved out of the source hook
# (`ptr_builtin_upload_build_ui`, which now returns only the fileInput) and
# is emitted as STATIC UI by `build_ui_for.ptr_ph_data_source`. The value
# seeding from `node$default` is unchanged -- it just lives one layer up.

test_that("build_ui_for emits a shortcut textInput seeded from node$default", {
  node <- make_upload_node(default = "penguins")
  ui <- build_ui_for(node)
  inputs <- collect_input_tags(ui)
  text_inputs <- Filter(
    function(t) identical(t$attribs$type, "text") &&
      identical(t$attribs$id, "up1_shortcut"),
    inputs
  )
  expect_equal(length(text_inputs), 1L)
  expect_identical(text_inputs[[1L]]$attribs$value, "penguins")
})

# ---- Scenario 7: static shortcut textInput is empty when default = NULL ----

test_that("build_ui_for leaves the shortcut textInput empty for NULL default", {
  node <- make_upload_node(default = NULL)
  ui <- build_ui_for(node)
  inputs <- collect_input_tags(ui)
  text_inputs <- Filter(
    function(t) identical(t$attribs$type, "text") &&
      identical(t$attribs$id, "up1_shortcut"),
    inputs
  )
  expect_equal(length(text_inputs), 1L)
  expect_identical(text_inputs[[1L]]$attribs$value, "")
})

# ---- Scenario 6b: the source hook itself now returns ONLY the fileInput ----
# Pins the moved contract from the other side: `ptr_builtin_upload_build_ui`
# must no longer emit the shortcut textInput (else it double-binds the id
# against the static one). Discriminates the regression where the hook
# re-grows the textInput.

test_that("ptr_builtin_upload_build_ui returns only the fileInput (no shortcut)", {
  node <- make_upload_node(default = "penguins")
  ui <- ptr_builtin_upload_build_ui(node)
  inputs <- collect_input_tags(ui)
  # fileInput renders BOTH an <input type="file"> picker and an internal
  # readonly <input type="text"> filename display, so we discriminate on the
  # shortcut id specifically: the shortcut textInput must NOT be emitted here.
  file_inputs <- Filter(function(t) identical(t$attribs$type, "file"), inputs)
  shortcut_inputs <- Filter(
    function(t) identical(t$attribs$id, "up1_shortcut"), inputs
  )
  expect_equal(length(file_inputs), 1L)
  expect_equal(length(shortcut_inputs), 0L)
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
