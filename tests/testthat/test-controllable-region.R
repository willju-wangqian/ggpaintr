# Coverage for the controllable-region UI helpers
# (R/paintr-region.R). Three callers in shipped UI go through these
# helpers; the third test below uses a fabricated region kind to prove
# the helper itself is generic — no shipped UI relies on it.

test_that("CR.1 controllable_region emits the documented contract DOM", {
  tag <- controllable_region(
    "stage_42",
    head_label = shiny::tags$code("head()"),
    body = shiny::tags$span("inner"),
    ns_fn = identity
  )
  expect_s3_class(tag, "shiny.tag")
  expect_identical(tag$attribs$class, "ptr-stage")
  expect_identical(tag$attribs$id, "stage_42_stage_block")

  head <- tag$children[[1]]
  expect_identical(head$attribs$class, "ptr-stage-head")

  cb <- head$children[[1]]
  # checkboxInput returns a wrapper div whose first input child carries the id
  inputs <- Filter(function(x) inherits(x, "shiny.tag") && x$name == "input",
                   unlist(list(cb), recursive = FALSE))
  # walk to find the input id
  find_input_id <- function(x) {
    if (inherits(x, "shiny.tag")) {
      if (identical(x$name, "input")) return(x$attribs$id)
      for (ch in x$children) {
        v <- find_input_id(ch)
        if (!is.null(v)) return(v)
      }
    }
    NULL
  }
  expect_identical(find_input_id(cb), "stage_42")

  fields <- tag$children[[2]]
  expect_identical(fields$attribs$class, "ptr-stage-fields")
})

test_that("CR.2 controllable_region honours ns_fn for both id and inputId", {
  ns <- shiny::NS("mod")
  tag <- controllable_region(
    "head_1",
    head_label = NULL,
    body = "x",
    ns_fn = ns
  )
  expect_identical(tag$attribs$id, "mod-head_1_stage_block")
  find_input_id <- function(x) {
    if (inherits(x, "shiny.tag")) {
      if (identical(x$name, "input")) return(x$attribs$id)
      for (ch in x$children) {
        v <- find_input_id(ch)
        if (!is.null(v)) return(v)
      }
    }
    NULL
  }
  cb_div <- tag$children[[1]]$children[[1]]
  expect_identical(find_input_id(cb_div), "mod-head_1")
})

test_that("CR.3 continuation helper emits a bare .ptr-stage-fields div", {
  tag <- controllable_region_continuation(shiny::tags$span("x"))
  expect_s3_class(tag, "shiny.tag")
  expect_identical(tag$attribs$class, "ptr-stage-fields")
  expect_identical(tag$name, "div")
  # No id, no head -- pure continuation.
  expect_null(tag$attribs$id)
  # Same class string as the inner fields div of the full helper -- this
  # asserts the two helpers stay in sync (regression-tests Risk #1).
  full <- controllable_region("r", NULL, "x", ns_fn = identity)
  inner_fields <- full$children[[2]]
  expect_identical(tag$attribs$class, inner_fields$attribs$class)
})

test_that("CR.4 helper is generic across region kinds (third-kind smoke)", {
  # Fabricated region kind not used by any shipped caller; proves the
  # helper does not embed assumptions about pipeline-stage or shared-orphan
  # naming.
  tag <- controllable_region(
    "fabricated_kind_xyz",
    head_label = shiny::tags$code("custom()"),
    body = shiny::tags$div(class = "user-widget", "anything"),
    ns_fn = shiny::NS("custom_mod"),
    default_on = FALSE
  )
  expect_identical(tag$attribs$class, "ptr-stage")
  expect_identical(tag$attribs$id, "custom_mod-fabricated_kind_xyz_stage_block")
  # default_on = FALSE flows through to the rendered checkbox attribute
  find_input <- function(x) {
    if (inherits(x, "shiny.tag")) {
      if (identical(x$name, "input")) return(x)
      for (ch in x$children) {
        v <- find_input(ch)
        if (!is.null(v)) return(v)
      }
    }
    NULL
  }
  inp <- find_input(tag$children[[1]]$children[[1]])
  expect_null(inp$attribs$checked)
})
