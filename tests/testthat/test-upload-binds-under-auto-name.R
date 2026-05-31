# ADR 0025 §3 / PLAN-02 — BDD scenarios 5 & 6: capture the `name`
# argument passed to `bind_source_value()` from `resolve_upload_source()`
# under two conditions:
#   1. shortcut textbox empty + file_info non-NULL → name == node$auto_name
#   2. shortcut textbox = "my_name" + file_info non-NULL → name == "my_name"
#
# Uses `testthat::local_mocked_bindings(.package = "ggpaintr", ...)`
# rather than `assignInNamespace()` per project memory mock-internal-calls
# (pkgload::load_all breaks assignInNamespace).

fake_state <- function() {
  env <- new.env(parent = emptyenv())
  bound <- new.env(parent = emptyenv())
  errors_val <- list()
  list(
    eval_env = env,
    bound_names = bound,
    resolve_errors = function(new_val) {
      if (missing(new_val)) errors_val else {
        errors_val <<- new_val
        invisible(errors_val)
      }
    }
  )
}

make_slot <- function() {
  v <- NULL
  function(new_val) {
    if (missing(new_val)) v else { v <<- new_val; invisible(v) }
  }
}

# ---- :34 MERGED into J2 journey on 2026-05-28 -------------------------
# empty shortcut -> auto_name binding name is covered DOM-faithfully by
# test-prologue-csv-upload.R (the prologue LHS `_ppUpload_NA <- read.csv
# ("mtcars.csv")` IS the auto_name as observed in output$ptr_code). v9
# routing: dev/audit/audit-test-fidelity-v9-j2-browser-faithfulness-
# 2026-05-28-0027.html

test_that("NULL shortcut textbox value → bind_source_value name = node$auto_name", {
  captured <- list()
  testthat::local_mocked_bindings(
    .package = "ggpaintr",
    bind_source_value = function(state, key, name, df, slot) {
      captured$name <<- name
      slot(df)
      invisible(NULL)
    }
  )
  st <- fake_state()
  st$bound_names[["k"]] <- make_slot()
  slot <- make_slot()
  node <- list(
    keyword = "ppUpload",
    shortcut_id = "any_shortcut",
    auto_name = "main_ds",
    default = NULL
  )
  entry <- list(
    resolve_data = function(file_info, node) datasets::iris
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "x.csv"),
    shortcut_slot = list(present = TRUE, value = NULL),
    node           = node,
    entry          = entry,
    envir          = st$eval_env,
    state          = st,
    key            = "k",
    slot           = slot
  )
  expect_identical(captured$name, "main_ds")
})

# ---- :114 MERGED into J2 journey on 2026-05-28 ------------------------
# non-empty shortcut wins over node$auto_name is covered DOM-faithfully
# by test-j2-prologue-csv-upload-journey.R stage 1: type "my_data" into
# the shortcut textbox AFTER upload, wait for ADR 0025 §7 A2 debounce-
# tick rebind, click Update Plot, assert the substituted formula body
# uses `data = my_data` (substitute_walk emits bound_names[[key]]). v9
# routing: dev/audit/audit-test-fidelity-v9-j2-browser-faithfulness-
# 2026-05-28-0027.html
