# Closed-loop `spec=` round trip — gate test.
#
# Drives widgets via `shiny::testServer`, reads the spec-mode code panel
# (ADR 0022: pre-ADR-0022 the "preserve" mode was a formula + spec view;
# post-ADR-0022 spec mode emits only the `ptr_spec <- list(...)` block),
# parses the block, feeds the result as `spec=` to a second
# `ptr_server_internal` boot, and asserts the second session's
# `state$spec_seed` matches the first session's `runtime()$snapshot` for
# the same ids.
#
# This is the *closed* loop the user-visible copy-paste workflow depends
# on. Without it, the emit-side panel could quietly drift from the
# apply-side parser (quoting, list ordering, type rules) while
# `test-ptr-spec-emission.R` and `test-adr12-spec-roundtrip.R` stayed
# green on their respective halves.
#
# Why `state$spec_seed` and not `input[[id]]`: under `testServer` there
# is no client, so renderUI'd widgets' JS bindings cannot round-trip the
# boot-seeded value back to `input[[id]]` -- the existing browser test
# `test-adr12-spec-roundtrip.R` documents this at L65-72 for the ppNum
# case. `state$spec_seed` is the environment `apply_spec_at_boot` writes
# BEFORE the renderUI fires; it is the most direct in-process observable
# for "what session #2 booted with".
#
# Complements `test-ptr-spec-emission.R` (emission-side unit tests) and
# `test-adr12-spec-roundtrip.R` (apply-side, browser e2e).

# ---- helpers ---------------------------------------------------------------

# Parse the `ptr_spec <- list(...)` block out of a spec-mode panel.
# Returns the evaluated named list, or NULL if no block is present.
extract_ptr_spec <- function(code_txt) {
  if (!grepl("ptr_spec <- list", code_txt, fixed = TRUE)) return(NULL)
  exprs <- parse(text = code_txt, keep.source = FALSE)
  for (i in seq_along(exprs)) {
    ex <- exprs[[i]]
    if (is.call(ex) && identical(ex[[1L]], as.name("<-")) &&
        identical(ex[[2L]], as.name("ptr_spec"))) {
      return(eval(ex[[3L]]))
    }
  }
  NULL
}

# Boot session #1, drive `picks(state$input_spec)`, click update, switch to
# spec mode, capture (snapshot, spec, code_txt).
drive_and_capture <- function(formula, picks, envir, ...) {
  out <- list()
  ctor_dots <- list(...)
  server <- function(input, output, session) {
    session$userData$state <- do.call(
      ggpaintr:::ptr_server_internal,
      c(list(input, output, session, formula, envir = envir), ctor_dots)
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    args <- c(picks(state$input_spec), list(ptr_update_plot = 1L))
    do.call(session$setInputs, args)
    session$flushReact()
    res <- state$runtime()
    out$runtime_ok <<- isTRUE(res$ok)
    out$snapshot   <<- res$snapshot
    session$setInputs(ptr_code_mode = "spec")
    session$flushReact()
    out$code_txt <<- output$ptr_code
    out$spec     <<- extract_ptr_spec(output$ptr_code)
  })
  out
}

# Boot session #2 with `spec=`, flush once to fire the apply callback,
# return `state$spec_seed` as a plain list.
reboot_with_spec <- function(formula, spec, envir, ...) {
  seed <- list()
  ctor_dots <- list(...)
  server <- function(input, output, session) {
    session$userData$state <- do.call(
      ggpaintr:::ptr_server_internal,
      c(list(input, output, session, formula, envir = envir, spec = spec),
        ctor_dots)
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state
    for (k in ls(state$spec_seed)) {
      seed[[k]] <<- get(k, envir = state$spec_seed)
    }
  })
  seed
}

expect_seed_matches_snapshot <- function(seed, snapshot, spec_keys) {
  for (k in spec_keys) {
    expect_identical(
      seed[[k]], snapshot[[k]],
      label = sprintf("seed[[%s]] vs snapshot[[%s]]", k, k)
    )
  }
}

# ---- Test A: built-in placeholders -----------------------------------------

test_that("closed loop: built-ins (ppVar, ppNum, ppText, ppExpr) round-trip via spec_seed", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  formula <- paste(
    "ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(hp))) +",
    "geom_point(size = ppNum(2), alpha = ppNum(0.5)) +",
    "labs(title = ppText('Title'), subtitle = ppExpr(paste('n =', nrow(mtcars))))"
  )
  picks <- function(sd) {
    pick_id <- function(kw, pk) {
      m <- !is.na(sd$keyword) & sd$keyword == kw &
           !is.na(sd$param_key) & sd$param_key == pk &
           sd$role == "placeholder"
      sd$input_id[which(m)[1L]]
    }
    stats::setNames(
      list("cyl", "mpg", 3, 0.7, "My title", "paste('hello')"),
      c(pick_id("ppVar", "x"),  pick_id("ppVar", "y"),
        pick_id("ppNum", "size"), pick_id("ppNum", "alpha"),
        pick_id("ppText", "title"), pick_id("ppExpr", "subtitle"))
    )
  }
  s1 <- drive_and_capture(formula, picks, envir = e)
  expect_true(s1$runtime_ok)
  expect_match(s1$code_txt, "ptr_spec <- list(", fixed = TRUE)
  expect_equal(length(s1$spec), 6L)

  seed <- reboot_with_spec(formula, s1$spec, envir = e)
  expect_seed_matches_snapshot(seed, s1$snapshot, names(s1$spec))
})

# ---- Test B: ppUpload spec must NOT leak temp filesystem paths -------------
#
# Regression guard for the ppUpload deparse hygiene rule: rows with
# role="placeholder" + keyword="ppUpload" are skipped inside
# `state$spec()` / `ptr_spec_from_snapshot`, so `format_spec_for_panel`
# never deparses an upload's fileInput data.frame and never bakes the
# session-local tmp path into the rendered code.

test_that("closed loop: ppUpload spec does not leak temp file paths", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  tf <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, tf, row.names = FALSE)

  formula <- "ppUpload(df) |> ggplot(aes(x = ppVar(mpg), y = ppVar(hp))) + geom_point()"
  picks <- function(sd) {
    id_up <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppUpload" &
                         sd$role == "placeholder"][1L]
    id_x  <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                         !is.na(sd$param_key) & sd$param_key == "x"][1L]
    id_y  <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                         !is.na(sd$param_key) & sd$param_key == "y"][1L]
    upload_val <- data.frame(
      name = "data.csv", size = file.info(tf)$size,
      type = "text/csv", datapath = tf,
      stringsAsFactors = FALSE
    )
    stats::setNames(list(upload_val, "mpg", "hp"), c(id_up, id_x, id_y))
  }
  s1 <- drive_and_capture(formula, picks, envir = e)

  # Hygiene: the rendered code panel must not contain any artifact of a
  # session-local tmp file (path components OR fileInput field names).
  expect_false(grepl("/tmp/",       s1$code_txt, fixed = TRUE),
               label = "ptr_code leaked '/tmp/' path component")
  expect_false(grepl("/var/folders", s1$code_txt, fixed = TRUE),
               label = "ptr_code leaked '/var/folders' path component")
  expect_false(grepl("Rtmp",         s1$code_txt, fixed = TRUE),
               label = "ptr_code leaked 'Rtmp' tmp-dir name")
  expect_false(grepl("datapath",     s1$code_txt, fixed = TRUE),
               label = "ptr_code leaked 'datapath' fileInput field")
})

# ---- Test C: custom value placeholder --------------------------------------

test_that("closed loop: custom value placeholder (ppPower) round-trips via spec_seed", {
  # Process-global registration per memory `project-placeholder-registry-global`.
  # The test runner is fresh-per-file, so no unregister needed.
  ptr_define_placeholder_value(
    keyword = "ppPower",
    build_ui = function(node, label = "Power", ...) {
      v <- if (is.numeric(node$default) && length(node$default) == 1L) {
        as.numeric(node$default)
      } else 0.5
      shiny::numericInput(node$id, label, value = v, min = 0, max = 1, step = 0.01)
    },
    resolve_expr = function(value, ...) rlang::call2("^", value, 2),
    validate_input = function(value, ctx) {
      if (is.numeric(value) && length(value) == 1L &&
          !is.na(value) && value >= 0 && value <= 1) TRUE
      else "must be in [0,1]"
    },
    positional_arg = ptr_arg_numeric()
  )

  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  formula <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(alpha = ppPower(0.5))"
  picks <- function(sd) {
    id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppPower" &
                      sd$role == "placeholder"][1L]
    stats::setNames(list(0.8), id)
  }
  s1 <- drive_and_capture(formula, picks, envir = e)
  expect_true(s1$runtime_ok)
  expect_equal(length(s1$spec), 1L)
  expect_equal(s1$spec[[1L]], 0.8)

  seed <- reboot_with_spec(formula, s1$spec, envir = e)
  expect_seed_matches_snapshot(seed, s1$snapshot, names(s1$spec))
})

# ---- Test D: shared= consumer ----------------------------------------------

test_that("closed loop: shared consumer placeholder round-trips via spec_seed", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  formula <- paste(
    "ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(hp),",
    "color = ppVar(cyl, shared = 'grp'))) + geom_point() +",
    "facet_wrap(vars(ppVar(cyl, shared = 'grp')))"
  )
  picks <- function(sd) {
    id_x   <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "x" &
                          is.na(sd$shared)][1L]
    id_y   <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "y" &
                          is.na(sd$shared)][1L]
    id_grp <- sd$input_id[!is.na(sd$shared) & sd$shared == "grp"][1L]
    stats::setNames(list("mpg", "hp", "cyl"), c(id_x, id_y, id_grp))
  }
  s1 <- drive_and_capture(formula, picks, envir = e, auto_bind_shared = TRUE)
  expect_true(s1$runtime_ok)
  expect_true("shared_grp" %in% names(s1$spec))

  seed <- reboot_with_spec(formula, s1$spec, envir = e, auto_bind_shared = TRUE)
  expect_seed_matches_snapshot(seed, s1$snapshot, names(s1$spec))
})
