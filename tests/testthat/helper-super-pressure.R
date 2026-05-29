# helper-super-pressure.R — sentinel-propagation primitives for the
# super-app pressure suite (test-super-pressure.R / ADR 0013).
#
# Why this exists, in one line: ADR 0013 binds the suite to
# **propagation assertions**, not presence proxies. The B3 regression
# (toggle wiring, fixed in 2c504da) survived a full cycle of presence-style
# fixture assertions (expect_dom_id, expect_code_nonempty) because those
# helpers proved only that *some* code text reached the panel. Project
# memory `e2e-assertion-weakness-lens` distills the lesson:
# presence proves the surface; propagation proves the integration.
#
# Every helper here takes an *exact sentinel string* and asserts that
# sentinel appears at an *exact rendered position* under an *explicitly
# named mode* (final or preserve). The helpers refuse to fall back to
# "code panel non-empty": that fallback is the failure shape we're
# defending against.
#
# Independence: this helper deliberately duplicates small fragments
# (set_input shape, draw shape, AppDriver boot) from
# `helper-vignette-apps.R` rather than sharing. Sharing across two e2e
# suites couples them; the new helpers stay independent so the
# super-pressure suite can evolve without disturbing the vignette-app
# suite (PLAN-01 constraint).
#
# get_values() is deliberately NOT used (project memory
# `shinytest2-appdir-pkgload`); all reads are targeted single-output
# (get_value) / targeted DOM (get_html).

# Boot one super-app fixture in a real headless-Chromium browser.
# Mirrors the boot_vignette_app() scaffolding (skip_on_cran +
# skip_if_not_installed + source-root guard + GGP_PKG env +
# prune_dead_ggpaintr_resource_paths + suppressWarnings(AppDriver$new) +
# withr::defer(app$stop())). Distinct name so the two boots can evolve
# independently.
boot_super_app <- function(slug, app_file = "app.R") {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  # Source-root guard. The fixture app.R boots via
  # pkgload::load_all(Sys.getenv("GGP_PKG")); that needs the package
  # SOURCE tree (a DESCRIPTION at GGP_PKG). skip_on_cran() above does
  # NOT cover devtools::check() (devtools runs the check subprocess with
  # NOT_CRAN=true). Skipping on absent DESCRIPTION turns that into a
  # clean SKIP under check while NOT_CRAN=true test_dir (source tree
  # present) runs every browser test. See CLAUDE.md authoritative-gate.
  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  testthat::skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "e2e super-app boot needs the package source root (pkgload::load_all); absent under the R CMD check .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  # Resource-path leak isolation (a prior `css =` test can leave a
  # process-global ggpaintr-user-* resource path pointing at a torn-down
  # tempdir; AppDriver$new() replays parent resourcePaths() into the
  # child and a dead entry aborts boot). Pruning dead paths is a
  # semantic no-op for live ones.
  prune_dead_ggpaintr_resource_paths()
  app_dir <- testthat::test_path("fixtures", "vignette-apps", slug)
  # `app_file` lets a caller boot a non-default file in the same fixture
  # dir (per ADR-0016: every super-app has a peer `app-no-default.R`
  # alongside its `app.R`). shinytest2's AppDriver resolves the directory
  # to `app.R` by name; to point it elsewhere we materialise a temp dir
  # copy with the requested file renamed to `app.R`, preserving every
  # sibling (CSVs, user.css, etc.).
  if (!identical(app_file, "app.R")) {
    src_path <- file.path(app_dir, app_file)
    if (!file.exists(src_path)) {
      stop(sprintf("boot_super_app: app_file '%s' not found in '%s'",
                   app_file, app_dir))
    }
    staging <- withr::local_tempdir(.local_envir = parent.frame())
    siblings <- list.files(app_dir, full.names = TRUE, no.. = TRUE)
    file.copy(siblings, staging, recursive = TRUE)
    # Drop the canonical app.R from the staging dir if present so the
    # rename doesn't collide; then rename the requested file in place.
    canonical_in_staging <- file.path(staging, "app.R")
    if (file.exists(canonical_in_staging)) file.remove(canonical_in_staging)
    file.rename(file.path(staging, app_file),
                file.path(staging, "app.R"))
    app_dir <- staging
  }
  # "Failed to locate globals" / htmlDependency-prefix warnings on
  # AppDriver$new() are benign (project memory); suppression is scoped
  # to construction only, never to assertion output.
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = paste0("super-", slug,
                    if (identical(app_file, "app.R")) "" else "-no-default"),
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  # AppDriver$new() returns once the Connect handshake completes; the first
  # server flush + client bindAll() (which mounts every source/value/consumer
  # renderUI, incl. ppUpload's fileInput + shortcut textInput) and the value
  # round-trip may still be in flight. Without this wait the first post-boot
  # get_value / upload_file / set_sentinel races that boot tail -- benign in
  # isolation (single-digit-ms tail) but flaky under full-suite chromote
  # contention (renderUI mounting grew heavier once shared widgets render from
  # each placeholder's build_ui, ADR 0025 / tests-inspect 7b0297c). Mirrors
  # boot_vignette_app() (helper-vignette-apps.R), which added this for the same
  # reason. Cheap on quiet fixtures; only matters for the heavier first flush.
  app$wait_for_idle(timeout = 15 * 1000)
  withr::defer(app$stop(), envir = parent.frame())
  app
}

# Set one input WITHOUT waiting for an output change. Identical semantics
# to set_input() in helper-vignette-apps.R but renamed so the
# sentinel-shaped intent is visible at the call site. ggpaintr only
# re-renders on an explicit Update/Draw click, so setting a placeholder
# widget never updates an output by itself — wait_ = TRUE would
# (correctly) time out.
#
# Pre-check the input is rendered in the DOM. Without this guard,
# shinytest2's `set_inputs(..., wait_ = FALSE)` swallows "Unable to find
# input binding for element with id X" as a printed console warning —
# the input never gets set, but the test reports no error. That swallow
# concealed an ADR-0016 diagnosis once: consumer pickers in no-default
# formulas don't render until upstream column scope is resolved, so
# `set_sentinel` calls against them were no-ops while the test thought
# it had driven the picker. Surface that failure mode here, at the call
# site, so the test stops with an actionable message instead of cascading
# into a downstream assertion mismatch (project memory
# `e2e-assertion-weakness-lens`).
set_sentinel <- function(app, input_id, sentinel, bind_timeout_ms = 5000) {
  page_html <- app$get_html(paste0("#", input_id))
  if (is.null(page_html) || !nzchar(page_html)) {
    stop(
      "set_sentinel: input '", input_id, "' is not rendered in the DOM. ",
      "Consumer pickers gate on upstream column scope — provide an ",
      "upload / source resolution / Controls-subtab activation before ",
      "driving this widget."
    )
  }
  # Element exists in the DOM but Shiny's input binding may not have wired
  # up yet (the renderUI-vs-bindAll race that produces shinytest2's
  # "Unable to find input binding" error). See `wait_for_input_binding()`
  # in helper-vignette-apps.R for the timeout rationale.
  if (bind_timeout_ms > 0) {
    wait_for_input_binding(app, input_id, bind_timeout_ms)
  }
  args <- stats::setNames(list(sentinel), input_id)
  do.call(app$set_inputs, c(args, list(wait_ = FALSE)))
}

# Click the draw/update button and wait for the app to go idle.
# Default button_id matches ptr_app()'s convention (see
# helper-vignette-apps.R::draw()).
draw_and_wait <- function(app, button_id = "draw") {
  app$click(button_id)
  app$wait_for_idle(timeout = 25 * 1000)
}

# Flip the B3 mode radio. Input id `ptr_code_mode` matches what
# ptr_register_code() registers (verified against R/paintr-server.R and
# R/paintr-app.R). Refuses any other mode string up-front so callers cannot
# smuggle a typo into a test label. Post-ADR-0022 only "final" and "spec"
# are valid radio values; the legacy "preserve" choice was retired and its
# render-walker shape is unit-tested directly in test-render-preserve.R.
toggle_code_mode <- function(app, mode) {
  if (!isTRUE(mode %in% c("final", "spec"))) {
    stop("mode must be 'final' or 'spec'")
  }
  set_sentinel(app, "ptr_code_mode", mode)
  app$wait_for_idle(timeout = 25 * 1000)
}

# Assert: the literal `sentinel` appears in the code-panel output at a
# position consistent with `position_regex`, under the named `mode`.
#
# Contract (refuses to weaken):
#   1. code_text <- app$get_value(output = code_output_id)
#   2. regexpr(position_regex, code_text, perl = TRUE) must match
#   3. The matched substring (NOT the whole code_text) must contain the
#      literal `sentinel` (fixed-string check).
#
# Any of: code_text empty, regex doesn't match, regex matches but the
# matched substring lacks the literal sentinel → expectation fails,
# label names all four inputs (code_output_id, sentinel,
# position_regex, mode).
expect_sentinel_in_code <- function(app, code_output_id, sentinel,
                                    position_regex, mode) {
  label_inputs <- sprintf(
    "code_output_id=%s, sentinel=%s, position_regex=%s, mode=%s",
    code_output_id, sentinel, position_regex, mode
  )
  code_text <- app$get_value(output = code_output_id)
  if (!is.character(code_text) || length(code_text) != 1L ||
        !nzchar(code_text)) {
    testthat::expect_true(
      FALSE,
      label = paste0(
        "expect_sentinel_in_code: code panel value is empty/non-scalar; ",
        label_inputs
      )
    )
    return(invisible(NULL))
  }
  m <- regexpr(position_regex, code_text, perl = TRUE)
  if (m[[1L]] < 0L) {
    testthat::expect_true(
      FALSE,
      label = paste0(
        "expect_sentinel_in_code: position_regex did NOT match in code text; ",
        label_inputs,
        "; actual code_text=", code_text
      )
    )
    return(invisible(NULL))
  }
  match_len <- attr(m, "match.length")
  matched_substr <- substr(code_text, m[[1L]],
                           m[[1L]] + match_len[[1L]] - 1L)
  testthat::expect_true(
    grepl(sentinel, matched_substr, fixed = TRUE),
    label = paste0(
      "expect_sentinel_in_code: position_regex matched but its capture did ",
      "NOT contain the literal sentinel; ",
      label_inputs,
      "; actual_capture=", matched_substr
    )
  )
  invisible(NULL)
}

# Assert: the literal `sentinel` does NOT appear anywhere in the rendered
# code-panel output. Used for the inverse claim in final mode when only
# preserve mode should reveal a placeholder wrapper. Fixed-string check
# (no regex semantics — the caller passed a literal token).
expect_sentinel_nowhere <- function(app, code_output_id, sentinel) {
  code_text <- app$get_value(output = code_output_id) %||% ""
  testthat::expect_false(
    grepl(sentinel, code_text, fixed = TRUE),
    label = paste0(
      "expect_sentinel_nowhere: sentinel ", sentinel,
      " appeared in #", code_output_id, "; actual code_text=", code_text
    )
  )
}

# Assert: the host output `host_id` shows no Shiny error class. ggpaintr
# emits the standard `shiny-output-error` class on a server-side render
# error and the project's own `ptr-alert--error` block on an inline-error
# pane. Default host_id matches ptr_app()'s default plot host.
#
# NARROW BY DESIGN: this helper checks ONLY the named host element. Inline
# errors that route into a sibling host (e.g. `#ptr_error`) are out of
# scope -- many super-pressure scenarios DELIBERATELY produce an inline
# error in the sibling host (validate_input rejections, denylist probe)
# and then assert the *plot* host stays clean. Use
# `expect_no_inline_error_anywhere` below when you need the stronger
# "no error pane anywhere" assertion. (Note: `expect_no_inline_error` in
# helper-vignette-apps.R is the per-error-host check; named differently
# to avoid a collision.)
expect_no_plot_error <- function(app, host_id = "ptr_plot") {
  html <- app$get_html(paste0("#", host_id)) %||% ""
  testthat::expect_false(
    grepl("shiny-output-error", html, fixed = TRUE) ||
      grepl("ptr-alert--error", html, fixed = TRUE),
    label = paste0(
      "expect_no_plot_error: host #", host_id,
      " contained a shiny-output-error or ptr-alert--error class"
    )
  )
}

# Assert: NO `.ptr-alert--error` block appears anywhere in the document.
#
# This is the stronger document-scoped check that catches missing-aesthetic
# / stat-failure errors that ggpaintr routes into a SIBLING error host
# (e.g. `#ptr_error` in the default host). The narrow `expect_no_plot_error`
# above looks only at `#ptr_plot`, which can carry a stale-but-successful
# `<img>` while the actual error renders elsewhere -- exactly the slip that
# let `aes(y = ppVar(adj))` empty-default through the existing super-1
# test (see test-consumer-default-derived-column.R for the bug context).
#
# Use this when the scenario expects EVERYTHING to be clean. Do NOT use it
# in tests that deliberately produce a validate_input / denylist rejection
# (those should use `expect_no_plot_error` alone).
expect_no_inline_error_anywhere <- function(app) {
  alert_html <- app$get_html(".ptr-alert--error") %||% ""
  testthat::expect_false(
    nzchar(alert_html),
    label = "expect_no_inline_error_anywhere: a .ptr-alert--error block was found in the document"
  )
}

# Assert: the picker input `input_id` has `choice` as its current selected
# value (not merely as one of its offered options). Counterpart to
# `expect_picker_populated`, which is intentionally a presence proxy on the
# OPTIONS list — that helper passes when `<option value="adj">` is in the
# HTML, regardless of whether `adj` is the selection. Use this stronger
# helper whenever the consumer default is a derived column from an upstream
# placeholder (e.g. `aes(y = ppVar(adj))` over `mutate(adj = ppExpr(...))`):
# the bug shape there is a picker that *offers* the column but doesn't
# *select* it, which would slip past `expect_picker_populated`.
expect_picker_selected <- function(app, input_id, choice) {
  value <- tryCatch(
    app$get_value(input = input_id),
    error = function(e) NULL
  )
  testthat::expect_equal(
    as.character(value %||% ""), as.character(choice),
    label = paste0("picker #", input_id, " selected value")
  )
}

# ---------------------------------------------------------------------------
# Boot-state reference oracle (handoff deliverable (a); gap #2:
# "the reference.R oracle is never consulted").
#
# Each super-app fixture ships a sibling `reference.R` -- the Path-B
# expression (every `pp*(default)` collapses to its positional default under
# the identity runtimes). Nothing in the suite ever diffed the app's
# *first-render* code against it, so the shared-consumer boot-default discard
# (cddc46e) lived entirely in the untouched boot state: every scenario drove
# the shared widget before asserting, masking the seed path.
#
# This oracle closes that gap with a DELIBERATELY NARROW granularity
# (chosen 2026-05-28): it does NOT do a brittle `boot == reference` string
# diff (that false-positives on every upload-headed layer -- df_rug/df_main
# legitimately diverge at boot; see handoff trap). It extracts only the
# CONSUMER-placeholder defaults (the `pp*(col)` calls that sit in an `aes()`
# slot or a `facet_wrap(vars(...))`), derives the column each slot SHOULD
# seed to from reference.R, and asserts the untouched boot `ptr_code`
# (final mode) carries that exact `slot = col` mapping. The extraction is a
# pure AST read of the reference source -- independent of ggpaintr's runtime
# seeding logic, which is the code path that had the bug, so the oracle is
# NOT circular.
#
# Upload-scoped slots are excluded structurally: any aes() under a layer (or
# the root ggplot) whose `data =` resolves through a `ppUpload(...)` is
# skipped. That is the documented exclusion for super-2a (all-upload) and
# super-2b's geom_rug.

# Is `x` a placeholder call `pp<Keyword>(...)` (head is a bare `pp[A-Z]...`
# symbol)? `dplyr::filter(...)` etc. have a `::` call head, not a symbol, so
# they are correctly rejected.
.ggp_is_pp_call <- function(x) {
  is.call(x) && is.symbol(x[[1]]) && grepl("^pp[A-Z]", as.character(x[[1]]))
}

# Does the AST subtree `x` contain a `ppUpload(...)` call anywhere? Used to
# decide whether a data scope is upload-headed (-> exclude its aes slots).
.ggp_subtree_has_upload <- function(x) {
  if (is.symbol(x) || is.atomic(x)) return(FALSE)
  if (is.call(x)) {
    if (is.symbol(x[[1]]) && identical(as.character(x[[1]]), "ppUpload")) {
      return(TRUE)
    }
    return(any(vapply(as.list(x), .ggp_subtree_has_upload, logical(1))))
  }
  FALSE
}

# First *positional* (unnamed) argument of a pp call, deparsed to a string:
# `ppVar(mpg)` -> "mpg", `ppFactor(Species, shared = "fac")` -> "Species".
# That positional is the formula default the consumer must seed to.
.ggp_pp_default <- function(call) {
  args <- as.list(call)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  pos <- which(!nzchar(nms))
  if (!length(pos)) return(NA_character_)
  paste(deparse(args[[pos[[1L]]]]), collapse = "")
}

# The `shared = "<key>"` arg of a pp call, or NULL if not shared. A shared
# consumer's widget id is the host-owned `shared_<key>` (never namespaced),
# so the oracle can additionally assert `expect_picker_selected` on it -- the
# precise surface of the shared-consumer boot-default bug.
.ggp_shared_key <- function(call) {
  args <- as.list(call)[-1L]
  nms <- names(args)
  if (is.null(nms) || !("shared" %in% nms)) return(NULL)
  key <- args[["shared"]]
  if (is.character(key) && length(key) == 1L) key else NULL
}

# Collect consumer mappings (slot, default, shared_key) from one aes() call,
# unless its data scope is upload-headed.
.ggp_mappings_from_aes <- function(aes_call, upload_scope) {
  if (isTRUE(upload_scope)) return(list())
  args <- as.list(aes_call)[-1L]
  nms <- names(args)
  if (is.null(nms)) return(list())
  out <- list()
  for (i in seq_along(args)) {
    if (nzchar(nms[[i]]) && .ggp_is_pp_call(args[[i]])) {
      out[[length(out) + 1L]] <- list(
        slot = nms[[i]],
        default = .ggp_pp_default(args[[i]]),
        shared_key = .ggp_shared_key(args[[i]])
      )
    }
  }
  out
}

# Walk one top-level component of the `+` chain (the ggplot head OR a layer /
# facet / scale / labs call), collecting consumer mappings. `root_upload` is
# whether the root data scope is upload-headed; a layer with its own
# `data =` overrides it.
.ggp_mappings_from_component <- function(comp, root_upload) {
  if (!is.call(comp)) return(list())
  layer_upload <- root_upload
  comp_args <- as.list(comp)[-1L]
  comp_nms <- names(comp_args)
  if (!is.null(comp_nms) && "data" %in% comp_nms) {
    layer_upload <- .ggp_subtree_has_upload(comp_args[["data"]])
  }
  out <- list()
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    head <- if (is.symbol(x[[1]])) as.character(x[[1]]) else ""
    if (identical(head, "aes")) {
      out <<- c(out, .ggp_mappings_from_aes(x, layer_upload))
      return(invisible())
    }
    if (head %in% c("facet_wrap", "facet_grid")) {
      if (!isTRUE(layer_upload)) {
        for (el in as.list(x)[-1L]) {
          if (is.call(el) && is.symbol(el[[1]]) &&
                identical(as.character(el[[1]]), "vars")) {
            for (v in as.list(el)[-1L]) {
              if (.ggp_is_pp_call(v)) {
                out <<- c(out, list(list(
                  slot = "facet",
                  default = .ggp_pp_default(v),
                  shared_key = .ggp_shared_key(v)
                )))
              }
            }
          }
        }
      }
      return(invisible())
    }
    for (el in as.list(x)[-1L]) rec(el)
  }
  rec(comp)
  out
}

# Flatten an `a + b + c` ggplot expression into list(a, b, c).
.ggp_flatten_plus <- function(expr) {
  if (is.call(expr) && is.symbol(expr[[1]]) &&
        identical(as.character(expr[[1]]), "+")) {
    c(.ggp_flatten_plus(expr[[2]]), list(expr[[3]]))
  } else {
    list(expr)
  }
}

# Parse a fixture's reference.R and return the ggplot formula AST with `!!`
# unquoting already resolved. Evaluates the file's local bindings (color_var,
# my_linewidth, df_*, y_arg, ...) in a sandbox so `rlang::expr()`/`paste0()`
# resolve, but SKIPS every `ptr_define_placeholder_*()` call (those mutate the
# process-global registry -- a side effect that must not leak into the parent
# test process) and SKIPS the trailing `eval(...)`.
#   formula_name = "formula1" / "formula" / "formula_a" -> that bound object
#                  (a call, used as-is; a character string -> parse_expr).
#   formula_name = NULL -> the last bare top-level expression (super-2a writes
#                  its ggplot chain bare, with no assignment).
.ggp_parse_reference_formula <- function(ref_path, formula_name = NULL) {
  exprs <- parse(ref_path)
  env <- new.env(parent = globalenv())
  last_bare <- NULL
  is_assign <- function(e) {
    is.call(e) && is.symbol(e[[1]]) &&
      as.character(e[[1]]) %in% c("<-", "=")
  }
  is_define_rhs <- function(rhs) {
    is.call(rhs) && is.symbol(rhs[[1]]) &&
      grepl("^ptr_define_placeholder", as.character(rhs[[1]]))
  }
  is_named_call <- function(e, nm) {
    is.call(e) && is.symbol(e[[1]]) && identical(as.character(e[[1]]), nm)
  }
  for (e in exprs) {
    if (is_assign(e)) {
      if (is_define_rhs(e[[3]])) next            # skip registry mutation
      eval(e, envir = env)
    } else if (is_named_call(e, "library") ||
                 is_named_call(e, "require") ||
                 is_named_call(e, "eval")) {
      next                                       # skip side-effecting calls
    } else {
      last_bare <- e                             # candidate bare formula
    }
  }
  if (!is.null(formula_name)) {
    obj <- get(formula_name, envir = env)
    if (is.character(obj)) return(rlang::parse_expr(obj))
    return(obj)
  }
  if (is.null(last_bare)) {
    stop(".ggp_parse_reference_formula: no formula_name and no bare ",
         "top-level expression in ", ref_path)
  }
  last_bare
}

# Public collector: reference.R path/name -> list of consumer mappings.
ggp_reference_consumer_mappings <- function(ref_path, formula_name = NULL) {
  fml <- .ggp_parse_reference_formula(ref_path, formula_name)
  components <- .ggp_flatten_plus(fml)
  ggplot_comp <- NULL
  for (comp in components) {
    if (is.call(comp) && is.symbol(comp[[1]]) &&
          identical(as.character(comp[[1]]), "ggplot")) {
      ggplot_comp <- comp
      break
    }
  }
  root_upload <- FALSE
  if (!is.null(ggplot_comp)) {
    gargs <- as.list(ggplot_comp)[-1L]
    gnms <- names(gargs)
    data_arg <- if (!is.null(gnms) && "data" %in% gnms) {
      gargs[["data"]]
    } else if (length(gargs) >= 1L) {
      gargs[[1L]]
    } else {
      NULL
    }
    if (!is.null(data_arg)) root_upload <- .ggp_subtree_has_upload(data_arg)
  }
  out <- list()
  for (comp in components) {
    out <- c(out, .ggp_mappings_from_component(comp, root_upload))
  }
  out
}

# ---------------------------------------------------------------------------
# SOURCE-default boot oracle (the symmetric counterpart of
# ggp_reference_consumer_mappings, added 2026-05-29). The consumer-default
# oracle is structurally BLIND to source placeholders, so a strip of a
# source's positional default -- `ppUpload(df_main)` -> `ppUpload()`, exactly
# what 332f7b7 did to super-2a's app.R -- slipped through the boot-oracle file
# (it was caught only by test-source-shortcut-preserves). This collector +
# expectation close that gap: under ADR 0025 §6/§8 a source's positional
# default is the seed for its shortcut textbox at boot, so the booted shortcut
# value must equal reference.R's `ppUpload(<default>)`.
#
# Scope: `ppUpload` (the only built-in shortcut source). Custom shortcut
# sources would need their keyword added here; super-2b's df_rug source is
# already covered end-to-end by its boot-PLOT oracle, so this is wired for the
# all-upload super-2a fixture, which has no other boot-state oracle.

# Multiset (sorted) of `ppUpload(<default>)` positional defaults declared in a
# reference formula; bare `ppUpload()` (or `ppUpload(shared=...)` with no
# positional) contributes "" -- the empty shortcut seed.
ggp_reference_source_defaults <- function(ref_path, formula_name = NULL) {
  fml <- .ggp_parse_reference_formula(ref_path, formula_name)
  defs <- character(0)
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    if (is.symbol(x[[1]]) && identical(as.character(x[[1]]), "ppUpload")) {
      d <- .ggp_pp_default(x)
      defs[[length(defs) + 1L]] <<- if (is.na(d)) "" else d
    }
    for (el in as.list(x)[-1L]) rec(el)
  }
  rec(fml)
  sort(defs)
}

# Multiset (sorted) of the boot values of every source-shortcut textbox in a
# booted app. Source shortcut ids end in "_shortcut" (R/paintr-ids.R; the
# suffix is reserved by ADR 0025's validator). Regex on rendered HTML, the
# same xml2-free approach as test-source-shortcut-preserves' sweep.
ggp_boot_source_shortcut_values <- function(app) {
  html <- app$get_html("body")
  m <- regmatches(
    html,
    gregexpr('id="([A-Za-z0-9_]+_shortcut)"', html, perl = TRUE)
  )[[1]]
  ids <- unique(gsub('^id="|"$', "", m))
  vals <- vapply(ids, function(id) {
    v <- tryCatch(app$get_value(input = id), error = function(e) NA_character_)
    as.character(v %||% "")
  }, character(1))
  sort(unname(vals))
}

# Assert booted source-shortcut seeds == reference.R's ppUpload() defaults.
expect_boot_source_defaults_match_reference <- function(app, slug,
                                                        formula_name = NULL) {
  ref_path <- testthat::test_path("fixtures", "vignette-apps", slug,
                                  "reference.R")
  expected <- ggp_reference_source_defaults(ref_path, formula_name)
  actual <- ggp_boot_source_shortcut_values(app)
  testthat::expect_equal(
    actual, expected,
    label = paste0(
      "booted source-shortcut seeds for ", slug,
      " must match reference.R ppUpload() defaults (a stripped default ",
      "boots \"\" and fails here)"
    )
  )
  invisible(expected)
}

# Source a SINGLE-PLOT reference.R and return both the ground-truth ggplot
# (its trailing `eval(formula)`) and the populated sandbox env (custom
# placeholder runtimes + local data bindings like `df_rug`). A caller can
# then eval the APP's emitted final-mode code in the SAME env and compare
# `ggplot_build()$data` -- the robust "same plot" assertion (object identity
# fails: every ggplot carries a distinct plot_env; built layer data does
# not). Unlike `.ggp_parse_reference_formula` (which SKIPS the
# `ptr_define_placeholder_*` calls to avoid registry mutation), this loader
# MUST run them so the runtimes exist for Path-B eval; it snapshots the
# registry keyword set and removes any keys the source added on exit, so the
# mutation does not leak into sibling tests. Only valid for single-plot
# references (super-1/2b/4 tail-eval shape); multi-cell refs (super-3
# formula_a/formula_b) need a formula-name-aware variant.
ggp_reference_plot <- function(slug, formula_name = NULL) {
  ref_path <- testthat::test_path("fixtures", "vignette-apps", slug,
                                  "reference.R")
  before <- ggpaintr:::ptr_registry_keywords()
  on.exit({
    added <- setdiff(ggpaintr:::ptr_registry_keywords(), before)
    for (k in added) {
      if (exists(k, envir = ggpaintr:::.ptr_registry, inherits = FALSE)) {
        rm(list = k, envir = ggpaintr:::.ptr_registry)
      }
    }
  }, add = TRUE)
  env <- new.env(parent = globalenv())
  # `formula_name = NULL` -> the trailing `eval(...)` value (single-plot refs:
  # super-1 `eval(formula1)`, super-2b `eval(formula)`, super-4
  # `eval(parse(text = formula_text))`). For multi-cell refs (super-3 evals
  # `formula_a` then `formula_b`), pass the formula object's name and eval
  # THAT one in the populated sandbox.
  val <- source(ref_path, local = env)$value
  p <- if (is.null(formula_name)) {
    val
  } else {
    eval(get(formula_name, envir = env), envir = env)
  }
  list(plot = p, env = env)
}

# Assert: the APP's boot final-mode code, eval'd in the reference sandbox,
# builds to the SAME plot (layer-by-layer `ggplot_build()$data`) as
# reference.R. `boot_super_app()` must already have been booted; this clicks
# Draw, switches the code panel to final mode, reads `ptr_code`, evals it in
# `ref$env`, and compares built data. Pairs with `expect_no_plot_error()`
# (which proves the LIVE server render did not error) -- the two catch
# different failure classes: a wrong-but-evaluable emitted expression
# (caught here) vs. a runtime resolution failure whose emitted code is still
# textually fine (caught there, e.g. an unbound `df_rug` shortcut).
expect_boot_plot_matches_reference <- function(app, ref,
                                               code_output_id = "ptr_code",
                                               draw_button_id = "ptr_update_plot",
                                               code_mode_id = "ptr_code_mode") {
  app$click(draw_button_id)
  app$wait_for_idle(timeout = 25 * 1000)
  # Document-wide error check: the unbound-`df_rug` boot error routes into the
  # inline `.ptr-alert--error` pane, NOT `#ptr_plot`, so the narrow
  # `expect_no_plot_error` (host-only) would miss it. Use the stronger
  # any-error-anywhere check so a failed live render is actually caught.
  expect_no_inline_error_anywhere(app)
  set_sentinel(app, code_mode_id, "final")
  app$wait_for_idle(timeout = 25 * 1000)
  code <- app$get_value(output = code_output_id)
  testthat::expect_true(is.character(code) && nzchar(code),
    label = "boot final-mode ptr_code is a non-empty string")
  p_app <- eval(parse(text = code), envir = ref$env)
  testthat::expect_s3_class(p_app, "ggplot")
  testthat::expect_equal(
    ggplot2::ggplot_build(p_app)$data,
    ggplot2::ggplot_build(ref$plot)$data
  )
  invisible(p_app)
}

# Assert: the untouched boot `ptr_code` (final mode, no widget driven)
# carries every consumer-default mapping that reference.R declares.
#
# Granularity is per-slot (handoff: "decide the comparison granularity
# deliberately"): for each consumer mapping the oracle asserts the boot code
# contains `<slot> = <default-col>` via the same trusted per-slot regex the
# super-pressure suite uses (expect_sentinel_in_code), and for SHARED
# consumers additionally asserts the host-owned `shared_<key>` picker has
# `<default-col>` selected. Upload-scoped aes slots are excluded by the
# collector. Returns the mappings invisibly so a caller can assert the count
# (e.g. the super-2a all-upload exclusion expects length 0).
# `xfail_shared_keys`: shared keys whose boot-default seeding is a KNOWN,
# handed-off product bug. Their slot + `shared_<key>` picker assertions are
# wrapped in `testthat::expect_failure()` so the gate stays FAIL 0 / SKIP 0
# AND flips loudly the moment the bug is fixed (forcing this pin to be
# removed). Non-shared slots in the same fixture stay actively asserted.
# Used for super-3's L3 host-scope shared consumer `linked` -- see
# .scratch/super3-l3-shared-consumer-boot-default/.
expect_boot_matches_reference <- function(app, slug, formula_name = NULL,
                                          code_output_id = "ptr_code",
                                          draw_button_id = "ptr_update_plot",
                                          code_mode_id = "ptr_code_mode",
                                          shared_prefix = "",
                                          xfail_shared_keys = character()) {
  ref_path <- testthat::test_path("fixtures", "vignette-apps", slug,
                                  "reference.R")
  mappings <- ggp_reference_consumer_mappings(ref_path, formula_name)

  # First render at boot defaults -- NO set_sentinel, no widget driven.
  app$click(draw_button_id)
  app$wait_for_idle(timeout = 25 * 1000)
  set_sentinel(app, code_mode_id, "final")
  app$wait_for_idle(timeout = 25 * 1000)

  for (m in mappings) {
    if (is.na(m$default) || !nzchar(m$default)) next
    xfail <- !is.null(m$shared_key) && m$shared_key %in% xfail_shared_keys
    slot_assert <- function() {
      if (identical(m$slot, "facet")) {
        expect_sentinel_in_code(app, code_output_id, m$default,
          "facet_wrap\\(vars\\(([^)]*)", "final")
      } else {
        regex <- sprintf("aes\\([^)]*%s\\s*=\\s*([^,)]*)", m$slot)
        expect_sentinel_in_code(app, code_output_id, m$default, regex, "final")
      }
    }
    if (xfail) {
      testthat::expect_failure(
        slot_assert(),
        info = paste0("KNOWN BUG (handed off): shared consumer '", m$shared_key,
                      "' does not seed default '", m$default,
                      "' at boot in this host scope; see ",
                      ".scratch/super3-l3-shared-consumer-boot-default/")
      )
    } else {
      slot_assert()
    }
    if (!is.null(m$shared_key)) {
      pid <- paste0(shared_prefix, "shared_", m$shared_key)
      if (xfail) {
        testthat::expect_failure(expect_picker_selected(app, pid, m$default))
      } else {
        expect_picker_selected(app, pid, m$default)
      }
    }
  }
  invisible(mappings)
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
