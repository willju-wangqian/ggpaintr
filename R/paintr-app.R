#' Build Standard Output Ids for ggpaintr Integration
#'
#' Create a validated set of top-level output and control ids for embedding the
#' `ggpaintr` runtime inside a larger Shiny app.
#'
#' @param control_panel Output id used for the generated control panel.
#' @param draw_button Input id used for the draw button.
#' @param plot_output Output id used for the plot output.
#' @param error_output Output id used for the inline error UI.
#' @param code_output Output id used for the generated code output.
#'
#' @return An object of class `ptr_build_ids`.
#' @examples
#' ids <- ptr_build_ids(
#'   control_panel = "custom_controls",
#'   draw_button = "run_plot"
#' )
#' ids$control_panel
#' @export
ptr_build_ids <- function(control_panel = "controlPanel",
                         draw_button = "draw",
                         plot_output = "outputPlot",
                         error_output = "outputError",
                         code_output = "outputCode") {
  ids <- list(
    control_panel = control_panel,
    draw_button = draw_button,
    plot_output = plot_output,
    error_output = error_output,
    code_output = code_output
  )

  ptr_validate_ids(ids)
  structure(ids, class = c("ptr_build_ids", "list"))
}

#' Validate a ggpaintr Id Registry
#'
#' @param ids A named list of ids.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_ids <- function(ids) {
  required_names <- c(
    "control_panel",
    "draw_button",
    "plot_output",
    "error_output",
    "code_output"
  )

  if (!is.list(ids) || is.null(names(ids))) {
    rlang::abort("ids must be a named list.")
  }

  missing_names <- setdiff(required_names, names(ids))
  if (length(missing_names) > 0) {
    rlang::abort(paste0("ids is missing required entries: ", paste(missing_names, collapse = ", "), "."))
  }

  extra_names <- setdiff(names(ids), required_names)
  if (length(extra_names) > 0) {
    rlang::abort(paste0("ids has unsupported entries: ", paste(extra_names, collapse = ", "), "."))
  }

  for (name in required_names) {
    value <- ids[[name]]
    if (!is.character(value) || length(value) != 1 || identical(trimws(value), "")) {
      rlang::abort(paste0("ids$", name, " must be a single non-empty string."))
    }
  }

  duplicate_ids <- unique(unname(ids)[duplicated(unname(ids))])
  if (length(duplicate_ids) > 0) {
    rlang::abort(paste0("ids must be unique. Duplicated values: ", paste(duplicate_ids, collapse = ", "), "."))
  }

  invisible(TRUE)
}

#' Normalize a ggpaintr Id Registry
#'
#' @param ids A `ptr_build_ids` object or named list.
#'
#' @return A validated `ptr_build_ids` object.
#' @noRd
ptr_normalize_ids <- function(ids = NULL) {
  if (is.null(ids)) {
    return(ptr_build_ids())
  }

  if (inherits(ids, "ptr_build_ids")) {
    return(ids)
  }

  ptr_validate_ids(ids)
  structure(ids, class = c("ptr_build_ids", "list"))
}

#' Build Top-Level ID Contract for ggpaintr State
#'
#' @param raw_ids A `ptr_build_ids` object or named list.
#' @param ui_ns Namespace function for rendered DOM ids.
#' @param server_ns Namespace function for Shiny input/output binding keys.
#'
#' @return A named list with `raw_ids`, `ui_ids`, and `server_ids`.
#' @noRd
ptr_build_id_contract <- function(raw_ids = ptr_build_ids(),
                                  ui_ns = shiny::NS(NULL),
                                  server_ns = ui_ns) {
  if (!is.function(ui_ns)) {
    rlang::abort("`ui_ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  if (!is.function(server_ns)) {
    rlang::abort("`server_ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }

  raw_ids <- ptr_normalize_ids(raw_ids)
  ui_ids <- structure(
    lapply(raw_ids, ui_ns),
    class = class(raw_ids)
  )
  server_ids <- structure(
    lapply(raw_ids, server_ns),
    class = class(raw_ids)
  )

  list(
    raw_ids = raw_ids,
    ui_ids = ui_ids,
    server_ids = server_ids
  )
}

#' Validate a ggpaintr State Object
#'
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_collect_stale_layers <- function(ptr_state) {
  env <- ptr_state$is_stale_env
  if (is.null(env) || !is.environment(env)) return(character())
  layer_names <- ls(env)
  if (length(layer_names) == 0L) return(character())
  is_stale <- vapply(layer_names, function(ln) {
    isTRUE(tryCatch(env[[ln]](), error = function(e) FALSE))
  }, logical(1))
  layer_names[is_stale]
}

ptr_validate_state <- function(ptr_state) {
  required_names <- c(
    "obj",
    "runtime",
    "extras",
    "var_ui_list",
    "shared_env_reactive",
    "raw_ui_text",
    "effective_ui_text",
    "placeholders",
    "custom_placeholders",
    "raw_ids",
    "ui_ids",
    "server_ids",
    "ids",
    "envir",
    "checkbox_defaults"
  )

  if (!inherits(ptr_state, "ptr_state")) {
    rlang::abort("ptr_state must inherit from 'ptr_state'.")
  }

  missing_names <- setdiff(required_names, names(ptr_state))
  if (length(missing_names) > 0) {
    rlang::abort(paste0("ptr_state is missing required entries: ", paste(missing_names, collapse = ", "), "."))
  }

  if (!is.function(ptr_state$obj) ||
      !is.function(ptr_state$runtime) ||
      !is.function(ptr_state$extras) ||
      !is.function(ptr_state$var_ui_list)) {
    rlang::abort("ptr_state reactive accessors must be functions.")
  }

  ptr_validate_ids(ptr_state$raw_ids)
  ptr_validate_ids(ptr_state$ui_ids)
  ptr_validate_ids(ptr_state$server_ids)
  ptr_validate_ids(ptr_state$ids)
  if (!inherits(ptr_state$placeholders, "ptr_define_placeholder_registry")) {
    rlang::abort("ptr_state$placeholders must inherit from 'ptr_define_placeholder_registry'.")
  }
  if (!is.null(ptr_state$safe_to_remove)) {
    validate_safe_to_remove(
      ptr_state$safe_to_remove,
      arg = "ptr_state$safe_to_remove"
    )
  }
  invisible(TRUE)
}

#' Build Reactive Server State for ggpaintr
#'
#' Create the shared reactive state used by the extensible `ptr_*` server
#' helpers. This object can be passed to the bind helpers or inspected directly
#' inside a larger Shiny app.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param checkbox_defaults Optional named list controlling the initial checked
#'   state of each layer's "include this layer" checkbox at app launch. Names
#'   match layer names from the formula (use `names(parsed$expr_list)` to
#'   inspect; duplicate layers receive a hyphen-numeric suffix starting at
#'   `-2`, e.g. a formula with two `geom_point()` calls produces layer names
#'   `geom_point` and `geom_point-2`). Each value is a single logical or a
#'   logical vector applied positionally over consecutive instances of that
#'   layer; vectors shorter than the count of instances are padded with `TRUE`
#'   and longer vectors are truncated with a warning. A deduped key wrapped in
#'   backticks (e.g. `` `geom_point-2` ``) addresses one specific instance.
#'   `NA` and non-logical values raise an error; unrecognized names raise a
#'   warning and are ignored. Default `NULL` keeps every layer checked
#'   (current behavior).
#' @param expr_check Controls `expr` placeholder validation.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Extends the curated default set: `theme()`, `labs()`,
#'   `xlab()`, `ylab()`, `ggtitle()`, `facet_wrap()`, `facet_grid()`,
#'   `facet_null()`, `xlim()`, `ylim()`, `lims()`, `expand_limits()`,
#'   `guides()`, `annotate()`, `annotation_custom()`, `annotation_map()`,
#'   `annotation_raster()`, `aes()`, `aes_()`, `aes_q()`, `aes_string()`,
#'   `vars()`, `element_text()`, `element_line()`, `element_rect()`,
#'   `element_point()`, `element_polygon()`, `element_geom()`. `geom_*()` /
#'   `stat_*()` standalone layers are always preserved. Defaults to `character()`.
#' @param shared Named list of Shiny reactives (or `NULL`/empty list).
#'   Each name must match a `shared = "<id>"` annotation on a placeholder
#'   in the formula. When a placeholder carries a `shared` annotation, its
#'   per-instance widget is suppressed and its runtime value is read from
#'   the matching reactive instead. Use this to drive multiple ptr_obj
#'   instances from a single externally-rendered control. Defaults to an
#'   empty list (no shared bindings).
#' @param draw_trigger Optional Shiny reactive (or `NULL`). When non-`NULL`,
#'   firing the reactive triggers a redraw in addition to the local draw
#'   button. Used by [ptr_app_grid()] to wire a top-level "draw all" button
#'   into every plot. Defaults to `NULL`.
#' @param ns An optional namespace function (`character -> character`) used to
#'   render UI ids and, by default, Shiny server binding ids. Pass
#'   `shiny::NS("page1")` to avoid id collisions when embedding two or more
#'   ggpaintr formulas in the same root Shiny session. Defaults to
#'   `shiny::NS(NULL)` (identity — no prefixing).
#' @param server_ns Optional namespace function for Shiny `input` / `output`
#'   binding keys. Defaults to `ns`. The module wrapper uses
#'   `shiny::NS(NULL)` because `shiny::moduleServer()` already scopes server
#'   bindings while rendered UI ids still need the module namespace.
#' @return An object of class `ptr_state`.
#'   The returned state stores `raw_ids` (canonical unprefixed ids),
#'   `ui_ids` (rendered DOM ids), and `server_ids` (Shiny binding keys).
#'   `ids` is retained as a compatibility alias for `server_ids`.
#' @examples
#' state <- ptr_server_state(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' is.function(state$runtime)
#' @export
ptr_server_state <- function(formula,
                             envir = parent.frame(),
                             ui_text = NULL,
                             ids = ptr_build_ids(),
                             placeholders = NULL,
                             checkbox_defaults = NULL,
                             expr_check = TRUE,
                             safe_to_remove = character(),
                             shared = list(),
                             draw_trigger = NULL,
                             ns = shiny::NS(NULL),
                             server_ns = ns) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  if (!is.function(server_ns)) {
    rlang::abort("`server_ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  if (!is.null(draw_trigger) && !shiny::is.reactive(draw_trigger)) {
    rlang::abort("`draw_trigger` must be a Shiny reactive or NULL.")
  }
  shared_bindings <- ptr_validate_shared_bindings(shared)
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)
  id_contract <- ptr_build_id_contract(
    raw_ids = ids,
    ui_ns = ns,
    server_ns = server_ns
  )
  placeholder_registry <- ptr_merge_placeholders(placeholders)

  parsed <- ptr_parse_formula(formula, placeholders = placeholder_registry)

  resolved_checkbox_defaults <- ptr_resolve_checkbox_defaults(
    checkbox_defaults,
    parsed$expr_list
  )

  data_layer_names <- names(parsed$data_pipeline_info %||% list())
  resolved_data <- stats::setNames(
    lapply(data_layer_names, function(.) shiny::reactiveVal(NULL)),
    data_layer_names
  )
  last_click_inputs <- stats::setNames(
    lapply(data_layer_names, function(.) shiny::reactiveVal(NULL)),
    data_layer_names
  )

  structure(
    list(
      obj = shiny::reactiveVal(parsed),
      runtime = shiny::reactiveVal(NULL),
      extras = shiny::reactiveVal(list()),
      var_ui_list = shiny::reactiveVal(list()),
      shared_env_reactive = NULL,
      shared_bindings = shared_bindings,
      draw_trigger = draw_trigger,
      raw_ui_text = ui_text,
      effective_ui_text = ptr_merge_ui_text(
        ui_text,
        placeholders = placeholder_registry,
        known_param_keys = ptr_known_param_keys_from_obj(parsed)
      ),
      placeholders = placeholder_registry,
      custom_placeholders = placeholder_registry$custom_placeholders,
      raw_ids = id_contract$raw_ids,
      ui_ids = id_contract$ui_ids,
      server_ids = id_contract$server_ids,
      ids = id_contract$server_ids,
      envir = envir,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove,
      ui_ns_fn = ns,
      server_ns_fn = server_ns,
      ns_fn = server_ns,
      checkbox_defaults = resolved_checkbox_defaults,
      resolved_data = resolved_data,
      last_click_inputs = last_click_inputs,
      is_stale_env = new.env(parent = emptyenv())
    ),
    class = c("ptr_state", "list")
  )
}

#' Bind the Generated Control Panel into a Shiny App
#'
#' Register the dynamic `var` controls and render the standard tabbed control
#' panel into the target `uiOutput()`.
#'
#' @param input A Shiny `input` object.
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_setup_controls(input, output, ps)
#'   ptr_register_draw(input, ps)
#'   ptr_register_plot(output, ps)
#' }
#' }
#' @export
ptr_setup_controls <- function(input,
                               output,
                               ptr_state) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ptr_state$server_ids)

  shared_env_reactive_raw <- shiny::reactive({
    shiny::req(ptr_state$obj())
    obj <- ptr_state$obj()
    eval_env <- tryCatch(
      ptr_prepare_eval_env(
        obj,
        input,
        envir = ptr_state$envir,
        ns_fn = ptr_state$server_ns_fn %||% shiny::NS(NULL)
      ),
      error = function(e) NULL
    )
    var_column_map <- if (!is.null(eval_env)) {
      context <- ptr_define_placeholder_context(
        obj, ui_text = NULL, envir = ptr_state$envir,
        expr_check = ptr_state$expr_check
      )
      context$ns_fn <- ptr_state$server_ns_fn %||% shiny::NS(NULL)
      context$input <- input
      context$eval_env <- eval_env
      context$shared_bindings <- ptr_state$shared_bindings %||% list()
      tryCatch(
        ptr_build_var_column_map(
          obj, input, context, eval_env,
          resolved_data = ptr_state$resolved_data
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    list(eval_env = eval_env, var_column_map = var_column_map)
  })
  shared_env_reactive <- shiny::debounce(shared_env_reactive_raw, millis = 300)
  ptr_state$shared_env_reactive <- shared_env_reactive

  shiny::observe({
    shiny::req(ptr_state$obj())
    cached <- shared_env_reactive()
    result <- tryCatch(
      ptr_bind_placeholder_ui(
        input,
        output,
        ptr_state$obj(),
        envir = ptr_state$envir,
        ui_text = ptr_state$effective_ui_text,
        eval_env = cached$eval_env,
        var_column_map = cached$var_column_map,
        expr_check = ptr_state$expr_check,
        ns_fn = ptr_state$server_ns_fn %||% shiny::NS(NULL),
        ui_ns_fn = ptr_state$ui_ns_fn %||% shiny::NS(NULL)
      ),
      error = function(e) list()
    )
    ptr_state$var_ui_list(result)
  })

  output[[ids$control_panel]] <- shiny::renderUI({
    shiny::req(ptr_state$obj())
    obj <- ptr_state$obj()
    ui_ns_fn <- ptr_state$ui_ns_fn %||% shiny::NS(NULL)
    data_tab <- ptr_get_data_tab_ui(
      obj,
      ui_text = ptr_state$effective_ui_text,
      ns_fn = ui_ns_fn
    )
    layer_tab <- ptr_get_tab_ui(
      obj,
      ui_text = ptr_state$effective_ui_text,
      ns_fn = ui_ns_fn,
      checkbox_defaults = ptr_state$checkbox_defaults
    )
    shiny::column(
      12,
      if (!is.null(data_tab)) {
        shiny::tagList(shiny::h4("Data"), data_tab)
      },
      layer_tab
    )
  })

  ptr_setup_data_pipeline_observers(input, ptr_state)

  invisible(ptr_state)
}

# Wire reactives that maintain the per-layer resolved-data cache, drive the
# Update Data click observer, and toggle a stale CSS class on the button when
# the cache is no longer in sync with the data-placeholder inputs.
ptr_setup_data_pipeline_observers <- function(input, ptr_state) {
  obj <- shiny::isolate(ptr_state$obj())
  if (is.null(obj)) return(invisible(NULL))
  pipeline_info <- obj$data_pipeline_info %||% list()
  if (length(pipeline_info) == 0L) return(invisible(NULL))

  ui_ns_fn <- ptr_state$ui_ns_fn %||% shiny::NS(NULL)
  server_ns_fn <- ptr_state$server_ns_fn %||% shiny::NS(NULL)
  stale_class <- ptr_state$effective_ui_text$shell$update_data_stale_class %||% ""

  build_context <- function(o, ev) {
    ctx <- ptr_define_placeholder_context(
      o,
      ui_text = ptr_state$effective_ui_text,
      envir = ptr_state$envir,
      expr_check = ptr_state$expr_check,
      eval_env = ev
    )
    ctx$ns_fn <- server_ns_fn
    ctx$input <- input
    ctx$shared_bindings <- ptr_state$shared_bindings %||% list()
    ctx
  }

  # Initial seed: run the resolver with the current (empty) inputs so var
  # dropdowns have a starting column set. The snapshot of the placeholder
  # values at seed time is recorded so the stale flag stays FALSE until the
  # user actually changes a data-pipeline input.
  shiny::isolate({
    eval_env <- tryCatch(
      ptr_prepare_eval_env(
        obj, input,
        envir = ptr_state$envir,
        ns_fn = server_ns_fn
      ),
      error = function(e) NULL
    )
    if (!is.null(eval_env)) {
      seed_ctx <- build_context(obj, eval_env)
      for (layer_name in names(pipeline_info)) {
        res <- tryCatch(
          ptr_resolve_layer_data(obj, layer_name, input, seed_ctx, eval_env),
          error = function(e) list(has_data = FALSE, data = NULL)
        )
        if (isTRUE(res$has_data) && !is.null(res$data)) {
          ptr_state$resolved_data[[layer_name]](res$data)
        }
        snap <- tryCatch(
          ptr_snapshot_data_placeholder_inputs(obj, layer_name, input, seed_ctx),
          error = function(e) list()
        )
        ptr_state$last_click_inputs[[layer_name]](snap)
      }
    }
  })

  # `is_stale_env` is an environment (reference type) so reactives inserted
  # here are visible to the caller's copy of ptr_state without having to
  # rebuild the structure.
  is_stale_env <- ptr_state$is_stale_env
  for (layer_name in names(pipeline_info)) {
    local({
      ln <- layer_name
      is_stale_env[[ln]] <- shiny::reactive({
        o <- ptr_state$obj()
        ev <- tryCatch(
          ptr_prepare_eval_env(o, input, envir = ptr_state$envir, ns_fn = server_ns_fn),
          error = function(e) NULL
        )
        ctx <- build_context(o, ev)
        snap <- ptr_state$last_click_inputs[[ln]]()
        current <- tryCatch(
          ptr_snapshot_data_placeholder_inputs(o, ln, input, ctx),
          error = function(e) list()
        )
        !identical(snap, current)
      })
    })
  }

  for (layer_name in names(pipeline_info)) {
    local({
      ln <- layer_name
      btn_local_id <- ptr_update_data_input_id(ln)

      # Default ignoreNULL = TRUE keeps the handler from firing while the
      # button has not been clicked yet, so we leave ignoreInit at its
      # default. Some testServer harnesses suppress the first non-NULL value
      # under ignoreInit = TRUE, which would silently swallow the first real
      # click.
      shiny::observeEvent(input[[btn_local_id]], {
        o <- ptr_state$obj()
        eval_env <- tryCatch(
          ptr_prepare_eval_env(o, input, envir = ptr_state$envir, ns_fn = server_ns_fn),
          error = function(e) NULL
        )
        if (is.null(eval_env)) {
          cli::cli_warn("Could not prepare evaluation environment for layer {.val {ln}}.")
          return()
        }
        ctx <- build_context(o, eval_env)
        res <- tryCatch(
          ptr_resolve_layer_data(o, ln, input, ctx, eval_env),
          error = function(e) list(has_data = FALSE, data = NULL, error = e)
        )
        if (isTRUE(res$has_data) && !is.null(res$data)) {
          ptr_state$resolved_data[[ln]](res$data)
          snap <- tryCatch(
            ptr_snapshot_data_placeholder_inputs(o, ln, input, ctx),
            error = function(e) list()
          )
          ptr_state$last_click_inputs[[ln]](snap)
        } else {
          cli::cli_warn(
            "Failed to update data for layer {.val {ln}}; cache left unchanged."
          )
        }
      })

      if (nzchar(stale_class)) {
        shiny::observe({
          is_stale <- tryCatch(is_stale_env[[ln]](), error = function(e) FALSE)
          session <- shiny::getDefaultReactiveDomain()
          if (is.null(session)) return()
          ui_id <- ptr_ns_id(ui_ns_fn, btn_local_id)
          session$sendCustomMessage(
            "ptr_set_class",
            list(id = ui_id, class = stale_class, add = isTRUE(is_stale))
          )
        })
      }
    })
  }

  invisible(NULL)
}

#' Bind Draw Behavior into a Shiny App
#'
#' @param input A Shiny `input` object.
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_setup_controls(input, output, ps)
#'   ptr_register_draw(input, ps)
#'   ptr_register_plot(output, ps)
#' }
#' }
#' @export
ptr_register_draw <- function(input,
                               ptr_state) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ptr_state$server_ids)
  draw_trigger <- ptr_state$draw_trigger

  draw_handler <- function() {
    shiny::req(ptr_state$obj())
    cached <- tryCatch(ptr_state$shared_env_reactive(), error = function(e) NULL)
    snapshots <- lapply(
      ptr_state$last_click_inputs %||% list(),
      function(rv) tryCatch(rv(), error = function(e) NULL)
    )
    runtime_result <- ptr_complete_expr_safe(
      ptr_state$obj(),
      input,
      envir = ptr_state$envir,
      eval_env = cached$eval_env,
      var_column_map = cached$var_column_map,
      expr_check = ptr_state$expr_check,
      ns_fn = ptr_state$server_ns_fn %||% shiny::NS(NULL),
      safe_to_remove = ptr_state$safe_to_remove %||% character(),
      shared_bindings = ptr_state$shared_bindings %||% list(),
      resolved_data = ptr_state$resolved_data,
      last_click_inputs = snapshots
    )
    runtime_result <- ptr_assemble_plot_safe(runtime_result, envir = ptr_state$envir, expr_check = ptr_state$expr_check)
    runtime_result <- ptr_validate_plot_render_safe(runtime_result)
    ptr_state$runtime(runtime_result)
  }

  shiny::observeEvent(input[[ids$draw_button]], draw_handler())

  if (!is.null(draw_trigger)) {
    shiny::observeEvent(draw_trigger(), draw_handler(), ignoreInit = TRUE)
  }

  invisible(ptr_state)
}

#' Return the Built Plot from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ptr_exec()`.
#'
#' @return A `ggplot` object or `NULL`.
#' @examples
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ptr_exec(
#'   obj,
#'   inputs
#' )
#' inherits(ptr_extract_plot(runtime), "ggplot")
#' @export
ptr_extract_plot <- function(runtime_result) {
  if (is.null(runtime_result) || !isTRUE(runtime_result[["ok"]])) {
    return(NULL)
  }

  runtime_result[["plot"]]
}

#' Return Default Error UI from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ptr_exec()`.
#'
#' @return A Shiny tag or `NULL`.
#' @examples
#' if (interactive()) {
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ptr_exec(obj, inputs)
#' ptr_extract_error(runtime)
#' }
#' @export
ptr_extract_error <- function(runtime_result) {
  if (is.null(runtime_result) || isTRUE(runtime_result[["ok"]])) {
    return(NULL)
  }

  ptr_error_ui(runtime_result[["message"]])
}

#' Return Generated Code Text from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ptr_exec()`.
#' @param extras Optional list of quosures captured by [ptr_gg_extra()],
#'   appended to the runtime code text when the runtime succeeded. Extras
#'   are suppressed when `runtime_result$ok` is not `TRUE`, so stale extras
#'   from a prior successful draw never surface during a failed draw.
#'
#' @return A character string or `NULL`.
#' @examples
#' if (interactive()) {
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ptr_exec(obj, inputs)
#' ptr_extract_code(runtime)
#' }
#' @export
ptr_extract_code <- function(runtime_result, extras = NULL) {
  if (is.null(runtime_result)) {
    return(NULL)
  }

  base <- runtime_result[["code_text"]]
  if (!isTRUE(runtime_result[["ok"]])) {
    return(base)
  }
  if (is.null(extras) || length(extras) == 0) {
    return(base)
  }

  extra_text <- vapply(extras, rlang::quo_text, character(1))
  if (is.null(base) || !nzchar(base)) {
    return(paste(extra_text, collapse = " +\n  "))
  }
  paste(c(base, extra_text), collapse = " +\n  ")
}

#' Bind Default Plot Rendering into a Shiny App
#'
#' Bind the same default `renderPlot()` behavior used by `ptr_server()`.
#' Advanced users who want to transform the plot should instead write their own
#' `renderPlot()` and call `ptr_extract_plot()`.
#'
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_register_draw(input, ps)
#'   ptr_register_plot(output, ps)
#' }
#' }
#' @export
ptr_register_plot <- function(output,
                               ptr_state) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ptr_state$server_ids)

  output[[ids$plot_output]] <- shiny::renderPlot({
    plot_obj <- ptr_extract_plot(ptr_state$runtime())
    if (is.null(plot_obj)) {
      graphics::plot.new()
      return(invisible(NULL))
    }

    plot_obj
  })

  invisible(ptr_state)
}

#' Bind Default Error Rendering into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_register_draw(input, ps)
#'   ptr_register_error(output, ps)
#' }
#' }
#' @export
ptr_register_error <- function(output,
                                ptr_state) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ptr_state$server_ids)

  output[[ids$error_output]] <- shiny::renderUI({
    notice <- ptr_stale_notice_ui(ptr_collect_stale_layers(ptr_state))
    err <- ptr_extract_error(ptr_state$runtime())
    if (is.null(notice) && is.null(err)) return(NULL)
    shiny::tagList(notice, err)
  })

  invisible(ptr_state)
}

#' Bind Default Code Rendering into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_register_draw(input, ps)
#'   ptr_register_code(output, ps)
#' }
#' }
#' @export
ptr_register_code <- function(output,
                               ptr_state) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ptr_state$server_ids)

  output[[ids$code_output]] <- shiny::renderText({
    ptr_extract_code(ptr_state$runtime(), extras = ptr_state$extras())
  })

  invisible(ptr_state)
}

#' Capture Out-of-Runtime ggplot Additions for the Code Output
#'
#' Advanced helper for custom `renderPlot({...})` blocks inside embedded
#' ggpaintr apps. When an advanced developer adds ggplot components (themes,
#' scales, coords, guides, labs, ...) to the plot returned by
#' [ptr_extract_plot()], those additions render visually but never reach the
#' generated code output. `ptr_gg_extra()` solves that by capturing the
#' expressions it receives and storing them on the `ptr_state` so the default
#' code binder ([ptr_register_code()]) can surface them alongside the
#' formula-driven code.
#'
#' The helper is intentionally **not** exposed through [ptr_app()] or
#' [ptr_app_bslib()]. It is only meaningful when the caller owns their own
#' `renderPlot({...})` block built on [ptr_server_state()] and the
#' `ptr_register_*()` helpers.
#'
#' Semantics:
#'
#' * Replace-per-call. Each call overwrites the previously captured extras.
#'   Pass every component you want in a single call, e.g.
#'   `ptr_gg_extra(ps, theme_minimal(), scale_x_log10())`.
#' * Suppression on failure. When the underlying runtime reports
#'   `ok = FALSE`, the code binder ignores extras so stale values from a
#'   prior successful draw never surface during a failed draw.
#' * No new S3 class. The return value is a plain list, so
#'   `plot_obj + ptr_gg_extra(ps, ...)` dispatches through ggplot2's built-in
#'   `ggplot_add.list` method.
#'
#' @param ptr_state A `ptr_state` object created by [ptr_server_state()].
#' @param ... ggplot components (theme, scale, coord, guides, labs, ...).
#'   Captured with [rlang::enquos()] for code output, evaluated for the
#'   returned list.
#'
#' @return A list of evaluated ggplot components, suitable for
#'   `plot_obj + ptr_gg_extra(ptr_state, ...)`.
#'
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state(
#'     "ggplot(data = iris, aes(x = var, y = var)) + geom_point() + labs(title = text)"
#'   )
#'   ptr_setup_controls(input, output, ps)
#'   ptr_register_draw(input, ps)
#'   ptr_register_error(output, ps)
#'   ptr_register_code(output, ps)
#'
#'   output$outputPlot <- shiny::renderPlot({
#'     plot_obj <- ptr_extract_plot(ps$runtime())
#'     if (is.null(plot_obj)) {
#'       graphics::plot.new()
#'       return(invisible(NULL))
#'     }
#'     plot_obj + ptr_gg_extra(ps, ggplot2::theme_minimal(base_size = 16))
#'   })
#' }
#' }
#' @export
ptr_gg_extra <- function(ptr_state, ...) {
  if (missing(ptr_state)) {
    rlang::abort("ptr_gg_extra() requires `ptr_state` as the first argument.")
  }
  if (!is.list(ptr_state) || !is.function(ptr_state$extras)) {
    rlang::abort(
      "`ptr_state` does not expose an `extras` reactiveVal. Rebuild it with ptr_server_state() from the current ggpaintr version."
    )
  }

  quos <- rlang::enquos(..., .named = FALSE)
  vals <- rlang::list2(...)

  ptr_state$extras(quos)

  vals
}

#' Build Default ggpaintr Control Widgets
#'
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param ns An optional namespace function (`character -> character`). Must
#'   match the `ns` passed to `ptr_server_state()`. Defaults to
#'   `shiny::NS(NULL)` (no prefixing).
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ptr_input_ui(
#'   ptr_build_ids(draw_button = "render_plot"),
#'   ui_text = list(shell = list(draw_button = list(label = "Render plot")))
#' )
#' inherits(ui, "shiny.tag.list")
#' @export
ptr_input_ui <- function(ids = ptr_build_ids(), ui_text = NULL,
                         ns = shiny::NS(NULL)) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  ids <- ptr_normalize_ids(ids)
  shell_copy <- ptr_resolve_shell_ui_text(ui_text)

  shiny::tagList(
    shiny::uiOutput(ns(ids$control_panel)),
    shiny::actionButton(ns(ids$draw_button), shell_copy$draw_copy$label)
  )
}

#' Build Default ggpaintr Output Widgets
#'
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param ns An optional namespace function (`character -> character`). Must
#'   match the `ns` passed to `ptr_server_state()`. Defaults to
#'   `shiny::NS(NULL)` (no prefixing).
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ptr_output_ui(ptr_build_ids(plot_output = "main_plot"))
#' inherits(ui, "shiny.tag.list")
#' @export
ptr_output_ui <- function(ids = ptr_build_ids(), ns = shiny::NS(NULL)) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  ids <- ptr_normalize_ids(ids)

  shiny::tagList(
    shiny::plotOutput(ns(ids$plot_output)),
    shiny::uiOutput(ns(ids$error_output)),
    shiny::verbatimTextOutput(ns(ids$code_output))
  )
}

#' Build ggpaintr UI for a Shiny Module
#'
#' Use this helper when you prefer Shiny modules for Level 2 integration. It is
#' also a compact template for the UI half of custom module wrappers built from
#' [ptr_input_ui()] and [ptr_output_ui()].
#'
#' @param id Module id.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#'
#' @return A Shiny UI object.
#' @examples
#' paintr_ui <- ptr_module_ui("plot1")
#' inherits(paintr_ui, "shiny.tag.list")
#' @export
ptr_module_ui <- function(id, ui_text = NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(
    ptr_input_ui(ui_text = ui_text, ns = ns),
    ptr_output_ui(ns = ns)
  )
}

#' Register ggpaintr Server Logic for a Shiny Module
#'
#' Use this helper with `ptr_module_ui()` when you prefer Shiny modules for
#' Level 2 integration. The function also documents the module ID contract:
#' server-side ids stay local to the module, while UI generated from
#' `renderUI()` is namespaced with `session$ns`.
#'
#' @param id Module id.
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes. See [ptr_server_state()].
#' @param expr_check Controls `expr` placeholder validation. See [ptr_app()].
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. See [ptr_app()] for the curated default set and full
#'   semantics. Defaults to `character()`.
#' @param shared Named list of Shiny reactives (or empty list). See
#'   [ptr_server_state()] for details.
#' @param draw_trigger Optional Shiny reactive (or `NULL`). See
#'   [ptr_server_state()] for details.
#'
#' @return A `ptr_state` object containing reactive accessors named `obj`,
#'   `runtime`, and `var_ui_list`, plus shared metadata used by the bind helpers.
#' @examples
#' ui <- shiny::fluidPage(ptr_module_ui("plot1"))
#' server <- function(input, output, session) {
#'   ptr_module_server(
#'     "plot1",
#'     "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#'   )
#' }
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#' @export
ptr_module_server <- function(id,
                              formula,
                              envir = parent.frame(),
                              ui_text = NULL,
                              placeholders = NULL,
                              checkbox_defaults = NULL,
                              expr_check = TRUE,
                              safe_to_remove = character(),
                              shared = list(),
                              draw_trigger = NULL) {
  force(formula)
  force(envir)
  force(ui_text)
  force(placeholders)
  force(checkbox_defaults)
  force(expr_check)
  force(shared)
  force(draw_trigger)
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)

  shiny::moduleServer(id, function(input, output, session) {
    ptr_state <- ptr_server_state(
      formula,
      envir = envir,
      ui_text = ui_text,
      placeholders = placeholders,
      checkbox_defaults = checkbox_defaults,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove,
      shared = shared,
      draw_trigger = draw_trigger,
      ns = session$ns,
      server_ns = shiny::NS(NULL)
    )

    ptr_state <- ptr_setup_controls(input, output, ptr_state)
    ptr_register_draw(input, ptr_state)
    ptr_register_plot(output, ptr_state)
    ptr_register_error(output, ptr_state)
    ptr_register_code(output, ptr_state)

    ptr_state
  })
}

#' Build a ggpaintr Shiny App
#'
#' Create a Shiny app from a single ggplot-like formula string. The app exposes
#' generated controls, a draw button, inline error handling, and code output.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes. See [ptr_server_state()].
#' @param expr_check Controls `expr` placeholder validation.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Extends the curated default set: `theme()`, `labs()`,
#'   `xlab()`, `ylab()`, `ggtitle()`, `facet_wrap()`, `facet_grid()`,
#'   `facet_null()`, `xlim()`, `ylim()`, `lims()`, `expand_limits()`,
#'   `guides()`, `annotate()`, `annotation_custom()`, `annotation_map()`,
#'   `annotation_raster()`, `aes()`, `aes_()`, `aes_q()`, `aes_string()`,
#'   `vars()`, `element_text()`, `element_line()`, `element_rect()`,
#'   `element_point()`, `element_polygon()`, `element_geom()`. `geom_*()` /
#'   `stat_*()` standalone layers are always preserved. Defaults to `character()`.
#'
#' @param ns An optional namespace function (`character -> character`). See
#'   [ptr_server_state()] for details. For standalone apps created with
#'   `ptr_app()`, namespacing is rarely needed; it is most useful when
#'   embedding more than one ggpaintr instance in a larger root Shiny session.
#' @return A `shiny.appobj`.
#' @examples
#' app <- ptr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
#' inherits(app, "shiny.appobj")
#' @export
ptr_app <- function(formula,
                         envir = parent.frame(),
                         ui_text = NULL,
                         placeholders = NULL,
                         checkbox_defaults = NULL,
                         expr_check = TRUE,
                         safe_to_remove = character(),
                         ns = shiny::NS(NULL)) {
  app_parts <- ptr_app_components(
    formula,
    envir = envir,
    ui_text = ui_text,
    placeholders = placeholders,
    checkbox_defaults = checkbox_defaults,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    ns = ns
  )
  shiny::shinyApp(ui = app_parts$ui, server = app_parts$server)
}


#' Multi-Plot Shiny App With Shared Placeholder Controls
#'
#' Build a single Shiny app that hosts multiple ggpaintr formulas in one
#' session and drives them from a top region of shared controls. Each
#' formula becomes its own ptr_obj with its own draw button and plot output.
#' Placeholders carrying a `shared = "<id>"` annotation read their value
#' from the matching widget in `shared_ui`; non-shared placeholders still
#' render their per-instance widgets in each plot's panel as usual.
#'
#' Layout is fixed: shared controls in a top `wellPanel`, then one
#' equally-sized column per plot below. For more flexible arrangements,
#' use `ptr_server()` directly inside your own custom Shiny UI.
#'
#' @param plots Character vector or list of formula strings; one ptr_obj
#'   per entry.
#' @param shared_ui Named list of `function(id) -> shiny.tag`. Each name
#'   must match a `shared = "<id>"` annotation used by at least one of the
#'   `plots` formulas. The function is called with the shared name as the
#'   widget input id; the resulting widget is rendered once at the top of
#'   the app and its `input[[id]]` is wired into every plot's
#'   `shared` argument.
#' @param envir Environment used to resolve local data objects.
#' @param title Title displayed in the app header.
#' @param draw_all_label Label for the top-level "draw all" button that
#'   triggers a redraw of every plot in the grid. The button is rendered
#'   alongside the shared widgets so changes to those widgets can be
#'   broadcast to all plots in one click. Defaults to `"Draw all"`.
#' @param expr_check Controls `expr` placeholder validation. See
#'   [ptr_server_state()].
#'
#' @return A `shiny.appobj`. Run with `shiny::runApp()` or print at the REPL.
#' @examples
#' if (interactive()) {
#'   app <- ptr_app_grid(
#'     plots = list(
#'       'ggplot(data = mtcars, aes(x = wt, y = mpg)) +
#'          geom_point(size = num(shared = "sz"))',
#'       'ggplot(data = mtcars, aes(x = hp, y = mpg)) +
#'          geom_point(size = num(shared = "sz"))'
#'     ),
#'     shared_ui = list(
#'       sz = function(id) shiny::sliderInput(id, "Size", 1, 10, value = 3)
#'     )
#'   )
#'   shiny::runApp(app)
#' }
#' @export
ptr_app_grid <- function(plots,
                         shared_ui = list(),
                         envir = parent.frame(),
                         title = "ggpaintr grid",
                         draw_all_label = "Draw all",
                         expr_check = TRUE) {
  parts <- ptr_app_grid_components(
    plots = plots,
    shared_ui = shared_ui,
    envir = envir,
    title = title,
    draw_all_label = draw_all_label,
    expr_check = expr_check
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

#' Build UI and Server for ptr_app_grid()
#'
#' Internal: returns the constructed UI tag and server function used by
#' [ptr_app_grid()]. Exposed so tests can inspect the UI without going
#' through `shinyApp()`.
#'
#' @param plots,shared_ui,envir,title,expr_check See [ptr_app_grid()].
#' @return A list with `ui` (a `shiny.tag`) and `server` (a function).
#' @noRd
ptr_app_grid_components <- function(plots,
                                    shared_ui = list(),
                                    envir = parent.frame(),
                                    title = "ggpaintr grid",
                                    draw_all_label = "Draw all",
                                    expr_check = TRUE) {
  if (is.character(plots)) plots <- as.list(plots)
  assertthat::assert_that(
    is.list(plots),
    length(plots) >= 1L,
    all(vapply(plots, rlang::is_string, logical(1)))
  )
  assertthat::assert_that(is.list(shared_ui))
  if (length(shared_ui) > 0L) {
    nms <- names(shared_ui)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort(
        "`shared_ui` must have unique non-empty names matching the `shared` annotations in `plots`."
      )
    }
    is_fn <- vapply(shared_ui, is.function, logical(1))
    if (!all(is_fn)) {
      rlang::abort(
        "Every entry of `shared_ui` must be a function `function(id) -> shiny.tag`."
      )
    }
  }

  shared_names <- names(shared_ui)
  plot_module_ids <- paste0("plot_", seq_along(plots))
  n_plots <- length(plots)
  col_width <- max(1L, 12L %/% n_plots)
  draw_all_id <- "ptr_grid_draw_all"

  shared_panel <- if (length(shared_ui) > 0L) {
    shiny::wellPanel(
      do.call(
        shiny::tagList,
        c(
          lapply(shared_names, function(nm) shared_ui[[nm]](nm)),
          list(shiny::actionButton(draw_all_id, draw_all_label))
        )
      )
    )
  } else {
    shiny::wellPanel(shiny::actionButton(draw_all_id, draw_all_label))
  }

  plot_columns <- do.call(
    shiny::fluidRow,
    lapply(plot_module_ids, function(mid) {
      shiny::column(width = col_width, ptr_module_ui(mid))
    })
  )

  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shared_panel,
    plot_columns
  )

  force(plots)
  force(envir)
  force(expr_check)

  server <- function(input, output, session) {
    shared_reactives <- if (length(shared_names) > 0L) {
      stats::setNames(
        lapply(shared_names, function(nm) shiny::reactive(input[[nm]])),
        shared_names
      )
    } else {
      list()
    }
    draw_all_trigger <- shiny::reactive(input[[draw_all_id]])

    for (i in seq_along(plots)) {
      local({
        idx <- i
        ptr_module_server(
          plot_module_ids[[idx]],
          formula = plots[[idx]],
          envir = envir,
          expr_check = expr_check,
          shared = shared_reactives,
          draw_trigger = draw_all_trigger
        )
      })
    }
  }

  list(ui = ui, server = server)
}

#' Register ggpaintr Server Logic
#'
#' Wire the standard `ggpaintr` server behavior into an existing Shiny app.
#' The returned state object exposes reactive access to the parsed formula,
#' latest runtime result, and current dynamic `var` UI definitions so callers
#' can extend the app with additional observers and outputs.
#'
#' @param input A Shiny `input` object.
#' @param output A Shiny `output` object.
#' @param session A Shiny `session` object.
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param ids A `ptr_build_ids` object controlling the Shiny element IDs used by
#'   the integration helpers. Defaults to `ptr_build_ids()`.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes. See [ptr_server_state()].
#' @param expr_check Controls `expr` placeholder validation.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Extends the curated default set: `theme()`, `labs()`,
#'   `xlab()`, `ylab()`, `ggtitle()`, `facet_wrap()`, `facet_grid()`,
#'   `facet_null()`, `xlim()`, `ylim()`, `lims()`, `expand_limits()`,
#'   `guides()`, `annotate()`, `annotation_custom()`, `annotation_map()`,
#'   `annotation_raster()`, `aes()`, `aes_()`, `aes_q()`, `aes_string()`,
#'   `vars()`, `element_text()`, `element_line()`, `element_rect()`,
#'   `element_point()`, `element_polygon()`, `element_geom()`. `geom_*()` /
#'   `stat_*()` standalone layers are always preserved. Defaults to `character()`.
#' @param shared Named list of Shiny reactives (or `NULL`/empty list).
#'   See [ptr_server_state()] for details. Use this to drive multiple
#'   ptr_obj instances from a single externally-rendered control.
#' @param draw_trigger Optional Shiny reactive (or `NULL`). See
#'   [ptr_server_state()] for details.
#' @param ns An optional namespace function (`character -> character`). See
#'   [ptr_server_state()] for details.
#'
#' @return A `ptr_state` object containing reactive accessors named `obj`,
#'   `runtime`, and `var_ui_list`, plus shared metadata used by the bind helpers.
#' @note For Shiny modules, prefer [ptr_module_ui()] and
#'   [ptr_module_server()]. `moduleServer()` already scopes `input` and
#'   `output`, while UI generated from `renderUI()` still needs `session$ns`;
#'   the module wrappers handle that split.
#' @export
ptr_server <- function(input,
                            output,
                            session,
                            formula,
                            envir = parent.frame(),
                            ui_text = NULL,
                            placeholders = NULL,
                            ids = ptr_build_ids(),
                            checkbox_defaults = NULL,
                            expr_check = TRUE,
                            safe_to_remove = character(),
                            shared = list(),
                            draw_trigger = NULL,
                            ns = shiny::NS(NULL)) {
  ptr_state <- ptr_server_state(
    formula,
    envir = envir,
    ui_text = ui_text,
    placeholders = placeholders,
    ids = ids,
    checkbox_defaults = checkbox_defaults,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    shared = shared,
    draw_trigger = draw_trigger,
    ns = ns
  )

  ptr_state <- ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_plot(output, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)

  ptr_state
}

#' Resolve Standard Shell Copy for ggpaintr
#'
#' @param ui_text Optional named list of copy overrides.
#'
#' @return A named list with shell copy entries.
#' @noRd
ptr_resolve_shell_ui_text <- function(ui_text = NULL) {
  list(
    title_copy = ptr_resolve_ui_text("title", ui_text = ui_text),
    draw_copy = ptr_resolve_ui_text("draw_button", ui_text = ui_text)
  )
}

#' Build the Standard ggpaintr App UI
#'
#' @param title_label App title text.
#' @param draw_label Draw button text.
#' @param ns An optional namespace function (`character -> character`).
#'
#' @return A Shiny UI object.
#' @noRd
ptr_build_app_ui <- function(title_label, draw_label,
                             ids = ptr_build_ids(),
                             ns = shiny::NS(NULL)) {
  shiny::fluidPage(
    shiny::titlePanel(title_label),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns(ids$control_panel)),
        shiny::actionButton(ns(ids$draw_button), draw_label)
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns(ids$plot_output)),
        shiny::uiOutput(ns(ids$error_output)),
        shiny::verbatimTextOutput(ns(ids$code_output))
      )
    )
  )
}

#' Build Reusable App Components for ggpaintr
#'
#' @param formula A single formula string.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. See [ptr_app()] for the curated default set and full
#'   semantics. Defaults to `character()`.
#' @param ns An optional namespace function (`character -> character`).
#'
#' @return A list with `ui` and `server`.
#' @noRd
ptr_app_components <- function(formula,
                                  envir = parent.frame(),
                                  ui_text = NULL,
                                  placeholders = NULL,
                                  checkbox_defaults = NULL,
                                  expr_check = TRUE,
                                  safe_to_remove = character(),
                                  ns = shiny::NS(NULL)) {
  shell_copy <- ptr_resolve_shell_ui_text(ui_text)
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)

  ui <- ptr_build_app_ui(
    shell_copy$title_copy$label,
    shell_copy$draw_copy$label,
    ns = ns
  )

  server <- function(input, output, session) {
    ptr_server(
      input,
      output,
      session,
      formula,
      envir = envir,
      ui_text = ui_text,
      placeholders = placeholders,
      checkbox_defaults = checkbox_defaults,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove,
      ns = ns
    )
    invisible(NULL)
  }

  list(ui = ui, server = server)
}
