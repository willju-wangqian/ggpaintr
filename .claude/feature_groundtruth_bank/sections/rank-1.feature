# verified against source at 75b18eb
# Executable backing (two files, 96 scenarios total):
#   ../tests/test-rank-1.R         — 79 scenarios AUTO-verified (assert vs loaded
#       source: pure fns, validators, mocked forwarding, post-translate tree
#       inspection). Last run: 79 PASS / 0 FAIL / 0 SKIP.
#   ../tests/test-rank-1-manual.R  — 17 scenarios that need a LIVE Shiny session
#       (reactive flush, widget rendering, upstream-data resolution, spec
#       round-trip). Runnable ptr_app() examples with "# EXPECTED:" comments; the
#       file SOURCES cleanly (every formula translates) but the behaviour must be
#       eyeballed by a human running each app.
# Granularity: ONE Rule per public entry point; for each ARGUMENT, one or more
# Scenarios. Each Scenario is single-polarity — it states either EXPECTED
# behavior or NON-EXPECTED behavior for one way of using that argument, never a
# mix. Callback arguments (build_ui/resolve_expr/validate_input/...) get their
# own Scenarios per callback-parameter (node/cols/data/value/ctx/...).
Feature: Rank 1 (most used) — ptr_app, pp* placeholders, ptr_define_placeholder_*
  The three highest-traffic entry points. ptr_app(formula) turns one ggplot-like
  formula string (or unquoted expression) carrying ppVar/ppText/ppNum/ppExpr/
  ppUpload/ppLayerOff/ppVerbSwitch tokens into a runnable shiny.appobj. The pp*
  functions are the placeholder vocabulary: identity functions in plain R,
  widget bindings inside ptr_app. ptr_define_placeholder_value/consumer/source
  register custom keywords with the same contract as the built-ins.

  Rule: ptr_app(formula, envir = parent.frame(), ui_text = NULL, expr_check = TRUE, safe_to_remove = character(), css = NULL, spec = NULL) -> shiny.appobj

    # ---- argument: formula -------------------------------------------------
    Scenario: [formula] a string literal is used verbatim — EXPECTED
      # verified: R/paintr-app.R:141,207-209
      Given the string "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
      When ptr_app(formula) is called
      Then ptr_capture_formula returns the string unchanged
      And it is translated to the typed tree and a shiny.appobj (ui + server) is returned

    Scenario: [formula] an unquoted ggplot expression is captured and deparsed — EXPECTED
      # verified: R/paintr-app.R:141,196,251-260; test test-rank-1.R "unquoted ggplot expression"
      Given the literal call ptr_app(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point())
      When the expression is passed inline (NOT via a variable)
      Then rlang::enexpr() captures it and ptr_capture_formula deparses it to a string
      And ptr_translate(<deparsed>) equals ptr_translate(<equivalent string-mode formula>)

    Scenario: [formula] a pre-captured expression is spliced with !! — EXPECTED
      # verified: R/paintr-app.R:141,196 (enexpr honours !!); test test-rank-1.R "spliced with !!"
      Given e <- rlang::expr(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point())
      When ptr_app(!!e) is called
      Then e is spliced in at capture time and deparses to the equivalent formula string
      And note: ptr_app(e) WITHOUT !! takes the bare-symbol path instead (e is resolved as a variable)

    Scenario: [formula] a bare symbol bound to a string resolves in envir — EXPECTED
      # verified: R/paintr-app.R:210-238; test test-rank-1.R "bare symbol bound to a string"
      Given f <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()" in the calling frame
      When ptr_app(f) is called
      Then the symbol is resolved once in envir (then via the call stack) and treated as string mode

    Scenario: [formula] a top-level braces block keeps only its last sub-expression — EXPECTED
      # verified: R/paintr-app.R:202-206; test test-rank-1.R "braces block"
      Given { "noise"; iris |> ggplot(aes(x = ppVar, y = ppVar)) } piped into ptr_app
      When the captured call head is `{`
      Then only the last sub-expression is used as the formula (the earlier "noise" is dropped)
      And the braces are not recovered in the rendered code panel

    Scenario: [formula] !! splicing inside expression mode is honoured — EXPECTED
      # verified: R/paintr-app.R:141,196 (roxygen R/paintr-app.R:116-119,129-131)
      Given col <- rlang::sym("mpg") and ptr_app(ggplot(mtcars, aes(x = !!col, y = ppVar)) + geom_point())
      When the expression is captured with rlang::enexpr()
      Then !!col is spliced to mpg before deparsing

    Scenario: [formula] a symbol resolving to neither string nor language is rejected — NOT EXPECTED
      # verified: R/paintr-app.R:242-249
      Given a symbol bound to a numeric value
      When ptr_app(sym) is called
      Then rlang::abort signals formula must be a single string or an unquoted ggplot expression

    Scenario: [formula] native pipe |> is NOT preserved in expression mode — NOT EXPECTED
      # verified: roxygen R/paintr-app.R:116-119
      Given an expression-mode formula using |>
      When R's parser desugars |> before capture
      Then the rendered code shows the desugared nested-call form, not |>
      And string mode (or %>%) must be used to keep |> intact

    # ---- argument: envir ---------------------------------------------------
    Scenario: [envir] defaults to the caller's frame — EXPECTED
      # verified: R/paintr-app.R:135
      Given ptr_app(formula) called with no envir
      Then envir defaults to parent.frame()

    Scenario: [envir] is the lookup scope for a shortcut/spec-named data frame — EXPECTED
      # verified: roxygen R/paintr-registry.R:769-785
      Given a source placeholder shortcut name "df" seeded via spec
      When the runtime resolves it
      Then get("df", envir, inherits = TRUE) loads the frame from the embedder's environment

    # ---- argument: ui_text -------------------------------------------------
    Scenario: [ui_text] NULL leaves the built-in copy in place — EXPECTED
      # verified: R/paintr-app.R:136,165-172
      Given ptr_app(formula) with no ui_text
      Then ui_text defaults to NULL and the default copy is used

    Scenario: [ui_text] an override list is forwarded to both UI and server — EXPECTED
      # verified: R/paintr-app.R:165-172,173-179
      Given a ui_text override list
      When ptr_app(formula, ui_text = ut) is called
      Then ut is passed to ptr_build_app_ui and to ptr_make_app_server

    # ---- argument: expr_check (TRUE | FALSE | list(deny_list=, allow_list=)) ----
    Scenario: [expr_check] defaults to TRUE → built-in denylist + AST walker — EXPECTED
      # verified: R/paintr-app.R:137,163; R/paintr-utils.R:560-561 (roxygen R/paintr-app.R:17-18)
      Given ptr_app(formula) with no expr_check
      Then expr_check defaults to TRUE, forwarded to ptr_translate, and resolve_expr_check returns mode = "denylist" with the built-in unsafe_expr_denylist

    Scenario: [expr_check] FALSE disables all validation — EXPECTED
      # verified: R/paintr-app.R:163; R/paintr-utils.R:556-557 (roxygen R/paintr-app.R:19)
      Given ptr_app(formula, expr_check = FALSE)
      Then resolve_expr_check returns mode = "off" and ppExpr code is not safety-checked (trusted input only)

    Scenario: [expr_check] a list with allow_list switches to allowlist mode — EXPECTED
      # verified: R/paintr-utils.R:571,578-583 (roxygen R/paintr-app.R:20-21)
      Given ptr_app(formula, expr_check = list(allow_list = c("Sys.time")))
      Then resolve_expr_check returns mode = "allowlist" with fns = the allow_list

    Scenario: [expr_check] a list with both allow_list and deny_list subtracts deny from allow — EXPECTED
      # verified: R/paintr-utils.R:580-583
      Given expr_check = list(allow_list = c("a", "b"), deny_list = c("b"))
      Then mode = "allowlist" with fns = setdiff(allow_list, deny_list) = c("a")

    Scenario: [expr_check] a list with only deny_list replaces the default denylist — EXPECTED
      # verified: R/paintr-utils.R:572,586
      Given expr_check = list(deny_list = c("system"))
      Then mode = "denylist" with fns = the supplied deny_list (not the built-in one)

    Scenario: [expr_check] an empty list() silently falls back to the default denylist — EXPECTED (gotcha)
      # verified: R/paintr-utils.R:574-575 (note R/paintr-utils.R:552-553)
      Given expr_check = list() (or any unnamed list lacking deny_list/allow_list keys)
      Then mode = "denylist" with the built-in denylist — pass TRUE explicitly for clarity

    Scenario: [expr_check] a value that is not TRUE/FALSE/list is rejected — NOT EXPECTED
      # verified: R/paintr-utils.R:564-568
      Given expr_check = "denylist" (a non-logical, non-list value)
      Then rlang::abort: "`expr_check` must be TRUE, FALSE, or a list with `deny_list` and/or `allow_list` -- see `?ptr_app`."

    # ---- argument: safe_to_remove ------------------------------------------
    Scenario: [safe_to_remove] defaults to empty so unknown calls are kept when empty — EXPECTED
      # verified: R/paintr-app.R:138 (roxygen R/paintr-app.R:96-98)
      Given ptr_app(formula) with no safe_to_remove
      Then safe_to_remove defaults to character() and a non-curated empty call (e.g. pcp_theme()) is NOT dropped

    Scenario: [safe_to_remove] an opted-in name is dropped when it ends up empty — EXPECTED
      # verified: R/paintr-app.R:173-179 (roxygen R/paintr-app.R:96-98)
      Given ptr_app(formula, safe_to_remove = c("pcp_theme"))
      When pcp_theme() resolves to an empty call
      Then the name is forwarded to the server closure and the empty call is dropped

    # ---- argument: css -----------------------------------------------------
    Scenario: [css] NULL ships only the default stylesheet — EXPECTED
      # verified: R/paintr-app.R:139,165-172
      Given ptr_app(formula) with no css
      Then css defaults to NULL and is forwarded as css = NULL to ptr_build_app_ui

    Scenario: [css] a path/ptr_css value is forwarded to the UI builder — EXPECTED
      # verified: R/paintr-app.R:171 (roxygen R/paintr-app.R:103-104)
      Given a css path vector (or ptr_css() object)
      When ptr_app(formula, css = paths) is called
      Then ptr_build_app_ui is called with css = paths

    # ---- argument: spec ----------------------------------------------------
    Scenario: [spec] NULL boots every widget at its formula default — EXPECTED
      # verified: R/paintr-app.R:140,173-179
      Given ptr_app(formula) with no spec
      Then spec defaults to NULL and no boot-time overrides are applied

    Scenario: [spec] a named list overrides widget values at session boot — EXPECTED
      # verified: R/paintr-app.R:178 (roxygen R/paintr-app.R:120-122)
      Given spec = list("<fully-qualified-input-id>" = value)
      When ptr_app(formula, spec = spec) is called
      Then spec is threaded to the server and seeds the named inputs on first render

    # ---- return ------------------------------------------------------------
    Scenario: [return] a runnable shiny.appobj is produced — EXPECTED
      # verified: R/paintr-app.R:151
      When ptr_app(formula) returns
      Then the value is shiny::shinyApp(ui = parts$ui, server = parts$server)

  Rule: ppVar(x = NULL, ...) / ppNum / ppText / ppExpr — value/consumer identity tokens

    # ---- argument: x -------------------------------------------------------
    Scenario: [x] outside ptr_app the call returns its argument unchanged — EXPECTED
      # verified: R/paintr-builtins.R:401,405,409,413 (roxygen :283-285)
      Given aes(x = ppVar(mpg)) evaluated under ggplot2 tidy-eval
      Then ppVar(mpg) returns mpg, so the plot equals aes(x = mpg)

    Scenario: [x] omitting x is valid (bare token) — EXPECTED
      # verified: R/paintr-builtins.R:401,405,409,413
      Given ppVar with default x = NULL
      Then a bare `ppVar` / `ppVar()` in a formula is a legal placeholder slot

    Scenario: [x] inside ptr_app the keyword binds to a typed widget — EXPECTED
      # verified: roxygen R/paintr-app.R:44-58
      Given a formula containing ppVar / ppText / ppNum / ppExpr
      Then ppVar renders a column-picker selectInput; ppText a textInput; ppNum a numericInput; ppExpr a code editor validated by expr_check

    # ---- argument: ... (named placeholder args) ----------------------------
    Scenario: [...] shared = "<id>" lifts the widget into a top-level shared section — EXPECTED
      # verified: roxygen R/paintr-app.R:61-64
      Given a placeholder occurrence carrying shared = "x"
      Then its widget is lifted out of the per-layer panel into the shared section (used by ptr_app_grid)

    Scenario: [...] extra named args are ignored by the built-in identity body — NOT EXPECTED to act
      # verified: roxygen R/paintr-builtins.R:295-297
      Given ppVar(mpg, foo = 1) outside a custom named_args schema
      Then ... is ignored by the identity implementation (named args matter only via a placeholder's named_args schema)

  Rule: ppUpload(x, ...) — source identity token

    Scenario: [x] with an argument it returns the value unchanged — EXPECTED
      # verified: R/paintr-builtins.R:417-422 (roxygen :286-288)
      Given ppUpload(penguins) with penguins in scope
      Then it returns penguins, so a naked formula evaluates as plain R

    Scenario: [x] the no-arg form aborts outside ptr_app — NOT EXPECTED to return
      # verified: R/paintr-builtins.R:418-419
      Given ppUpload() called with x missing
      Then rlang::abort signals "`ppUpload()` is only meaningful inside `ptr_app()`."

    Scenario: [x] inside ptr_app it renders a file picker + optional name box — EXPECTED
      # verified: roxygen R/paintr-app.R:54-58; R/paintr-builtins.R:229-251
      Given a formula using ppUpload
      Then a fileInput accepting .csv/.tsv/.rds/.xlsx/.xls/.json plus an optional dataset-name textbox is rendered
      And uploaded data is normalized via ptr_normalize_column_names

  Rule: ppLayerOff(layer_expr, hide = TRUE) — structural off-by-default layer wrapper

    Scenario: [hide] defaults to TRUE and drops the layer to NULL in naked R — EXPECTED
      # verified: R/paintr-builtins.R:464,469
      Given ppLayerOff(geom_point()) outside ptr_app
      Then hide defaults to TRUE and the call returns NULL (layer omitted)

    Scenario: [hide] FALSE returns the evaluated layer in naked R — EXPECTED
      # verified: R/paintr-builtins.R:470
      Given ppLayerOff(geom_point(), FALSE)
      Then the geom_point() layer is returned

    Scenario: [hide] a non-logical / non-scalar / NA value is rejected — NOT EXPECTED
      # verified: R/paintr-builtins.R:465-468
      Given ppLayerOff(geom_point(), hide = "yes") (or NA, or length>1)
      Then assertthat aborts: "`hide` must be a length-1 non-NA logical."

    Scenario: [hide] inside ptr_app it must be a literal and sets default_active = FALSE — EXPECTED
      # verified: roxygen R/paintr-builtins.R:426-431,444-447
      Given a ppLayerOff occurrence in a ptr_app formula
      Then the translator unwraps it to a ptr_layer with default_active = FALSE and a boot-off checkbox
      And a non-literal hide aborts at translate time

    # ---- argument: layer_expr ----------------------------------------------
    Scenario: [layer_expr] is evaluated only when hide = FALSE — EXPECTED
      # verified: R/paintr-builtins.R:469-470 (roxygen :443)
      Given ppLayerOff(<layer>, TRUE)
      Then <layer> is never evaluated (NULL is returned before touching it)

  Rule: ppVerbSwitch(.data, verb_expr, switch_on = TRUE, label = NULL) — structural switchable pipeline stage

    Scenario: [switch_on] defaults to TRUE and routes .data through the verb — EXPECTED
      # verified: R/paintr-builtins.R:530,545-553
      Given ppVerbSwitch(mtcars, filter(mpg > 20)) outside ptr_app
      Then switch_on defaults to TRUE, .data is inserted as the first positional arg and the verb is evaluated in parent.frame()

    Scenario: [switch_on] FALSE returns .data unchanged — EXPECTED
      # verified: R/paintr-builtins.R:545
      Given ppVerbSwitch(mtcars, mutate(mpg = mpg + 100), FALSE)
      Then .data (mtcars) is returned unchanged

    Scenario: [switch_on] a non-logical / non-scalar / NA value is rejected — NOT EXPECTED
      # verified: R/paintr-builtins.R:531-534
      Given ppVerbSwitch(mtcars, mutate(x = 1), switch_on = NA)
      Then assertthat aborts: "`switch_on` must be a length-1 non-NA logical."

    # ---- argument: verb_expr -----------------------------------------------
    Scenario: [verb_expr] must be a verb call even when switch_on = FALSE — NOT EXPECTED otherwise
      # verified: R/paintr-builtins.R:539-544
      Given ppVerbSwitch(mtcars, 42, FALSE) (verb_expr is not a call)
      Then rlang::abort fires regardless of switch_on: "`ppVerbSwitch(verb_expr = )` must be a verb call"

    Scenario: [verb_expr] .data is inserted as the FIRST positional argument only — EXPECTED
      # verified: R/paintr-builtins.R:546-553 (roxygen :490-496)
      Given a tidyverse verb taking data in slot 1
      Then .data is spliced as the first positional arg; verbs taking data later are unsupported

    # ---- argument: label ---------------------------------------------------
    Scenario: [label] is metadata-only and ignored in naked R — NOT EXPECTED to affect output
      # verified: roxygen R/paintr-builtins.R:507-508,488
      Given ppVerbSwitch(.data, verb, TRUE, label = "Filter") outside ptr_app
      Then label is ignored by the naked-R path (it only labels the checkbox inside ptr_app)

  Rule: ptr_define_placeholder_value(keyword, build_ui, resolve_expr, validate_input = NULL, default_arg = NULL, named_args = list(), runtime = NULL, copy_defaults = list(label = "Enter a value for {param}")) -> runtime callable

    # ---- argument: keyword -------------------------------------------------
    Scenario: [keyword] a syntactically valid, non-shadowing name registers a value placeholder — EXPECTED
      # verified: R/paintr-registry.R:514,529-538
      Given keyword = "pct"
      Then the entry is registered with role = "value", data_aware = FALSE

    Scenario: [keyword] a non-string / empty / NA value is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:142-146
      Given keyword = "" (or NA, or a non-character)
      Then assertthat aborts (must be a length-1 non-NA non-empty character)

    Scenario: [keyword] a reserved word is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:147-149
      Given keyword is an R reserved word
      Then rlang::abort: "`keyword` cannot be a reserved word: ..."

    Scenario: [keyword] a name that isn't a valid R identifier is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:150-152
      Given keyword = "my keyword" (make.names(keyword) != keyword)
      Then rlang::abort: "`keyword` is not a syntactically valid R name: ..."

    Scenario: [keyword] a name shadowing base R / ggplot2 is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:201-213
      Given keyword = "range" (collides with base::range) and it is not grandfathered
      Then rlang::abort explains it shadows base/ggplot2 and to use the pp-prefix convention

    Scenario: [keyword] re-registering an existing keyword warns about overwrite — EXPECTED (warn, still registers)
      # verified: R/paintr-registry.R:336-340
      Given a keyword already in the registry
      Then cli::cli_warn "Overwriting placeholder registry entry: ..." and the entry is replaced

    Scenario: [keyword] a top-level mismatched binding does NOT warn (best-effort tripwire) — EXPECTED
      # verified: R/paintr-registry.R:282-304; test test-rank-1.R "does NOT warn" (probed 2026-05-29)
      Given ppFoo <- ptr_define_placeholder_value(keyword = "ppBar", ...) at top level
      Then no warning fires — R's `<-` primitive pushes no call frame, so the drift walker finds nothing to compare
      And the LHS/keyword-mismatch warning is only a tripwire for rare nested re-binding shapes (documented best-effort)

    # ---- argument: build_ui (callback: node, label = NULL, selected = NULL, ...) ----
    Scenario: [build_ui] a function declaring node (or ...) is accepted — EXPECTED
      # verified: R/paintr-registry.R:516,156-180; roxygen :365-370
      Given build_ui = function(node, label = NULL, selected = NULL, ...) <tag>
      Then it passes validate_hook (required arg "node" present)

    Scenario: [build_ui] a non-function is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:157-159
      Given build_ui = "not a function"
      Then rlang::abort: "`build_ui` must be a function."

    Scenario: [build_ui.node] node carries $id/$keyword/$param for wiring the widget — EXPECTED
      # verified: roxygen R/paintr-registry.R:365-370
      Given the build_ui body
      Then node$id is used as the underlying inputId and node$keyword/$param are readable

    Scenario: [build_ui.selected] declaring selected = NULL receives the seeded value per precedence — EXPECTED
      # verified: roxygen R/paintr-registry.R:372-380
      Given build_ui with an explicit selected = NULL formal
      Then on first render a spec seed (else a positional default) is delivered as selected; an empty selected renders the widget empty

    Scenario: [build_ui] declaring ONLY ... still works but warns when required args exist — EXPECTED (warn)
      # verified: R/paintr-registry.R:161-168
      Given build_ui = function(...) <tag>
      Then cli::cli_warn notes it only declares ... and lists required args; the hook is still accepted

    # ---- argument: resolve_expr (callback: value, node, ...) ----------------
    Scenario: [resolve_expr] a function declaring value, node (or ...) is accepted — EXPECTED
      # verified: R/paintr-registry.R:517,156-180
      Given resolve_expr = function(value, node, ...) <code>
      Then it passes validate_hook (required args value, node present)

    Scenario: [resolve_expr] missing a required arg with no ... is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:170-178
      Given resolve_expr = function(value) <code> (no node, no ...)
      Then rlang::abort: "`resolve_expr` must accept argument(s): node (or `...`)."

    Scenario: [resolve_expr.return] returning NULL prunes the placeholder's argument — EXPECTED
      # verified: roxygen R/paintr-registry.R:496-499,581-583
      Given resolve_expr returns NULL for an empty/invalid value
      Then the surrounding argument is dropped from the rendered code

    # ---- argument: validate_input (callback: value, ctx) --------------------
    Scenario: [validate_input] NULL means no pre-resolve validation — EXPECTED
      # verified: R/paintr-registry.R:506,518
      Given validate_input left at its NULL default
      Then no validation hook runs before resolve_expr

    Scenario: [validate_input] returning TRUE/NULL accepts, a string rejects with that message — EXPECTED
      # verified: roxygen R/paintr-registry.R:448-453
      Given validate_input(value, ctx) returns "Pick a valid value"
      Then the layer is pruned and the string is surfaced inline as the error

    Scenario: [validate_input.ctx] for a VALUE placeholder ctx$upstream_cols and ctx$data are always NULL — EXPECTED
      # verified: roxygen R/paintr-registry.R:454-462
      Given a value-role validate_input
      Then ctx carries node/keyword/upstream_cols/data, with upstream_cols and data always NULL

    # ---- argument: default_arg ---------------------------------------------
    Scenario: [default_arg] NULL rejects positional formula arguments at translate — EXPECTED
      # verified: R/paintr-registry.R:507; roxygen :433-440
      Given default_arg left NULL
      Then a positional argument like pct(50) is rejected at translate time

    Scenario: [default_arg] a validator closure accepts a positional default — EXPECTED
      # verified: roxygen R/paintr-registry.R:433-440
      Given default_arg = ptr_default_numeric()
      Then pct(50) is accepted and 50 becomes the boot default

    Scenario: [default_arg] a non-NULL non-function value is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:218-224
      Given default_arg = 50 (not a function)
      Then rlang::abort: "`default_arg` ... must be NULL or a function"

    # ---- argument: named_args ----------------------------------------------
    Scenario: [named_args] empty list (default) means no extra named args — EXPECTED
      # verified: R/paintr-registry.R:508,232
      Given named_args left at list()
      Then only the reserved shared = ... named arg is accepted

    Scenario: [named_args] a non-list is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:227-231
      Given named_args = c(a = 1)
      Then rlang::abort: "`named_args` ... must be a list."

    Scenario: [named_args] an unnamed / partially-named list is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:233-239
      Given named_args = list(function(x) x) (no name)
      Then rlang::abort: must be a fully-named list

    Scenario: [named_args] an entry named "shared" is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:247-253
      Given named_args = list(shared = function(x) x)
      Then rlang::abort: "shared" is reserved by ggpaintr

    Scenario: [named_args] non-function entries are rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:254-262
      Given named_args = list(foo = 1)
      Then rlang::abort: must contain only validator functions

    # ---- argument: runtime -------------------------------------------------
    Scenario: [runtime] NULL supplies the identity function and returns it — EXPECTED
      # verified: R/paintr-registry.R:526-527,538; roxygen :464-471
      Given runtime left NULL
      Then runtime_fn = function(x, ...) x is registered and returned to the caller

    Scenario: [runtime] an override gives the keyword a non-identity plain-R meaning — EXPECTED
      # verified: R/paintr-registry.R:526; roxygen :464-471
      Given runtime = function(x, ...) x / 100
      Then calling the keyword as a plain-R function uses that body

    # ---- argument: copy_defaults -------------------------------------------
    Scenario: [copy_defaults] a named list of leaf-field strings is accepted; {param} interpolates — EXPECTED
      # verified: R/paintr-registry.R:523,308-334; roxygen :430-431
      Given copy_defaults = list(label = "Percent for {param}")
      Then it is stored and {param} is interpolated to the formal-arg name at render time

    Scenario: [copy_defaults] an unsupported field name is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:317-324
      Given copy_defaults = list(bogus = "x")
      Then rlang::abort lists the unsupported field and the allowed leaf fields

    Scenario: [copy_defaults] a non-string / multi-length / NA value is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:325-332
      Given copy_defaults = list(label = c("a", "b"))
      Then rlang::abort: "`copy_defaults$label` must be a single non-NA string."

    # ---- return ------------------------------------------------------------
    Scenario: [return] the runtime callable is returned for binding under the keyword name — EXPECTED
      # verified: R/paintr-registry.R:538; roxygen :473-476
      When ptr_define_placeholder_value(...) returns
      Then the runtime callable is returned (also called for its registration side effect)

  Rule: ptr_define_placeholder_consumer(keyword, build_ui, resolve_expr, validate_input = NULL, default_arg = NULL, named_args = list(), runtime = NULL, copy_defaults = list(label = "Pick a column for {param}")) -> runtime callable

    Scenario: [registration] registers a data-aware consumer entry — EXPECTED
      # verified: R/paintr-registry.R:670-678
      Given a valid consumer definition
      Then the entry is registered with role = "consumer", data_aware = TRUE

    # ---- argument: build_ui (callback: node, cols, data, label = NULL, selected = NULL, ...) ----
    Scenario: [build_ui] requires node, cols AND data (or ...) — EXPECTED
      # verified: R/paintr-registry.R:657,156-180; roxygen :551-557
      Given build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) <tag>
      Then it passes validate_hook (required args node, cols, data present)

    Scenario: [build_ui] missing cols/data with no ... is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:170-178
      Given build_ui = function(node) <tag> (consumer requires cols, data)
      Then rlang::abort: "`build_ui` must accept argument(s): cols, data (or `...`)."

    Scenario: [build_ui.cols] cols is the upstream column-name vector, character(0) before resolve — EXPECTED
      # verified: roxygen R/paintr-registry.R:553-556
      Given the build_ui body
      Then cols supplies the picker choices and is character(0) until upstream data resolves

    Scenario: [build_ui.data] data is the upstream data frame, NULL while pending — EXPECTED
      # verified: roxygen R/paintr-registry.R:555-557
      Given the build_ui body
      Then data is the upstream frame (read it for column types/levels/ranges) or NULL while pending

    Scenario: [build_ui.selected] must be filtered through intersect(selected, cols) — EXPECTED
      # verified: roxygen R/paintr-registry.R:564-577
      Given a stale pick naming a column no longer in cols
      Then intersect(selected %||% character(0), cols) drops it cleanly (no silent fallback to the first choice)

    # ---- argument: validate_input (callback: value, ctx) — consumer differs from value ----
    Scenario: [validate_input.ctx] consumer ctx$upstream_cols and ctx$data ARE populated — EXPECTED
      # verified: roxygen R/paintr-registry.R:585-601
      Given a consumer validate_input
      Then ctx$upstream_cols and ctx$data hold the same values build_ui received as cols/data (not always NULL, unlike value role)

    Scenario: [validate_input] is NOT invoked while upstream resolution is pending — EXPECTED
      # verified: roxygen R/paintr-registry.R:596-598
      Given upstream data has not resolved yet
      Then the substitute walker skips the validate_input hook

  Rule: ptr_define_placeholder_source(keyword, build_ui, resolve_data, resolve_expr = NULL, shortcut = FALSE, default_arg = NULL, named_args = list(), runtime = NULL, copy_defaults = list(label = "Provide a data source for {param}")) -> runtime callable

    Scenario: [registration] registers a data-aware source entry — EXPECTED
      # verified: R/paintr-registry.R:854-862
      Given a valid source definition
      Then the entry is registered with role = "source", data_aware = TRUE

    # ---- argument: build_ui (callback: node, label, ...) -------------------
    Scenario: [build_ui] requires only node (or ...) — EXPECTED
      # verified: R/paintr-registry.R:828; roxygen :692-697
      Given build_ui = function(node, label, ...) <tag>
      Then it passes validate_hook (required arg node)

    # ---- argument: resolve_data (callback: value, node, ...) ---------------
    Scenario: [resolve_data] required; returns a data.frame or NULL for "no data yet" — EXPECTED
      # verified: R/paintr-registry.R:829; roxygen :709-713
      Given resolve_data = function(value, node, ...) <df>
      Then it must accept value, node and returns a data.frame (or NULL), aborting on malformed input

    # ---- argument: resolve_expr (optional here) ----------------------------
    Scenario: [resolve_expr] NULL defaults to rlang::sym(value) — EXPECTED
      # verified: R/paintr-registry.R:830-834
      Given resolve_expr left NULL
      Then it defaults to function(value, node, ...) rlang::sym(value)

    Scenario: [resolve_expr] an override controls how the data is referred to in rendered code — EXPECTED
      # verified: roxygen R/paintr-registry.R:714-724
      Given resolve_expr = function(value, node, ...) <re-fetch call>
      Then the rendered code re-fetches instead of referencing an in-session object; with shortcut=TRUE, value is the shortcut input's value

    # ---- argument: shortcut ------------------------------------------------
    Scenario: [shortcut] FALSE (default) renders a single bound input — EXPECTED
      # verified: R/paintr-registry.R:818,857
      Given shortcut left FALSE
      Then no node$shortcut_id is stamped; one bound input is the common case

    Scenario: [shortcut] TRUE stamps node$shortcut_id and expects two bound inputs — EXPECTED
      # verified: roxygen R/paintr-registry.R:725-739
      Given shortcut = TRUE
      Then the framework stamps node$shortcut_id = paste0(node$id, "_shortcut") and build_ui must render inputs at node$id and node$shortcut_id

    Scenario: [shortcut] a non-logical / non-scalar / NA value is rejected — NOT EXPECTED
      # verified: R/paintr-registry.R:835-840
      Given shortcut = "yes"
      Then rlang::abort (class ptr_registry_error): "`shortcut` must be a single logical (TRUE or FALSE)."

    # ---- argument: runtime (source default DIFFERS from value/consumer) ----
    Scenario: [runtime] NULL supplies an abort guard, not identity — EXPECTED
      # verified: R/paintr-registry.R:846-851; roxygen :744-750
      Given runtime left NULL
      Then the default runtime aborts "`<keyword>()` is only meaningful inside `ptr_app()`." when called at the REPL

    # ---- spec round-trip ---------------------------------------------------
    Scenario: [spec] shortcut sources round-trip via the shortcut name, dropping the per-session payload — EXPECTED
      # verified: roxygen R/paintr-registry.R:757-785
      Given shortcut = TRUE and a spec snapshot
      Then the shortcut text value carries the round-trip identity and the node$id payload (e.g. a fileInput data.frame) is dropped from spec

    Scenario: [spec] a complex-valued source without shortcut cannot round-trip — NOT EXPECTED
      # verified: roxygen R/paintr-registry.R:787-797
      Given shortcut = FALSE and a primary value that is not a deparse-able literal
      Then the source cannot round-trip through spec; opt into shortcut = TRUE instead
