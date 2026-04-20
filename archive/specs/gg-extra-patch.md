---
name: gg-extra-patch
type: decision
status: accepted
scope: [api, runtime, advanced-embed]
created: 2026-04-15
---

# `ptr_gg_extra()` — Capturing Out-of-Runtime ggplot Additions

## Understanding Summary

- **What**: A new exported helper `ptr_gg_extra(ptr_state, ...)` that captures ggplot components (theme, scale, coord, guides, labs, etc.) added *outside* the ggpaintr runtime — inside a user-authored `renderPlot({...})` block — so those components appear in the generated code output alongside the formula-driven code.
- **Why**: In the custom-embed pattern shown by Formula 11 of `tests/manual/manual-test-ggpaintr.Rmd`, `plot_obj + ggplot2::theme_minimal(base_size = 16)` renders visually but the `theme_minimal()` call never reaches `outputCode`. The rendered plot and the code pane silently diverge.
- **Who**: Advanced Shiny app developers building custom embeds via `ptr_server_state()` + the `ptr_extract_*()` / `ptr_register_*()` helpers. **Not** a high-level API — no `ptr_app()` / `ptr_app_bslib()` argument.
- **Non-goals**: Not a way to modify the core plot pipeline; not accessible from the wrapper apps; not a general "inject arbitrary R code into outputCode" mechanism.

## Decision

Introduce one new exported helper and extend `ptr_server_state()` + the default code binder additively.

### API

```r
ptr_gg_extra(ptr_state, ...)
```

- `ptr_state`: the object returned by `ptr_server_state()`. **Explicit first argument, no magic lookup.**
- `...`: zero or more ggplot components. Captured via `rlang::enquos(..., .named = FALSE)` for code display and `rlang::list2(...)` for evaluation.
- **Returns**: a plain `list` of evaluated components. `plot_obj + ptr_gg_extra(...)` dispatches via ggplot2's built-in `ggplot_add.list` method — no new S3 class or `+` method.
- **Side effect**: replaces `ptr_state$extras()` with the captured quosures (replace-per-call semantics — last call wins in a given `renderPlot` pass).

### ptr_state extension

`ptr_server_state()` creates one additional reactiveVal:

```r
extras <- shiny::reactiveVal(list())   # list of quosures; empty = no extras
```

Returned on `ptr_state$extras`. Consumers read with `ptr_state$extras()`, write with `ptr_state$extras(new_val)` — same convention as `runtime`.

### Default code binder update

`ptr_extract_code()` gains an optional `extras = NULL` parameter:

```r
ptr_extract_code <- function(runtime_result, extras = NULL) {
  if (is.null(runtime_result)) return(NULL)
  if (!isTRUE(runtime_result$ok)) return(runtime_result$code_text)  # suppress extras on failure
  base <- runtime_result$code_text
  if (is.null(extras) || length(extras) == 0) return(base)
  extra_text <- vapply(extras, rlang::quo_text, character(1))
  paste(c(base, extra_text), collapse = " +\n  ")
}
```

`ptr_register_code()` reads `ptr_state$extras()` reactively and passes it through. Users who never call `ptr_gg_extra()` see byte-identical output to pre-patch.

### Reactive flow (no loop)

- `renderPlot({...})` depends on `ptr_state$runtime()` via `ptr_extract_plot()`.
- Inside the pass, `ptr_gg_extra()` writes to `ptr_state$extras` — a **different** reactiveVal. No dependency cycle.
- `ptr_register_code()` depends on **both** `runtime()` and `extras()`. When either changes, the code binder re-renders.
- On a draw: runtime updates first (from `ptr_register_draw`) → renderPlot executes → `ptr_gg_extra` writes extras → code binder re-runs with both fresh. Two code-binder passes per draw; both cheap (string concat only).

## Alternatives Considered

### B — Pure return value, user wires their own code binder
`ptr_gg_extra()` returns an S3 object carrying `(values, exprs)`; no `ptr_state` field, no default-binder change; user writes their own `renderText` for `outputCode`.
- **Pros**: zero framework coupling, maximum YAGNI, no contract changes.
- **Cons**: defeats the intent — the advanced user wanted a *patch* that makes the built-in output reflect the extras. Every adopter would reimplement the code binder.
- **Why rejected**: boilerplate burden falls on exactly the audience we're helping.

### C — Opt-in extras binder
New `ptr_register_extras(ptr_state)` installs the reactiveVal + an observer that resets on `runtime()` updates; a separate code-binder variant opts into including extras.
- **Pros**: strictly opt-in — users who don't use extras get zero new surface.
- **Cons**: two new binders + a flag; observer ordering interacts subtly with reactive timing; the default code binder still needs extras-awareness gated somewhere.
- **Why rejected**: more surface than A for no meaningful gain; A's `!ok` suppression is simpler than an observer-reset.

### Accumulate-across-calls (Q3 alternative)
`ptr_gg_extra()` appends to a buffer rather than replacing.
- **Why rejected**: requires a "start of pass" signal inside `renderPlot` with no natural hook. Replace-per-call matches `renderPlot`'s re-run model cleanly.

### Implicit `ptr_state` lookup (Q-open alternative)
`ptr_gg_extra()` would find `ptr_state` via a module-local environment or calling-frame walk.
- **Why rejected**: magic, harder to unit-test, worse error messages when users get it wrong. Explicit arg is readable and testable.

### New S3 class + `ggplot_add` method
`ptr_gg_extra()` returns a `ptr_extras` S3 object with a custom `ggplot_add` method.
- **Why rejected**: ggplot2 already handles `plot + list(...)` via `ggplot_add.list`. No reason to add a new class.

## Key Design Decisions Log

| # | Decision | Why |
|---|---|---|
| 1 | Advanced-API only, no wrapper-app exposure | Keeps high-level surface small; extras are meaningful only when user owns `renderPlot` |
| 2 | Separate `ptr_state$extras` reactiveVal | Clean reactive semantics, no loop, default code binder "just works" |
| 3 | Replace-per-call `...` API | Single call per pass matches `renderPlot` re-run model; `...` feels native to ggplot2 |
| 4 | Suppress extras when `!runtime()$ok` | One-line check, no observer ordering, matches "extras describe successful draws" |
| 5 | Explicit `ptr_state` arg | No magic, unit-testable, readable call site |
| 6 | Return bare `list`, rely on `ggplot_add.list` | Zero new S3 surface, ggplot2 already handles lists |
| 7 | `rlang::quo_text()` for display | Preserves user's literal source including `pkg::` prefixes |

## Acceptance Criteria

- `ptr_gg_extra(ptr_state, ggplot2::theme_minimal(base_size = 16))` returns a `list` usable on the right side of `plot_obj + ...`.
- After a successful draw, `outputCode` includes `ggplot2::theme_minimal(base_size = 16)` appended with ` +\n  ` to the existing formula-driven code.
- After a failed draw (plot-stage or completion-stage), `outputCode` shows the runtime's existing code (or blank) **without** any previously captured extras.
- Users who never call `ptr_gg_extra()` see byte-identical `outputCode` to pre-patch behavior.
- Multiple extras in one call work: `ptr_gg_extra(ptr_state, theme_minimal(), scale_x_log10())`.
- Multiple `ptr_gg_extra()` calls in one `renderPlot` pass: last call wins.
- Missing `ptr_state` or older-build `ptr_state` without `extras` yields a clear `rlang::abort()` message.
- `devtools::test()` passes; `devtools::check()` remains 0E/0W.

## Testing Strategy

### Unit (`tests/testthat/test-gg-extra.R`, new)

- `ptr_gg_extra()` captures expressions via `enquos` and writes quosures to `ptr_state$extras` — mock `ptr_state` as a plain list holding a mutable closure behaving like a reactiveVal.
- Empty `...` path writes `list()` and returns `list()`.
- Non-evaluable `...`: side-effect must not fire (write happens only after `list2(...)` succeeds).
- Missing `ptr_state` and missing-`$extras` paths produce `rlang::abort()` with expected messages.
- `ptr_extract_code(runtime_result, extras)`:
  - base only (NULL extras)
  - base + extras happy path
  - `!ok` suppression
  - NULL runtime_result

### Integration (`test-embedded-extras.R` or extension of existing embedded-app test)

- `shiny::testServer()` harness exercising: build state → trigger draw → call `ptr_gg_extra()` in a simulated renderPlot block → assert `outputCode` contains extras text → trigger a failing draw → assert extras are suppressed.

### Manual

- `tests/manual/manual-test-ggpaintr.Rmd` Formula 11: update the renderPlot body to `plot_obj + ptr_gg_extra(ptr_state, ggplot2::theme_minimal(base_size = 16))`; add expected-check that `outputCode` contains the `theme_minimal` line.

## Files Touched

| File | Change |
|---|---|
| `R/paintr-app.R` | Add `ptr_gg_extra()` export with roxygen; extend `ptr_extract_code()` signature with `extras = NULL`; update `ptr_register_code()` to pass `ptr_state$extras()` through reactively; update `ptr_server_state()` to create `extras <- reactiveVal(list())` and expose on `ptr_state$extras` |
| `NAMESPACE` | Auto-regenerated — adds `ptr_gg_extra` export |
| `man/ptr_gg_extra.Rd` | Auto-generated |
| `man/ptr_extract_code.Rd` | Auto-regenerated for new `extras` arg |
| `tests/testthat/test-gg-extra.R` | New unit tests |
| `tests/manual/manual-test-ggpaintr.Rmd` | Formula 11 updated |

**Not touched**: `paintr-runtime.R`, `paintr-parse.R`, `paintr-placeholders.R`, `paintr-copy.R`, `paintr-data.R`, `paintr-upload.R`, `paintr-ui.R`, `paintr-app-bslib.R`.

## Risks

- **Two code-binder passes per draw** (runtime update → renderPlot → extras update). Both are cheap string concatenation; no perceptible user impact expected.
- **Contract extension on `ptr_state`**: embedded-app authors who introspect `ptr_state` will see a new field. Additive — nothing existing breaks.
- **Stale extras across sessions of the same draw**: suppressed by `!ok` check. If the user never calls `ptr_gg_extra()` after a successful draw, extras from the prior successful draw persist — acceptable because the user explicitly opted in, and the behavior matches "last user-declared extras" semantics.
