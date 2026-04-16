---
name: expr-safety-verbose
type: decision
status: accepted
scope: [safety, expr, verbose, api]
created: 2026-04-10
---

# Expr Safety Guard & Verbose Mode

## Understanding

- ggpaintr's `expr` placeholder lets users type arbitrary R expressions into a Shiny text input
- These expressions are parsed via `rlang::parse_expr()` and evaluated via `eval()` in `ptr_assemble_plot()` (`R/paintr-runtime.R:184`); `ggplot_build()` appears only as a post-assembly render probe in `ptr_validate_plot_render_safe()` — confirmed dangerous (`system("bad")` executes at eval time)
- R has no sandboxing; sourcing a malicious `.R` script has the same risk
- Primary use case: self-use / trusted users. Public deployment is secondary but supported.
- Current implementation uses a ~90-function allowlist — too restrictive and high-maintenance

## Decisions

### 1. Replace allowlist with denylist as default

**Default mode**: denylist of obviously dangerous functions.
Categories blocked:
- System escape: `system`, `system2`, `shell`, `shell.exec`
- File I/O: `file.remove`, `unlink`, `readLines`, `writeLines`, `saveRDS`, `download.file`, etc.
- Meta-eval (bypass vectors): `eval`, `evalq`, `parse`, `do.call`, `match.fun`, `get`, `mget`
- State mutation: `assign`, `rm`, `attach`, `source`, `library`, `require`, `Sys.setenv`, `options`
- Dangerous internals: `.Internal`, `.Primitive`, `.Call`, `.External`, `q`, `quit`

Anonymous `function()` is **allowed** — legitimate use in `stat_function(fun = function(x) ...)`.

### 2. Single `expr_check` parameter with override portal

```r
ptr_app(..., expr_check = TRUE)
ptr_exec(..., expr_check = TRUE)
```

Values:
- `FALSE` → no checking
- `TRUE` → package default denylist
- `list(deny_list = c(...))` → custom denylist
- `list(allow_list = c(...))` → strict allowlist (for public deployments)
- `list(allow_list = c(...), deny_list = c(...))` → deny wins conflicts: `setdiff(allow_list, deny_list)`
- `list()` (empty) → package default denylist

Replaces old `safe_expr` parameter.

### 3. Validation logic

```
validate_expr_safety(expr, expr_check):
  if FALSE → skip
  if TRUE → walk AST against package default denylist
  if list with allow_list → strict allowlist mode (after setdiff with deny_list if present)
  if list with deny_list only → custom denylist mode
  if empty list → package default denylist
```

Namespaced calls (`base::system`) checked by matching both qualified and bare names.

### 4. `verbose` via R option

```r
options(ggpaintr.verbose = TRUE)   # default TRUE during dev, FALSE at publication
```

- Helper: `ptr_verbose()` wraps `getOption("ggpaintr.verbose", default = TRUE)`
- Controls only dev diagnostics (`cli_inform` internals)
- Errors (`rlang::abort`) and warnings (`cli_warn`) always fire
- No new function parameters — zero threading

## Alternatives Considered

| Option | Rejected because |
|--------|-----------------|
| Keep allowlist as default | Too restrictive, high maintenance, users hit "not allowed" on legitimate functions |
| Three separate params (`expr_check`, `expr_denylist`, `expr_allowlist`) | Clutters API; single structured param is cleaner |
| Block `function` keyword | Breaks `stat_function(fun = function(x) ...)` |
| Thread `verbose` as parameter | Adds params to too many internal functions |
| `verbose` on `ptr_state` or `context` | Still requires threading; R option is zero-cost |
| Single callback for validation | Too low-level for most users |

## Risks

- Denylist is bypassable via anonymous functions: `(function() system("bad"))()`. Accepted trade-off — primary users are trusted; public deployers can use `allow_list` for strict mode.
- R option for verbose is process-level, not per-session. Acceptable since it controls only dev diagnostics.

## Acceptance Criteria

- [ ] `validate_expr_safety` uses denylist by default
- [ ] `expr_check = FALSE` disables all checking
- [ ] `expr_check = list(allow_list = ...)` enables strict allowlist
- [ ] `expr_check = list(deny_list = ...)` uses custom denylist
- [ ] Conflicting entries resolved via `setdiff(allow_list, deny_list)`
- [ ] Namespaced calls (`pkg::fn`) caught in both modes
- [ ] Old `safe_expr` parameter renamed to `expr_check` everywhere
- [ ] `ptr_verbose()` helper reads `getOption("ggpaintr.verbose")`
- [ ] Dev diagnostics gated behind `ptr_verbose()`
- [ ] Tests cover denylist defaults, custom lists, conflict resolution, verbose gating
