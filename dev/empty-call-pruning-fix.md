# Empty-call pruning: replace name-based heuristic with diff + curated remove-list + user override

## Problem

Today, after placeholder substitution, `ptr_complete_expr()` runs two passes that delete "empty" calls:

1. `expr_remove_emptycall2()` (`R/paintr-utils.R:289`) — recursively walks the AST. For any zero-argument call whose name does not start with a hard-coded ggplot2 prefix (`geom_`, `stat_`, `scale_`, `coord_`, `facet_`, `theme_`/`theme`, `labs`, `xlab`, `ylab`, `ggtitle`, `guides`, `guide_`, `annotation_`, `borders`, `expand_limits`, `lims`, `xlim`, `ylim`, `after_stat`, `after_scale`, `stage` — see `ptr_is_gg_layer_name()` at `R/paintr-utils.R:257`), the call is set to `NULL` and dropped from the parent.
2. `ptr_remove_empty_nonstandalone_layers()` (`R/paintr-utils.R:335`) — at the top level only, deletes a layer call that is empty unless the function name starts with `geom_`/`stat_` (the "standalone" set, `ptr_can_stand_alone()` at `R/paintr-utils.R:278`).

Both passes use a name-based heuristic: "if the function name doesn't match a known ggplot2 prefix, the empty call is meaningless — delete it." That heuristic is wrong outside ggplot2.

### Repro

User formula:

```r
ggplot(data = flea |>
         pcp_select(cols) |>
         pcp_scale(method = "uniminmax") |>
         pcp_arrange(),
       mapping = aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species))
```

`aes_pcp()` (length-1 call, name not gg-prefixed) and `pcp_arrange()` (same) are both stripped from the AST during `expr_remove_emptycall2()`, breaking `mapping` and the data pipeline.

The same bug fires when the user routes a third-party call through an `expr` placeholder (`ptr_resolve_expr_expr()` parses the input with `rlang::parse_exprs()` and substitutes; the post-pass deletes the substituted call).

`pcp_theme()` written as a top-level layer is killed by pass 2 instead of pass 1, because `pcp_theme` is not in the `geom_*`/`stat_*` standalone whitelist.

### Root cause

The heuristic conflates two unrelated questions:

- "Did substitution leave an empty call behind that I should clean up?"
- "Is this name meaningful as a zero-arg call?"

It answers both via "is this name in a known ggplot2 prefix list?", which only holds for code written entirely in stock ggplot2.

## Design

Replace the two passes with a three-piece rule.

### Piece 1 — diff guard

Only consider a call for deletion if substitution actually emptied it. Concretely: walk the post-substitution tree alongside `original_expr_list` (the pre-substitution layer list, which `ptr_complete_expr()` already retains and `ptr_remove_empty_nonstandalone_layers()` already consults). Take the same path in both trees; pruning logic only fires when the post-call is empty AND the corresponding original-call was non-empty.

This single change preserves user-authored zero-arg calls everywhere:

- `aes_pcp()` written by the user (originally empty, still empty) → kept regardless of name.
- `pcp_theme()` written by the user (originally empty, still empty) → kept regardless of name.
- `pcp_arrange()` at the tail of a data pipe (originally empty, still empty) → kept regardless of name.

### Piece 2 — curated remove-list of ggplot2 names

For calls that *did* lose all their args via substitution, only delete those whose names appear in a small curated list of stock ggplot2 functions where zero-arg is provably a no-op or error. Initial list:

- `theme`
- `labs`, `xlab`, `ylab`, `ggtitle`
- `facet_wrap`, `facet_grid`, `facet_null`
- `xlim`, `ylim`, `lims`, `expand_limits`
- `guides`, `annotate`

Exclusions (deliberately *not* in the list — empty form is meaningful):

- `aes`, `vars` — empty `aes()` is valid ggplot2 (no aesthetic overrides). Stripping changes semantics.
- `element_text`, `element_line`, `element_rect`, `element_blank` — empty form means "use defaults for this slot"; provides graceful fallback when a nested placeholder misses.
- `geom_*`, `stat_*` — already handled by aesthetic inheritance from `ggplot()`. Keep as a separate `is_standalone()` protection at the top level (see Piece 3).
- `coord_*`, `scale_*`, `theme_*` — most are valid empty (`coord_cartesian()`, `scale_x_continuous()`, `theme_minimal()`), some error; mixed bag, leave to user override.

The curated list is small (~13 names), slow-moving (matches ggplot2's stable surface), and easy to audit.

### Piece 3 — user override

Add a `safe_to_remove = character()` argument on the public-facing parsing/app functions. Character vector of function names. Extends Piece 2's list. Lets the formula author opt third-party functions into the cleanup pass when they want a layer dropped on missing input.

Per the audit (see prior discussion), other ggpaintr collection-shaped args stay as lists because they're heterogeneous; `safe_to_remove` is uniformly a set of names, so a flat character vector is the right shape.

### Top-level standalone guard

Keep the `geom_*`/`stat_*` standalone whitelist as a separate guard at the top level, so a `geom_point(colour = var)` whose `var` resolves missing still survives as `geom_point()` (it inherits aesthetics from `ggplot()`).

### Combined rule

For each call in the post-substitution tree at path `p`:

```
delete iff
  is_empty_now(p)
  AND original_was_non_empty_at(p)
  AND name(p) ∈ (curated_remove_list ∪ user_safe_to_remove)
  AND (p is not top-level OR !is_standalone(name(p)))
```

The standalone clause is a no-op for nested calls; it only protects top-level `geom_*`/`stat_*`.

### Truth-table check

| Scenario | Originally | Now | Name | Outcome | Correct? |
|---|---|---|---|---|---|
| User writes `+ pcp_theme()`, no placeholder | empty | empty | not in lists | kept (diff guard fails) | ✓ |
| User writes `+ pcp_theme(title = text)`, `text` missing | non-empty | empty | not in lists | kept | ✓ |
| User writes `+ pcp_theme(title = text)`, with `safe_to_remove = "pcp_theme"`, `text` missing | non-empty | empty | in user list | deleted | ✓ |
| User writes `+ theme(plot.title = element_text(size = num))`, `num` missing | non-empty (theme); `element_text` non-empty | empty after element_text strips out | nested `element_text` not in list → kept; outer `theme(plot.title = element_text())` kept | yes, slight UX change — see "behavior change" below | ✓ |
| User writes `+ theme(plot.title = text)`, `text` missing | non-empty | empty | `theme` in curated list | deleted | ✓ |
| User writes `+ geom_point(colour = var)`, `var` missing | non-empty | empty | `geom_point` standalone | kept by standalone clause | ✓ |
| User writes `mapping = aes_pcp()` | empty | empty | not in lists | kept (diff guard fails) | ✓ |
| User pipes `... |> pcp_arrange()` | empty | empty | not in lists | kept | ✓ |
| `expr` placeholder substituted with `aes_pcp()` | placeholder originally | call now | not in lists | kept | ✓ |

### Behavior change worth flagging in NEWS

Today, `theme(plot.title = element_text(size = num))` with `num` missing collapses to nothing (cascades through deletion). Under the new rule, it renders as `theme(plot.title = element_text())` — the default plot title styling. This is more conservative and arguably more user-friendly, but it is a behavior change. Document explicitly.

Today, `+ labs()`, `+ theme()`, `+ guides()` written *literally* by the user (no placeholders) get deleted as no-op layers. Under the new rule, they're kept (diff guard fails). Almost no real formulas write these, so the regression risk is low; document anyway.

## Implementation steps

### File: `R/paintr-utils.R`

1. **Add `ptr_default_safe_to_remove()`**, an internal accessor returning the curated character vector. Define it at module load so tests can reference it.

   ```r
   ptr_default_safe_to_remove <- function() {
     c("theme", "labs", "xlab", "ylab", "ggtitle",
       "facet_wrap", "facet_grid", "facet_null",
       "xlim", "ylim", "lims", "expand_limits",
       "guides", "annotate")
   }
   ```

2. **Replace `expr_remove_emptycall2()`** with `ptr_prune_empty_substitution_artifacts()`, the single bottom-up walk described in "Sentinel-driven single-pass walk". It takes the post-substitution expr, the original expr, the resolved `remove_set` (curated ∪ user), and recurses both trees in parallel. Use `rlang::call_name()` for every name extraction; treat `NULL` as "no match, keep". Use the existing `_NULL_PLACEHOLDER` sentinel from `expr_remove_null`. Keep the depth/recursion-limit safeguard.

   Because this walk subsumes `expr_remove_null` (it sweeps sentinel-valued slots inline), the separate `expr_remove_null` call in `ptr_complete_expr()` step 2 is removed.

3. **Update `ptr_remove_empty_nonstandalone_layers()`** to take the resolved `safe_to_remove` set as well. Its rule becomes:

   ```
   delete iff
     post-layer is empty
     AND original-layer was non-empty
     AND name ∈ (curated_remove_list ∪ user_safe_to_remove)
     AND !is_standalone(name)
   ```

   The bare-symbol skip (`if (is.symbol(original_expr_list[[nn]])) next`) stays as is.

4. **Remove `ptr_is_gg_layer_name()`** if it has no remaining callers. Verify with `grep -rn 'ptr_is_gg_layer_name' R/`. Keep `ptr_can_stand_alone()` — still used by piece 3.

5. **Add `ptr_validate_safe_to_remove()`** alongside the helpers above (so the runtime/app layers can call it). Per "Name extraction handles `::` and anonymous heads": require character vector, reject NA / empty strings / entries containing `"::"` / entries that fail a `make.names()` round-trip. Use `rlang::abort()` with a `cli`-formatted message naming the offending entries. Validation should be called at every public entry that accepts `safe_to_remove` (apps, runtime, parse) — cheap to repeat and surfaces errors close to the user.

### File: `R/paintr-runtime.R`

6. **`ptr_complete_expr()`** (line 96): replace the `expr_remove_null` + `expr_remove_emptycall2` pair with a single call to the new walk per layer. Pass `original_expr_list` through and resolve `remove_set <- union(ptr_default_safe_to_remove(), user_safe_to_remove)` once at the top, then reuse it across layers. Keep `ptr_remove_empty_nonstandalone_layers()` as the top-level pass after the per-layer walk (it also receives `remove_set`). Call `ptr_validate_safe_to_remove()` on entry.

7. **`ptr_complete_expr()`** signature: add `safe_to_remove = character()` argument, default empty (since the curated list is always applied).

8. **`ptr_complete_expr_safe()`**: forward the new arg.

9. **`ptr_assemble_plot_safe()`**: no change (operates on the already-cleaned tree).

10. **`ptr_exec()`**: forward the new arg.

### File: `R/paintr-parse.R`

11. **`ptr_parse_formula()`**: no functional change required (it doesn't run the cleanup). Skip adding `safe_to_remove` here — it's a runtime concern, not a parse concern.

### File: `R/paintr-app.R`

12. **`ptr_app()`** (line 880), **`ptr_app_components()`** (line 1018), **`ptr_server_state()`** (line 240), **`ptr_module_server()`** (line 814), **`ptr_server()`** (line 936): add `safe_to_remove = character()` arg, call `ptr_validate_safe_to_remove()` on entry, store on `ptr_state`, forward to `ptr_complete_expr()`/`ptr_complete_expr_safe()` at runtime.

13. **`ptr_validate_state()`** (line 141): add validation that `safe_to_remove` is a character vector if present (cheap second-line check).

### File: `R/paintr-app-bslib.R`

14. **`ptr_app_bslib()`** (line 36): add `safe_to_remove = character()`, validate, forward.

### NEWS / docs

15. **`NEWS.md`**: drop in the following bullet (adjust release header as appropriate):

    > Empty calls left over from missing placeholder input are now preserved unless the function name is in a curated ggplot2 cleanup list (`theme()`, `labs()`, `xlab`/`ylab`/`ggtitle`, `facet_*()`, `xlim`/`ylim`/`lims`, `expand_limits`, `guides`, `annotate`) or in the new `safe_to_remove` argument on `ptr_app()` and friends. Third-party theme/coord/scale layers (e.g. `pcp_theme()`) and zero-arg calls authored directly by the user (e.g. `mapping = aes_pcp()`) now survive by default. Pass `safe_to_remove = c("pcp_theme")` to opt specific names back into the cleanup pass.

16. **Roxygen** on every public entry point that gained the arg (`ptr_app`, `ptr_app_bslib`, `ptr_app_components`, `ptr_server_state`, `ptr_module_server`, `ptr_server`, `ptr_complete_expr`, `ptr_exec`). The `@param safe_to_remove` description should enumerate the curated default list inline so users don't have to read source — `ptr_default_safe_to_remove()` is internal (not exported), but its current contents are listed in the prose.

17. **Vignette / README**: add a worked example with a third-party theme function (e.g. `pcp_theme(title = text)` + `safe_to_remove = "pcp_theme"`).

## Tests

Add a new test file `tests/testthat/test-prune-empty-substitution.R`. Cover:

- User-authored zero-arg call survives: `aes_pcp()`, `pcp_theme()`, `pcp_arrange()` at any depth.
- ggplot2 cleanup still works: `theme(plot.title = text)` with missing text → layer deleted.
- Standalone protection: `geom_point(colour = var)` with missing var → kept as `geom_point()`.
- Nested missing placeholder no longer cascades: `theme(plot.title = element_text(size = num))` with missing num → renders as `theme(plot.title = element_text())`, not deleted.
- `safe_to_remove = "pcp_theme"`: `pcp_theme(title = text)` with missing text → deleted.
- `safe_to_remove` validation: non-character, NA, empty string all error.
- `expr` placeholder substituted with `aes_pcp()` survives.
- `expr` placeholder substituted with `theme()` (zero-arg, in curated list) survives — substituted subtrees are honored verbatim regardless of `safe_to_remove`.
- `expr` placeholder substituted with `theme(plot.title = element_text())` survives, including the nested zero-arg `element_text()`.
- Original empty-call kept regardless of name (e.g. user's literal `+ labs()`).
- `patchwork::plot_layout()` with all args resolved missing: matches `safe_to_remove = "plot_layout"` (bare-name match across `::` form).
- Anonymous-head call `(function() x)()` post-substitution: kept (name extraction returns `NULL`).

Update existing tests that currently assert deletion of unknown empty calls — those expectations are inverting, so they should now assert *survival*. Run `Rscript -e 'devtools::test()'` and surface every failing test for review before changing it; do not bulk-update.

## Manual verification

Run `tests/manual/` flows for:

- A formula with a third-party theme function and a placeholder feeding its sole arg.
- A formula chaining `pcp_*` data helpers under `data = ...`.
- An `expr` placeholder with a user-typed `aes_pcp()` value.

Confirm `R CMD check` passes (`devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))`).

## Resolved design points

### Sentinel-driven single-pass walk

Collapse the current two-pass split (`expr_remove_null` then `expr_remove_emptycall2`) into one bottom-up walk per layer. `expr_remove_null` already speaks a sentinel protocol (`_NULL_PLACEHOLDER` symbol — see `R/paintr-utils.R:227`); the new pass plugs into the same protocol instead of doing its own inline `NULL` assignment.

Pseudo-implementation (target the new helper at `R/paintr-utils.R`, replace `expr_remove_emptycall2`):

```
prune(post, orig, remove_set, depth = 0L):
  if !is.call(post): return post

  for each child slot i (skip function head at index 1):
    if is.call(post[[i]]):
      orig_child <- (is.call(orig) && i <= length(orig)) ? orig[[i]] : NULL
      post[[i]]  <- prune(post[[i]], orig_child, remove_set, depth + 1)
    # else leave atomic / symbol children alone

  # sweep child slots equal to the missing sentinel — this is expr_remove_null's job
  for each slot i (descending): if identical(post[[i]], SENTINEL) post[[i]] <- NULL

  # decide if THIS node should signal deletion to its parent
  if depth > 0 AND length(post) == 1L:
    nm <- rlang::call_name(post)              # NULL for anonymous / computed heads
    if (!is.null(nm) AND nm %in% remove_set
        AND is.call(orig) AND length(orig) > 1L):
      return SENTINEL

  return post
```

Children processed first; deletions propagate upward via the sentinel return; parent sweeps its slots after children finish, so cascading (`element_text()` deleted → `theme(plot.title = )` swept → `theme()` empty → also qualifies if `theme` is in `remove_set`) falls out naturally without a fixpoint loop. Keeps the existing depth-cap safeguard from `expr_remove_emptycall2`.

The top-level layer pass (`ptr_remove_empty_nonstandalone_layers`) stays separate. "Delete a layer" means `expr_list[[nn]] <- NULL` on the layer-list dictionary, not a sentinel inside an expression.

### Path correspondence when a placeholder substituted a call

When a placeholder slot in the original tree was a bare symbol (e.g. `expr`, `var`, `text`) and substitution replaced it with a call, the post-tree and original-tree no longer have parallel substructure at that path. The original is a symbol; the post is a call with its own children.

The walk handles this gracefully: when recursing from `post[[i]]` into a child, compute `orig_child` only if `is.call(orig) && i <= length(orig)` — otherwise pass `orig_child = NULL`. With `orig = NULL` (or non-call), the diff guard `is.call(orig) AND length(orig) > 1L` evaluates `FALSE`, so nothing inside a substituted call ever gets pruned.

This is intentional behavior: an `expr` placeholder is the user's escape hatch into raw expression authoring. Whatever they typed is honored verbatim, including nested zero-arg calls and including names that appear in `safe_to_remove`. If they typed `theme()` into an `expr` input, they get `theme()`. If they want it dropped, they shouldn't have used `expr`.

This adds one more truth-table row:

| Scenario | Originally | Now | Outcome | Correct? |
|---|---|---|---|---|
| `expr` placeholder, user inputs `theme(plot.title = element_text())` | placeholder symbol | `theme(plot.title = element_text())` | kept verbatim, `safe_to_remove` ignored inside the substituted subtree | ✓ |

### Name extraction handles `::` and anonymous heads

Verified in R:

- `patchwork::plot_layout()` → `expr[[1]]` is a `::` call (not a symbol). The current `rlang::as_string(.expr[[i]][[1]])` would error on it.
- `rlang::call_name(expr)` returns `"plot_layout"` for both `plot_layout()` and `patchwork::plot_layout()`, and `NULL` for anonymous heads like `(function() 1)()`.

Use `rlang::call_name()` for every name extraction in the new pass. A `NULL` name means "unknown / computed head" — never matches any list, so the conservative default (keep) applies and the standalone clause's `is_standalone(NULL)` returns `FALSE`.

`safe_to_remove` matches on bare names. `"plot_layout"` matches both `plot_layout()` and `patchwork::plot_layout()`. Validation in `ptr_validate_safe_to_remove()` should:

- require `is.character(x)` (reject lists, factors, function objects)
- reject `NA`, empty strings
- reject entries containing `"::"` (namespaced matching isn't supported and would silently never fire — surface as an explicit error)
- reject entries that aren't valid R names (`make.names()` round-trip check) — guards against typos like `"plot layout"`

## Behavior change scope

Worth re-validating against `tests/manual/` before merging:

- `theme(plot.title = element_text(size = num))` with `num` missing now renders as `theme(plot.title = element_text())` (default styling) instead of collapsing the whole theme layer. Largest user-visible change.
- User-authored literal `+ labs()`, `+ theme()`, `+ guides()` survive. Almost no real formulas write these, but document.
- `aes()` written empty by the user is preserved (was preserved before, still is — call this out so reviewers don't regress it).

## Out of scope

- Per-call escape hatch in formula syntax (e.g. wrapping with a `keep_when_empty()` helper). Skipped — the global `safe_to_remove` plus the diff guard cover the cases we have.
- Function introspection (using `formals()` to detect "all-default-args"). Skipped — produces the wrong answer for `theme()` and friends.
- Changes to denylist content. The `expr_check` denylist is unrelated and considered complete.

---

## Prompt for fresh session

> Please implement the empty-call pruning fix described in `dev/empty-call-pruning-fix.md` for the ggpaintr R package.
>
> Context you should pick up from the file:
> - The current bug in `expr_remove_emptycall2()` (`R/paintr-utils.R:289`) and `ptr_remove_empty_nonstandalone_layers()` (`R/paintr-utils.R:335`) — name-based heuristic deletes user-authored zero-arg calls from non-ggplot2 packages.
> - The replacement design: diff guard (only prune what substitution emptied) + curated `ptr_default_safe_to_remove()` list of stock ggplot2 names + user-extensible `safe_to_remove` character vector.
> - The combined deletion rule and its truth table.
>
> Steps:
> 1. Read `dev/empty-call-pruning-fix.md` end to end.
> 2. Use Serena symbolic tools (`mcp__serena__get_symbols_overview`, `mcp__serena__find_symbol`, `mcp__serena__find_referencing_symbols`) to map every caller of `expr_remove_emptycall2`, `ptr_remove_empty_nonstandalone_layers`, and `ptr_is_gg_layer_name` before changing anything. Only fall back to `Grep`/`Read` when symbolic tools don't apply.
> 3. Implement the changes file-by-file in the order listed under "Implementation steps". Run `devtools::test()` after each file to keep regressions localized.
> 4. Add the new test file `tests/testthat/test-prune-empty-substitution.R` covering every row in the truth table.
> 5. Before adjusting any pre-existing test that fails, surface its name and assertion to the user — some are encoding the old bug as a "feature". Do not bulk-update.
> 6. Run `devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))`. Fix any new NOTEs before declaring done.
> 7. Update `NEWS.md` and the roxygen for every public entry point that gained `safe_to_remove`.
>
> Constraints:
> - Tidyverse style; snake_case; 2-space indent.
> - Internal helpers stay unexported (no `ptr_` prefix needed unless they cross into the public API — see `feedback_naming-convention` memory).
> - Use `rlang::abort()` and `cli::cli_warn()` for messages, not `stop()`/`warning()`.
> - `safe_to_remove` is a character vector. Reject lists, factors, NA, empty strings.
> - Do not edit `dev/developer-notes.md` or `dev/tasks/*.md`.
> - Do not introduce new layers of abstraction beyond what the plan calls for.
>
> Confirm completion by running `devtools::test()` and `devtools::check(...)` clean.

Confidence: 80

---

## Implementation note: diff guard removed

The original plan above introduced a "diff guard" — only flag a call for deletion if substitution emptied it (compare post tree to original tree). In review, that proved unnecessary and confusing. The shipped rule is simpler:

```
delete iff
  is_empty_now(p)
  AND name(p) ∈ (curated_remove_list ∪ user_safe_to_remove)
  AND (p is not top-level OR !is_standalone(name(p)))
  AND (orig at p is a call — i.e. NOT a bare-symbol substitution)
```

Reframed principle: presence in the remove set is the "100% safe to remove" signal; absence is "we don't know — keep". Empty calls outside the set survive regardless of how they got empty, including user-authored literal third-party calls (`aes_pcp()`, `pcp_arrange()`, `pcp_theme()`).

The `is.call(orig)` clause is the only structural check that remains. Its job is narrower than the old diff guard: it preserves expression substitutions performed by an `expr` placeholder. When the user types an expression into an `expr` input, the original at that path is a bare symbol (the placeholder name); the post is the typed expression. `is.call(orig)` is FALSE there, so the substituted subtree is honoured verbatim — even if its name is in the remove set. Justification: typing into `expr` is an explicit "keep this" signal that overrides the curated list.

Consequence vs. the diff-guard plan: user-authored literal `+ labs()`, `+ theme()`, `+ guides()` now drop. They were preserved under the diff guard. Since these are no-ops in stock ggplot2, removal is semantically safe and produces cleaner generated code.

`element_text` / `element_line` / `element_rect` / `element_blank` are still NOT in the default remove set under the shipped rule (see "Exclusions" in the original Design section). Whether to add them is deferred to a later pass.
