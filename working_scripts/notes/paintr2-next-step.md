# Plan: `paintr2-next-step.md`

## Summary

Prioritize the next `paintr2` work in this order:

1. error handling and plot-panel feedback
2. column-name robustness for spaces and non-syntactic names
3. better labels for unnamed arguments
4. support boundary for `reorder()` and richer expression patterns
5. cleanup of function files and legacy debug paths
6. namespacing support with `NS()`

## Next Tasks

### 1. Error Handling and Plot Feedback

- Add a consistent error-capture path around expression completion and plot rendering.
- Show readable errors in the Shiny UI instead of failing silently or only through `renderPlot()`.
- Keep generated code visible when plot construction fails, when possible.
- Cover malformed `expr`, missing data, unknown objects, and invalid uploads.

### 2. Column-Name Robustness

- Make `var` work reliably with non-syntactic column names, especially names containing spaces.
- Distinguish plain selected columns from expression-style input where needed.
- Ensure uploaded datasets and static datasets behave the same way.
- Add tests for spaced column names and non-standard names.

### 3. Unnamed Argument Labels

- Improve fallback labels for placeholders inside unnamed arguments.
- Make repeated unnamed placeholders readable and distinguishable.
- Cover nested and multi-placeholder calls such as `facet_grid(var ~ var + var)`.
- Add tests for labeling behavior in complex calls.

### 4. Expression Pattern Support

- Define whether `reorder()`-style expressions are supported, partially supported, or rejected.
- Define whether standalone `(expr)` layers are supported or should error clearly.
- Add supported/unsupported tests for these patterns.
- Update notes to reflect the supported boundary.

### 5. Cleanup of Runtime Files

- Remove stale debug code and non-maintained experimental paths from the current runtime path.
- Remove or isolate `browser()` calls that should not remain in maintained code.
- Reduce duplication and improve readability in the `paintr2` helper files.
- Keep current examples and tests intact.

### 6. Namespacing with `NS()`

- Design namespaced IDs for generated UI and server logic.
- Make dynamic `var`, checkbox controls, upload controls, and exported apps respect the namespace.
- Ensure multiple generated modules can coexist without ID collisions.
- Add integration tests after the current runtime semantics are stable.

## Backlog Cleanup

- Remove `upload is not really implemented` from the backlog.
- Remove items already marked `[DONE]`.
- Move `var + 1` from open backlog to implementation notes or tests.

## Test Plan

Automated:
- add focused `testthat` coverage for each new task
- expand fixtures and supported/unsupported use-case tests
- add tests for malformed `expr`, spaced column names, unnamed argument labels, and `reorder()` formulas

Manual:
- verify Shiny error display in the live app
- verify upload plus spaced-column workflows
- verify improved labels in generated UI
- verify exported apps preserve new behavior

## Assumptions

- `upload` is no longer an open implementation gap.
- The next implementation cycle should focus on stability and usability before modular composition.
- The maintained target is the current `paintr2` runtime, not older trial scripts.
