# Next Steps

## Purpose

This file tracks the prioritized next work for the maintained `paintr2`
workflow.

- Update when priorities materially change.
- Do not keep completed work here except to roll priorities forward.

## Current Priorities

1. Column-name robustness
   - make `var` work better with spaces and other non-syntactic column names
   - align static-data and upload-data behavior
   - add focused test coverage for spaced and non-standard names

2. Better labels for unnamed arguments
   - improve fallback labels for placeholders inside unnamed arguments
   - make repeated unnamed placeholders easier to distinguish
   - add tests for nested and repeated-placeholder calls

3. Expression support boundary
   - define support vs rejection for `reorder()`-style expressions
   - define whether standalone `(expr)` layers are supported or should error
     clearly
   - add supported and unsupported tests for the chosen boundary

4. Runtime cleanup
   - reduce stale debug code and duplicated runtime paths
   - improve readability in maintained `paintr2` helper files now that
     launch-time vs draw-time error boundaries are more explicit
   - keep existing examples and tests intact

5. Namespacing with `NS()`
   - design namespaced IDs for generated UI and server logic
   - ensure dynamic controls and exported apps can coexist without collisions
   - treat this as follow-on work after runtime semantics are stable

## Near-Term Exit Conditions

- next feature work should update `tests/testthat/` as part of the same change
- current `paintr2` runtime should remain the maintained target over older trial
  paths
- any new support boundary should be documented in both tests and notes
