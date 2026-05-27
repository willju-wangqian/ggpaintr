# Forensic audit: presence-proxy assertions in tests/testthat — 2026-05-27

Trigger: `aes(y = ppVar(adj))` default-seed bug slipped past super-1. Three blind spots: (a) `expect_picker_populated` is an OPTIONS-list presence proxy that passes when `<option value="adj">` exists regardless of selection state, (b) `expect_no_plot_error` originally scoped to `#ptr_plot` and missed `#ptr_error`-routed inline errors, (c) no `aes(... y = ...)` propagation probe.

This report classifies every call site of the relevant helpers in `tests/testthat/`. Read tools only — **no test files modified**.

## Summary (counts)

| Helper | Total | Classification | Notes |
|---|---|---|---|
| `expect_picker_populated` | 40 | S=6, O=33, ?=1 | S = should be `expect_picker_selected`. O = legit Options check (post-upload column scope, default-arg fallback proof, or paired with `expect_equal(get_value(input))`). |
| `expect_no_plot_error` | 22 | **C=5** (critical), other=17 | C = test deliberately produces an inline `#ptr_error` then calls helper. The newly widened helper now scans `.ptr-alert--error` anywhere and will INVERT these tests (regress). |
| `expect_dom_id` | 99 | W=0, L≈99 | Vast majority are structural anchors (`ptr_update_plot`, `ptr_plot`, `ptr_code`, `shared_ds`, host scaffolds, ns-prefixed plot/code ids). The four placeholder-id calls (`ggplot_1_1_ppVar_NA`, `xlim_1_range_NA`, `ggplot_2_1_colvars_NA`, `ggplot_1_2_ppVar_NA`) are each followed by `expect_equal(get_value(input))` / `expect_match(code, ...)` — anchor only, not weak. |
| `expect_code_nonempty` | 12 | W=2-3, L≈10 | Most are paired with `expect_match(code, "<literal>", fixed = TRUE)` for a propagation literal. Two clearly weak: l.191 (use-cases l2-noid), l.485 (code-window-options-html). One borderline at l.397 (use-cases l3-gg-extra) but it's an intermediate snapshot before injecting `add_log`. |
| `grepl(... html)` | 64 | mostly L | Almost all are precise: error-message substrings, class-name substrings, id-attribute substrings, file-path-in-shortcut-input. No presence-proxy patterns hiding bugs beyond the `expect_picker_populated` shape itself. |

## Top priority strengthenings

### Critical: `expect_no_plot_error` widening WILL REGRESS 5 tests

The uncommitted helper widening (verified via `git diff HEAD tests/testthat/helper-super-pressure.R`) makes `expect_no_plot_error` fail whenever ANY `.ptr-alert--error` exists in the document. But several `test-super-pressure.R` scenarios deliberately PRODUCE an inline error in `#ptr_error` and then call `expect_no_plot_error(app)` to assert the plot host is OK while the error pane is populated. These will now invert.

1. **`tests/testthat/test-super-pressure.R:302`** — D9 validate_input scenario sets ppPower to 1.5; line 295 asserts `"must be in [0,1]"` appears in `#ptr_error`; line 302 then calls `expect_no_plot_error(app)`. The widened helper will FAIL because `.ptr-alert--error` is now present.
2. **`tests/testthat/test-super-pressure.R:900`** — super-4 I5 scenario, validate_input rejection of non-hex ppColor. `#ptr_error` carries `"must be #RRGGBB hex"` (line 895); line 900 calls `expect_no_plot_error(app)`. Same inversion.
3. **`tests/testthat/test-super-pressure.R:936`** — super-4 J1/J3 denylist probe. Line 932 asserts `"is not allowed in an \`expr\` input"` in `#ptr_error`; line 936 calls `expect_no_plot_error(app)`. Same inversion.
4. **`tests/testthat/test-super-pressure.R:952`** — super-4 B3 toggle differential at the end of the same test_that. The denylist error pane from the J1 block has NOT been cleared. Same inversion.
5. **`tests/testthat/test-super-pressure.R:1014`** — super-4 no-default validate_input scenario, parallel of #2. Same inversion.

Recommended replacement (host-scoped variant): introduce `expect_no_host_plot_error(app, host_id)` that scans ONLY `#<host_id>` (the original semantic), and keep the widened variant for the happy-path checks that want "no error anywhere". Or: add a `tolerate_inline_error_in = "ptr_error"` arg so the deliberate-inline-error tests opt out.

### S-class (`_populated` should be `_selected`): default-seeding intent

6. **`tests/testthat/test-default-seeding-with-source-upstream.R:31, 53`** — Already paired with `expect_equal(get_value(input))` on the preceding line, so the bug shape IS caught — `expect_picker_populated` is redundant but the test still fails on regression. Low priority: drop or convert to `expect_picker_selected` for clarity.
7. **`tests/testthat/test-adr15-consumer-binding.R:54`** — `expect_picker_populated(app, "geom_point_1_1_ppVar_NA", "mpg")` after uploading sample.csv. Fixture formula `aes(x = ppVar(mpg), y = ppVar(shared = "v"))` has positional default `mpg`. Intent IS "default seeds". Replace with `expect_picker_selected(app, "geom_point_1_1_ppVar_NA", "mpg")`.
8. **`tests/testthat/test-adr15-consumer-binding.R:121`** — `expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "mpg")` for fixture `ggplot(mtcars, aes(x = ppVar(mpg)))`. Pure default-seed assertion. Replace with `expect_picker_selected`.
9. **`tests/testthat/test-source-default-fallback.R:46-47`** — fixture has `aes(x = ppVar(mpg), y = ppVar(hp))` with `default_arg` fallback contract. The whole test's purpose is "default seeds picker at boot, no upload". Two calls; both should be `expect_picker_selected`.
10. **`tests/testthat/test-shortcut-custom-source-env-bind.R:37`** — comment at l.36 literally says `"mpg" is the default selection from the formula` but the assertion is `_populated`. Replace with `expect_picker_selected(app, var_id, "mpg")`.

### W-class (`expect_code_nonempty` without a follow-up content check)

11. **`tests/testthat/test-e2e-vignette-examples-shinytest2.R:191`** — `use-cases l2-noid`. Inputs set, draw clicked, `expect_code_nonempty` is the only assertion on `ptr_code`. A regression that emits `"placeholder"` or empty-AES code would still pass. Add `expect_match(app$get_value(output = "ptr_code"), "Sepal.Length", fixed = TRUE)` mirroring the sibling test at l.142.
12. **`tests/testthat/test-e2e-vignette-examples-shinytest2.R:485`** — `customization custom-app-id`. Same shape: code-nonempty is the only ptr_code claim. Add a literal-match for one set input value.

### `expect_picker_populated` cases worth a closer look

13. **`tests/testthat/test-super-pressure.R:85, 153`** — `expect_picker_populated(app, "ggplot_1_2_ppVar_NA", "adj")` is the original bug-class site. Line 94 ALREADY added `expect_picker_selected(...)` for the with-default variant. The no-default variant at line 153 has NO `_selected` peer because that fixture's `aes(y = ppVar())` (no default) doesn't expect a particular column. Verify there's no analogous default-seed claim — looks correct as-is.
14. **`tests/testthat/test-super-pressure.R:443`** — `expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "mpg")` is AFTER `set_sentinel(app, "ggplot_1_1_ppVar_NA", "mpg")` (l.437). Pure post-set echo — should be `expect_picker_selected` (intent is "the value I set is now the current selection", not "mpg is in the options list").
15. **`tests/testthat/test-super-pressure.R:521, 522`** — layer-data ppUpload populated with df_rug's columns (mpg, wt). Options check is correct (positive scope vs. negative-scope test on Sepal.Length at l.527). Legit O.

## Notes & uncertainties

- **The `expect_picker_populated` helper's substring match has a second blind spot beyond selection state**: `grepl("adj", html, fixed = TRUE)` matches `adj` anywhere in the HTML — including in a column-name that *contains* the substring (e.g. `adjusted_value` would pass an `expect_picker_populated(app, id, "adj")` check). Low real-world risk on the current fixture column names (mpg, hp, cyl, Sepal.Length, …), but flag-worthy. The new `expect_picker_selected` uses `expect_equal` so it's exact.

- **`?` for `tests/testthat/test-finding3-panel-source-nonshared-consumer.R:59,62`** — formula has `ppVar('xa')`/`ppVar('xb')` where `xa`/`xb` are NOT columns in `penguins.csv`. The picker should fall back to first column. The assertion checks `bill_length_mm`/`bill_depth_mm` (the first columns of the upload). Intent could be (a) "first-column fallback selection" (S) or (b) "options list contains real columns" (O). The test reads to me as (b), since it's testing panel-source resolution end-to-end. Keep as O but note this is a candidate for `expect_picker_selected` if the first-column-fallback contract is what we want to pin.

- **Same iceberg bug shape elsewhere?** I searched for other helpers that do substring-on-HTML where a value assertion would be stronger. The `grepl(col, html)` loop at `test-shortcut-custom-source-env-bind.R:38-43` is the same shape as `_populated` but for multiple columns — it's deliberately a "scope contains these columns" Options check, so it's legit.

- **Sibling output-error class scoping**: when widening `expect_no_plot_error`, the natural alternative is `expect_no_inline_error(app, error_id = "ptr_error")` — that helper already exists in `helper-vignette-apps.R:134` and is widely used. The super-pressure widening would be cleaner as a SEPARATE `expect_no_inline_error_anywhere(app)` rather than overloading the existing host-scoped semantic. Recommend backing out the widening on `expect_no_plot_error` and introducing the new helper instead, then audit the 22 callers to decide which want the host-scoped vs. document-scoped check.

- **The newly added `expect_picker_selected` helper at `helper-super-pressure.R:266-275`** uses `app$get_value(input = input_id)` which returns the live binding-registry value (the canonical source of truth per project memory `adr12-spec-roundtrip` block at l.42-47). Strong.

## Trust caveats

The following classifications were made by skim or by reading 10-30 lines of context, not by tracing the full reactive graph of each fixture. Verify by hand before mass-rewriting:

- **All 33 `O`-class `_populated` callers** — I assumed Options-check intent when (a) the fixture formula has no positional default for that placeholder, OR (b) the assertion follows an upload/source-set action and checks that a column from the upload becomes a choice, OR (c) the call is preceded/followed by an `expect_equal(get_value(input))` that nails the selection. Did NOT trace every fixture's formula.

- **The 5 `C`-class `expect_no_plot_error` regressions** — confirmed by reading the helper diff (`git diff HEAD tests/testthat/helper-super-pressure.R`) AND each callsite's preceding lines that assert the inline error is present. Did NOT actually run the suite to observe the failures. The widening is uncommitted on the working tree, so the authoritative gate (`NOT_CRAN=true Rscript -e devtools::test()`) would currently demonstrate this — but I deliberately did not run tests per task constraints.

- **`expect_dom_id` skim verdict** — I sampled the 99 callers via `grep -v` filter and full-line list; concluded all 99 are anchor / structural. There may be 1-2 placeholder-id anchors I'd reclassify as redundant-but-not-weak on closer reading; the safe verdict is "no obvious bug-hiding W-class instance".

- **`expect_code_nonempty` line 397** — `use-cases l3-gg-extra` is an intermediate snapshot before clicking `add_log`. I called it "borderline" because the post-injection assertions DO check the new layer reaches the code, so the intermediate snapshot is a soft anchor not a weak claim. Could be argued either way.

## Recommended ordering

1. **Back out or split the `expect_no_plot_error` widening immediately** (items #1-5 above). It will break 5 super-pressure cases as soon as it lands.
2. Convert items #7-10 to `expect_picker_selected` (low-risk targeted swaps; the bug shape from the trigger event would have been caught).
3. Add follow-up `expect_match` for items #11-12 (the two genuinely weak `expect_code_nonempty` callers).
4. Consider tightening item #14 (`test-super-pressure.R:443`) as a clarity improvement; not a bug-hiding case but reads more honestly as `_selected`.
