# ggpaintr browser test report — 2026-05-07

**Verdict: PASS with minor observations.** All 6 testable gallery sections render plots end-to-end, generated code matches expectations, all adversarial probes (empty selections, non-numeric inputs, bad column names, rapid clicks, big thresholds, mid-flight shared-widget changes) leave the Shiny session alive and surface errors via `#ptr_error` rather than crashing. Four sections (7.2–7.5) skipped because their extension packages are not installed.

## Summary

| Section | Formula label  | Result | Notes |
|---------|----------------|--------|-------|
| 4       | mpg-paintr     | PASS   | All widgets functional, code panel + plot render correctly |
| 5       | pipe-paintr    | PASS   | Per-stage Data tab + Update Data + stage-enable checkbox all work |
| 6.1     | plotly-paintr  | PASS   | `interactive_plot` populates with plotly graph after Update Plot |
| 6.2     | ggiraph-paintr | PASS   | `interactive_plot` populates; tooltip-aes widget appears only after switching layer tab (see Discoveries) |
| 7.1     | pcp-paintr     | PASS   | `colvars` consumer + Update Data + Update Plot all work |
| 7.2     | ridges-paintr  | SKIP   | `ggridges` not installed |
| 7.3     | repel-paintr   | SKIP   | `ggrepel` not installed |
| 7.4     | alluvial-paintr| SKIP   | `ggalluvial` not installed |
| 7.5     | dist-paintr    | SKIP   | `ggdist` not installed |
| 8       | shared-paintr  | PASS   | Top-level `vars` widget + Draw all → both plots render with shared selection |
| 9       | gg-extra       | PASS   | Toggle log-scale updates code panel and plot; 5 rapid clicks no error |

No FAIL bugs found.

## Discoveries (non-bug observations)

1. **Inner-aes widgets only materialize after layer-tab activation (6.2 ggiraph).** On initial load, `aes(tooltip = var)` inside `geom_point_interactive(...)` does NOT register an input. The widget id `ggiraph_demo-geom_point_interactive_1_var_NA` only appears after `Shiny.setInputValue("...-ptr_layer_select", "geom_point_interactive", {priority:"event"})`. This matches the visible accordion/tab UI behavior (widgets are built when their tab is rendered), but is a discoverability gotcha for headless drivers. Outer aes widgets and other layer args appeared at load. Severity: cosmetic / docs.

2. **Non-numeric input to `num` widget is silently coerced to `NA_real_` (4 mpg-paintr).** Setting `geom_point_1_num_NA` to the string `"abc"` via `Shiny.setInputValue` produces generated code `geom_point(alpha = NA_real_, size = 2)` with no error surfaced. The plot still renders (alpha becomes default). Browser-side `numericInput` would normally reject this, so the path only exercises adversarially. Not a security concern; a stricter type-check could surface it as an inline error. Severity: minor.

3. **Empty `colvars` propagates a useful pivot_longer error (8 shared).** Setting `vars` to `[]` then Draw all surfaces `Error: cols must select at least one column.` in `plot_2`'s error panel; `plot_1` simply skips the layer (pcp_select with one column draws a degenerate but valid axes plot). Recovery on next selection works cleanly.

4. **Stage-enable toggle correctly elides pipe stages.** Disabling `ggplot_2_stage_enabled` (the `dplyr::filter(displ > num)` step) in section 5 rewrites the code panel as if that stage were never there: `mpg |> dplyr::group_by(class) |> dplyr::filter(dplyr::n() > 5) |> dplyr::ungroup() |> ggplot(...)`. Re-enabling restores it. Matches G11 spec.

5. **Range slider integer coercion.** Setting `coord_cartesian_1_range_NA` to `[1, 7]` produces generated code `coord_cartesian(xlim = c(1L, 7L), ylim = c(10L, 45L))` — integers (1L) instead of doubles. Pretty cosmetic; result is functionally identical for `coord_cartesian`. The custom `range` placeholder's `resolve_expr` does `rlang::expr(c(!!value[1], !!value[2]))`, and `Shiny.setInputValue([1,7])` gives JS numbers that R receives as integers. Severity: cosmetic.

6. **`labs()` defaults render with empty strings.** On first Update Plot in section 4 with no text inputs touched, `labs_1_text_NA` etc. are `""`, so the generated code does NOT include a `labs()` call (it's elided). Filling them adds it back. Expected behavior given `text` placeholders' empty-string semantics, but worth knowing.

## Console

No JavaScript errors observed in any session (`read_console_messages` filter `error|warning` returned nothing relevant). All R errors propagated cleanly via the inline error panel rather than killing the R process.

## Test environment

- Branch: `core-rewrite-impl`
- Port: 127.0.0.1:4321
- Browser: Chrome MCP
- Installed extension pkgs: plotly, ggiraph, colourpicker, ggpcp, GGally, dplyr, tidyr (the four skipped sections need ggridges, ggrepel, ggalluvial, ggdist)

## Reproduction launchers

Per-section launchers are at `/tmp/ggp/sec*.R` (preamble at `/tmp/ggp/preamble.R`). Each is self-contained and uses `devtools::load_all()` + the global-env binding pattern from `dev/scripts/feature-sweep.R`.

## 2026-05-07 follow-up — section 7.2–7.5

**Verdict: PASS for all four previously-skipped sections.** With `ggridges`, `ggrepel`, `ggalluvial`, `ggdist` now installed, every gallery section in the vignette renders end-to-end. Generated code matches the originals exactly (including negative `justification = -0.2` and zero `.width = 0` in 7.5). Adversarial probes (cleared `var`, non-numeric `num`) leave the Shiny session alive; errors surface via `#ptr_error` rather than crashing.

### Summary (delta)

| Section | Formula label   | Result | Notes |
|---------|-----------------|--------|-------|
| 7.2     | ridges-paintr   | PASS   | `aes(x = var, y = var)` + 3 `num`s on `geom_density_ridges_gradient`; output matches original |
| 7.3     | repel-paintr    | PASS   | Local `mt` resolved via `envir = environment()`; 3 `var`s + 4 `num`s; both layers populate cleanly |
| 7.4     | alluvial-paintr | PASS   | `fill = var` swap to/from `survey` works; clearing fill elides aes entry without error |
| 7.5     | dist-paintr     | PASS   | 4 `num`s on `stat_halfeye` (indices 1,2,3,5 — `point_colour = NA` is index 4 literal); negative + zero values round-trip correctly |

### Per-section verification

- **7.2 ridges-paintr** — set `x=Mean Temperature [F]`, `y=Month`, `scale=3`, `rel_min_height=0.01`, `bandwidth=3`. Code panel: `geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, bandwidth = 3)`. Plot rendered.
- **7.3 repel-paintr** — set `x=wt`, `y=mpg`, `label=model`, `geom_point` size=3, `geom_text_repel` size=3.5/max.overlaps=12/box.padding=0.3. Generated code matches original literal-for-literal. Local `mt` data frame visible to runtime via the `envir = environment()` seam.
- **7.4 alluvial-paintr** — set `fill=response`, `alpha=0.7`. Code panel matches original. Switched `fill` to `survey` (the same column already used on `x`) — also renders without error (per-survey strata).
- **7.5 dist-paintr** — set `y=mpg`, halfeye `adjust=0.5`, `justification=-0.2`, `.width=0`, `slab_alpha=0.6`, boxplot `width=0.15`. Code panel: `stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA, slab_alpha = 0.6)` — exact match.

### Adversarial probes

- **7.2 empty `var`** → inline error `! 'stat_density_ridges()' requires the following missing aesthetics: x.` Session alive.
- **7.2 nonsense `num="abc"`** → coerced to `NA_real_` in code panel; plot still renders with NA scale (matches existing observation #2).
- **7.3 cleared `label` var** → inline error `! 'geom_text_repel()' requires the following missing aesthetics: label.` Session alive.
- **7.3 nonsense `max.overlaps="abc"`** → `max.overlaps = NA_real_` in code; plot renders.
- **7.4 nonsense fill (`survey` reused)** → renders without error; `fill=survey` propagates as-is.
- **7.4 cleared fill `var`** → `fill =` aes entry elided from generated code; remaining plot is valid.
- **7.5 cleared y `var`** → `aes(x = factor(cyl))` (no y); both `stat_halfeye` and `geom_boxplot` still render (degenerate but no crash).
- **7.5 negative + zero `num` values** — `justification = -0.2` and `.width = 0` accepted and round-trip correctly.

### Discoveries (new)

7. **Cleared aes entries are silently elided rather than triggering an inline error (7.4 fill, 7.5 y).** When a `var` widget for an aesthetic is cleared (`null`), the runtime drops that `aes(name = ...)` entry from the generated code. For non-required aesthetics (`fill`, sometimes `y`) the plot still renders. This differs from the required-aesthetic case (7.2 `x`, 7.3 `label`) where downstream stat/geom raises the missing-aes error. Behaviour is consistent and useful — worth knowing for headless drivers that compare expected vs actual code panel.

8. **Literal `NA` arguments occupy the placeholder index sequence (7.5).** `stat_halfeye(adjust = num, justification = num, .width = num, point_colour = NA, slab_alpha = num)` produces input ids `stat_halfeye_1_num_NA, _2, _3, _5` (no `_4`). Index 4 is the literal `point_colour = NA`, which gets a position in the per-call argument numbering even though it is not a placeholder. Not a bug — but if a test driver assumed dense numbering, the missing index 4 would surprise them.

### Console / R stderr

No JavaScript errors. R stderr clean — only the expected `ggdist` `ll` mask warning on package load. No crashes; all errors surfaced inline.

### Reproduction launchers

`/tmp/ggp2/sec72.R`, `/tmp/ggp2/sec73.R`, `/tmp/ggp2/sec74.R`, `/tmp/ggp2/sec75.R` (preamble at `/tmp/ggp2/preamble.R`). Each is self-contained.
