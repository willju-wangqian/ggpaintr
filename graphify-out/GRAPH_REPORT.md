# Graph Report - .  (2026-04-08)

## Corpus Check
- 94 files · ~81,925 words
- Verdict: corpus is large enough that graph structure adds value.

## Summary
- 240 nodes · 355 edges · 63 communities detected
- Extraction: 81% EXTRACTED · 19% INFERRED · 0% AMBIGUOUS · INFERRED: 67 edges (avg confidence: 0.88)
- Token cost: 0 input · 0 output

## God Nodes (most connected - your core abstractions)
1. `ggpaintr` - 22 edges
2. `ggpaintr` - 22 edges
3. `Package Index` - 21 edges
4. `ggpaintr Extensibility` - 19 edges
5. `ggpaintr Placeholder Registry` - 16 edges
6. `Current status` - 14 edges
7. `Manual Checklist for ggpaintr` - 13 edges
8. `paintr_build_runtime()` - 12 edges
9. `Notes index` - 11 edges
10. `ggpaintr_server_state()` - 11 edges

## Surprising Connections (you probably didn't know these)
- `NEWS` --conceptually_related_to--> `Supported public surface`  [INFERRED]
  NEWS.md → README.md
- `Project overview` --rationale_for--> `Formula string authoring model`  [INFERRED]
  working_scripts/notes/project-overview.md → README.md
- `Project overview` --rationale_for--> `Placeholder registry`  [INFERRED]
  working_scripts/notes/project-overview.md → README.md
- `Apple Touch Icon` --conceptually_related_to--> `ggpaintr Logo Mark`  [INFERRED]
  docs/apple-touch-icon.png → pkgdown/favicon/apple-touch-icon.png
- `Current status` --references--> `Archived legacy package`  [EXTRACTED]
  working_scripts/notes/current-status.md → NEWS.md

## Hyperedges (group relationships)
- **Publication Readiness Workflow** — codex_workflow_readme_doc, publication_loop_prompt_doc, publication_phase_prompt_doc, publication_assessment_prompt_doc, codex_batch1_prompt_doc, codex_pass1_prompt_doc, publication_improvements_batch1_prompt_doc, publication_improvements_batch2_3_prompt_doc, publication_readiness_prompt_doc [INFERRED 0.86]
- **Maintained Note System** — knowledge_schema_doc, notes_index_doc, current_status_doc, project_overview_doc, testing_strategy_doc, start_codex_doc, next_steps_doc [INFERRED 0.89]
- **Shiny integration helper surface** — ggpaintr_server_state, ggpaintr_bind_helpers, ggpaintr_bind_code, ggpaintr_bind_export, ggpaintr_bind_draw, ggpaintr_bind_control_panel, ggpaintr_bind_plot, ggpaintr_bind_error, ggpaintr_controls_ui, ggpaintr_outputs_ui [EXTRACTED 1.00]
- **Placeholder registry flow** — ggpaintr_placeholder, ggpaintr_effective_placeholders, ggpaintr_missing_expr, copy_defaults, copy_rules, generate_shiny [EXTRACTED 1.00]
- **Shiny integration helpers** — ggpaintr_server, ggpaintr_server_state, ggpaintr_bind_control_panel, ggpaintr_bind_draw, ggpaintr_bind_plot, ggpaintr_bind_error, ggpaintr_app [INFERRED 0.86]
- **Placeholder and copy customization system** — ggpaintr_placeholder, ggpaintr_effective_placeholders, ggpaintr_missing_expr, paintr_effective_copy_rules, paintr_resolve_copy, paintr_formula [INFERRED 0.87]
- **Runtime build and extraction flow** — paintr_formula, ggpaintr_runtime_input_spec, paintr_build_runtime, paintr_get_plot, ggpaintr_plot_value, ggpaintr_code_value [INFERRED 0.84]
- **Mtcars scatter plot structure** — mtcars_scatter, mtcars_dataset, mpg_variable, disp_variable [INFERRED 0.95]
- **ggpaintr App Icon Composition** — docs_apple_touch_icon_120x120, docs_apple_touch_icon_120x120_palette, docs_apple_touch_icon_120x120_brushes, docs_apple_touch_icon_120x120_hexagonal_badge [INFERRED 0.90]
- **ggpaintr Icon Composition** — apple_touch_icon_ggpaintr_brandmark, apple_touch_icon_paint_palette, apple_touch_icon_brushes, apple_touch_icon_hexagonal_badge [EXTRACTED 1.00]
- **ggpaintr Logo Composition** — apple_touch_icon_120x120_png_hexagonal_badge, apple_touch_icon_120x120_png_painter_palette, apple_touch_icon_120x120_png_paint_brushes, apple_touch_icon_120x120_png_wordmark [INFERRED 0.95]
- **ggpaintr Favicon Composition** — hexagonal_badge, paint_palette, paintbrushes, ggpaintr_wordmark [INFERRED 0.93]
- **ggpaintr Logo Composition** — hexagonal_badge, paint_palette, paintbrushes, ggpaintr_wordmark [EXTRACTED 1.00]
- **Favicon Brand Identity** — apple_touch_icon_60x60, ggpaintr_brand_mark, paint_palette_brush_and_droplet_motif [INFERRED 0.91]
- **Shiny integration helper surface** — graph_report_ggpaintr_server_state, graph_report_ggpaintr_bind_helpers, graph_report_ggpaintr_bind_code, graph_report_ggpaintr_bind_export, graph_report_ggpaintr_bind_draw, graph_report_ggpaintr_bind_control_panel, graph_report_ggpaintr_bind_plot, graph_report_ggpaintr_bind_error, graph_report_ggpaintr_controls_ui, graph_report_ggpaintr_outputs_ui [EXTRACTED 1.00]
- **Placeholder registry flow** — graph_report_ggpaintr_placeholder, graph_report_ggpaintr_effective_placeholders, graph_report_ggpaintr_missing_expr, graph_report_copy_defaults, graph_report_copy_rules, graph_report_generate_shiny [EXTRACTED 1.00]
- **Runtime build and extraction flow** — graph_report_paintr_formula, graph_report_ggpaintr_runtime_input_spec, graph_report_paintr_build_runtime, graph_report_paintr_get_plot, graph_report_ggpaintr_plot_value, graph_report_ggpaintr_code_value [INFERRED 0.94]
- **ggpaintr Icon Composition** — graph_report_apple_touch_icon_ggpaintr_brandmark, graph_report_apple_touch_icon_paint_palette, graph_report_apple_touch_icon_brushes, graph_report_apple_touch_icon_hexagonal_badge [EXTRACTED 1.00]
- **ggpaintr Logo Composition** — graph_report_apple_touch_icon_120x120_png_hexagonal_badge, graph_report_apple_touch_icon_120x120_png_painter_palette, graph_report_apple_touch_icon_120x120_png_paint_brushes, graph_report_apple_touch_icon_120x120_png_wordmark [EXTRACTED 1.00]
- **Docs Site Interaction Stack** — docs_pkgdown_site_script, bootstrap_toc_bootstrap_table_of_contents, headroom_jquery_headroom_plugin, clipboard_clipboard_js, search_mark_js, search_fuse_js, search_autocomplete_js, bootstrap_bootstrap_bundle_5_2_2, jquery_jquery_3_6_0 [INFERRED 0.90]
- **Docs Theme Controls** — docs_lightswitch_theme_toggle, docs_lightswitch_theme_persistence, bootstrap_bootstrap_bundle_5_2_2 [INFERRED 0.90]

## Communities

### Community 0 - "Publication Planning Notes"
Cohesion: 0.1
Nodes (41): AGENTS instructions, Archived legacy package, Batch 1 run prompt, Pass 1 run prompt, Codex publication workflow README, Copy-rule ergonomics, CRAN comments, Current repo state tracking (+33 more)

### Community 1 - "Package Overview Features"
Cohesion: 0.13
Nodes (36): Column name normalization, Copy customization, Custom placeholder registries, Custom Shiny integration hooks, ggpaintr Workflow, ggpaintr, ggpaintr, ggpaintr: Formula-Driven ggplot Shiny Apps (+28 more)

### Community 2 - "Placeholder Workflow Docs"
Cohesion: 0.12
Nodes (24): bind_ui() hook, build_ui() hook, copy_defaults, copy_rules, Articles, ggpaintr Placeholder Registry, Generate a Standalone Shiny App Script, Exportable custom placeholders (+16 more)

### Community 3 - "Shiny Binding API"
Cohesion: 0.16
Nodes (21): Compiled internal runtime contract, ggpaintr Extensibility, Bind Default Code Rendering into a Shiny App, Bind Export Behavior into a Shiny App, Return Default Error UI from a Runtime Result, ggpaintr_bind_code(), ggpaintr_bind_control_panel(), ggpaintr_bind_draw() (+13 more)

### Community 4 - "Docs Site JS Stack"
Cohesion: 0.23
Nodes (13): Bootstrap Bundle 5.2.2, Bootstrap Table of Contents, clipboard.js, KaTeX Auto Render Script, Persistent Theme Preference, Docs Light/Dark Theme Toggle, Pkgdown Docs Site Script, headroom.js (+5 more)

### Community 5 - "Brand Asset Variants"
Cohesion: 0.33
Nodes (9): Apple Touch Icon, Apple Touch Icon 152x152, ggpaintr Brandmark, ggpaintr Logo, ggpaintr Logo Mark, ggpaintr Wordmark, Hexagonal Blue Badge, Paint Palette (+1 more)

### Community 6 - "Release and Cleanup"
Cohesion: 0.29
Nodes (7): CRAN cleanup prepared, Docs-first documentation, Changelog, Legacy package archived, Public API focus, Roxygen2 documentation, ggpaintr 0.1.0 release

### Community 7 - "Column Name Normalization"
Cohesion: 0.33
Nodes (6): Exact `var` placeholder matching at runtime, Normalize Dataset Column Names for `ggpaintr`, `ggpaintr`-safe column names, Preserve `data.frame` subclasses, Reserved-word collision avoidance, Syntactic, unique column names

### Community 8 - "Authors and Citation"
Cohesion: 0.4
Nodes (5): Authors and Citation, Jinji Pang, ggpaintr package citation, Wangqian Ju, Zhili Qiao

### Community 9 - "Wordmark Icon Variant"
Cohesion: 0.4
Nodes (5): ggpaintr Logo, Hexagonal Badge, Paint Brushes, Painter's Palette, ggpaintr Wordmark

### Community 10 - "App Icon Variant A"
Cohesion: 0.5
Nodes (4): ggpaintr App Icon, Paintbrushes, Hexagonal Badge, Paint Palette

### Community 11 - "App Icon Variant B"
Cohesion: 0.5
Nodes (4): ggpaintr App Icon, Paintbrushes, Paint Palette, ggpaintr Wordmark

### Community 12 - "Cardinal Image Variants"
Cohesion: 0.5
Nodes (4): Cardinal (bird), Photograph of a cardinal perched on a branch, Bird Perched on Branch, Bright Red Plumage

### Community 13 - "Touch Icon Motif"
Cohesion: 0.67
Nodes (3): ggpaintr touch icon, ggpaintr Brand Mark, Paint Palette, Brush, and Droplet Motif

### Community 14 - "404 Page Structure"
Cohesion: 0.67
Nodes (3): Content not found notice, Navbar, Page not found (404)

### Community 15 - "Site Dependency Metadata"
Cohesion: 1.0
Nodes (2): Site data dependencies, Frontend dependency assets

### Community 16 - "Community 16"
Cohesion: 1.0
Nodes (0): 

### Community 17 - "Graph Report Changelog Mirror"
Cohesion: 1.0
Nodes (2): Changelog, ggpaintr 0.1.0

### Community 18 - "Mtcars Scatter Example"
Cohesion: 1.0
Nodes (1): Mtcars scatter

### Community 19 - "Mtcars Dataset"
Cohesion: 1.0
Nodes (1): `mtcars` dataset

### Community 20 - "MPG Placeholder"
Cohesion: 1.0
Nodes (1): `mpg`

### Community 21 - "Disp Placeholder"
Cohesion: 1.0
Nodes (1): `disp`

### Community 22 - "Circular Favicon Variant"
Cohesion: 1.0
Nodes (1): Blue-centered circular favicon with warm orange rim

### Community 23 - "Palette Icon"
Cohesion: 1.0
Nodes (1): Paint Palette

### Community 24 - "Brushes Icon"
Cohesion: 1.0
Nodes (1): Paintbrushes

### Community 25 - "Hex Badge Icon"
Cohesion: 1.0
Nodes (1): Hexagonal Badge

### Community 26 - "Link Symbol"
Cohesion: 1.0
Nodes (1): Link Icon

### Community 27 - "Logo Asset"
Cohesion: 1.0
Nodes (1): ggpaintr logo

### Community 28 - "Logo Icon Asset"
Cohesion: 1.0
Nodes (1): ggpaintr logo icon

### Community 29 - "Project Favicon"
Cohesion: 1.0
Nodes (1): Project Favicon

### Community 30 - "Orange Blue Favicon"
Cohesion: 1.0
Nodes (1): Abstract circular favicon logo

### Community 31 - "Favicon Asset"
Cohesion: 1.0
Nodes (1): ggpaintr favicon

### Community 32 - "Apple Touch Icon"
Cohesion: 1.0
Nodes (1): ggpaintr Apple Touch Icon

### Community 33 - "Orange Blue Mark"
Cohesion: 1.0
Nodes (1): Abstract Orange-and-Blue Logo Mark

### Community 34 - "Community 34"
Cohesion: 1.0
Nodes (0): 

### Community 35 - "Community 35"
Cohesion: 1.0
Nodes (0): 

### Community 36 - "Community 36"
Cohesion: 1.0
Nodes (0): 

### Community 37 - "Community 37"
Cohesion: 1.0
Nodes (0): 

### Community 38 - "Community 38"
Cohesion: 1.0
Nodes (0): 

### Community 39 - "Community 39"
Cohesion: 1.0
Nodes (0): 

### Community 40 - "Community 40"
Cohesion: 1.0
Nodes (0): 

### Community 41 - "Community 41"
Cohesion: 1.0
Nodes (0): 

### Community 42 - "Community 42"
Cohesion: 1.0
Nodes (0): 

### Community 43 - "Graph Report Placeholder"
Cohesion: 1.0
Nodes (1): ggpaintr_placeholder

### Community 44 - "Graph Report Effective Placeholders"
Cohesion: 1.0
Nodes (1): ggpaintr_effective_placeholders

### Community 45 - "Graph Report Missing Expr"
Cohesion: 1.0
Nodes (1): ggpaintr_missing_expr

### Community 46 - "Graph Report Copy Defaults"
Cohesion: 1.0
Nodes (1): copy_defaults

### Community 47 - "Graph Report Copy Rules"
Cohesion: 1.0
Nodes (1): copy_rules

### Community 48 - "Graph Report Generate Shiny"
Cohesion: 1.0
Nodes (1): generate_shiny

### Community 49 - "Graph Report Formula"
Cohesion: 1.0
Nodes (1): paintr_formula

### Community 50 - "Graph Report Runtime Input Spec"
Cohesion: 1.0
Nodes (1): ggpaintr_runtime_input_spec

### Community 51 - "Graph Report Build Runtime"
Cohesion: 1.0
Nodes (1): paintr_build_runtime

### Community 52 - "Graph Report Get Plot"
Cohesion: 1.0
Nodes (1): paintr_get_plot

### Community 53 - "Graph Report Plot Value"
Cohesion: 1.0
Nodes (1): ggpaintr_plot_value

### Community 54 - "Graph Report Code Value"
Cohesion: 1.0
Nodes (1): ggpaintr_code_value

### Community 55 - "Graph Report Brandmark"
Cohesion: 1.0
Nodes (1): apple_touch_icon_ggpaintr_brandmark

### Community 56 - "Graph Report Palette"
Cohesion: 1.0
Nodes (1): apple_touch_icon_paint_palette

### Community 57 - "Graph Report Brushes"
Cohesion: 1.0
Nodes (1): apple_touch_icon_brushes

### Community 58 - "Graph Report Hex Badge"
Cohesion: 1.0
Nodes (1): apple_touch_icon_hexagonal_badge

### Community 59 - "Graph Report Hex Logo"
Cohesion: 1.0
Nodes (1): apple_touch_icon_120x120_png_hexagonal_badge

### Community 60 - "Graph Report Palette Logo"
Cohesion: 1.0
Nodes (1): apple_touch_icon_120x120_png_painter_palette

### Community 61 - "Graph Report Brush Logo"
Cohesion: 1.0
Nodes (1): graph_report_apple_touch_icon_120x120_png_paint_brushes

### Community 62 - "Graph Report Wordmark Logo"
Cohesion: 1.0
Nodes (1): graph_report_apple_touch_icon_120x120_png_wordmark

## Knowledge Gaps
- **97 isolated node(s):** `Graphify knowledge graph`, `Developer scratchpad`, `Copy-rule ergonomics`, `Current repo state tracking`, `Wangqian Ju` (+92 more)
  These have ≤1 connection - possible missing edges or undocumented components.
- **Thin community `Site Dependency Metadata`** (2 nodes): `Site data dependencies`, `Frontend dependency assets`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 16`** (2 nodes): `ggpaintr_ids()`, `ggpaintr_server_state()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Changelog Mirror`** (2 nodes): `Changelog`, `ggpaintr 0.1.0`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Mtcars Scatter Example`** (1 nodes): `Mtcars scatter`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Mtcars Dataset`** (1 nodes): ``mtcars` dataset`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `MPG Placeholder`** (1 nodes): ``mpg``
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Disp Placeholder`** (1 nodes): ``disp``
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Circular Favicon Variant`** (1 nodes): `Blue-centered circular favicon with warm orange rim`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Palette Icon`** (1 nodes): `Paint Palette`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Brushes Icon`** (1 nodes): `Paintbrushes`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Hex Badge Icon`** (1 nodes): `Hexagonal Badge`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Link Symbol`** (1 nodes): `Link Icon`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Logo Asset`** (1 nodes): `ggpaintr logo`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Logo Icon Asset`** (1 nodes): `ggpaintr logo icon`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Project Favicon`** (1 nodes): `Project Favicon`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Orange Blue Favicon`** (1 nodes): `Abstract circular favicon logo`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Favicon Asset`** (1 nodes): `ggpaintr favicon`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Apple Touch Icon`** (1 nodes): `ggpaintr Apple Touch Icon`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Orange Blue Mark`** (1 nodes): `Abstract Orange-and-Blue Logo Mark`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 34`** (1 nodes): `ggpaintr_bind_helpers()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 35`** (1 nodes): `ggpaintr_bind_code()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 36`** (1 nodes): `ggpaintr_bind_export()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 37`** (1 nodes): `ggpaintr_bind_draw()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 38`** (1 nodes): `ggpaintr_bind_control_panel()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 39`** (1 nodes): `ggpaintr_bind_plot()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 40`** (1 nodes): `ggpaintr_bind_error()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 41`** (1 nodes): `ggpaintr_controls_ui()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Community 42`** (1 nodes): `ggpaintr_outputs_ui()`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Placeholder`** (1 nodes): `ggpaintr_placeholder`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Effective Placeholders`** (1 nodes): `ggpaintr_effective_placeholders`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Missing Expr`** (1 nodes): `ggpaintr_missing_expr`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Copy Defaults`** (1 nodes): `copy_defaults`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Copy Rules`** (1 nodes): `copy_rules`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Generate Shiny`** (1 nodes): `generate_shiny`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Formula`** (1 nodes): `paintr_formula`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Runtime Input Spec`** (1 nodes): `ggpaintr_runtime_input_spec`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Build Runtime`** (1 nodes): `paintr_build_runtime`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Get Plot`** (1 nodes): `paintr_get_plot`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Plot Value`** (1 nodes): `ggpaintr_plot_value`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Code Value`** (1 nodes): `ggpaintr_code_value`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Brandmark`** (1 nodes): `apple_touch_icon_ggpaintr_brandmark`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Palette`** (1 nodes): `apple_touch_icon_paint_palette`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Brushes`** (1 nodes): `apple_touch_icon_brushes`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Hex Badge`** (1 nodes): `apple_touch_icon_hexagonal_badge`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Hex Logo`** (1 nodes): `apple_touch_icon_120x120_png_hexagonal_badge`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Palette Logo`** (1 nodes): `apple_touch_icon_120x120_png_painter_palette`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Brush Logo`** (1 nodes): `graph_report_apple_touch_icon_120x120_png_paint_brushes`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.
- **Thin community `Graph Report Wordmark Logo`** (1 nodes): `graph_report_apple_touch_icon_120x120_png_wordmark`
  Too small to be a meaningful cluster - may be noise or needs more connections extracted.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **Why does `NEWS` conceptually_related_to `Supported public surface` across `NEWS.md` and `README.md`?**
  _inferred connection - not explicitly stated in source; connects across different repos/directories; peripheral node `NEWS` unexpectedly reaches hub `Supported public surface`_
- **Why does `Project overview` rationale_for `Formula string authoring model` across `working_scripts/notes/project-overview.md` and `README.md`?**
  _inferred connection - not explicitly stated in source; connects across different repos/directories; peripheral node `Formula string authoring model` unexpectedly reaches hub `Project overview`_
- **Why does `Project overview` rationale_for `Placeholder registry` across `working_scripts/notes/project-overview.md` and `README.md`?**
  _inferred connection - not explicitly stated in source; connects across different repos/directories; peripheral node `Placeholder registry` unexpectedly reaches hub `Project overview`_
- **What connects `Graphify knowledge graph`, `Developer scratchpad`, `Copy-rule ergonomics` to the rest of the system?**
  _97 weakly-connected nodes found in the extracted graph._
- **Should `Publication Planning Notes` be split into smaller, more focused modules?**
  _Cohesion score 0.1 with 41 nodes._
- **Should `Package Overview Features` be split into smaller, more focused modules?**
  _Cohesion score 0.13 with 36 nodes._
- **Should `Placeholder Workflow Docs` be split into smaller, more focused modules?**
  _Cohesion score 0.12 with 25 nodes._