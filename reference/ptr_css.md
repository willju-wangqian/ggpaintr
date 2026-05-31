# CSS customisation hooks for ggpaintr apps

Every raw-Shiny entry point
([`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md),
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md),
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md),
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md))
accepts a `css =` argument: a character vector of paths to `.css` files.
Each path's parent directory is registered as a Shiny resource under a
hash-derived prefix, and one `<link>` tag is emitted per file. User
stylesheets are linked *after* `ggpaintr`'s bundled stylesheet, so rules
at equal specificity win.

## Details

The bundled stylesheet exposes its theme as CSS custom properties under
the `.ptr-app` selector. Override any subset of these in a user-supplied
stylesheet to retheme without touching individual rules.

## Tokens

- `--ptr-bg`:

  App background. Default `#fafbfc`.

- `--ptr-surface`:

  Card / sidebar background. Default `#ffffff`.

- `--ptr-border`:

  Standard border colour. Default `#e7e9ee`.

- `--ptr-ink`:

  Body text colour. Default `#1f2530`.

- `--ptr-muted`:

  Muted / secondary text. Default `#6b7280`.

- `--ptr-accent`:

  Primary accent (action button bg). Default `#c1372c`.

- `--ptr-accent-strong`:

  Hover / active accent. Default `#9a2b22`.

- `--ptr-accent-weak`:

  Faint accent tint. Default `rgba(193,55,44,0.13)`.

- `--ptr-accent-line`:

  Accent border. Default `rgba(193,55,44,0.22)`.

- `--ptr-radius`:

  Large radius (cards, sidebar). Default `10px`.

- `--ptr-radius-sm`:

  Small radius (buttons, controls). Default `7px`.

- `--ptr-shadow`:

  Card shadow. Default two-layer drop.

- `--ptr-shadow-lg`:

  Floating-window shadow. Default two-layer drop.

- `--ptr-font`:

  UI font stack. Default system-ui stack.

- `--ptr-mono`:

  Mono font stack. Default system mono stack.

## Specificity

Some rules use a doubly-scoped selector (e.g.
`.ptr-app .btn.action-button`). To override these, match or exceed that
specificity in your stylesheet (either with the same selector, or with
`!important`).

## Examples

``` r
if (FALSE) { # \dontrun{
  # accent.css:
  #   .ptr-app { --ptr-accent: #2563eb; --ptr-accent-strong: #1e40af; }
  ptr_app("ggplot(mtcars, aes(ppVar, ppVar)) + geom_point()",
          css = "accent.css")
} # }
```
