# Render a typed tree back to R source code

Render a typed tree back to R source code

## Usage

``` r
ptr_render(node, preserve_placeholders = FALSE)
```

## Arguments

- node:

  Typed tree root (or any node) to render.

- preserve_placeholders:

  If `FALSE` (default), placeholder nodes are assumed to have been
  replaced by literal nodes during substitution and are rendered as
  today (`expr_text(node$expr)` as a fallback). If `TRUE`, any surviving
  placeholder node is emitted as `ppX(current_pick)` – the
  round-trippable "formula with placeholders" view. The keyword and
  `current_pick` are read directly from the node; tests in this layer
  stub them by hand.

## Value

Character scalar of R source code.
