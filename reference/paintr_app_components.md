# Build Reusable App Components for ggpaintr

Build Reusable App Components for ggpaintr

## Usage

``` r
paintr_app_components(
  formula,
  envir = parent.frame(),
  copy_rules = NULL,
  placeholders = NULL
)
```

## Arguments

- formula:

  A single formula string.

- envir:

  Environment used to resolve local data objects.

- copy_rules:

  Optional named list of copy overrides.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

## Value

A list with `ui` and `server`.
