# Clone an Evaluation Environment and Inject Uploads

Clone an Evaluation Environment and Inject Uploads

## Usage

``` r
paintr_prepare_eval_env(paintr_obj, input, envir = parent.frame())
```

## Arguments

- paintr_obj:

  A `paintr_obj`.

- input:

  A Shiny input-like object.

- envir:

  A parent environment.

## Value

An evaluation environment containing uploaded datasets.
