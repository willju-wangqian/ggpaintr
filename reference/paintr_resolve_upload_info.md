# Resolve Uploaded Dataset Metadata

Resolve Uploaded Dataset Metadata

## Usage

``` r
paintr_resolve_upload_info(input, upload_id, strict = FALSE)
```

## Arguments

- input:

  A Shiny input-like object.

- upload_id:

  The upload placeholder id.

- strict:

  Whether missing uploads should error.

## Value

A list with `data`, `object_name`, `file_name`, and `code_text`, or
`NULL` when `strict = FALSE` and no upload was supplied.
