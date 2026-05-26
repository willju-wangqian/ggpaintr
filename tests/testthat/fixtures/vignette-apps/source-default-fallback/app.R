# Boot scaffolding (NOT vignette code). Fixture for the source-default
# fallback fix: a pipeline-head `ppUpload(df_main)` with a `default_arg`-
# validated symbol must resolve via `state$eval_env`'s parent chain when
# no file is uploaded — i.e. the consumer pickers nested under it must
# populate at boot, with NO upload click.
#
# Pipeline-head shape (NOT bare-layer): one intermediate `filter()` stage
# is enough to push the source out of `data_arg` and into `pipeline_source_ids`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

df_main <- mtcars

ptr_app(
  paste(
    "ppUpload(df_main) |>",
    "dplyr::filter(ppExpr(mpg > 0)) |>",
    "ggplot(aes(x = ppVar(mpg), y = ppVar(hp))) +",
    "geom_point()"
  )
)
