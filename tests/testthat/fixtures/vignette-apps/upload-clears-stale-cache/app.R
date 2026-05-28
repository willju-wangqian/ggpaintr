pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# upload-clears-stale-cache: regression for `state$upstream_cache` returning
# stale resolves after a source rebind.
#
# The cache key in `ptr_resolve_upstream()` is the deparsed substituted
# expression. For `ppUpload(df_main)`, the substituted symbol is always
# "df_main" (= node$auto_name, derived from node$default in paintr-ids.R).
# With `df_main <- mtcars` at top level here, the first consumer resolve
# at boot evaluates `df_main` via eval_env's parent.frame() chain and gets
# mtcars. That result is cached. Uploading a CSV with disjoint columns
# correctly binds the new frame into eval_env[["df_main"]] (local), but
# the cache short-circuits on the same key and returns the stale mtcars
# rows -- so non-shared consumer pickers (x, y) keep showing "mpg" /
# "cyl" indefinitely.
library(shiny)
df_main <- mtcars
ptr_app("ppUpload(df_main) |> ggplot(aes(x = ppVar(mpg), y = ppVar(cyl))) + geom_point()")
