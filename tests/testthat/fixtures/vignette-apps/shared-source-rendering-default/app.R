# Boot scaffolding (NOT vignette code). Cut 1 + eyeball-play interaction:
# `ppUpload(df_main, shared='ds')` combines a default-arg with the shared
# annotation. After Cut 1 drops the partition-blind skip in
# `ptr_setup_source_uis()`, the fileInput renders AND the per-instance
# pipeline observer (now reached for shared sources too) calls
# `try_bind_source_default()` (added on eyeball-play a3d1636), which
# resolves `df_main` via `state$eval_env`'s parent chain and primes
# `state$resolved_sources` / `state$bound_names` so the downstream
# `ppVar(shared='a')` picker populates with mtcars's columns at boot —
# no upload click.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

df_main <- mtcars

ptr_app(
  ppUpload(df_main, shared = "ds") |>
    ggplot(aes(x = ppVar(shared = "a"), y = ppVar(shared = "a"))) +
    geom_point()
)
