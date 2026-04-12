# Build the pkgdown site while excluding top-level .md files that pkgdown's
# hardcoded allowlist (README, LICENSE, NEWS) does not cover.
#
# pkgdown 2.2.0's internal package_mds() globs every root .md file and renders
# each one via build_home_md(). cran-comments.md is hardcoded as no-render, but
# CLAUDE.md (and any other developer-only markdown) is not. There is no public
# config option to extend that list, so we temporarily shadow package_mds in
# the pkgdown namespace for the duration of the build.

build_pkgdown_clean <- function(exclude = c("CLAUDE.md"),
                                fn = function() pkgdown::build_site_github_pages(
                                  new_process = FALSE, install = TRUE
                                )) {
  stopifnot(requireNamespace("pkgdown", quietly = TRUE))
  orig <- pkgdown:::package_mds
  patched <- function(path, in_dev = FALSE) {
    mds <- orig(path, in_dev)
    mds[!basename(mds) %in% exclude]
  }
  utils::assignInNamespace("package_mds", patched, ns = "pkgdown")
  on.exit(utils::assignInNamespace("package_mds", orig, ns = "pkgdown"), add = TRUE)
  fn()
}
