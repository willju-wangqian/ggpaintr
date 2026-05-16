# Asset-rendering test helpers.
#
# Post the asset -> htmlDependency() refactor, ggpaintr's CSS/JS no longer
# render as inline <style>/<script>/<link> tags. They ship as
# htmltools::htmlDependency() objects, which renderTags() collects into
# $dependencies (deduped by name) rather than $html. A plain as.character()
# or renderTags()$html therefore no longer contains ggpaintr.css /
# ptr_set_class etc. Tests that assert on the asset surface must render the
# resolved dependencies too.

render_with_deps <- function(ui) {
  rt <- htmltools::renderTags(ui)
  paste0(
    as.character(
      htmltools::renderDependencies(rt$dependencies, srcType = "file")
    ),
    as.character(rt$html)
  )
}

# Count non-overlapping fixed-string occurrences, returning 0 (not 1) when
# absent -- gregexpr() returns -1 with length 1 for "no match".
count_occurrences <- function(haystack, needle) {
  m <- gregexpr(needle, haystack, fixed = TRUE)[[1L]]
  if (length(m) == 1L && m[1L] == -1L) 0L else length(m)
}
