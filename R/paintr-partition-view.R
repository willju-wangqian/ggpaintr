# ---- partition_view: one origin for shared-key ownership -------------------
#
# ggpaintr answers one yes/no question for every shared key and every UI
# surface: "does THIS surface render and bind THIS shared key?" Historically
# that question was answered by three hand-derived `host_owned_keys` complement
# sets (single-instance / panel-host / embed-instance) plus a parallel
# `render_shared_section` boolean, kept consistent by hand. When they drifted, a
# shared widget rendered but never bound (the 2026-05-22 partition-binder bug).
#
# `partition_view` is the single, pure, non-reactive origin for that decision.
# Every shared surface constructs a view from the partition facts it already
# holds and QUERIES it, rather than re-encoding the complement. The accessors
# return exactly the value each call site used to compute inline, so behaviour
# is byte-stable; the partition ALGORITHM (`shared_partition()`) is unchanged --
# this seam is about the CONSUMPTION side only. See ADR 0026 (rank 11).
#
# Roles (the three surfaces):
#   "single"      single-instance `ptr_app` -- no coordinator; owns ALL shared
#                 keys and renders its inline shared section.
#   "panel_host"  the standalone shared panel (L2) -- owns the cross-formula
#                 (>=2 formula) panel keys; formula-local keys belong to each
#                 instance.
#   "instance"    an embedded `ptr_server` instance -- owns its formula-local
#                 keys; the coordinator's panel keys belong to the host.

ptr_partition_roles <- c("single", "panel_host", "instance")

# Construct a partition view.
#
# role          one of `ptr_partition_roles`.
# panel_keys    shared keys owned by the standalone panel (>=2 formulas).
#               character(0) for "single". For "instance" this is the set the
#               coordinator handed in (the bundle's `names(dots$shared)`).
# consumer_keys all shared consumer/value keys reachable from this surface's
#               tree(s). Consulted ONLY for role "panel_host".
new_partition_view <- function(role,
                               panel_keys = character(),
                               consumer_keys = character()) {
  role <- match.arg(role, ptr_partition_roles)
  # NULL is R's empty-names sentinel (`names()` of an unnamed/empty list) and
  # maps to "no keys" -- normalise it to character(0) before validating, so the
  # is.character check still rejects genuinely-wrong types (e.g. integer).
  panel_keys <- panel_keys %||% character()
  consumer_keys <- consumer_keys %||% character()
  assertthat::assert_that(
    is.character(panel_keys),
    is.character(consumer_keys)
  )
  structure(
    list(
      role = role,
      panel_keys = panel_keys,
      consumer_keys = consumer_keys
    ),
    class = "ptr_partition_view"
  )
}

# The complement set the shared-consumer binder must NOT bind, because some
# other surface owns those keys (today's `host_owned_keys` argument to
# `ptr_bind_local_shared_consumers()`):
#   single     -> character(0)                      (binds every key)
#   panel_host -> setdiff(consumer_keys, panel_keys) (binds only panel keys)
#   instance   -> panel_keys                        (binds only formula-local)
partition_host_owned_keys <- function(view) {
  if (!inherits(view, "ptr_partition_view")) {
    rlang::abort("`view` must be a <ptr_partition_view> object.")
  }
  switch(view$role,
    single     = character(0),
    panel_host = setdiff(view$consumer_keys, view$panel_keys),
    instance   = view$panel_keys
  )
}

# Whether this surface renders its inline formula-local shared section (today's
# `render_shared_section` boolean). TRUE iff single-instance: only that surface
# renders all shared keys inline with no standalone panel.
partition_renders_section <- function(view) {
  if (!inherits(view, "ptr_partition_view")) {
    rlang::abort("`view` must be a <ptr_partition_view> object.")
  }
  identical(view$role, "single")
}
