# Tests for the partition_view seam (ADR 0026 rank 11 / plan 0026-partition-view).
#
# DERIVED IMPLEMENTATION-BLIND from the plan's Interface Contract + BDD ONLY
# (dev/plans/0026-partition-view/01-partition-view-seam.html). The entry points
# under test (new_partition_view / partition_host_owned_keys /
# partition_renders_section, R/paintr-partition-view.R) did not exist when these
# were written; expected values come from the contract's stated role->value
# mapping and the partition rule (CONTEXT.md), never from the source.
#
# Pure-function tests: no app boot, no NOT_CRAN, no registry. Run via
#   Rscript -e 'devtools::test(filter="partition-view")'
#
# Traceability: each test names the contract clause / BDD scenario it covers.

# ---- new_partition_view: construction + worked examples -------------------

test_that("new_partition_view returns a ptr_partition_view object [Contract: constructor / return type]", {
  v <- new_partition_view("single")
  expect_s3_class(v, "ptr_partition_view")
})

test_that("BDD S1: single instance owns all keys and renders its section", {
  # Contract worked example: new_partition_view("single") ->
  #   host_owned_keys = character(0), renders_section = TRUE.
  v <- new_partition_view("single")
  expect_equal(partition_host_owned_keys(v), character(0))
  expect_true(partition_renders_section(v))
})

test_that("BDD S2: panel host binds only the panel keys (formula-local excluded)", {
  # Contract worked example: panel_host, panel_keys={b}, consumer_keys={a,b} ->
  #   host_owned_keys = setdiff({a,b},{b}) = {a}  (a is formula-local by the
  #   partition rule: referenced in exactly one formula => not panel-owned).
  v <- new_partition_view("panel_host", panel_keys = "b", consumer_keys = c("a", "b"))
  expect_equal(partition_host_owned_keys(v), "a")
  expect_false(partition_renders_section(v))
})

test_that("BDD S3: embed instance defers the panel keys to the host", {
  # Contract worked example: instance, panel_keys={g,h} ->
  #   host_owned_keys = panel_keys = {g,h} (instance returns panel_keys directly).
  v <- new_partition_view("instance", panel_keys = c("g", "h"))
  expect_equal(partition_host_owned_keys(v), c("g", "h"))
  expect_false(partition_renders_section(v))
})

# ---- negative space: invalid inputs the contract rejects ------------------

test_that("BDD S4: an unknown surface role is rejected at construction [Contract: error mode match.arg]", {
  # Contract: role checked with match.arg(role, c("single","panel_host","instance")).
  # Assert the SPECIFIC error mode: the message lists at least one allowed role.
  expect_error(new_partition_view("bogus"), "single|panel_host|instance")
})

test_that("new_partition_view rejects a non-character panel_keys [Contract: assert is.character]", {
  # Contract validation clause: panel_keys asserted is.character. Pin the
  # message to "character" (the stated check) so a missing-function error does
  # NOT satisfy this -- it must fail for the contract's reason, not absence.
  expect_error(new_partition_view("instance", panel_keys = 1:2), "character")
})

test_that("new_partition_view rejects a non-character consumer_keys [Contract: assert is.character]", {
  # Contract validation clause: consumer_keys asserted is.character.
  expect_error(
    new_partition_view("panel_host", panel_keys = "b", consumer_keys = 1:2),
    "character"
  )
})

test_that("partition_host_owned_keys rejects a non-view input [Contract: assert inherits ptr_partition_view]", {
  # Contract: accessors assert inherits(view,"ptr_partition_view") and fail
  # fast on a raw list. Pin to the class name so a missing-function error is
  # not mistaken for the rejection.
  expect_error(partition_host_owned_keys(list(role = "single")), "ptr_partition_view")
})

test_that("partition_renders_section rejects a non-view input [Contract: assert inherits ptr_partition_view]", {
  expect_error(partition_renders_section(list(role = "single")), "ptr_partition_view")
})

# ---- boundary cases over the input domain ---------------------------------

test_that("single role ignores supplied panel_keys/consumer_keys -> still character(0) [Contract: single -> character(0)]", {
  # Contract: for role single, host_owned is character(0) unconditionally;
  # consumer_keys is consulted ONLY for panel_host.
  v <- new_partition_view("single", panel_keys = c("x", "y"), consumer_keys = c("x", "y", "z"))
  expect_equal(partition_host_owned_keys(v), character(0))
})

test_that("panel_host where every consumer key is panel-owned -> empty complement [boundary: setdiff -> character(0)]", {
  # consumer={a}, panel={a} => setdiff = character(0): nothing is formula-local,
  # the host binds all of them.
  v <- new_partition_view("panel_host", panel_keys = "a", consumer_keys = "a")
  expect_equal(partition_host_owned_keys(v), character(0))
})

test_that("panel_host with no panel keys -> complement is every consumer key [boundary: empty panel_keys]", {
  # panel=character(0) => setdiff(consumer, {}) = consumer: all keys formula-local.
  v <- new_partition_view("panel_host", panel_keys = character(0), consumer_keys = c("a", "b"))
  expect_equal(partition_host_owned_keys(v), c("a", "b"))
})

test_that("instance with no panel keys -> empty complement [boundary: empty panel_keys]", {
  v <- new_partition_view("instance", panel_keys = character(0))
  expect_equal(partition_host_owned_keys(v), character(0))
})

test_that("instance ignores consumer_keys -> returns panel_keys verbatim [Contract: instance -> panel_keys]", {
  # Contract: instance returns panel_keys directly; consumer_keys is not consulted.
  v <- new_partition_view("instance", panel_keys = c("g"), consumer_keys = c("a", "b", "c"))
  expect_equal(partition_host_owned_keys(v), "g")
})

# ---- invariants / postconditions ------------------------------------------

test_that("partition_host_owned_keys always returns a character vector [Invariant]", {
  for (v in list(
    new_partition_view("single"),
    new_partition_view("panel_host", panel_keys = "b", consumer_keys = c("a", "b")),
    new_partition_view("instance", panel_keys = c("g", "h"))
  )) {
    expect_type(partition_host_owned_keys(v), "character")
  }
})

test_that("partition_renders_section returns a single logical, TRUE iff single [Invariant]", {
  expect_identical(partition_renders_section(new_partition_view("single")), TRUE)
  expect_identical(
    partition_renders_section(new_partition_view("panel_host", panel_keys = "b", consumer_keys = c("a", "b"))),
    FALSE
  )
  expect_identical(
    partition_renders_section(new_partition_view("instance", panel_keys = c("g", "h"))),
    FALSE
  )
})

test_that("panel_host net-binding invariant: resolutions minus complement == panel-owned subset [Invariant: binder net effect]", {
  # The binder binds setdiff(names(resolutions), host_owned_keys). For a panel
  # host that resolves the consumer keys, this must equal exactly the panel keys
  # present among the consumers -- the partition's whole point. Derived from the
  # contract's host_owned = setdiff(consumer, panel), independent of impl.
  consumer_keys <- c("a", "b", "c")
  panel_keys <- c("b", "c")
  v <- new_partition_view("panel_host", panel_keys = panel_keys, consumer_keys = consumer_keys)
  host_owned <- partition_host_owned_keys(v)
  bound <- setdiff(consumer_keys, host_owned)
  expect_equal(sort(bound), sort(intersect(consumer_keys, panel_keys)))
})

test_that("panel_host complement never includes a panel key [Invariant: panel keys stay host-bound]", {
  v <- new_partition_view("panel_host", panel_keys = c("b", "c"), consumer_keys = c("a", "b", "c"))
  expect_length(intersect(partition_host_owned_keys(v), c("b", "c")), 0L)
})
