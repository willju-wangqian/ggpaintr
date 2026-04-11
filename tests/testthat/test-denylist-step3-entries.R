# Tests for the 17 new denylist entries added in the refine-feature Step 3
# workflow (codex/publication-loop branch).
#
# New entries covered here:
#   str2lang, str2expression, call, as.call, quote, bquote, as.symbol, as.name,
#   pipe, ls, objects, search, searchpaths, attr, attributes, slot
#   (pipe added in "system escape" group; the other 16 span meta-eval and info
#   disclosure groups)
#
# Also probes the attr<- / attributes<- assignment-form gap identified in review.

# =============================================================================
# str2lang — string-to-code bypass
# =============================================================================

test_that("denylist-step3: str2lang() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('str2lang("system(\'id\')")')),
    "not allowed"
  )
})

test_that("denylist-step3: str2lang as bare symbol (higher-order) is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('sapply(1, str2lang)')),
    "not allowed"
  )
})

test_that("denylist-step3: parenthesised str2lang bypass attempt is blocked", {
  # (str2lang)("system('id')") — compound head path
  expect_error(
    validate_expr_safety(rlang::parse_expr('(str2lang)("system(\'id\')")')),
    "not allowed"
  )
})

# =============================================================================
# str2expression — string-to-code bypass
# =============================================================================

test_that("denylist-step3: str2expression() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('str2expression("system(\'id\')")')),
    "not allowed"
  )
})

test_that("denylist-step3: str2expression as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), str2expression)")),
    "not allowed"
  )
})

# =============================================================================
# call — call constructor bypass
# =============================================================================

test_that("denylist-step3: call() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('call("system", "id")')),
    "not allowed"
  )
})

test_that("denylist-step3: call as bare symbol (higher-order) is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(list(), call)")),
    "not allowed"
  )
})

# =============================================================================
# as.call — call constructor from list bypass
# =============================================================================

test_that("denylist-step3: as.call() call is blocked", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('as.call(list(as.symbol("system"), "id"))')
    ),
    "not allowed"
  )
})

test_that("denylist-step3: as.call as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), as.call)")),
    "not allowed"
  )
})

# =============================================================================
# quote — quote bypass
# =============================================================================

test_that("denylist-step3: quote() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('quote(system("id"))')),
    "not allowed"
  )
})

test_that("denylist-step3: quote as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), quote)")),
    "not allowed"
  )
})

# =============================================================================
# bquote — quasi-quote bypass
# =============================================================================

test_that("denylist-step3: bquote() call is blocked", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('bquote(.(quote(system))("id"))')
    ),
    "not allowed"
  )
})

test_that("denylist-step3: bquote as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, bquote)")),
    "not allowed"
  )
})

# =============================================================================
# as.symbol — symbol constructor bypass
# =============================================================================

test_that("denylist-step3: as.symbol() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('as.symbol("system")')),
    "not allowed"
  )
})

test_that("denylist-step3: as.symbol as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), as.symbol)")),
    "not allowed"
  )
})

# =============================================================================
# as.name — alias for as.symbol
# =============================================================================

test_that("denylist-step3: as.name() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('as.name("system")')),
    "not allowed"
  )
})

test_that("denylist-step3: as.name as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), as.name)")),
    "not allowed"
  )
})

# =============================================================================
# pipe — system pipe bypass
# =============================================================================

test_that("denylist-step3: pipe() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('pipe("id", "r")')),
    "not allowed"
  )
})

test_that("denylist-step3: pipe as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), pipe)")),
    "not allowed"
  )
})

# =============================================================================
# ls — information disclosure
# =============================================================================

test_that("denylist-step3: ls() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("ls()")),
    "not allowed"
  )
})

test_that("denylist-step3: ls as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, ls)")),
    "not allowed"
  )
})

# =============================================================================
# objects — alias for ls
# =============================================================================

test_that("denylist-step3: objects() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("objects()")),
    "not allowed"
  )
})

test_that("denylist-step3: objects as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), objects)")),
    "not allowed"
  )
})

# =============================================================================
# search — information disclosure
# =============================================================================

test_that("denylist-step3: search() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("search()")),
    "not allowed"
  )
})

test_that("denylist-step3: search as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), search)")),
    "not allowed"
  )
})

# =============================================================================
# searchpaths — information disclosure
# =============================================================================

test_that("denylist-step3: searchpaths() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("searchpaths()")),
    "not allowed"
  )
})

test_that("denylist-step3: searchpaths as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), searchpaths)")),
    "not allowed"
  )
})

# =============================================================================
# attr — attribute access
# =============================================================================

test_that("denylist-step3: attr() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('attr(x, "class")')),
    "not allowed"
  )
})

test_that("denylist-step3: attr as bare symbol (higher-order) is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), attr)")),
    "not allowed"
  )
})

# =============================================================================
# attributes — bulk attribute access
# =============================================================================

test_that("denylist-step3: attributes() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("attributes(x)")),
    "not allowed"
  )
})

test_that("denylist-step3: attributes as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), attributes)")),
    "not allowed"
  )
})

# =============================================================================
# slot — S4 attribute access
# =============================================================================

test_that("denylist-step3: slot() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('slot(x, "data")')),
    "not allowed"
  )
})

test_that("denylist-step3: slot as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), slot)")),
    "not allowed"
  )
})

# =============================================================================
# Concrete attack vectors from the Step 3 scan
# =============================================================================

test_that("attack-vector: str2lang string-to-code bypass is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('str2lang("system(\'id\')")')),
    "not allowed"
  )
})

test_that("attack-vector: call() constructor bypass is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('call("system", "id")')),
    "not allowed"
  )
})

test_that("attack-vector: as.call(list(as.symbol(...))) combined bypass is blocked", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('as.call(list(as.symbol("system"), "id"))')
    ),
    "not allowed"
  )
})

test_that("attack-vector: quote(system('id')) bypass is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('quote(system("id"))')),
    "not allowed"
  )
})

test_that("attack-vector: pipe('id', 'r') system pipe is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('pipe("id", "r")')),
    "not allowed"
  )
})

test_that("attack-vector: ls() info disclosure is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("ls()")),
    "not allowed"
  )
})

test_that("attack-vector: attr(x, 'class') attribute access is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('attr(x, "class")')),
    "not allowed"
  )
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("edge-case: bquote nested with quote bypass attempt is blocked", {
  # bquote(.(quote(system))("id")) — both bquote and quote appear in the AST
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('bquote(.(quote(system))("id"))')
    ),
    "not allowed"
  )
})

test_that("edge-case: sapply(1, str2lang) — function-as-arg form of str2lang is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, str2lang)")),
    "not allowed"
  )
})

test_that("edge-case: (str2lang)('...') parenthesised bypass is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('(str2lang)("system(\'id\')")')),
    "not allowed"
  )
})

test_that("edge-case: ls nested inside otherwise-safe call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("paste(ls(), collapse = ' ')")),
    "not allowed"
  )
})

test_that("edge-case: as.name inside as.call list is blocked", {
  # as.call appears first and will trigger
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('as.call(list(as.name("eval"), quote(1)))')
    ),
    "not allowed"
  )
})

# =============================================================================
# Bug probe: attr<- and attributes<- assignment forms (assignment-form gap)
#
# The reviewer noted that `attr<-` and `attributes<-` are NOT in the denylist.
# These tests document the current (broken) behaviour — they are expected to
# FAIL until the bug is fixed.  Run them as passing by checking that the
# functions are NOT blocked, which demonstrates the gap.
# =============================================================================

test_that("bug-probe: attr<- assignment form — NOT currently blocked (gap)", {
  # `attr<-(x, 'class') <- 'evil'` desugars to a call headed by `attr<-`.
  # This SHOULD be blocked but is not — we document the current state.
  # If this test starts failing it means the gap was fixed; update accordingly.
  result <- tryCatch(
    {
      validate_expr_safety(rlang::parse_expr('`attr<-`(x, "class", "evil")'))
      "not_blocked"
    },
    error = function(e) "blocked"
  )
  # Now blocked — gap closed
  expect_equal(result, "blocked",
    label = "attr<- is blocked by the denylist")
})

test_that("bug-probe: attributes<- assignment form — now blocked", {
  result <- tryCatch(
    {
      validate_expr_safety(rlang::parse_expr('`attributes<-`(x, list(class = "evil"))'))
      "not_blocked"
    },
    error = function(e) "blocked"
  )
  expect_equal(result, "blocked",
    label = "attributes<- is blocked by the denylist")
})

# =============================================================================
# No false positives — safe expressions unaffected
# =============================================================================

test_that("no-false-positive: ggplot(data = mtcars, aes(x = mpg)) passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("ggplot(data = mtcars, aes(x = mpg))"))
  )
})

test_that("no-false-positive: geom_point(color = 'red', size = 3) passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("geom_point(color = 'red', size = 3)"))
  )
})

test_that("no-false-positive: sqrt(x + 1) passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("sqrt(x + 1)"))
  )
})

test_that("no-false-positive: paste0('a', 'b') passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("paste0('a', 'b')"))
  )
})
