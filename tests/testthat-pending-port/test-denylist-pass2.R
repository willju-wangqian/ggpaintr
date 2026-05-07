# Tests for the 31 new denylist entries added in Pass 2
# (codex/publication-loop branch).
#
# Groups covered:
#   Binary I/O (4): writeBin, readBin, readChar, writeChar
#   Deserialization / workspace I/O (5): serialize, unserialize, load, save, save.image
#   Function mutation (3): body<-, formals<-, environment<-
#   Native code loading (2): dyn.load, dyn.unload
#   System mutation (5): Sys.chmod, Sys.umask, Sys.readlink, Sys.setlocale, Sys.setFileTime
#   Debugger hooks (4): debug, debugonce, undebug, browser
#   Info disclosure (3): R.home, .libPaths, .packages
#   Delayed / deferred code execution (5): reg.finalizer, addTaskCallback,
#     taskCallbackManager, setHook, packageEvent
#
# Also covers:
#   Pairlist guard in ptr_resolve_placeholder_expr (dangerous + safe cases)
#   No false positives (3 safe ggplot expressions)

# =============================================================================
# Binary I/O
# =============================================================================

test_that("denylist-pass2: writeBin() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("writeBin(x, con)")),
    "not allowed"
  )
})

test_that("denylist-pass2: writeBin as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), writeBin)")),
    "not allowed"
  )
})

test_that("denylist-pass2: readBin() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("readBin(con, what)")),
    "not allowed"
  )
})

test_that("denylist-pass2: readBin as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), readBin)")),
    "not allowed"
  )
})

test_that("denylist-pass2: readChar() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("readChar(con, nchars)")),
    "not allowed"
  )
})

test_that("denylist-pass2: readChar as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, readChar)")),
    "not allowed"
  )
})

test_that("denylist-pass2: writeChar() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("writeChar(x, con)")),
    "not allowed"
  )
})

test_that("denylist-pass2: writeChar as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), writeChar)")),
    "not allowed"
  )
})

# =============================================================================
# Deserialization / workspace I/O
# =============================================================================

test_that("denylist-pass2: serialize() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("serialize(x, NULL)")),
    "not allowed"
  )
})

test_that("denylist-pass2: serialize as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), serialize)")),
    "not allowed"
  )
})

test_that("denylist-pass2: unserialize() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("unserialize(x)")),
    "not allowed"
  )
})

test_that("denylist-pass2: unserialize as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), unserialize)")),
    "not allowed"
  )
})

test_that("denylist-pass2: load() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('load("file.RData")')),
    "not allowed"
  )
})

test_that("denylist-pass2: load as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, load)")),
    "not allowed"
  )
})

test_that("denylist-pass2: save() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('save(x, file = "f")')),
    "not allowed"
  )
})

test_that("denylist-pass2: save as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), save)")),
    "not allowed"
  )
})

test_that("denylist-pass2: save.image() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("save.image()")),
    "not allowed"
  )
})

test_that("denylist-pass2: save.image as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), save.image)")),
    "not allowed"
  )
})

# =============================================================================
# Function mutation
# =============================================================================

test_that("denylist-pass2: `body<-` assignment form is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("`body<-`(f, value)")),
    "not allowed"
  )
})

test_that("denylist-pass2: `formals<-` assignment form is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("`formals<-`(f, value)")),
    "not allowed"
  )
})

test_that("denylist-pass2: `environment<-` assignment form is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("`environment<-`(f, value)")),
    "not allowed"
  )
})

# =============================================================================
# Native code loading
# =============================================================================

test_that("denylist-pass2: dyn.load() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('dyn.load("lib.so")')),
    "not allowed"
  )
})

test_that("denylist-pass2: dyn.load as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), dyn.load)")),
    "not allowed"
  )
})

test_that("denylist-pass2: dyn.unload() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('dyn.unload("lib.so")')),
    "not allowed"
  )
})

test_that("denylist-pass2: dyn.unload as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), dyn.unload)")),
    "not allowed"
  )
})

# =============================================================================
# System mutation
# =============================================================================

test_that("denylist-pass2: Sys.chmod() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('Sys.chmod("f", "777")')),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.chmod as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), Sys.chmod)")),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.umask() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('Sys.umask("022")')),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.umask as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), Sys.umask)")),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.readlink() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('Sys.readlink("f")')),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.readlink as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), Sys.readlink)")),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.setlocale() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('Sys.setlocale("LC_ALL", "C")')),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.setlocale as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), Sys.setlocale)")),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.setFileTime() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.setFileTime(\"f\", Sys.time())")),
    "not allowed"
  )
})

test_that("denylist-pass2: Sys.setFileTime as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), Sys.setFileTime)")),
    "not allowed"
  )
})

# =============================================================================
# Debugger hooks
# =============================================================================

test_that("denylist-pass2: debug() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("debug(f)")),
    "not allowed"
  )
})

test_that("denylist-pass2: debug as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), debug)")),
    "not allowed"
  )
})

test_that("denylist-pass2: debugonce() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("debugonce(f)")),
    "not allowed"
  )
})

test_that("denylist-pass2: debugonce as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), debugonce)")),
    "not allowed"
  )
})

test_that("denylist-pass2: undebug() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("undebug(f)")),
    "not allowed"
  )
})

test_that("denylist-pass2: undebug as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), undebug)")),
    "not allowed"
  )
})

test_that("denylist-pass2: browser() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("browser()")),
    "not allowed"
  )
})

test_that("denylist-pass2: browser as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), browser)")),
    "not allowed"
  )
})

# =============================================================================
# Info disclosure
# =============================================================================

test_that("denylist-pass2: R.home() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("R.home()")),
    "not allowed"
  )
})

test_that("denylist-pass2: R.home as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), R.home)")),
    "not allowed"
  )
})

test_that("denylist-pass2: .libPaths() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr(".libPaths()")),
    "not allowed"
  )
})

test_that("denylist-pass2: .libPaths as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, .libPaths)")),
    "not allowed"
  )
})

test_that("denylist-pass2: .packages() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr(".packages()")),
    "not allowed"
  )
})

test_that("denylist-pass2: .packages as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("sapply(1, .packages)")),
    "not allowed"
  )
})

# =============================================================================
# Delayed / deferred code execution
# =============================================================================

test_that("denylist-pass2: reg.finalizer() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("reg.finalizer(e, f)")),
    "not allowed"
  )
})

test_that("denylist-pass2: reg.finalizer as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), reg.finalizer)")),
    "not allowed"
  )
})

test_that("denylist-pass2: addTaskCallback() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("addTaskCallback(f)")),
    "not allowed"
  )
})

test_that("denylist-pass2: addTaskCallback as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), addTaskCallback)")),
    "not allowed"
  )
})

test_that("denylist-pass2: taskCallbackManager() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("taskCallbackManager()")),
    "not allowed"
  )
})

test_that("denylist-pass2: taskCallbackManager as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), taskCallbackManager)")),
    "not allowed"
  )
})

test_that("denylist-pass2: setHook() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('setHook("onLoad", f)')),
    "not allowed"
  )
})

test_that("denylist-pass2: setHook as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), setHook)")),
    "not allowed"
  )
})

test_that("denylist-pass2: packageEvent() call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('packageEvent("pkg")')),
    "not allowed"
  )
})

test_that("denylist-pass2: packageEvent as bare symbol is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), packageEvent)")),
    "not allowed"
  )
})

# =============================================================================
# Pairlist guard in ptr_resolve_placeholder_expr
# =============================================================================

test_that("pairlist-guard: dangerous symbol in pairlist result is blocked", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) as.pairlist(list(quote(system)))
  )
  expect_error(
    ggpaintr:::ptr_resolve_placeholder_expr(
      spec, "val", list(keyword = "test"), list(expr_check = TRUE)
    ),
    "not allowed"
  )
})

test_that("pairlist-guard: safe pairlist result passes", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) as.pairlist(list(quote(x)))
  )
  result <- ggpaintr:::ptr_resolve_placeholder_expr(
    spec, "val", list(keyword = "test"), list(expr_check = TRUE)
  )
  expect_true(is.pairlist(result))
})

# =============================================================================
# No false positives
# =============================================================================

test_that("no-false-positive: ggplot(data, aes(x, y)) passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("ggplot(data, aes(x, y))"))
  )
})

test_that("no-false-positive: geom_point(color = 'red') passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("geom_point(color = 'red')"))
  )
})

test_that("no-false-positive: labs(title = 'My Plot') passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("labs(title = 'My Plot')"))
  )
})
