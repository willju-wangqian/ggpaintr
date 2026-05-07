# Helpers for the rewrite test suite (test-rewrite-*.R).
#
# `find_nodes` was promoted to the package (R/paintr-build-ui.R) so observers
# inside ptr_server can call it. Tests use the exported (or unexported via
# devtools::load_all) version directly — no helper redefinition needed.
