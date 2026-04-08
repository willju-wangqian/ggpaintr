#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="/workspace/ggpaintr"
BOOTSTRAP_STAMP="/tmp/ggpaintr-codex-bootstrap.done"
MODE="${1:-workflow}"
CODEX_HOME_DIR="${HOME:-/tmp/codex-home}/.codex"

if [ $# -gt 0 ]; then
  shift
fi

if [ ! -d "$ROOT_DIR" ]; then
  echo "Expected mounted repository at $ROOT_DIR but it was not found." >&2
  exit 1
fi

cd "$ROOT_DIR"
git config --global --add safe.directory "$ROOT_DIR"

maybe_bootstrap() {
  if [ "${CODEX_SKIP_BOOTSTRAP:-0}" = "1" ]; then
    return 0
  fi

  if [ -f "$BOOTSTRAP_STAMP" ] && [ "${CODEX_BOOTSTRAP_ALWAYS:-0}" != "1" ]; then
    return 0
  fi

  "$ROOT_DIR/dev/codex/bootstrap-container.sh"
  touch "$BOOTSTRAP_STAMP"
}

prepare_site_dir() {
  "$ROOT_DIR/dev/codex/prepare-site-dir.sh" "$ROOT_DIR"
}

if [ ! -d "$CODEX_HOME_DIR" ] && [ -z "${OPENAI_API_KEY:-}" ]; then
  echo "Warning: $CODEX_HOME_DIR is not mounted and OPENAI_API_KEY is not set; Codex authentication may fail." >&2
fi

case "$MODE" in
  bootstrap)
    exec "$ROOT_DIR/dev/codex/bootstrap-container.sh"
    ;;
  workflow)
    maybe_bootstrap
    prepare_site_dir
    exec "$ROOT_DIR/dev/codex/run-publication-workflow.sh" "$@"
    ;;
  loop)
    maybe_bootstrap
    prepare_site_dir
    exec "$ROOT_DIR/dev/codex/run-publication-loop.sh" "$@"
    ;;
  shell)
    maybe_bootstrap
    prepare_site_dir
    exec /bin/bash "$@"
    ;;
  *)
    maybe_bootstrap
    prepare_site_dir
    exec "$MODE" "$@"
    ;;
esac
