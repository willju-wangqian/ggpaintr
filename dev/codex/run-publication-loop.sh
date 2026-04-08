#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: run-publication-loop.sh [AGENT_COUNT]

Runs repeated unattended publication-loop Codex passes until the structured
result reports `completed` or `blocked`, or until `CODEX_LOOP_MAX_PASSES` is hit.

Environment overrides:
  CODEX_LOOP_MAX_PASSES           Default: 6
  CODEX_LOOP_MODE                 safe | danger (default: safe)
  CODEX_LOOP_RUN_DIR              Override artifact directory
  CODEX_LOOP_PROMPT_TEMPLATE      Override prompt template path
  CODEX_LOOP_SCHEMA_FILE          Override JSON schema path
  CODEX_LOOP_INITIAL_CONTEXT_FILE Markdown/JSON/text appended to the first pass
  CODEX_LOOP_MODEL                Optional `codex exec --model` override
  CODEX_LOOP_ENABLE_SEARCH        1 to pass `--search`
  CODEX_LOOP_DRY_RUN              1 to print resolved configuration and exit
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

AGENT_COUNT="${1:-${CODEX_LOOP_AGENT_COUNT:-4}}"
MAX_PASSES="${CODEX_LOOP_MAX_PASSES:-6}"
MODE="${CODEX_LOOP_MODE:-safe}"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="${CODEX_LOOP_RUN_DIR:-$ROOT_DIR/dev/codex/runs/$STAMP}"
PROMPT_TEMPLATE="${CODEX_LOOP_PROMPT_TEMPLATE:-$ROOT_DIR/dev/codex/publication-loop-prompt.md}"
SCHEMA_FILE="${CODEX_LOOP_SCHEMA_FILE:-$ROOT_DIR/dev/codex/publication-loop-schema.json}"
INITIAL_CONTEXT_FILE="${CODEX_LOOP_INITIAL_CONTEXT_FILE:-}"
MODEL="${CODEX_LOOP_MODEL:-}"
ENABLE_SEARCH="${CODEX_LOOP_ENABLE_SEARCH:-0}"
DRY_RUN="${CODEX_LOOP_DRY_RUN:-0}"

if ! command -v codex >/dev/null 2>&1; then
  echo "codex CLI is required but was not found on PATH." >&2
  exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required but was not found on PATH." >&2
  exit 1
fi

case "$MODE" in
  safe)
    AUTO_FLAGS=(--full-auto)
    ;;
  danger)
    AUTO_FLAGS=(--dangerously-bypass-approvals-and-sandbox)
    ;;
  *)
    echo "CODEX_LOOP_MODE must be either 'safe' or 'danger'." >&2
    exit 1
    ;;
esac

EXTRA_FLAGS=()

if [ -n "$MODEL" ]; then
  EXTRA_FLAGS+=(--model "$MODEL")
fi

if [ "$ENABLE_SEARCH" = "1" ]; then
  EXTRA_FLAGS+=(--search)
fi

if [ "$DRY_RUN" = "1" ]; then
  cat <<EOF
run_dir=$RUN_DIR
prompt_template=$PROMPT_TEMPLATE
schema_file=$SCHEMA_FILE
agent_count=$AGENT_COUNT
max_passes=$MAX_PASSES
mode=$MODE
initial_context_file=${INITIAL_CONTEXT_FILE:-<none>}
model=${MODEL:-<default>}
enable_search=$ENABLE_SEARCH
EOF
  exit 0
fi

if [ -n "$INITIAL_CONTEXT_FILE" ] && [ ! -f "$INITIAL_CONTEXT_FILE" ]; then
  echo "CODEX_LOOP_INITIAL_CONTEXT_FILE was set but the file was not found: $INITIAL_CONTEXT_FILE" >&2
  exit 1
fi

mkdir -p "$RUN_DIR"

previous_json=""
pass=1
status="continue"
RESULT_FILE=""

while [ "$pass" -le "$MAX_PASSES" ]; do
  PASS_DIR="$RUN_DIR/pass-$pass"
  PROMPT_FILE="$PASS_DIR/prompt.md"
  RESULT_FILE="$PASS_DIR/result.json"
  EVENTS_FILE="$PASS_DIR/events.jsonl"

  mkdir -p "$PASS_DIR"

  sed \
    -e "s/__AGENT_COUNT__/$AGENT_COUNT/g" \
    -e "s/__PASS__/$pass/g" \
    "$PROMPT_TEMPLATE" > "$PROMPT_FILE"

  if [ -n "$INITIAL_CONTEXT_FILE" ] && [ "$pass" -eq 1 ]; then
    printf "\n\nInitial workflow context:\n\n" >> "$PROMPT_FILE"
    cat "$INITIAL_CONTEXT_FILE" >> "$PROMPT_FILE"
    printf "\n" >> "$PROMPT_FILE"
  fi

  if [ -n "$previous_json" ]; then
    printf "\n\nPrevious pass result:\n\n\`\`\`json\n" >> "$PROMPT_FILE"
    cat "$previous_json" >> "$PROMPT_FILE"
    printf "\n\`\`\`\n\nContinue from the current repository state. Re-run the strict assessment before choosing the next batch.\n" >> "$PROMPT_FILE"
  fi

  codex exec \
    "${AUTO_FLAGS[@]}" \
    "${EXTRA_FLAGS[@]}" \
    --cd "$ROOT_DIR" \
    --output-schema "$SCHEMA_FILE" \
    --output-last-message "$RESULT_FILE" \
    --json \
    - < "$PROMPT_FILE" | tee "$EVENTS_FILE"

  status="$(jq -r '.status' "$RESULT_FILE")"

  if [ "$status" = "completed" ] || [ "$status" = "blocked" ]; then
    break
  fi

  previous_json="$RESULT_FILE"
  pass=$((pass + 1))
done

echo
echo "Run artifacts: $RUN_DIR"
echo

if [ -f "$RESULT_FILE" ]; then
  jq . "$RESULT_FILE"
fi

if [ "$pass" -gt "$MAX_PASSES" ] && [ "${status:-continue}" = "continue" ]; then
  echo
  echo "Reached CODEX_LOOP_MAX_PASSES=$MAX_PASSES while the workflow still requested another pass." >&2
  exit 2
fi
