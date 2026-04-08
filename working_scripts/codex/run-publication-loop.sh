#!/usr/bin/env bash
set -euo pipefail

AGENT_COUNT="${1:-4}"
MAX_PASSES="${CODEX_LOOP_MAX_PASSES:-6}"
MODE="${CODEX_LOOP_MODE:-safe}"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="$ROOT_DIR/working_scripts/codex/runs/$STAMP"
PROMPT_TEMPLATE="$ROOT_DIR/working_scripts/codex/publication-loop-prompt.md"
SCHEMA_FILE="$ROOT_DIR/working_scripts/codex/publication-loop-schema.json"

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

mkdir -p "$RUN_DIR"

previous_json=""
pass=1

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

  if [ -n "$previous_json" ]; then
    printf "\n\nPrevious pass result:\n\n\`\`\`json\n" >> "$PROMPT_FILE"
    cat "$previous_json" >> "$PROMPT_FILE"
    printf "\n\`\`\`\n\nContinue from the current repository state. Re-run the strict assessment before choosing the next batch.\n" >> "$PROMPT_FILE"
  fi

  codex exec \
    "${AUTO_FLAGS[@]}" \
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
