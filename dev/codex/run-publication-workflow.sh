#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: run-publication-workflow.sh [AGENT_COUNT]

Runs the unattended publication workflow in this order:
  1. Batch 1 implementation prompt
  2. Structured reassessment
  3. Batch 2-3 implementation prompt
  4. Structured reassessment
  5. Follow-up publication loop passes until completion/blocking/max-pass

Environment overrides:
  CODEX_WORKFLOW_RUN_DIR            Override artifact directory
  CODEX_WORKFLOW_FOLLOWUP_MAX_PASSES Default: 4
  CODEX_WORKFLOW_SKIP_FOLLOWUP_LOOP 1 to stop after the second reassessment
  CODEX_LOOP_MODE                   safe | danger (default: safe)
  CODEX_LOOP_MODEL                  Optional `codex exec --model` override
  CODEX_LOOP_ENABLE_SEARCH          1 to pass `--search`
  CODEX_WORKFLOW_DRY_RUN            1 to print resolved configuration and exit
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

AGENT_COUNT="${1:-${CODEX_LOOP_AGENT_COUNT:-4}}"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="${CODEX_WORKFLOW_RUN_DIR:-$ROOT_DIR/dev/codex/runs/$STAMP}"
MODE="${CODEX_LOOP_MODE:-safe}"
MODEL="${CODEX_LOOP_MODEL:-}"
ENABLE_SEARCH="${CODEX_LOOP_ENABLE_SEARCH:-0}"
FOLLOWUP_MAX_PASSES="${CODEX_WORKFLOW_FOLLOWUP_MAX_PASSES:-4}"
SKIP_FOLLOWUP_LOOP="${CODEX_WORKFLOW_SKIP_FOLLOWUP_LOOP:-0}"
DRY_RUN="${CODEX_WORKFLOW_DRY_RUN:-0}"

PHASE_TEMPLATE="$ROOT_DIR/dev/codex/publication-phase-prompt.md"
PHASE_SCHEMA="$ROOT_DIR/dev/codex/publication-loop-schema.json"
ASSESS_TEMPLATE="$ROOT_DIR/dev/codex/publication-assessment-prompt.md"
ASSESS_SCHEMA="$ROOT_DIR/dev/codex/publication-assessment-schema.json"
RUN_LOOP_SCRIPT="$ROOT_DIR/dev/codex/run-publication-loop.sh"

BATCH1_PROMPT="$ROOT_DIR/dev/notes/prompts/publication-improvements-batch-1-prompt.md"
BATCH23_PROMPT="$ROOT_DIR/dev/notes/prompts/publication-improvements-batch-2-3-prompt.md"

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "$1 is required but was not found on PATH." >&2
    exit 1
  fi
}

require_cmd codex
require_cmd jq

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
agent_count=$AGENT_COUNT
mode=$MODE
model=${MODEL:-<default>}
enable_search=$ENABLE_SEARCH
followup_max_passes=$FOLLOWUP_MAX_PASSES
skip_followup_loop=$SKIP_FOLLOWUP_LOOP
batch_1_prompt=$BATCH1_PROMPT
batch_2_3_prompt=$BATCH23_PROMPT
phase_template=$PHASE_TEMPLATE
assessment_template=$ASSESS_TEMPLATE
EOF
  exit 0
fi

mkdir -p "$RUN_DIR"

last_result_file=""
last_status=""

append_context() {
  local destination="$1"
  local label="$2"
  local source="$3"

  if [ -z "$source" ] || [ ! -f "$source" ]; then
    return 0
  fi

  printf "\n\n%s:\n\n" "$label" >> "$destination"
  case "$source" in
    *.json)
      printf "\`\`\`json\n" >> "$destination"
      cat "$source" >> "$destination"
      printf "\n\`\`\`\n" >> "$destination"
      ;;
    *)
      cat "$source" >> "$destination"
      printf "\n" >> "$destination"
      ;;
  esac
}

run_phase() {
  local phase_slug="$1"
  local phase_name="$2"
  local task_prompt="$3"
  local prompt_template="$4"
  local output_schema="$5"
  local context_file="${6:-}"

  local phase_dir="$RUN_DIR/$phase_slug"
  local prompt_file="$phase_dir/prompt.md"
  local result_file="$phase_dir/result.json"
  local events_file="$phase_dir/events.jsonl"

  mkdir -p "$phase_dir"

  sed \
    -e "s|__PHASE_NAME__|$phase_name|g" \
    -e "s|__TASK_PROMPT__|$task_prompt|g" \
    -e "s|__AGENT_COUNT__|$AGENT_COUNT|g" \
    "$prompt_template" > "$prompt_file"

  append_context "$prompt_file" "Stored task prompt contents" "$task_prompt"

  if [ -n "$context_file" ]; then
    append_context "$prompt_file" "Workflow context" "$context_file"
  fi

  if [ -n "$last_result_file" ]; then
    append_context "$prompt_file" "Previous structured result" "$last_result_file"
  fi

  codex exec \
    "${AUTO_FLAGS[@]}" \
    "${EXTRA_FLAGS[@]}" \
    --cd "$ROOT_DIR" \
    --output-schema "$output_schema" \
    --output-last-message "$result_file" \
    --json \
    - < "$prompt_file" | tee "$events_file"

  last_result_file="$result_file"

  if jq -e '.status' "$result_file" >/dev/null 2>&1; then
    last_status="$(jq -r '.status' "$result_file")"
  else
    last_status=""
  fi

  echo
  echo "Phase artifacts: $phase_dir"
  echo
  jq . "$result_file"
}

write_followup_context() {
  local context_file="$RUN_DIR/followup-loop-context.md"
  cat > "$context_file" <<EOF
The publication workflow has already completed these phases in the current run:

- Batch 1 implementation phase: \`$RUN_DIR/01-batch-1\`
- Reassessment after Batch 1: \`$RUN_DIR/02-assessment-after-batch-1\`
- Batch 2-3 implementation phase: \`$RUN_DIR/03-batch-2-3\`
- Reassessment after Batch 2-3: \`$RUN_DIR/04-assessment-after-batch-2-3\`

Use these artifacts as prior context, but reassess from the current repository
state before planning the next substantive batch.
EOF

  if [ -f "$RUN_DIR/01-batch-1/result.json" ]; then
    append_context "$context_file" "Batch 1 result" "$RUN_DIR/01-batch-1/result.json"
  fi

  if [ -f "$RUN_DIR/02-assessment-after-batch-1/result.json" ]; then
    append_context "$context_file" "Assessment after Batch 1" "$RUN_DIR/02-assessment-after-batch-1/result.json"
  fi

  if [ -f "$RUN_DIR/03-batch-2-3/result.json" ]; then
    append_context "$context_file" "Batch 2-3 result" "$RUN_DIR/03-batch-2-3/result.json"
  fi

  if [ -f "$RUN_DIR/04-assessment-after-batch-2-3/result.json" ]; then
    append_context "$context_file" "Assessment after Batch 2-3" "$RUN_DIR/04-assessment-after-batch-2-3/result.json"
  fi

  printf '%s\n' "$context_file"
}

run_phase \
  "01-batch-1" \
  "Batch 1" \
  "$BATCH1_PROMPT" \
  "$PHASE_TEMPLATE" \
  "$PHASE_SCHEMA"

if [ "$last_status" = "blocked" ]; then
  echo "Batch 1 reported a blocking external dependency. Stopping workflow." >&2
  exit 3
fi

run_phase \
  "02-assessment-after-batch-1" \
  "Assessment After Batch 1" \
  "$ROOT_DIR/dev/notes/prompts/publication-readiness-cran-prep-prompt.md" \
  "$ASSESS_TEMPLATE" \
  "$ASSESS_SCHEMA" \
  "$RUN_DIR/01-batch-1/result.json"

run_phase \
  "03-batch-2-3" \
  "Batch 2-3" \
  "$BATCH23_PROMPT" \
  "$PHASE_TEMPLATE" \
  "$PHASE_SCHEMA" \
  "$RUN_DIR/02-assessment-after-batch-1/result.json"

if [ "$last_status" = "blocked" ]; then
  echo "Batch 2-3 reported a blocking external dependency. Stopping workflow." >&2
  exit 4
fi

run_phase \
  "04-assessment-after-batch-2-3" \
  "Assessment After Batch 2-3" \
  "$ROOT_DIR/dev/notes/prompts/publication-readiness-cran-prep-prompt.md" \
  "$ASSESS_TEMPLATE" \
  "$ASSESS_SCHEMA" \
  "$RUN_DIR/03-batch-2-3/result.json"

if [ "$SKIP_FOLLOWUP_LOOP" = "1" ]; then
  echo
  echo "Workflow artifacts: $RUN_DIR"
  exit 0
fi

FOLLOWUP_CONTEXT_FILE="$(write_followup_context)"

CODEX_LOOP_AGENT_COUNT="$AGENT_COUNT" \
CODEX_LOOP_MAX_PASSES="$FOLLOWUP_MAX_PASSES" \
CODEX_LOOP_MODE="$MODE" \
CODEX_LOOP_MODEL="$MODEL" \
CODEX_LOOP_ENABLE_SEARCH="$ENABLE_SEARCH" \
CODEX_LOOP_RUN_DIR="$RUN_DIR/05-followup-loop" \
CODEX_LOOP_INITIAL_CONTEXT_FILE="$FOLLOWUP_CONTEXT_FILE" \
  "$RUN_LOOP_SCRIPT"

echo
echo "Workflow artifacts: $RUN_DIR"
