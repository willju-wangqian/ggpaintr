#!/usr/bin/env bash
# Gather /export-ed conversation transcripts into one corpus dir for harvesting.
#
#   collect.sh <src>... [--dest DIR]
#     <src>   files and/or directories holding exported transcripts (.md/.txt/.json)
#     --dest  destination corpus dir (default: .claude/harvest-findings/raw_conversation)
#
# Copies (does not move) matching files in, flattening, with a timestamp+name so
# re-running accumulates rather than clobbers. Idempotent on identical content.
set -euo pipefail

DEST=".claude/harvest-findings/raw_conversation"
srcs=()
while [ $# -gt 0 ]; do
  case "$1" in
    --dest) DEST=$2; shift 2 ;;
    *) srcs+=("$1"); shift ;;
  esac
done
[ ${#srcs[@]} -gt 0 ] || { echo "usage: collect.sh <src>... [--dest DIR]" >&2; exit 1; }

mkdir -p "$DEST"
n=0
add() {
  local f=$1
  case "$f" in *.md|*.txt|*.json|*.jsonl) ;; *) return ;; esac
  local stamp base
  stamp=$(stat -f '%Sm' -t '%Y%m%dT%H%M%S' "$f" 2>/dev/null || date -r "$f" +%Y%m%dT%H%M%S 2>/dev/null || echo unknown)
  base=$(basename "$f")
  local out="$DEST/${stamp}__${base}"
  if [ ! -e "$out" ] || ! cmp -s "$f" "$out"; then
    cp "$f" "$out"; n=$((n+1))
  fi
}

for s in "${srcs[@]}"; do
  if [ -d "$s" ]; then
    while IFS= read -r f; do add "$f"; done < <(find "$s" -type f \( -name '*.md' -o -name '*.txt' -o -name '*.json' -o -name '*.jsonl' \))
  elif [ -f "$s" ]; then
    add "$s"
  else
    echo "skip (not found): $s" >&2
  fi
done

echo "collected $n new file(s) into $DEST/ ($(find "$DEST" -type f | wc -l | tr -d ' ') total)"
