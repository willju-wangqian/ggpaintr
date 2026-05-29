#!/usr/bin/env bash
# Locate Claude Code session logs containing a token, ranked oldest->newest by mtime.
# Searches ALL ~/.claude/projects/*/ (incl. nested subagents/ + workflow logs), because
# valuable findings often come from a SIBLING-WORKTREE session, not the current one.
# Narrowing is done by the token's uniqueness, not by path — pick a distinctive phrase.
#
#   locate.sh <token> [path-filter]
#     token        near-unique substring from the finding (e.g. a coined phrase, a fn name)
#     path-filter  optional substring to restrict to certain project dirs (e.g. "ggpaintr")
#
# Excludes the current session log when $CLAUDE_SESSION_ID is set (it self-matches if you
# pasted the finding into this chat). Output: TAB-separated  <mtime>\t<path>.
set -euo pipefail

QUERY=${1:?usage: locate.sh <token> [path-filter]}
FILTER=${2:-}
PROJECTS="${CLAUDE_PROJECTS_DIR:-$HOME/.claude/projects}"

list=$(grep -rl -- "$QUERY" "$PROJECTS"/*/ --include='*.jsonl' 2>/dev/null || true)
[ -n "$FILTER" ] && list=$(printf '%s\n' "$list" | grep -F -- "$FILTER" || true)
[ -n "$list" ] || { echo "no logs matched token: $QUERY" >&2; exit 1; }

printf '%s\n' "$list" | while IFS= read -r f; do
  [ -z "$f" ] && continue
  if [ -n "${CLAUDE_SESSION_ID:-}" ] && [[ "$f" == *"$CLAUDE_SESSION_ID"* ]]; then continue; fi
  mt=$(stat -f '%Sm' -t '%Y-%m-%dT%H:%M' "$f" 2>/dev/null || date -r "$f" +%FT%H:%M 2>/dev/null || echo '?')
  printf '%s\t%s\n' "$mt" "$f"
done | sort
