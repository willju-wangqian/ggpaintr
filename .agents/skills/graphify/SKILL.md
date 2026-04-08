---
name: graphify
description: Build and query a knowledge graph for this repository using the installed Graphify workflow.
trigger: /graphify
---

# /graphify

Use the installed Graphify Codex workflow at `/Users/willju/.codex/skills/graphify/skill-codex.md`.

When invoked:

1. Read `/Users/willju/.codex/skills/graphify/skill-codex.md`.
2. Follow those instructions exactly for the user's `/graphify ...` invocation.
3. Default the target path to `.` when the user does not provide one.
4. Respect this repository's [`.graphifyignore`](/Users/willju/Research/ggpaintr/.graphifyignore) during detection and extraction.
5. If the installed Graphify skill file is missing, explain that Graphify is not installed locally and stop.
