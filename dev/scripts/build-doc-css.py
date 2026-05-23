#!/usr/bin/env python3
"""
build-doc-css.py — Minify / dedupe dev/assets/doc.css in place.

The first version of this script extracted inline <style> blocks from
internal-docs HTMLs and concatenated them into dev/assets/doc.css. That
was a one-time bootstrap. After it, dev/assets/doc.css IS the source of
truth — this script reads it back, parses every rule, and rewrites the
file as a flat deduplicated stylesheet:

  * One rule per selector signature, first-seen wins on declarations.
    Visual drift between original buckets is accepted.
  * `:root` variables are unioned across every `:root { ... }` block so
    no `var(--...)` reference loses its definition.
  * `@media` / `@keyframes` / `@supports` blocks are preserved as opaque
    units, deduped by their full text.
  * `@import` and other at-rule statements are preserved (deduped).

Re-run after hand-editing dev/assets/doc.css if you want to canonicalize.
Companion: dev/scripts/rewrite-html-link.py rewrites HTMLs from inline
<style> to <link rel="stylesheet" href="../assets/doc.css">.
"""
import re, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CSS_PATH = ROOT / "dev/assets/doc.css"


# ---- CSS parser (depth-aware top-level tokenizer) -----------------------

def strip_comments(text: str) -> str:
    return re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)


def parse_css(text: str) -> list[tuple[str, str | None]]:
    """Return a list of (prelude, body) pairs.

    For a normal rule: prelude is the selector list, body is the declarations.
    For an at-rule with a block (@media etc.): prelude is the @rule line,
    body is the full inner block including any nested rules.
    For an at-rule statement (@import): body is None.
    """
    text = strip_comments(text)
    items: list[tuple[str, str | None]] = []
    i, n = 0, len(text)
    while i < n:
        while i < n and text[i] in " \t\n\r":
            i += 1
        if i >= n:
            break
        j = i
        while j < n and text[j] != "{" and text[j] != ";":
            j += 1
        if j >= n:
            break
        if text[j] == ";":
            stmt = text[i:j].strip()
            if stmt:
                items.append((stmt, None))
            i = j + 1
            continue
        prelude = text[i:j].strip()
        depth = 1
        k = j + 1
        while k < n and depth > 0:
            if text[k] == "{":
                depth += 1
            elif text[k] == "}":
                depth -= 1
            k += 1
        body = text[j + 1 : k - 1].strip()
        items.append((prelude, body))
        i = k
    return items


def parse_root_vars(body: str) -> list[tuple[str, str]]:
    """Split a :root body into (--var-name, value) pairs, ignoring nested braces."""
    out = []
    depth = 0
    cur: list[str] = []
    for ch in body:
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
        if ch == ";" and depth == 0:
            decl = "".join(cur).strip()
            cur = []
            if decl:
                m = re.match(r"(--[\w-]+)\s*:\s*(.+)", decl, re.DOTALL)
                if m:
                    out.append((m.group(1), m.group(2).strip()))
        else:
            cur.append(ch)
    decl = "".join(cur).strip()
    if decl:
        m = re.match(r"(--[\w-]+)\s*:\s*(.+)", decl, re.DOTALL)
        if m:
            out.append((m.group(1), m.group(2).strip()))
    return out


def normalize_decls(decls: str) -> str:
    """Collapse interior whitespace runs to single spaces so rules format cleanly."""
    return re.sub(r"\s+", " ", decls).strip()


def normalize_selector(sel: str) -> str:
    """Normalize a selector list for use as a dedup key. Splits the comma list,
    strips each piece, sorts them so `a, b` and `b, a` collide.
    """
    parts = [p.strip() for p in sel.split(",") if p.strip()]
    parts = [re.sub(r"\s+", " ", p) for p in parts]
    parts.sort()
    return ", ".join(parts)


def main() -> int:
    if not CSS_PATH.exists():
        print(f"error: {CSS_PATH} missing", file=sys.stderr)
        return 1

    source = CSS_PATH.read_text()

    rules: dict[str, tuple[str, str]] = {}   # key -> (display-selector, declarations)
    at_blocks: dict[str, None] = {}          # opaque @-block text -> sentinel
    root_vars: dict[str, str] = {}           # var-name -> first-seen value
    statements: dict[str, None] = {}         # @import etc.

    for prelude, decls in parse_css(source):
        if decls is None:
            statements.setdefault(prelude, None)
            continue
        head_word = prelude.split()[0] if prelude else ""
        if head_word.startswith("@"):
            key = f"{prelude} {{ {normalize_decls(decls)} }}"
            at_blocks.setdefault(key, None)
            continue
        if prelude.strip() == ":root":
            for name, value in parse_root_vars(decls):
                root_vars.setdefault(name, value)
            continue
        key = normalize_selector(prelude)
        if not key:
            continue
        rules.setdefault(key, (key, normalize_decls(decls)))

    # ---- Emit ----
    out: list[str] = []
    out.append("/* dev/assets/doc.css")
    out.append(" * Flat deduplicated stylesheet for internal docs under dev/.")
    out.append(" * Generated by dev/scripts/build-doc-css.py — one rule per selector")
    out.append(" * signature, :root variables unioned, @media/@keyframes preserved.")
    out.append(f" * {len(rules)} rules, {len(at_blocks)} at-blocks, {len(root_vars)} :root vars.")
    out.append(" */")
    out.append("")

    for stmt in statements:
        out.append(f"{stmt};")
    if statements:
        out.append("")

    if root_vars:
        out.append(":root {")
        for name, value in root_vars.items():
            out.append(f"  {name}: {value};")
        out.append("}")
        out.append("")

    for key, (display_sel, decls) in rules.items():
        out.append(f"{display_sel} {{ {decls} }}")
    if rules:
        out.append("")

    for block in at_blocks:
        out.append(block)
    if at_blocks:
        out.append("")

    CSS_PATH.write_text("\n".join(out))
    print(
        f"wrote {CSS_PATH.relative_to(ROOT)}: "
        f"{len(rules)} rules, {len(at_blocks)} at-blocks, {len(root_vars)} :root vars"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
