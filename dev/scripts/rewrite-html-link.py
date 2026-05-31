#!/usr/bin/env python3
"""
rewrite-html-link.py — Replace inline <style>...</style> with a relative
<link rel="stylesheet" href="..."> pointing at dev/assets/doc.css.

Run AFTER dev/scripts/build-doc-css.py has produced the merged stylesheet.
Idempotent: files that already use <link> (no <style> block to match) are
left untouched.

Same scope rules as build-doc-css.py — see that file's docstring.
"""
import re, os, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def in_scope_files() -> list[str]:
    paths: list[Path] = []
    for d in ("dev/plans", "dev/adr", "dev/audit", "dev/notes", "dev/feature_bank"):
        paths.extend((ROOT / d).rglob("*.html"))
    paths.append(ROOT / "dev/ast-walk-diagram.html")
    rels = []
    for p in paths:
        if not p.is_file():
            continue
        rel = p.relative_to(ROOT).as_posix()
        if rel.startswith("dev/adr/0013-") or rel.startswith("dev/plans/0013-"):
            continue
        rels.append(rel)
    rels.sort()
    return rels


def main() -> int:
    target = (ROOT / "dev/assets/doc.css").resolve()
    if not target.exists():
        print(f"error: {target} missing — run dev/scripts/build-doc-css.py first", file=sys.stderr)
        return 1

    style_re = re.compile(r"([ \t]*)<style>.*?</style>", re.DOTALL)
    changed: list[tuple[str, str]] = []
    untouched: list[str] = []

    for rel in in_scope_files():
        p = (ROOT / rel).resolve()
        link_href = os.path.relpath(target, p.parent)
        text = p.read_text()
        new_text, n = style_re.subn(
            lambda m: f'{m.group(1)}<link rel="stylesheet" href="{link_href}">',
            text, count=1
        )
        if n == 0:
            untouched.append(rel)
            continue
        p.write_text(new_text)
        changed.append((rel, link_href))

    print(f"rewrote: {len(changed)} file(s)")
    print(f"already-linked or styleless: {len(untouched)} file(s)")
    sample_seen = set()
    for rel, href in changed:
        if href in sample_seen:
            continue
        sample_seen.add(href)
        print(f"  sample: {rel}  -->  {href}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
