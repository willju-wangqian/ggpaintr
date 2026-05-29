#!/usr/bin/env python3
"""Extract ⟦FINDING⟧ knowledge stamps from /export-ed transcripts (or any text).

Grammar (see .claude/rules/knowledge-stamps.md):

    ⟦FINDING id=<slug> status=… scope=… source="…" [feature=…] [tags="…"]⟧
    claim: <one sentence>
    detail: <optional, multiline>
    ⟦/FINDING⟧

Usage:
    extract-stamps.py <file-or-dir>...  [--md]

Scans each path (dirs walked recursively for *.md/*.txt/*.json), parses every
stamp, dedupes by `id` (latest-modified source file wins), and prints a JSON
ledger to stdout. --md prints a human-readable markdown ledger instead.
"""
import sys, os, re, json, argparse

OPEN = "⟦FINDING"          # ⟦FINDING
CLOSE = "⟦/FINDING⟧"  # ⟦/FINDING⟧
# open tag: ⟦FINDING <attrs>⟧ ; body ; ⟦/FINDING⟧
BLOCK = re.compile(r"⟦FINDING\b(?P<attrs>[^⟧]*)⟧(?P<body>.*?)⟦/FINDING⟧", re.DOTALL)
# attribute tokens:  key="quoted val"  |  key=barewordval
ATTR = re.compile(r'(\w+)\s*=\s*(?:"([^"]*)"|(\S+))')


def parse_attrs(s):
    out = {}
    for m in ATTR.finditer(s):
        out[m.group(1)] = m.group(2) if m.group(2) is not None else m.group(3)
    return out


def parse_body(body):
    body = body.strip("\n")
    claim, detail_lines, in_detail = None, [], False
    for line in body.splitlines():
        if not in_detail and line.strip().lower().startswith("claim:"):
            claim = line.split(":", 1)[1].strip()
        elif line.strip().lower().startswith("detail:"):
            in_detail = True
            rest = line.split(":", 1)[1].strip()
            if rest:
                detail_lines.append(rest)
        elif in_detail:
            detail_lines.append(line)
        elif claim is None and line.strip():
            claim = line.strip()  # tolerate a bare first line as the claim
    return claim, "\n".join(detail_lines).strip()


def iter_files(paths):
    exts = (".md", ".txt", ".json", ".jsonl")
    for p in paths:
        if os.path.isdir(p):
            for root, _, files in os.walk(p):
                for fn in files:
                    if fn.endswith(exts):
                        yield os.path.join(root, fn)
        else:
            yield p


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("paths", nargs="+")
    ap.add_argument("--md", action="store_true", help="markdown ledger instead of JSON")
    args = ap.parse_args()

    by_id = {}  # id -> (mtime, stamp dict)  ; keep newest source file
    anon = 0
    for fpath in iter_files(args.paths):
        try:
            text = open(fpath, encoding="utf-8", errors="replace").read()
        except OSError:
            continue
        if OPEN not in text:
            continue
        mtime = os.path.getmtime(fpath)
        for m in BLOCK.finditer(text):
            attrs = parse_attrs(m.group("attrs"))
            claim, detail = parse_body(m.group("body"))
            sid = attrs.get("id")
            if not sid:
                anon += 1
                sid = f"_anon-{anon}"
            stamp = {
                "id": sid,
                "status": attrs.get("status", "derived-unverified"),
                "scope": attrs.get("scope", ""),
                "source": attrs.get("source", ""),
                "feature": attrs.get("feature", ""),
                "tags": attrs.get("tags", ""),
                "claim": claim or "",
                "detail": detail,
                "transcript": os.path.relpath(fpath),
            }
            prev = by_id.get(sid)
            if prev is None or mtime >= prev[0]:
                by_id[sid] = (mtime, stamp)

    stamps = [s for _, s in sorted(by_id.values(), key=lambda kv: (kv[1]["scope"], kv[1]["id"]))]

    if args.md:
        print(f"# Stamp ledger — {len(stamps)} unique finding(s)\n")
        for s in stamps:
            print(f"## `{s['id']}`  ({s['scope'] or 'no-scope'}, {s['status']})")
            print(f"- **claim:** {s['claim']}")
            if s["source"]:
                print(f"- **source:** `{s['source']}`")
            if s["feature"]:
                print(f"- **feature:** {s['feature']}")
            print(f"- **from:** {s['transcript']}")
            if s["detail"]:
                print(f"\n{s['detail']}\n")
            print()
    else:
        json.dump({"count": len(stamps), "stamps": stamps}, sys.stdout, indent=2, ensure_ascii=False)
        print()

    if not stamps:
        print("(no ⟦FINDING⟧ stamps found)", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
