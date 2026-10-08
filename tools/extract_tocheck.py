"""List every claim marked [text]{.tocheck note="..."} in the chapters.
Usage: python3 tools/extract_tocheck.py  > reports/tocheck_claims.md"""
import glob, re
pat = re.compile(r'\[([^\]]+)\]\{\.tocheck(?:\s+note="([^"]*)")?\}', re.S)
print("# Claims marked for checking\n\n| Chapter | Line | Claim | Note |\n|---|---|---|---|")
n = 0
for f in sorted(glob.glob("[0-9][0-9]-*.qmd")):
    text = open(f).read()
    for m in pat.finditer(text):
        line = text.count("\n", 0, m.start()) + 1
        claim = " ".join(m.group(1).split()).replace("|", "\\|")
        print(f"| {f} | {line} | {claim} | {m.group(2) or ''} |"); n += 1
print(f"\n{n} claims marked.")
