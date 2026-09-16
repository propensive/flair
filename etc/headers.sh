#!/usr/bin/env bash
# Replace the 32-line licence header at the top of every Scala source with etc/header.txt, or
# prepend it to a source that has none. Run after editing the header (a version bump, say).
set -e
cd "$(dirname "$0")/.."
for f in $(find src -name '*.scala'); do
  if head -1 "$f" | grep -q '/\*$'; then
    { cat etc/header.txt; tail -n +33 "$f"; } > "$f.tmp"
  else
    { cat etc/header.txt; cat "$f"; } > "$f.tmp"
  fi
  mv "$f.tmp" "$f"
done
