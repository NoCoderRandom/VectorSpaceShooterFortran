#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN="$ROOT/build/bin/vector_space_shooter"

if [[ ! -x "$BIN" ]]; then
    "$ROOT/build.sh"
fi

exec "$BIN" "$@"
