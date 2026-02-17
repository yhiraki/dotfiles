#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
KARABINER_DIR="$(dirname "$SCRIPT_DIR")"
SRC_DIR="${KARABINER_DIR}/src"
OUT_DIR="${KARABINER_DIR}/assets/complex_modifications"

if ! command -v yq &>/dev/null; then
  echo "yq is required. Install with: brew install yq" >&2
  exit 1
fi

for yaml in "${SRC_DIR}"/*.yaml; do
  [[ -e "$yaml" ]] || break
  base=$(basename "$yaml" .yaml)
  out="${OUT_DIR}/${base}.json"
  # YAML (with anchor) -> JSON; remove temporary x-conditions key used for anchor definition
  yq -o=json 'del(.rules[].x-conditions)' "$yaml" > "$out"
  echo "Generated: $out"
done
