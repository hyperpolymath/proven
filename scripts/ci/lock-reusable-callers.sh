#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Metadata-only adapter for gh-actions-lock v0.4.1's external-workflow gap.
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
repo=metadatastician/standards
ref=571cc734cd69fb846032ec77a662aa8ee4fc32cd
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/.github/workflows"

# Refuse to replace a later, wider lockfile with this deliberately scoped one.
if [[ -f "$root/.github/workflows/actions.lock" ]]; then
    unexpected=$(sed -n "s/^    '\(.github\/workflows\/[^']*\)':.*/\1/p" "$root/.github/workflows/actions.lock" |
        grep -vE '^\.github/workflows/(governance|hypatia-scan|secret-scanner|spark-theatre-gate)\.yml$' || true)
    [[ -z "$unexpected" ]] || { echo 'Refusing to overwrite a wider workflow lock' >&2; exit 1; }
fi

while read -r caller callee; do
    grep -Fq "uses: $repo/.github/workflows/$callee@$ref" "$root/.github/workflows/$caller"
    gh api "repos/$repo/contents/.github/workflows/$callee?ref=$ref" \
        -H 'Accept: application/vnd.github.raw+json' > "$work/$callee.source"
    # These four fixed workflows contain only remote, SHA-pinned action steps.
    # They are read as data; neither the downloaded jobs nor this projection run.
    sed -nE 's/^[[:space:]]*(-[[:space:]]*)?uses:[[:space:]]*([^[:space:]#]+).*/\2/p' \
        "$work/$callee.source" | sort -u > "$work/uses"
    [[ -s "$work/uses" ]]
    if grep -vE '^[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+@[0-9a-f]{40}$' "$work/uses"; then
        echo 'Unsupported dependency in pinned reusable workflow' >&2; exit 1
    fi
    {
        printf 'name: Metadata projection\non: workflow_dispatch\njobs:\n  metadata:\n    runs-on: ubuntu-latest\n    steps:\n'
        sed 's/^/      - uses: /' "$work/uses"
    } > "$work/.github/workflows/$caller"
done <<'CALLERS'
governance.yml governance-reusable.yml
hypatia-scan.yml hypatia-scan-reusable.yml
secret-scanner.yml secret-scanner-reusable.yml
spark-theatre-gate.yml spark-theatre-gate.yml
CALLERS

# Resolve upstream ownership, commit reachability and transitive action pins
# using the official tool. A failure cannot replace the checked-in lockfile.
(
    cd "$work"
    GH_ACTIONS_LOCK_REPO_ROOT="$work" gh actions-lock --no-interactive --no-narrow
    GH_ACTIONS_LOCK_REPO_ROOT="$work" gh actions-lock --verify
    # Resolve reusable-repository identity only after verifying actual actions.
    # This metadata reference has no action.yml: it is a workflow repository.
    # Native GitHub validates the real job-level workflow calls on publication.
    for caller in governance hypatia-scan secret-scanner spark-theatre-gate; do
        printf '      - uses: %s@%s\n' "$repo" "$ref" >> ".github/workflows/$caller.yml"
    done
    GH_ACTIONS_LOCK_REPO_ROOT="$work" gh actions-lock --no-interactive --no-narrow
)
cp "$work/.github/workflows/actions.lock" "$root/.github/workflows/actions.lock"
echo 'Updated lock metadata for four existing reusable callers; workflow bytes unchanged.'
