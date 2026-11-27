#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# BLD (sweep 1): the Zig standalone FFI library builds, every shell script parses, and
# a deliberately broken script is rejected. A missing zig is a failure (DOG-01), never a skip.
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2
. tests/kit/lib/ptb.sh
command -v zig >/dev/null || { echo "build: zig not found; absence is not a pass" >&2; exit 2; }
mapfile -d '' scripts < <(git ls-files -z -- 'scripts/*.sh' 'tests/*.sh')
[[ ${#scripts[@]} -gt 0 ]] || { echo "build: no shell scripts found" >&2; exit 2; }
for s in "${scripts[@]}"; do ptb_ok "BLD-syntax-$s" bash -n "$s"; done
ptb_fails BLD-negative-control-syntax bash -n <(echo 'if then fi')
ptb_ok BLD-zig-standalone-lib zig build --build-file ffi/zig/build_standalone.zig
ptb_ok BLD-zig-lib-artifact test -s ffi/zig/zig-out/lib/libproven_ffi.a
ptb_done
