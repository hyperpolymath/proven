#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# SMK (sweep 1): the standalone FFI entry starts and answers. Without an Idris backend it
# must refuse with status -99 rather than guess (fail-closed), which its Zig test asserts;
# here the real test binary is built and run, and a failing run fails the smoke.
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2
. tests/kit/lib/ptb.sh
command -v zig >/dev/null || { echo "smoke: zig not found; absence is not a pass" >&2; exit 2; }
out=$(zig build test --build-file ffi/zig/build_standalone.zig --summary all 2>&1); rc=$?
ptb_eq SMK-zig-standalone-tests-exit 0 "$rc"
case $out in *"tests passed"*) ptb_ok SMK-tests-executed true ;; *) ptb_fails SMK-tests-executed true ;; esac
ptb_fails SMK-missing-binary-is-not-a-pass tests/kit/lib/smoke.sh --expect x -- /nonexistent/binary
ptb_done
