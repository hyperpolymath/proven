#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# P2P (sweep 1): the producer-to-consumer seams that need no Idris: RefC staging into the
# Zig tree and the native symbol validator reading a real archive. Real components on both
# sides of each seam; planted-failure controls prove each seam rejects bad input.
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2
. tests/kit/lib/ptb.sh
ptb_ok P2P-refc-staging-seam bash tests/check-refc-staging.sh
ptb_ok P2P-header-staging-seam bash scripts/test-header-staging.sh
ptb_ok P2P-native-symbol-validator-seam bash tests/check-native-nif-exports.sh
ptb_ok P2P-abi-ledger-vs-header bash scripts/test-abi-ledger.sh
ptb_done
