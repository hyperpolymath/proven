#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# UT (sweep 1): every toolchain-free test script, run as one assertion each, plus a
# negative control proving a failing script is reported as a failure.
# Scripts that need Idris 2 0.8.0 are NOT run here (the umbrella cannot provision it
# in v0.1); they stay in header-refc.yml and are waived in .proven.toml until v0.2.
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2
. tests/kit/lib/ptb.sh
for t in scripts/test-abi-ledger.sh scripts/test-header-staging.sh scripts/test-refc-staging.sh \
         tests/check-native-nif-exports.sh tests/check-refc-staging.sh; do
  [[ -f $t ]] || { echo "unit: listed test $t is missing"; exit 2; }
  ptb_ok "UT-$t" bash "$t"
done
bad=$(mktemp); printf '#!/usr/bin/env bash\nexit 1\n' > "$bad"
ptb_fails UT-negative-control-failing-script bash "$bad"
rm -f "$bad"
ptb_ok UT-coverage-manifest bash tests/kit/check-coverage.sh --through-sweep 1 .proven/coverage.tsv
ptb_done
