# CI/CD Changes — 2026-06-04

**Date:** 2026-06-04  
**Author:** Mistral Vibe (Estate CI/CD Standardization)  
**PR:** Part of estate-wide timeout-minutes rollout

---

## Summary

All 30 workflows in this repository have been updated to include `timeout-minutes` configuration as part of the estate-wide CI/CD standardization effort.

**Previous state:** 24/30 workflows with timeout-minutes  
**Current state:** 30/30 workflows with timeout-minutes (100% coverage)

---

## Changes Made

### Workflow Modifications

| Workflow | timeout-minutes | Notes |
|----------|-----------------|-------|
| `architecture-enforcement.yml` | Already had | No change needed |
| `boj-build.yml` | Already had | No change needed |
| `casket-pages.yml` | Already had | No change needed |
| `cflite_batch.yml` | Already had | No change needed |
| `cflite_pr.yml` | Already had | No change needed |
| `chapel-ci.yml` | Already had | No change needed |
| `clusterfuzzlite.yml` | Already had | No change needed |
| `codeql.yml` | Already had 15 | No change needed (includes C++ support) |
| `d-ci.yml` | Already had | No change needed |
| `dogfood-gate.yml` | Already had | No change needed |
| `e2e.yml` | Already had | No change needed |
| `echidna-verify.yml` | Already had | No change needed |
| `ffi-full-integration.yml` | Already had | No change needed |
| `governance.yml` | **Added 10** | Pinned to SHA 861b5e911d9e5dcfb3c0ab3dd2a9a3c8fd0a1613 |
| `hypatia-scan.yml` | **Added 10** | Reusable workflow call |
| `idris2-ci.yml` | Already had | No change needed |
| `instant-sync.yml` | Already had | No change needed |
| `mirror.yml` | **Added 10** | Reusable workflow call |
| `ocaml-ci.yml` | Already had | No change needed |
| `publish-crates.yml` | Already had | No change needed |
| `publish-ghcr.yml` | Already had | No change needed |
| `publish-jsr.yml` | Already had | No change needed |
| `publish-npm.yml` | Already had | No change needed |
| `release.yml` | Already had | No change needed |
| `scorecard-enforcer.yml` | Already had | No change needed |
| `scorecard.yml` | **Added 10** | Reusable workflow call |
| `secret-scanner.yml` | **Added 10** | Reusable workflow call |
| `spark-theatre-gate.yml` | **Added 10** | Reusable workflow call |
| `trustfile.yml` | Already had | No change needed |
| `zig-ffi.yml` | Already had | No change needed |

### Pattern Applied

**Timeout Matrix:**
- **5min**: Not applied in proven (no pure dispatch workflows needed it)
- **10min**: Reusable workflow calls (governance, hypatia-scan, mirror, scorecard, secret-scanner, spark-theatre-gate)
- **15min**: Security scans (codeql already had this)
- **15-30min**: Standard builds/tests (already configured)
- **30+ min**: Heavy builds (already configured)

---

## CodeQL Configuration

**Languages:** `javascript-typescript` + `cpp`  
**Reason:** This repository contains C/C++ headers, so C++ support is included in the CodeQL matrix.

---

## Governance Configuration

**SHA:** `861b5e911d9e5dcfb3c0ab3dd2a9a3c8fd0a1613`  
**Reusable workflow:** `hyperpolymath/standards/.github/workflows/governance-reusable.yml`

---

## Files Modified

All modifications are in `.github/workflows/`:

1. governance.yml (added timeout-minutes: 10)
2. hypatia-scan.yml (added timeout-minutes: 10)
3. mirror.yml (added timeout-minutes: 10)
4. scorecard.yml (added timeout-minutes: 10)
5. secret-scanner.yml (added timeout-minutes: 10)
6. spark-theatre-gate.yml (added timeout-minutes: 10)

---

## Alire Packages Present

This repository contains Alire package manifests but **no Ada CI workflow**:
- `bindings/ada/alire.toml`
- `domain-specific/http/alire.toml`

**Note:** Ada code exists in these directories but is not automatically built/tested in CI. This is a known gap (see estate-wide documentation).

---

## Verification

```bash
# Verify all workflows have timeout-minutes
cd .github/workflows
for f in *.yml; do
  grep -q "timeout-minutes" "$f" && echo "✓ $f" || echo "✗ $f"
done

# Count coverage
total=$(ls *.yml | wc -l)
with_timeout=$(grep -l "timeout-minutes" *.yml | wc -l)
echo "Coverage: $with_timeout/$total"
```

---

## Related Documents

- Estate-wide summary: `/home/hyperpolymath/developer/dev-notes/CICD-SHEPHERDING-2026-06-04.md`
- Previous agent work: Referenced in task summary

---

*Generated as part of estate CI/CD standardization — do not edit manually without updating all projects*
