<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Project Issues Kanban Board

**Repository:** hyperpolymath/proven  
**Last Updated:** 2026-06-03  
**Purpose:** Track GitHub issues through kanban workflow columns

## Kanban Columns

| Issue | Title | Status | Priority | Assignee | Labels | Blocked By | Due Date | Notes |
|-------|-------|--------|----------|----------|--------|-----------|----------|-------|
| #80 | SafePassword.Strength windows/detectPatterns/analyzeStrength covering→total refactor | In Progress | High | - | proof-debt, Phase-3 | - | - | Discharges 3 OWED sites: strengthScoreBounded, higherImpliesLower, veryStrongSatisfiesAll |
| #83 | - | Backlog | High | - | proof-debt, Phase-3 | - | - | Phase 3 unblocking |

## Column Definitions

- **Backlog** - Issues not yet started, awaiting triage or resources
- **In Progress** - Actively being worked on
- **Blocked** - Waiting on dependencies or external factors  
- **Review** - Ready for review, testing, or approval
- **Done** - Completed and verified

## Status Transitions

```
Backlog → In Progress → Review → Done
         ↓
      Blocked (when dependencies arise)
```

## Issue #80 Details

**Related Files:**
- `src/Proven/SafePassword/Proofs.idr` (lines 257-258, 454-458, 471-474)
- `src/Proven/SafePassword.idr` (strength analysis functions)

**Tasks:**
1. Refactor `windows` function from covering to total
2. Refactor `detectPatterns` function from covering to total  
3. Refactor `analyzeStrength` function from covering to total
4. Discharge `strengthScoreBounded` theorem
5. Discharge `higherImpliesLower` theorem
6. Discharge `veryStrongSatisfiesAll` theorem

**Estimated Effort:** ~2-3 hours

**Dependencies:** None identified

## Issue #83 Details  

**Status:** Backlog - needs investigation to determine specific scope

---

*Auto-generated from GitHub issues - sync with hyperpolymath/proven/issues*
