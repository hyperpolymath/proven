-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- CA : Top-level module for the proven-ca Certificate Authority server.
-- Re-exports CA.Types and defines server constants.

module CA

import public CA.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Maximum certificate chain depth (path length constraint).
public export
maxPathLength : Nat
maxPathLength = 3

||| Default certificate validity period in days.
public export
defaultValidityDays : Nat
defaultValidityDays = 365

||| CRL update interval in hours.
public export
crlUpdateHours : Nat
crlUpdateHours = 24
