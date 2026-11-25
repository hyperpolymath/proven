-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module RawSQLParameter
import Proven.SafeSQL
%default total
forged : BoundParameters [SQLRaw "DROP TABLE users"]
forged = MoreParameters Refl NoParameters
