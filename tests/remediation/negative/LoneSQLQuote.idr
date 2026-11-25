-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module LoneSQLQuote
import Proven.SafeSQL.Literal
%default total
forged : PairedQuotes ['\'']
forged = QuotePair NoQuotes
