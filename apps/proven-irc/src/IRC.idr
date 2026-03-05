-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level IRC module. Re-exports IRC.Types and defines protocol constants.
module IRC

import public IRC.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 2812)
-------------------------------------------------------------------------------

||| Default IRC plaintext port.
public export
ircPort : Nat
ircPort = 6667

||| Default IRC over TLS port.
public export
ircsPort : Nat
ircsPort = 6697

||| Maximum nickname length per RFC 2812 Section 1.2.1.
public export
maxNickLength : Nat
maxNickLength = 9

||| Maximum IRC message line length including CR-LF (RFC 2812 Section 2.3).
public export
maxLineLength : Nat
maxLineLength = 512
