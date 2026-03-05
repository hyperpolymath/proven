-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-ldap: An LDAP client/server that cannot crash on malformed PDUs.
--
-- Architecture:
--   - LDAP.Types: Operation, SearchScope, ResultCode as closed sum types
--     with Show/Eq instances.
--
-- This module defines core LDAP constants (RFC 4511) and re-exports LDAP.Types.

module LDAP

import public LDAP.Types

%default total

-- ============================================================================
-- LDAP constants (RFC 4511)
-- ============================================================================

||| Standard LDAP port (RFC 4511).
public export
ldapPort : Bits16
ldapPort = 389

||| LDAP over TLS/SSL port (LDAPS, RFC 4513).
public export
ldapsPort : Bits16
ldapsPort = 636
