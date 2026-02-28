-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.Network - Convenience re-export module for networking and web safety
|||
||| Groups all network-related Safe* modules into a single import for common
||| web application and network service development. Covers IP addressing,
||| DNS resolution, SSH key management, URL parsing, email validation,
||| HTTP headers and cookies, content negotiation, OAuth/JWT authentication,
||| webhook verification, HTML sanitisation, and phone number validation.
|||
||| Usage:
|||   import Proven.Network
|||
||| This single import provides access to all networking and web safety types,
||| constructors, and validation functions without needing 13 separate imports.
module Proven.Network

import public Proven.SafeNetwork
import public Proven.SafeDNS
import public Proven.SafeSSH
import public Proven.SafeUrl
import public Proven.SafeEmail
import public Proven.SafeHeader
import public Proven.SafeCookie
import public Proven.SafeContentType
import public Proven.SafeOAuth
import public Proven.SafeJWT
import public Proven.SafeWebhook
import public Proven.SafeHtml
import public Proven.SafePhone
