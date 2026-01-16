# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Proven - Code that cannot crash
#
# A verified safety library for Nickel providing:
# - SafeMath: Arithmetic without overflow/underflow/division-by-zero
# - SafeString: XSS/SQL injection prevention
# - SafePath: Path operations without traversal attacks
# - SafeEmail: RFC-compliant email validation
# - SafeNetwork: IP/CIDR/port validation
# - SafeConfig: Safe configuration value handling

{
  version = "1.0.0",

  # Re-export all modules
  SafeMath = import "./proven/safe-math.ncl",
  SafeString = import "./proven/safe-string.ncl",
  SafePath = import "./proven/safe-path.ncl",
  SafeEmail = import "./proven/safe-email.ncl",
  SafeNetwork = import "./proven/safe-network.ncl",
  SafeConfig = import "./proven/safe-config.ncl",
  SafeUuid = import "./proven/safe-uuid.ncl",
  SafeHex = import "./proven/safe-hex.ncl",
}
