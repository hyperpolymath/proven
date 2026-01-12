-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Proven - Safety-first utility functions with formal verification guarantees.
--- @module proven

local proven = {
    _VERSION = "0.3.0",
    _DESCRIPTION = "Safety-first utility functions with formal verification guarantees",
    _LICENSE = "PMPL-1.0",
}

proven.safe_math = require("proven.safe_math")
proven.safe_string = require("proven.safe_string")
proven.safe_path = require("proven.safe_path")
proven.safe_email = require("proven.safe_email")
proven.safe_network = require("proven.safe_network")
proven.safe_crypto = require("proven.safe_crypto")

return proven
