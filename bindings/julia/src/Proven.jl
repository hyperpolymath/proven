# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    Proven

Safety-first utility functions with formal verification guarantees.
Provides safe operations for math, strings, paths, emails, URLs, networking,
UUIDs, currencies, phone numbers, and hex encoding.
"""
module Proven

include("SafeMath.jl")
include("SafeString.jl")
include("SafePath.jl")
include("SafeEmail.jl")
include("SafeUrl.jl")
include("SafeNetwork.jl")
include("SafeCrypto.jl")
include("SafeUUID.jl")
include("SafeCurrency.jl")
include("SafePhone.jl")
include("SafeHex.jl")

export SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork, SafeCrypto
export SafeUUID, SafeCurrency, SafePhone, SafeHex

end # module
