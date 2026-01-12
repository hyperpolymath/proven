# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    Proven

Safety-first utility functions with formal verification guarantees.
Provides safe operations for math, strings, paths, emails, URLs, and networking.
"""
module Proven

include("SafeMath.jl")
include("SafeString.jl")
include("SafePath.jl")
include("SafeEmail.jl")
include("SafeUrl.jl")
include("SafeNetwork.jl")
include("SafeCrypto.jl")

export SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork, SafeCrypto

end # module
