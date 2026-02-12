# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeVersion

Semantic versioning operations following SemVer 2.0.0 specification.
"""
module SafeVersion

export SemVer, VersionBump
export parse_semver, format_semver, is_valid_semver
export major, minor, patch, prerelease, build_metadata
export bump_major, bump_minor, bump_patch
export is_prerelease, is_stable, compare_versions
export satisfies_range, is_compatible

"""
    VersionBump

Types of version bumps.
"""
@enum VersionBump begin
    BumpMajor
    BumpMinor
    BumpPatch
end

"""
    SemVer

A semantic version following SemVer 2.0.0.
"""
struct SemVer
    major::UInt64
    minor::UInt64
    patch::UInt64
    prerelease::Union{String, Nothing}
    build::Union{String, Nothing}
end

"""
    SemVer(major, minor, patch) -> SemVer

Create a simple version without prerelease or build metadata.
"""
SemVer(major::Integer, minor::Integer, patch::Integer) =
    SemVer(UInt64(major), UInt64(minor), UInt64(patch), nothing, nothing)

"""
    major(v::SemVer) -> UInt64

Get major version number.
"""
major(v::SemVer)::UInt64 = v.major

"""
    minor(v::SemVer) -> UInt64

Get minor version number.
"""
minor(v::SemVer)::UInt64 = v.minor

"""
    patch(v::SemVer) -> UInt64

Get patch version number.
"""
patch(v::SemVer)::UInt64 = v.patch

"""
    prerelease(v::SemVer) -> Union{String, Nothing}

Get prerelease string.
"""
prerelease(v::SemVer)::Union{String, Nothing} = v.prerelease

"""
    build_metadata(v::SemVer) -> Union{String, Nothing}

Get build metadata string.
"""
build_metadata(v::SemVer)::Union{String, Nothing} = v.build

"""
    is_prerelease(v::SemVer) -> Bool

Check if version is a prerelease.
"""
is_prerelease(v::SemVer)::Bool = v.prerelease !== nothing

"""
    is_stable(v::SemVer) -> Bool

Check if version is stable (not prerelease and major > 0).
"""
is_stable(v::SemVer)::Bool = v.major > 0 && v.prerelease === nothing

"""
    parse_semver(s::AbstractString) -> Union{SemVer, Nothing}

Parse a semantic version string.
"""
function parse_semver(s::AbstractString)::Union{SemVer, Nothing}
    str = strip(s)

    # Remove leading 'v' if present
    if !isempty(str) && str[1] == 'v'
        str = str[2:end]
    end

    isempty(str) && return nothing

    # Extract build metadata (after +)
    build = nothing
    if contains(str, "+")
        idx = findfirst('+', str)
        build = str[idx+1:end]
        str = str[1:idx-1]
        isempty(build) && return nothing
    end

    # Extract prerelease (after -)
    prerelease = nothing
    if contains(str, "-")
        idx = findfirst('-', str)
        prerelease = str[idx+1:end]
        str = str[1:idx-1]
        isempty(prerelease) && return nothing
    end

    # Parse major.minor.patch
    parts = split(str, ".")
    length(parts) != 3 && return nothing

    major_val = tryparse(UInt64, parts[1])
    minor_val = tryparse(UInt64, parts[2])
    patch_val = tryparse(UInt64, parts[3])

    (major_val === nothing || minor_val === nothing || patch_val === nothing) && return nothing

    # Reject leading zeros (except for "0")
    for part in parts
        length(part) > 1 && startswith(part, "0") && return nothing
    end

    SemVer(major_val, minor_val, patch_val, prerelease, build)
end

"""
    is_valid_semver(s::AbstractString) -> Bool

Check if string is a valid semantic version.
"""
is_valid_semver(s::AbstractString)::Bool = parse_semver(s) !== nothing

"""
    format_semver(v::SemVer) -> String

Format version as string.
"""
function format_semver(v::SemVer)::String
    result = "$(v.major).$(v.minor).$(v.patch)"
    v.prerelease !== nothing && (result *= "-$(v.prerelease)")
    v.build !== nothing && (result *= "+$(v.build)")
    result
end

"""
    bump_major(v::SemVer) -> SemVer

Bump major version, resetting minor and patch to 0.
"""
function bump_major(v::SemVer)::SemVer
    SemVer(v.major + 1, 0, 0, nothing, nothing)
end

"""
    bump_minor(v::SemVer) -> SemVer

Bump minor version, resetting patch to 0.
"""
function bump_minor(v::SemVer)::SemVer
    SemVer(v.major, v.minor + 1, 0, nothing, nothing)
end

"""
    bump_patch(v::SemVer) -> SemVer

Bump patch version.
"""
function bump_patch(v::SemVer)::SemVer
    SemVer(v.major, v.minor, v.patch + 1, nothing, nothing)
end

"""
    compare_versions(a::SemVer, b::SemVer) -> Int

Compare two versions. Returns -1, 0, or 1.
"""
function compare_versions(a::SemVer, b::SemVer)::Int
    # Compare major.minor.patch
    a.major != b.major && return a.major < b.major ? -1 : 1
    a.minor != b.minor && return a.minor < b.minor ? -1 : 1
    a.patch != b.patch && return a.patch < b.patch ? -1 : 1

    # Prerelease comparison
    # No prerelease > prerelease
    if a.prerelease === nothing && b.prerelease !== nothing
        return 1
    elseif a.prerelease !== nothing && b.prerelease === nothing
        return -1
    elseif a.prerelease !== nothing && b.prerelease !== nothing
        # Compare prerelease strings
        a_parts = split(a.prerelease, ".")
        b_parts = split(b.prerelease, ".")

        for i in 1:min(length(a_parts), length(b_parts))
            a_num = tryparse(UInt64, a_parts[i])
            b_num = tryparse(UInt64, b_parts[i])

            if a_num !== nothing && b_num !== nothing
                a_num != b_num && return a_num < b_num ? -1 : 1
            elseif a_num !== nothing
                return -1  # Numeric < alphanumeric
            elseif b_num !== nothing
                return 1
            else
                cmp = cmp_strings(a_parts[i], b_parts[i])
                cmp != 0 && return cmp
            end
        end

        length(a_parts) != length(b_parts) &&
            return length(a_parts) < length(b_parts) ? -1 : 1
    end

    0
end

function cmp_strings(a::AbstractString, b::AbstractString)::Int
    a < b ? -1 : (a > b ? 1 : 0)
end

"""
    is_compatible(base::SemVer, candidate::SemVer) -> Bool

Check if candidate is compatible with base (same major, >= minor.patch).
"""
function is_compatible(base::SemVer, candidate::SemVer)::Bool
    # For 0.x versions, minor version changes are breaking
    if base.major == 0
        candidate.major == base.major &&
        candidate.minor == base.minor &&
        candidate.patch >= base.patch
    else
        candidate.major == base.major &&
        compare_versions(candidate, base) >= 0
    end
end

"""
    satisfies_range(v::SemVer, range::AbstractString) -> Bool

Check if version satisfies a simple range expression (^, ~, >=, >, <, <=, =).
"""
function satisfies_range(v::SemVer, range::AbstractString)::Bool
    str = strip(range)
    isempty(str) && return false

    # Caret range (^): compatible with version
    if startswith(str, "^")
        base = parse_semver(str[2:end])
        base === nothing && return false
        return is_compatible(base, v)
    end

    # Tilde range (~): patch-level changes
    if startswith(str, "~")
        base = parse_semver(str[2:end])
        base === nothing && return false
        return v.major == base.major && v.minor == base.minor && v.patch >= base.patch
    end

    # Comparison operators
    if startswith(str, ">=")
        other = parse_semver(str[3:end])
        other === nothing && return false
        return compare_versions(v, other) >= 0
    elseif startswith(str, "<=")
        other = parse_semver(str[3:end])
        other === nothing && return false
        return compare_versions(v, other) <= 0
    elseif startswith(str, ">")
        other = parse_semver(str[2:end])
        other === nothing && return false
        return compare_versions(v, other) > 0
    elseif startswith(str, "<")
        other = parse_semver(str[2:end])
        other === nothing && return false
        return compare_versions(v, other) < 0
    elseif startswith(str, "=")
        other = parse_semver(str[2:end])
        other === nothing && return false
        return compare_versions(v, other) == 0
    end

    # Exact match
    other = parse_semver(str)
    other === nothing && return false
    compare_versions(v, other) == 0
end

# Comparison operators
Base.:(==)(a::SemVer, b::SemVer) = compare_versions(a, b) == 0
Base.:<(a::SemVer, b::SemVer) = compare_versions(a, b) < 0
Base.:>(a::SemVer, b::SemVer) = compare_versions(a, b) > 0
Base.:<=(a::SemVer, b::SemVer) = compare_versions(a, b) <= 0
Base.:>=(a::SemVer, b::SemVer) = compare_versions(a, b) >= 0

# Display
Base.show(io::IO, v::SemVer) = print(io, format_semver(v))
Base.string(v::SemVer) = format_semver(v)

end # module
