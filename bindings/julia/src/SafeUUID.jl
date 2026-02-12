# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeUUID

Safe UUID generation and validation following RFC 4122.
"""
module SafeUUID

export Uuid, UuidVersion, UuidVariant
export parse_uuid, format_uuid, format_urn, is_valid_uuid, is_nil
export uuid_version, uuid_variant, uuid_from_bytes, v4_from_bytes
export NAMESPACE_DNS, NAMESPACE_URL, NIL_UUID

"""
    UuidVersion

UUID version types per RFC 4122.
"""
@enum UuidVersion begin
    V1 = 1   # Time-based
    V2 = 2   # DCE Security
    V3 = 3   # Name-based (MD5)
    V4 = 4   # Random
    V5 = 5   # Name-based (SHA-1)
    Nil = 0  # Nil UUID
end

"""
    UuidVariant

UUID variant types per RFC 4122.
"""
@enum UuidVariant begin
    Ncs
    Rfc4122
    Microsoft
    Future
end

"""
    Uuid

A validated UUID (128 bits).
"""
struct Uuid
    bytes::NTuple{16, UInt8}
end

"""
    NIL_UUID

The nil UUID (all zeros).
"""
const NIL_UUID = Uuid(ntuple(_ -> UInt8(0), 16))

"""
    NAMESPACE_DNS

DNS namespace UUID for name-based UUIDs.
"""
const NAMESPACE_DNS = Uuid((
    0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
))

"""
    NAMESPACE_URL

URL namespace UUID for name-based UUIDs.
"""
const NAMESPACE_URL = Uuid((
    0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
))

"""
    uuid_from_bytes(bytes::NTuple{16, UInt8}) -> Uuid

Create UUID from a 16-byte tuple.
"""
function uuid_from_bytes(bytes::NTuple{16, UInt8})::Uuid
    Uuid(bytes)
end

"""
    uuid_from_bytes(bytes::AbstractVector{UInt8}) -> Union{Uuid, Nothing}

Create UUID from a byte vector. Returns `nothing` if wrong length.
"""
function uuid_from_bytes(bytes::AbstractVector{UInt8})::Union{Uuid, Nothing}
    length(bytes) != 16 && return nothing
    Uuid(NTuple{16, UInt8}(bytes))
end

"""
    uuid_version(uuid::Uuid) -> UuidVersion

Get the UUID version.
"""
function uuid_version(uuid::Uuid)::UuidVersion
    version_nibble = (uuid.bytes[7] >> 4) & 0x0F
    if version_nibble == 1
        V1
    elseif version_nibble == 2
        V2
    elseif version_nibble == 3
        V3
    elseif version_nibble == 4
        V4
    elseif version_nibble == 5
        V5
    else
        Nil
    end
end

"""
    uuid_variant(uuid::Uuid) -> UuidVariant

Get the UUID variant.
"""
function uuid_variant(uuid::Uuid)::UuidVariant
    byte = uuid.bytes[9]
    if (byte >> 7) == 0
        Ncs
    elseif (byte >> 6) == 0b10
        Rfc4122
    elseif (byte >> 5) == 0b110
        Microsoft
    else
        Future
    end
end

"""
    is_nil(uuid::Uuid) -> Bool

Check if this is the nil UUID.
"""
function is_nil(uuid::Uuid)::Bool
    all(b -> b == 0x00, uuid.bytes)
end

"""
    format_uuid(uuid::Uuid) -> String

Format UUID as canonical string (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
"""
function format_uuid(uuid::Uuid)::String
    b = uuid.bytes
    @sprintf("%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
        b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8],
        b[9], b[10], b[11], b[12], b[13], b[14], b[15], b[16])
end

"""
    format_urn(uuid::Uuid) -> String

Format UUID as URN (urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
"""
function format_urn(uuid::Uuid)::String
    "urn:uuid:" * format_uuid(uuid)
end

"""
    parse_uuid(s::AbstractString) -> Union{Uuid, Nothing}

Parse UUID from canonical string format.
Returns `nothing` if parsing fails.
"""
function parse_uuid(s::AbstractString)::Union{Uuid, Nothing}
    str = strip(s)
    length(str) != 36 && return nothing

    # Check dash positions
    chars = collect(str)
    (chars[9] != '-' || chars[14] != '-' || chars[19] != '-' || chars[24] != '-') && return nothing

    # Extract hex characters
    hex_str = filter(c -> c != '-', str)
    length(hex_str) != 32 && return nothing

    # Validate all characters are hex
    all(c -> c in '0':'9' || c in 'a':'f' || c in 'A':'F', hex_str) || return nothing

    # Parse bytes
    bytes = UInt8[]
    for i in 1:2:32
        byte_str = hex_str[i:i+1]
        byte = tryparse(UInt8, byte_str; base=16)
        byte === nothing && return nothing
        push!(bytes, byte)
    end

    Uuid(NTuple{16, UInt8}(bytes))
end

"""
    is_valid_uuid(s::AbstractString) -> Bool

Check if string is a valid UUID format.
"""
function is_valid_uuid(s::AbstractString)::Bool
    parse_uuid(s) !== nothing
end

"""
    v4_from_bytes(random_bytes::NTuple{16, UInt8}) -> Uuid

Generate a v4 (random) UUID from provided random bytes.
Sets version to 4 and variant to RFC 4122.
"""
function v4_from_bytes(random_bytes::NTuple{16, UInt8})::Uuid
    bytes = collect(random_bytes)
    # Set version to 4
    bytes[7] = (bytes[7] & 0x0F) | 0x40
    # Set variant to RFC 4122
    bytes[9] = (bytes[9] & 0x3F) | 0x80
    Uuid(NTuple{16, UInt8}(bytes))
end

"""
    v4_from_bytes(random_bytes::AbstractVector{UInt8}) -> Union{Uuid, Nothing}

Generate a v4 (random) UUID from provided random bytes vector.
Returns `nothing` if wrong length.
"""
function v4_from_bytes(random_bytes::AbstractVector{UInt8})::Union{Uuid, Nothing}
    length(random_bytes) != 16 && return nothing
    v4_from_bytes(NTuple{16, UInt8}(random_bytes))
end

# Display methods
Base.show(io::IO, uuid::Uuid) = print(io, "Uuid(", format_uuid(uuid), ")")
Base.string(uuid::Uuid) = format_uuid(uuid)

# For Printf.@sprintf
using Printf

end # module
