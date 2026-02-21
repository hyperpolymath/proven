# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeJson

Safe JSON parsing and generation with depth limits and cycle prevention.
"""
module SafeJson

export JsonValue, JsonType
export parse_json, stringify, pretty_print
export is_object, is_array, is_string, is_number, is_bool, is_null
export get_object, get_array, get_string, get_number, get_bool
export object_keys, array_length, get_field, get_index
export MAX_DEPTH, MAX_STRING_LENGTH

"""Maximum nesting depth for JSON parsing."""
const MAX_DEPTH = 128

"""Maximum string length in JSON."""
const MAX_STRING_LENGTH = 10_000_000

"""
    JsonType

Enum representing JSON value types.
"""
@enum JsonType begin
    JsonNull
    JsonBool
    JsonNumber
    JsonString
    JsonArray
    JsonObject
end

"""
    JsonValue

Represents a parsed JSON value.
"""
struct JsonValue
    type::JsonType
    bool_value::Union{Bool, Nothing}
    number_value::Union{Float64, Nothing}
    string_value::Union{String, Nothing}
    array_value::Union{Vector{JsonValue}, Nothing}
    object_value::Union{Dict{String, JsonValue}, Nothing}
end

# Constructors for different JSON types
JsonValue(::Nothing) = JsonValue(JsonNull, nothing, nothing, nothing, nothing, nothing)
JsonValue(b::Bool) = JsonValue(JsonBool, b, nothing, nothing, nothing, nothing)
JsonValue(n::Number) = JsonValue(JsonNumber, nothing, Float64(n), nothing, nothing, nothing)
JsonValue(s::AbstractString) = JsonValue(JsonString, nothing, nothing, String(s), nothing, nothing)
JsonValue(arr::Vector{JsonValue}) = JsonValue(JsonArray, nothing, nothing, nothing, arr, nothing)
JsonValue(obj::Dict{String, JsonValue}) = JsonValue(JsonObject, nothing, nothing, nothing, nothing, obj)

"""
    is_null(v::JsonValue) -> Bool

Check if value is null.
"""
is_null(v::JsonValue)::Bool = v.type == JsonNull

"""
    is_bool(v::JsonValue) -> Bool

Check if value is boolean.
"""
is_bool(v::JsonValue)::Bool = v.type == JsonBool

"""
    is_number(v::JsonValue) -> Bool

Check if value is number.
"""
is_number(v::JsonValue)::Bool = v.type == JsonNumber

"""
    is_string(v::JsonValue) -> Bool

Check if value is string.
"""
is_string(v::JsonValue)::Bool = v.type == JsonString

"""
    is_array(v::JsonValue) -> Bool

Check if value is array.
"""
is_array(v::JsonValue)::Bool = v.type == JsonArray

"""
    is_object(v::JsonValue) -> Bool

Check if value is object.
"""
is_object(v::JsonValue)::Bool = v.type == JsonObject

"""
    get_bool(v::JsonValue) -> Union{Bool, Nothing}

Get boolean value or nothing.
"""
get_bool(v::JsonValue)::Union{Bool, Nothing} = v.bool_value

"""
    get_number(v::JsonValue) -> Union{Float64, Nothing}

Get number value or nothing.
"""
get_number(v::JsonValue)::Union{Float64, Nothing} = v.number_value

"""
    get_string(v::JsonValue) -> Union{String, Nothing}

Get string value or nothing.
"""
get_string(v::JsonValue)::Union{String, Nothing} = v.string_value

"""
    get_array(v::JsonValue) -> Union{Vector{JsonValue}, Nothing}

Get array value or nothing.
"""
get_array(v::JsonValue)::Union{Vector{JsonValue}, Nothing} = v.array_value

"""
    get_object(v::JsonValue) -> Union{Dict{String, JsonValue}, Nothing}

Get object value or nothing.
"""
get_object(v::JsonValue)::Union{Dict{String, JsonValue}, Nothing} = v.object_value

"""
    object_keys(v::JsonValue) -> Vector{String}

Get keys of an object, or empty vector if not an object.
"""
function object_keys(v::JsonValue)::Vector{String}
    v.object_value === nothing ? String[] : collect(keys(v.object_value))
end

"""
    array_length(v::JsonValue) -> Int

Get length of an array, or 0 if not an array.
"""
function array_length(v::JsonValue)::Int
    v.array_value === nothing ? 0 : length(v.array_value)
end

"""
    get_field(v::JsonValue, key::AbstractString) -> Union{JsonValue, Nothing}

Get field from object by key.
"""
function get_field(v::JsonValue, key::AbstractString)::Union{JsonValue, Nothing}
    v.object_value === nothing && return nothing
    get(v.object_value, key, nothing)
end

"""
    get_index(v::JsonValue, index::Integer) -> Union{JsonValue, Nothing}

Get element from array by index (1-based).
"""
function get_index(v::JsonValue, index::Integer)::Union{JsonValue, Nothing}
    v.array_value === nothing && return nothing
    (index < 1 || index > length(v.array_value)) && return nothing
    v.array_value[index]
end

"""
    parse_json(input::AbstractString; max_depth::Integer=MAX_DEPTH) -> Union{JsonValue, Nothing}

Parse JSON string into JsonValue. Returns nothing on parse error.
"""
function parse_json(input::AbstractString; max_depth::Integer=MAX_DEPTH)::Union{JsonValue, Nothing}
    length(input) > MAX_STRING_LENGTH && return nothing

    pos = Ref(1)
    result = try_parse_value(input, pos, 0, max_depth)
    result === nothing && return nothing

    skip_whitespace(input, pos)
    pos[] > length(input) || return nothing

    result
end

"""
    stringify(v::JsonValue) -> String

Convert JsonValue to compact JSON string.
"""
function stringify(v::JsonValue)::String
    io = IOBuffer()
    write_json(io, v)
    String(take!(io))
end

"""
    pretty_print(v::JsonValue; indent::Integer=2) -> String

Convert JsonValue to formatted JSON string with indentation.
"""
function pretty_print(v::JsonValue; indent::Integer=2)::String
    io = IOBuffer()
    write_json_pretty(io, v, 0, indent)
    String(take!(io))
end

# Internal parsing functions
function skip_whitespace(input::AbstractString, pos::Ref{Int})
    while pos[] <= length(input) && input[pos[]] in " \t\n\r"
        pos[] += 1
    end
end

function try_parse_value(input::AbstractString, pos::Ref{Int}, depth::Int, max_depth::Int)::Union{JsonValue, Nothing}
    depth > max_depth && return nothing
    skip_whitespace(input, pos)
    pos[] > length(input) && return nothing

    c = input[pos[]]

    if c == 'n'
        parse_null(input, pos)
    elseif c == 't' || c == 'f'
        parse_bool(input, pos)
    elseif c == '"'
        parse_string(input, pos)
    elseif c == '['
        parse_array(input, pos, depth, max_depth)
    elseif c == '{'
        parse_object(input, pos, depth, max_depth)
    elseif c == '-' || isdigit(c)
        parse_number(input, pos)
    else
        nothing
    end
end

function parse_null(input::AbstractString, pos::Ref{Int})::Union{JsonValue, Nothing}
    if pos[] + 3 <= length(input) && input[pos[]:pos[]+3] == "null"
        pos[] += 4
        JsonValue(nothing)
    else
        nothing
    end
end

function parse_bool(input::AbstractString, pos::Ref{Int})::Union{JsonValue, Nothing}
    if pos[] + 3 <= length(input) && input[pos[]:pos[]+3] == "true"
        pos[] += 4
        JsonValue(true)
    elseif pos[] + 4 <= length(input) && input[pos[]:pos[]+4] == "false"
        pos[] += 5
        JsonValue(false)
    else
        nothing
    end
end

function parse_string(input::AbstractString, pos::Ref{Int})::Union{JsonValue, Nothing}
    pos[] > length(input) && return nothing
    input[pos[]] != '"' && return nothing
    pos[] += 1

    result = IOBuffer()
    while pos[] <= length(input)
        c = input[pos[]]
        if c == '"'
            pos[] += 1
            return JsonValue(String(take!(result)))
        elseif c == '\\'
            pos[] += 1
            pos[] > length(input) && return nothing
            escaped = input[pos[]]
            if escaped == 'n'
                write(result, '\n')
            elseif escaped == 'r'
                write(result, '\r')
            elseif escaped == 't'
                write(result, '\t')
            elseif escaped == '"'
                write(result, '"')
            elseif escaped == '\\'
                write(result, '\\')
            elseif escaped == '/'
                write(result, '/')
            else
                write(result, escaped)
            end
            pos[] += 1
        else
            write(result, c)
            pos[] += 1
        end
    end
    nothing
end

function parse_number(input::AbstractString, pos::Ref{Int})::Union{JsonValue, Nothing}
    start = pos[]

    # Optional minus
    if pos[] <= length(input) && input[pos[]] == '-'
        pos[] += 1
    end

    # Digits
    while pos[] <= length(input) && isdigit(input[pos[]])
        pos[] += 1
    end

    # Decimal
    if pos[] <= length(input) && input[pos[]] == '.'
        pos[] += 1
        while pos[] <= length(input) && isdigit(input[pos[]])
            pos[] += 1
        end
    end

    # Exponent
    if pos[] <= length(input) && (input[pos[]] == 'e' || input[pos[]] == 'E')
        pos[] += 1
        if pos[] <= length(input) && (input[pos[]] == '+' || input[pos[]] == '-')
            pos[] += 1
        end
        while pos[] <= length(input) && isdigit(input[pos[]])
            pos[] += 1
        end
    end

    num_str = input[start:pos[]-1]
    num = tryparse(Float64, num_str)
    num === nothing ? nothing : JsonValue(num)
end

function parse_array(input::AbstractString, pos::Ref{Int}, depth::Int, max_depth::Int)::Union{JsonValue, Nothing}
    pos[] > length(input) && return nothing
    input[pos[]] != '[' && return nothing
    pos[] += 1

    elements = JsonValue[]
    skip_whitespace(input, pos)

    if pos[] <= length(input) && input[pos[]] == ']'
        pos[] += 1
        return JsonValue(elements)
    end

    while true
        element = try_parse_value(input, pos, depth + 1, max_depth)
        element === nothing && return nothing
        push!(elements, element)

        skip_whitespace(input, pos)
        pos[] > length(input) && return nothing

        if input[pos[]] == ']'
            pos[] += 1
            return JsonValue(elements)
        elseif input[pos[]] == ','
            pos[] += 1
        else
            return nothing
        end
    end
end

function parse_object(input::AbstractString, pos::Ref{Int}, depth::Int, max_depth::Int)::Union{JsonValue, Nothing}
    pos[] > length(input) && return nothing
    input[pos[]] != '{' && return nothing
    pos[] += 1

    obj = Dict{String, JsonValue}()
    skip_whitespace(input, pos)

    if pos[] <= length(input) && input[pos[]] == '}'
        pos[] += 1
        return JsonValue(obj)
    end

    while true
        skip_whitespace(input, pos)
        key_value = parse_string(input, pos)
        key_value === nothing && return nothing
        key = get_string(key_value)
        key === nothing && return nothing

        skip_whitespace(input, pos)
        pos[] > length(input) && return nothing
        input[pos[]] != ':' && return nothing
        pos[] += 1

        value = try_parse_value(input, pos, depth + 1, max_depth)
        value === nothing && return nothing
        obj[key] = value

        skip_whitespace(input, pos)
        pos[] > length(input) && return nothing

        if input[pos[]] == '}'
            pos[] += 1
            return JsonValue(obj)
        elseif input[pos[]] == ','
            pos[] += 1
        else
            return nothing
        end
    end
end

# Internal serialization functions
function write_json(io::IO, v::JsonValue)
    if v.type == JsonNull
        write(io, "null")
    elseif v.type == JsonBool
        write(io, v.bool_value ? "true" : "false")
    elseif v.type == JsonNumber
        if isinteger(v.number_value) && abs(v.number_value) < 1e15
            write(io, string(Int64(v.number_value)))
        else
            write(io, string(v.number_value))
        end
    elseif v.type == JsonString
        write_json_string(io, v.string_value)
    elseif v.type == JsonArray
        write(io, '[')
        for (i, elem) in enumerate(v.array_value)
            i > 1 && write(io, ',')
            write_json(io, elem)
        end
        write(io, ']')
    elseif v.type == JsonObject
        write(io, '{')
        first = true
        for (k, val) in v.object_value
            !first && write(io, ',')
            first = false
            write_json_string(io, k)
            write(io, ':')
            write_json(io, val)
        end
        write(io, '}')
    end
end

function write_json_pretty(io::IO, v::JsonValue, level::Int, indent::Int)
    pad = " " ^ (level * indent)
    inner_pad = " " ^ ((level + 1) * indent)

    if v.type == JsonNull
        write(io, "null")
    elseif v.type == JsonBool
        write(io, v.bool_value ? "true" : "false")
    elseif v.type == JsonNumber
        if isinteger(v.number_value) && abs(v.number_value) < 1e15
            write(io, string(Int64(v.number_value)))
        else
            write(io, string(v.number_value))
        end
    elseif v.type == JsonString
        write_json_string(io, v.string_value)
    elseif v.type == JsonArray
        if isempty(v.array_value)
            write(io, "[]")
        else
            write(io, "[\n")
            for (i, elem) in enumerate(v.array_value)
                write(io, inner_pad)
                write_json_pretty(io, elem, level + 1, indent)
                i < length(v.array_value) && write(io, ',')
                write(io, '\n')
            end
            write(io, pad, ']')
        end
    elseif v.type == JsonObject
        if isempty(v.object_value)
            write(io, "{}")
        else
            write(io, "{\n")
            pairs_vec = collect(v.object_value)
            for (i, (k, val)) in enumerate(pairs_vec)
                write(io, inner_pad)
                write_json_string(io, k)
                write(io, ": ")
                write_json_pretty(io, val, level + 1, indent)
                i < length(pairs_vec) && write(io, ',')
                write(io, '\n')
            end
            write(io, pad, '}')
        end
    end
end

function write_json_string(io::IO, s::AbstractString)
    write(io, '"')
    for c in s
        if c == '"'
            write(io, "\\\"")
        elseif c == '\\'
            write(io, "\\\\")
        elseif c == '\n'
            write(io, "\\n")
        elseif c == '\r'
            write(io, "\\r")
        elseif c == '\t'
            write(io, "\\t")
        elseif c < ' '
            write(io, "\\u", lpad(string(Int(c); base=16), 4, '0'))
        else
            write(io, c)
        end
    end
    write(io, '"')
end

end # module
