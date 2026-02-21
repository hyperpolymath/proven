# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeDateTime

Safe date and time operations with overflow protection.
"""
module SafeDateTime

using Dates

export Timestamp, Duration
export from_unix_secs, from_unix_millis, from_unix_nanos
export to_unix_secs, to_unix_millis, to_unix_nanos
export parse_iso8601, format_iso8601, format_rfc2822
export timestamp_add, timestamp_sub, timestamp_diff
export duration_from_secs, duration_from_millis, duration_from_nanos
export duration_to_secs, duration_to_millis, duration_to_nanos
export is_leap_year, days_in_month, days_in_year

"""
    Timestamp

A UTC timestamp stored as nanoseconds since Unix epoch.
"""
struct Timestamp
    nanos::Int64
end

"""
    Duration

A duration stored as nanoseconds.
"""
struct Duration
    nanos::Int64
end

# Constants
const NANOS_PER_SEC = Int64(1_000_000_000)
const NANOS_PER_MILLI = Int64(1_000_000)
const NANOS_PER_MICRO = Int64(1_000)
const SECS_PER_MIN = Int64(60)
const SECS_PER_HOUR = Int64(3600)
const SECS_PER_DAY = Int64(86400)

"""
    from_unix_secs(secs::Integer) -> Timestamp

Create timestamp from Unix seconds.
"""
function from_unix_secs(secs::Integer)::Timestamp
    Timestamp(Int64(secs) * NANOS_PER_SEC)
end

"""
    from_unix_millis(millis::Integer) -> Timestamp

Create timestamp from Unix milliseconds.
"""
function from_unix_millis(millis::Integer)::Timestamp
    Timestamp(Int64(millis) * NANOS_PER_MILLI)
end

"""
    from_unix_nanos(nanos::Integer) -> Timestamp

Create timestamp from Unix nanoseconds.
"""
function from_unix_nanos(nanos::Integer)::Timestamp
    Timestamp(Int64(nanos))
end

"""
    to_unix_secs(ts::Timestamp) -> Int64

Get Unix seconds from timestamp.
"""
function to_unix_secs(ts::Timestamp)::Int64
    div(ts.nanos, NANOS_PER_SEC)
end

"""
    to_unix_millis(ts::Timestamp) -> Int64

Get Unix milliseconds from timestamp.
"""
function to_unix_millis(ts::Timestamp)::Int64
    div(ts.nanos, NANOS_PER_MILLI)
end

"""
    to_unix_nanos(ts::Timestamp) -> Int64

Get Unix nanoseconds from timestamp.
"""
function to_unix_nanos(ts::Timestamp)::Int64
    ts.nanos
end

"""
    duration_from_secs(secs::Integer) -> Duration

Create duration from seconds.
"""
function duration_from_secs(secs::Integer)::Duration
    Duration(Int64(secs) * NANOS_PER_SEC)
end

"""
    duration_from_millis(millis::Integer) -> Duration

Create duration from milliseconds.
"""
function duration_from_millis(millis::Integer)::Duration
    Duration(Int64(millis) * NANOS_PER_MILLI)
end

"""
    duration_from_nanos(nanos::Integer) -> Duration

Create duration from nanoseconds.
"""
function duration_from_nanos(nanos::Integer)::Duration
    Duration(Int64(nanos))
end

"""
    duration_to_secs(d::Duration) -> Int64

Get seconds from duration.
"""
function duration_to_secs(d::Duration)::Int64
    div(d.nanos, NANOS_PER_SEC)
end

"""
    duration_to_millis(d::Duration) -> Int64

Get milliseconds from duration.
"""
function duration_to_millis(d::Duration)::Int64
    div(d.nanos, NANOS_PER_MILLI)
end

"""
    duration_to_nanos(d::Duration) -> Int64

Get nanoseconds from duration.
"""
function duration_to_nanos(d::Duration)::Int64
    d.nanos
end

"""
    timestamp_add(ts::Timestamp, d::Duration) -> Union{Timestamp, Nothing}

Add duration to timestamp. Returns nothing on overflow.
"""
function timestamp_add(ts::Timestamp, d::Duration)::Union{Timestamp, Nothing}
    try
        result = Base.checked_add(ts.nanos, d.nanos)
        Timestamp(result)
    catch e
        e isa OverflowError ? nothing : rethrow(e)
    end
end

"""
    timestamp_sub(ts::Timestamp, d::Duration) -> Union{Timestamp, Nothing}

Subtract duration from timestamp. Returns nothing on overflow.
"""
function timestamp_sub(ts::Timestamp, d::Duration)::Union{Timestamp, Nothing}
    try
        result = Base.checked_sub(ts.nanos, d.nanos)
        Timestamp(result)
    catch e
        e isa OverflowError ? nothing : rethrow(e)
    end
end

"""
    timestamp_diff(a::Timestamp, b::Timestamp) -> Union{Duration, Nothing}

Calculate duration between two timestamps. Returns nothing on overflow.
"""
function timestamp_diff(a::Timestamp, b::Timestamp)::Union{Duration, Nothing}
    try
        result = Base.checked_sub(a.nanos, b.nanos)
        Duration(result)
    catch e
        e isa OverflowError ? nothing : rethrow(e)
    end
end

"""
    parse_iso8601(s::AbstractString) -> Union{Timestamp, Nothing}

Parse ISO 8601 datetime string.
"""
function parse_iso8601(s::AbstractString)::Union{Timestamp, Nothing}
    try
        # Handle basic ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
        str = strip(s)

        # Try parsing with Dates
        dt = if endswith(str, "Z")
            DateTime(str[1:end-1], dateformat"yyyy-mm-ddTHH:MM:SS")
        elseif contains(str, "T")
            DateTime(str, dateformat"yyyy-mm-ddTHH:MM:SS")
        else
            DateTime(str, dateformat"yyyy-mm-dd")
        end

        # Convert to Unix timestamp
        unix_ms = Dates.datetime2unix(dt) * 1000
        from_unix_millis(round(Int64, unix_ms))
    catch
        nothing
    end
end

"""
    format_iso8601(ts::Timestamp) -> String

Format timestamp as ISO 8601 string.
"""
function format_iso8601(ts::Timestamp)::String
    secs = to_unix_secs(ts)
    dt = Dates.unix2datetime(secs)
    Dates.format(dt, dateformat"yyyy-mm-ddTHH:MM:SS") * "Z"
end

"""
    format_rfc2822(ts::Timestamp) -> String

Format timestamp as RFC 2822 string.
"""
function format_rfc2822(ts::Timestamp)::String
    secs = to_unix_secs(ts)
    dt = Dates.unix2datetime(secs)

    day_names = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    month_names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

    dow = dayofweek(dt)
    day_name = day_names[dow]
    month_name = month_names[month(dt)]

    "$day_name, $(lpad(day(dt), 2, '0')) $month_name $(year(dt)) " *
    "$(lpad(hour(dt), 2, '0')):$(lpad(minute(dt), 2, '0')):$(lpad(second(dt), 2, '0')) +0000"
end

"""
    is_leap_year(year::Integer) -> Bool

Check if year is a leap year.
"""
function is_leap_year(year::Integer)::Bool
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
end

"""
    days_in_month(year::Integer, month::Integer) -> Union{Int, Nothing}

Get number of days in a month. Returns nothing for invalid month.
"""
function days_in_month(year::Integer, month::Integer)::Union{Int, Nothing}
    (month < 1 || month > 12) && return nothing

    days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    if month == 2 && is_leap_year(year)
        29
    else
        days[month]
    end
end

"""
    days_in_year(year::Integer) -> Int

Get number of days in a year.
"""
function days_in_year(year::Integer)::Int
    is_leap_year(year) ? 366 : 365
end

# Operator overloads
Base.:+(ts::Timestamp, d::Duration) = something(timestamp_add(ts, d), error("Overflow"))
Base.:-(ts::Timestamp, d::Duration) = something(timestamp_sub(ts, d), error("Overflow"))
Base.:-(a::Timestamp, b::Timestamp) = something(timestamp_diff(a, b), error("Overflow"))

# Comparison operators
Base.:(==)(a::Timestamp, b::Timestamp) = a.nanos == b.nanos
Base.:<(a::Timestamp, b::Timestamp) = a.nanos < b.nanos
Base.:>(a::Timestamp, b::Timestamp) = a.nanos > b.nanos
Base.:<=(a::Timestamp, b::Timestamp) = a.nanos <= b.nanos
Base.:>=(a::Timestamp, b::Timestamp) = a.nanos >= b.nanos

Base.:(==)(a::Duration, b::Duration) = a.nanos == b.nanos
Base.:<(a::Duration, b::Duration) = a.nanos < b.nanos

# Display
Base.show(io::IO, ts::Timestamp) = print(io, "Timestamp(", format_iso8601(ts), ")")
Base.show(io::IO, d::Duration) = print(io, "Duration(", duration_to_secs(d), "s)")

end # module
