-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe date and time operations with validation.
--- @module proven.safe_datetime

local safe_datetime = {}

--- DateTime structure.
--- @class DateTime
--- @field year number Year (1-9999)
--- @field month number Month (1-12)
--- @field day number Day (1-31)
--- @field hour number Hour (0-23)
--- @field minute number Minute (0-59)
--- @field second number Second (0-59)
--- @field millisecond number Millisecond (0-999)
--- @field timezone_offset number Offset in minutes from UTC

local DateTime = {}
DateTime.__index = DateTime

--- Days in each month (non-leap year).
local DAYS_IN_MONTH = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}

--- Check if a year is a leap year.
--- @param year number The year to check
--- @return boolean is_leap Whether the year is a leap year
function safe_datetime.is_leap_year(year)
    return (year % 4 == 0 and year % 100 ~= 0) or (year % 400 == 0)
end

--- Get the number of days in a month.
--- @param year number The year
--- @param month number The month (1-12)
--- @return number days Number of days in the month
function safe_datetime.days_in_month(year, month)
    if month < 1 or month > 12 then
        return 0
    end
    if month == 2 and safe_datetime.is_leap_year(year) then
        return 29
    end
    return DAYS_IN_MONTH[month]
end

--- Create a new DateTime object with validation.
--- @param year number Year (1-9999)
--- @param month number Month (1-12)
--- @param day number Day (1-31)
--- @param hour number|nil Hour (0-23), default 0
--- @param minute number|nil Minute (0-59), default 0
--- @param second number|nil Second (0-59), default 0
--- @param millisecond number|nil Millisecond (0-999), default 0
--- @return DateTime|nil datetime The DateTime or nil if invalid
--- @return string|nil error Error message if validation failed
function safe_datetime.new(year, month, day, hour, minute, second, millisecond)
    hour = hour or 0
    minute = minute or 0
    second = second or 0
    millisecond = millisecond or 0

    -- Validate ranges
    if type(year) ~= "number" or year < 1 or year > 9999 or year ~= math.floor(year) then
        return nil, "Invalid year"
    end
    if type(month) ~= "number" or month < 1 or month > 12 or month ~= math.floor(month) then
        return nil, "Invalid month"
    end

    local max_days = safe_datetime.days_in_month(year, month)
    if type(day) ~= "number" or day < 1 or day > max_days or day ~= math.floor(day) then
        return nil, "Invalid day"
    end

    if type(hour) ~= "number" or hour < 0 or hour > 23 or hour ~= math.floor(hour) then
        return nil, "Invalid hour"
    end
    if type(minute) ~= "number" or minute < 0 or minute > 59 or minute ~= math.floor(minute) then
        return nil, "Invalid minute"
    end
    if type(second) ~= "number" or second < 0 or second > 59 or second ~= math.floor(second) then
        return nil, "Invalid second"
    end
    if type(millisecond) ~= "number" or millisecond < 0 or millisecond > 999 or millisecond ~= math.floor(millisecond) then
        return nil, "Invalid millisecond"
    end

    local self = setmetatable({}, DateTime)
    self.year = year
    self.month = month
    self.day = day
    self.hour = hour
    self.minute = minute
    self.second = second
    self.millisecond = millisecond
    self.timezone_offset = 0

    return self
end

--- Get current date and time.
--- @return DateTime datetime The current date and time
function safe_datetime.now()
    local time = os.date("*t")
    local dt = setmetatable({}, DateTime)
    dt.year = time.year
    dt.month = time.month
    dt.day = time.day
    dt.hour = time.hour
    dt.minute = time.min
    dt.second = time.sec
    dt.millisecond = 0
    dt.timezone_offset = 0
    return dt
end

--- Parse an ISO 8601 date string.
--- @param iso_string string The ISO 8601 string (e.g., "2025-01-17T14:30:00Z")
--- @return DateTime|nil datetime The parsed DateTime or nil if invalid
--- @return string|nil error Error message if parsing failed
function safe_datetime.parse_iso8601(iso_string)
    if type(iso_string) ~= "string" then
        return nil, "Input must be a string"
    end

    -- Basic format: YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD
    local year, month, day, hour, minute, second, tz =
        iso_string:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)T?(%d?%d?):?(%d?%d?):?(%d?%d?)(.*)$")

    if not year then
        year, month, day = iso_string:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)$")
        if not year then
            return nil, "Invalid ISO 8601 format"
        end
        hour, minute, second = "0", "0", "0"
    end

    year = tonumber(year)
    month = tonumber(month)
    day = tonumber(day)
    hour = tonumber(hour) or 0
    minute = tonumber(minute) or 0
    second = tonumber(second) or 0

    local dt, err = safe_datetime.new(year, month, day, hour, minute, second, 0)
    if not dt then
        return nil, err
    end

    -- Parse timezone
    if tz and #tz > 0 then
        if tz == "Z" then
            dt.timezone_offset = 0
        else
            local sign, th, tm = tz:match("^([+-])(%d%d):?(%d%d)$")
            if sign then
                local offset = tonumber(th) * 60 + tonumber(tm)
                dt.timezone_offset = sign == "+" and offset or -offset
            end
        end
    end

    return dt
end

--- Format DateTime as ISO 8601.
--- @return string The ISO 8601 formatted string
function DateTime:to_iso8601()
    local result = string.format("%04d-%02d-%02dT%02d:%02d:%02d",
        self.year, self.month, self.day, self.hour, self.minute, self.second)

    if self.timezone_offset == 0 then
        return result .. "Z"
    else
        local sign = self.timezone_offset >= 0 and "+" or "-"
        local abs_offset = math.abs(self.timezone_offset)
        local hours = math.floor(abs_offset / 60)
        local minutes = abs_offset % 60
        return result .. string.format("%s%02d:%02d", sign, hours, minutes)
    end
end

--- Format DateTime as a custom format string.
--- @param format_string string Format string (YYYY, MM, DD, HH, mm, ss)
--- @return string The formatted string
function DateTime:format(format_string)
    return format_string
        :gsub("YYYY", string.format("%04d", self.year))
        :gsub("MM", string.format("%02d", self.month))
        :gsub("DD", string.format("%02d", self.day))
        :gsub("HH", string.format("%02d", self.hour))
        :gsub("mm", string.format("%02d", self.minute))
        :gsub("ss", string.format("%02d", self.second))
end

--- Convert DateTime to Unix timestamp (seconds since epoch).
--- @return number timestamp The Unix timestamp
function DateTime:to_timestamp()
    return os.time({
        year = self.year,
        month = self.month,
        day = self.day,
        hour = self.hour,
        min = self.minute,
        sec = self.second
    }) - self.timezone_offset * 60
end

--- Create DateTime from Unix timestamp.
--- @param timestamp number The Unix timestamp
--- @return DateTime datetime The DateTime
function safe_datetime.from_timestamp(timestamp)
    local time = os.date("*t", timestamp)
    local dt = setmetatable({}, DateTime)
    dt.year = time.year
    dt.month = time.month
    dt.day = time.day
    dt.hour = time.hour
    dt.minute = time.min
    dt.second = time.sec
    dt.millisecond = 0
    dt.timezone_offset = 0
    return dt
end

--- Add duration to DateTime.
--- @param days number|nil Days to add
--- @param hours number|nil Hours to add
--- @param minutes number|nil Minutes to add
--- @param seconds number|nil Seconds to add
--- @return DateTime datetime The new DateTime
function DateTime:add(days, hours, minutes, seconds)
    local timestamp = self:to_timestamp()
    timestamp = timestamp + (seconds or 0)
    timestamp = timestamp + (minutes or 0) * 60
    timestamp = timestamp + (hours or 0) * 3600
    timestamp = timestamp + (days or 0) * 86400

    local result = safe_datetime.from_timestamp(timestamp)
    result.timezone_offset = self.timezone_offset
    return result
end

--- Calculate difference between two DateTimes in seconds.
--- @param other DateTime The other DateTime
--- @return number seconds Difference in seconds
function DateTime:diff_seconds(other)
    return self:to_timestamp() - other:to_timestamp()
end

--- Compare two DateTimes.
--- @param other DateTime The other DateTime
--- @return number -1 if self < other, 0 if equal, 1 if self > other
function DateTime:compare(other)
    local diff = self:diff_seconds(other)
    if diff < 0 then return -1
    elseif diff > 0 then return 1
    else return 0
    end
end

--- Check if DateTime is before another.
--- @param other DateTime The other DateTime
--- @return boolean is_before Whether self is before other
function DateTime:is_before(other)
    return self:compare(other) < 0
end

--- Check if DateTime is after another.
--- @param other DateTime The other DateTime
--- @return boolean is_after Whether self is after other
function DateTime:is_after(other)
    return self:compare(other) > 0
end

--- Get the day of week (1 = Monday, 7 = Sunday).
--- @return number day_of_week Day of the week
function DateTime:day_of_week()
    local timestamp = self:to_timestamp()
    local time = os.date("*t", timestamp)
    return time.wday == 1 and 7 or time.wday - 1
end

--- Get day of year (1-366).
--- @return number day_of_year Day of the year
function DateTime:day_of_year()
    local timestamp = self:to_timestamp()
    local time = os.date("*t", timestamp)
    return time.yday
end

safe_datetime.DateTime = DateTime

return safe_datetime
