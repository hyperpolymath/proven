-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe phone number validation following E.164.
--- @module proven.safe_phone

local safe_phone = {}

--- Country calling codes.
safe_phone.country_codes = {
    US = { code = 1, name = "United States / Canada", iso = "US" },
    CA = { code = 1, name = "Canada", iso = "CA" },
    RU = { code = 7, name = "Russia", iso = "RU" },
    EG = { code = 20, name = "Egypt", iso = "EG" },
    ZA = { code = 27, name = "South Africa", iso = "ZA" },
    GR = { code = 30, name = "Greece", iso = "GR" },
    NL = { code = 31, name = "Netherlands", iso = "NL" },
    BE = { code = 32, name = "Belgium", iso = "BE" },
    FR = { code = 33, name = "France", iso = "FR" },
    ES = { code = 34, name = "Spain", iso = "ES" },
    HU = { code = 36, name = "Hungary", iso = "HU" },
    IT = { code = 39, name = "Italy", iso = "IT" },
    RO = { code = 40, name = "Romania", iso = "RO" },
    CH = { code = 41, name = "Switzerland", iso = "CH" },
    AT = { code = 43, name = "Austria", iso = "AT" },
    UK = { code = 44, name = "United Kingdom", iso = "GB" },
    GB = { code = 44, name = "United Kingdom", iso = "GB" },
    DK = { code = 45, name = "Denmark", iso = "DK" },
    SE = { code = 46, name = "Sweden", iso = "SE" },
    NO = { code = 47, name = "Norway", iso = "NO" },
    PL = { code = 48, name = "Poland", iso = "PL" },
    DE = { code = 49, name = "Germany", iso = "DE" },
    MX = { code = 52, name = "Mexico", iso = "MX" },
    CU = { code = 53, name = "Cuba", iso = "CU" },
    AR = { code = 54, name = "Argentina", iso = "AR" },
    BR = { code = 55, name = "Brazil", iso = "BR" },
    CL = { code = 56, name = "Chile", iso = "CL" },
    CO = { code = 57, name = "Colombia", iso = "CO" },
    VE = { code = 58, name = "Venezuela", iso = "VE" },
    MY = { code = 60, name = "Malaysia", iso = "MY" },
    AU = { code = 61, name = "Australia", iso = "AU" },
    ID = { code = 62, name = "Indonesia", iso = "ID" },
    PH = { code = 63, name = "Philippines", iso = "PH" },
    NZ = { code = 64, name = "New Zealand", iso = "NZ" },
    SG = { code = 65, name = "Singapore", iso = "SG" },
    TH = { code = 66, name = "Thailand", iso = "TH" },
    JP = { code = 81, name = "Japan", iso = "JP" },
    KR = { code = 82, name = "South Korea", iso = "KR" },
    VN = { code = 84, name = "Vietnam", iso = "VN" },
    CN = { code = 86, name = "China", iso = "CN" },
    TR = { code = 90, name = "Turkey", iso = "TR" },
    IN = { code = 91, name = "India", iso = "IN" },
    PK = { code = 92, name = "Pakistan", iso = "PK" },
    AF = { code = 93, name = "Afghanistan", iso = "AF" },
    LK = { code = 94, name = "Sri Lanka", iso = "LK" },
    IR = { code = 98, name = "Iran", iso = "IR" },
}

-- Reverse lookup table: code number -> country info
local code_to_country = {}
for iso, info in pairs(safe_phone.country_codes) do
    if not code_to_country[info.code] then
        code_to_country[info.code] = { iso = iso, name = info.name }
    end
end

-- PhoneNumber metatable for OOP behavior.
local PhoneNumber = {}
PhoneNumber.__index = PhoneNumber

--- Create a new PhoneNumber object.
--- @param country_code number Country calling code
--- @param national_number string National number (digits only)
--- @return table|nil PhoneNumber object or nil on error
--- @return string|nil Error message if creation failed
function PhoneNumber.new(country_code, national_number)
    if type(country_code) ~= "number" then
        return nil, "country_code must be a number"
    end
    if type(national_number) ~= "string" then
        return nil, "national_number must be a string"
    end
    if not national_number:match("^%d+$") then
        return nil, "national_number must contain only digits"
    end
    if #national_number < 4 then
        return nil, "national_number too short"
    end
    if #national_number > 15 then
        return nil, "national_number too long"
    end

    local self = setmetatable({}, PhoneNumber)
    self._country_code = country_code
    self._national_number = national_number
    return self
end

--- Get the country code.
--- @return number
function PhoneNumber:country_code()
    return self._country_code
end

--- Get the national number.
--- @return string
function PhoneNumber:national_number()
    return self._national_number
end

--- Get country information.
--- @return table|nil Country info or nil if unknown
function PhoneNumber:country_info()
    return code_to_country[self._country_code]
end

--- Format in E.164 format (+15551234567).
--- @return string
function PhoneNumber:to_e164()
    return "+" .. self._country_code .. self._national_number
end

--- Format with spaces for readability.
--- @return string
function PhoneNumber:to_international()
    local cc = tostring(self._country_code)
    local nat = self._national_number
    local nat_len = #nat

    if nat_len <= 4 then
        return "+" .. cc .. " " .. nat
    elseif nat_len <= 7 then
        return "+" .. cc .. " " .. nat:sub(1, 3) .. " " .. nat:sub(4)
    elseif nat_len <= 10 then
        return "+" .. cc .. " " .. nat:sub(1, 3) .. " " .. nat:sub(4, 6) .. " " .. nat:sub(7)
    else
        return "+" .. cc .. " " .. nat
    end
end

--- Format with dashes for readability.
--- @return string
function PhoneNumber:to_national()
    local nat = self._national_number
    local nat_len = #nat

    if nat_len <= 4 then
        return nat
    elseif nat_len <= 7 then
        return nat:sub(1, 3) .. "-" .. nat:sub(4)
    elseif nat_len <= 10 then
        return "(" .. nat:sub(1, 3) .. ") " .. nat:sub(4, 6) .. "-" .. nat:sub(7)
    else
        return nat
    end
end

--- Get total digit count (country code + national number).
--- @return number
function PhoneNumber:digit_count()
    local cc_digits = #tostring(self._country_code)
    return cc_digits + #self._national_number
end

--- String representation.
function PhoneNumber:__tostring()
    return self:to_e164()
end

--- Equality comparison.
function PhoneNumber:__eq(other)
    if getmetatable(other) ~= PhoneNumber then
        return false
    end
    return self._country_code == other._country_code and
           self._national_number == other._national_number
end

-- Expose the PhoneNumber class
safe_phone.PhoneNumber = PhoneNumber

--- Parse phone number from string.
--- @param input string Phone number string
--- @return table|nil PhoneNumber object or nil on error
--- @return string|nil Error message if parsing failed
function safe_phone.parse(input)
    if type(input) ~= "string" then
        return nil, "Input must be a string"
    end

    local trimmed = input:match("^%s*(.-)%s*$")
    if #trimmed == 0 then
        return nil, "Empty input"
    end

    -- Extract digits only
    local digits = trimmed:gsub("%D", "")

    if #digits < 7 then
        return nil, "Phone number too short"
    end
    if #digits > 15 then
        return nil, "Phone number too long"
    end

    -- Try to parse country code (try 3-digit, then 2-digit, then 1-digit)
    local country_code = nil
    local national_start = nil

    for _, len in ipairs({3, 2, 1}) do
        if #digits >= len + 4 then  -- Need at least 4 digits for national number
            local potential_cc = tonumber(digits:sub(1, len))
            if code_to_country[potential_cc] then
                country_code = potential_cc
                national_start = len + 1
                break
            end
        end
    end

    if not country_code then
        return nil, "Unknown country code"
    end

    local national_number = digits:sub(national_start)
    if #national_number < 4 then
        return nil, "National number too short"
    end

    return PhoneNumber.new(country_code, national_number)
end

--- Check if string is a valid phone number.
--- @param input string Phone number string
--- @return boolean
function safe_phone.is_valid(input)
    local phone, _ = safe_phone.parse(input)
    return phone ~= nil
end

--- Format a phone number string to E.164.
--- @param input string Phone number string
--- @return string|nil E.164 formatted string or nil on error
--- @return string|nil Error message if formatting failed
function safe_phone.format_e164(input)
    local phone, err = safe_phone.parse(input)
    if not phone then
        return nil, err
    end
    return phone:to_e164()
end

--- Format a phone number string to international format.
--- @param input string Phone number string
--- @return string|nil International formatted string or nil on error
--- @return string|nil Error message if formatting failed
function safe_phone.format_international(input)
    local phone, err = safe_phone.parse(input)
    if not phone then
        return nil, err
    end
    return phone:to_international()
end

--- Get country code from number.
--- @param calling_code number Country calling code
--- @return table|nil Country info or nil if unknown
function safe_phone.get_country_by_code(calling_code)
    return code_to_country[calling_code]
end

--- Get country code from ISO code.
--- @param iso_code string ISO country code (e.g., "US", "GB")
--- @return table|nil Country info or nil if unknown
function safe_phone.get_country_by_iso(iso_code)
    if type(iso_code) ~= "string" then
        return nil
    end
    return safe_phone.country_codes[iso_code:upper()]
end

return safe_phone
