-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe currency operations with type-safe monetary values.
--- @module proven.safe_currency

local safe_currency = {}

--- ISO 4217 currency codes with metadata.
safe_currency.currency_codes = {
    USD = { code = "USD", name = "US Dollar", symbol = "$", decimals = 2 },
    EUR = { code = "EUR", name = "Euro", symbol = "€", decimals = 2 },
    GBP = { code = "GBP", name = "British Pound", symbol = "£", decimals = 2 },
    JPY = { code = "JPY", name = "Japanese Yen", symbol = "¥", decimals = 0 },
    CHF = { code = "CHF", name = "Swiss Franc", symbol = "Fr", decimals = 2 },
    CAD = { code = "CAD", name = "Canadian Dollar", symbol = "$", decimals = 2 },
    AUD = { code = "AUD", name = "Australian Dollar", symbol = "$", decimals = 2 },
    NZD = { code = "NZD", name = "New Zealand Dollar", symbol = "$", decimals = 2 },
    CNY = { code = "CNY", name = "Chinese Yuan", symbol = "¥", decimals = 2 },
    INR = { code = "INR", name = "Indian Rupee", symbol = "₹", decimals = 2 },
    BRL = { code = "BRL", name = "Brazilian Real", symbol = "R$", decimals = 2 },
    MXN = { code = "MXN", name = "Mexican Peso", symbol = "$", decimals = 2 },
    KRW = { code = "KRW", name = "South Korean Won", symbol = "₩", decimals = 0 },
    SGD = { code = "SGD", name = "Singapore Dollar", symbol = "$", decimals = 2 },
    HKD = { code = "HKD", name = "Hong Kong Dollar", symbol = "$", decimals = 2 },
    SEK = { code = "SEK", name = "Swedish Krona", symbol = "kr", decimals = 2 },
    NOK = { code = "NOK", name = "Norwegian Krone", symbol = "kr", decimals = 2 },
    DKK = { code = "DKK", name = "Danish Krone", symbol = "kr", decimals = 2 },
    PLN = { code = "PLN", name = "Polish Zloty", symbol = "zł", decimals = 2 },
    RUB = { code = "RUB", name = "Russian Ruble", symbol = "₽", decimals = 2 },
    ZAR = { code = "ZAR", name = "South African Rand", symbol = "R", decimals = 2 },
    TRY = { code = "TRY", name = "Turkish Lira", symbol = "₺", decimals = 2 },
    THB = { code = "THB", name = "Thai Baht", symbol = "฿", decimals = 2 },
    MYR = { code = "MYR", name = "Malaysian Ringgit", symbol = "RM", decimals = 2 },
    IDR = { code = "IDR", name = "Indonesian Rupiah", symbol = "Rp", decimals = 2 },
    PHP = { code = "PHP", name = "Philippine Peso", symbol = "₱", decimals = 2 },
    VND = { code = "VND", name = "Vietnamese Dong", symbol = "₫", decimals = 0 },
    AED = { code = "AED", name = "UAE Dirham", symbol = "د.إ", decimals = 2 },
    SAR = { code = "SAR", name = "Saudi Riyal", symbol = "﷼", decimals = 2 },
    ILS = { code = "ILS", name = "Israeli Shekel", symbol = "₪", decimals = 2 },
    CZK = { code = "CZK", name = "Czech Koruna", symbol = "Kč", decimals = 2 },
    HUF = { code = "HUF", name = "Hungarian Forint", symbol = "Ft", decimals = 2 },
    RON = { code = "RON", name = "Romanian Leu", symbol = "lei", decimals = 2 },
    BGN = { code = "BGN", name = "Bulgarian Lev", symbol = "лв", decimals = 2 },
    HRK = { code = "HRK", name = "Croatian Kuna", symbol = "kn", decimals = 2 },
    ISK = { code = "ISK", name = "Icelandic Krona", symbol = "kr", decimals = 0 },
    CLP = { code = "CLP", name = "Chilean Peso", symbol = "$", decimals = 0 },
    COP = { code = "COP", name = "Colombian Peso", symbol = "$", decimals = 2 },
    PEN = { code = "PEN", name = "Peruvian Sol", symbol = "S/", decimals = 2 },
    ARS = { code = "ARS", name = "Argentine Peso", symbol = "$", decimals = 2 },
    BTC = { code = "BTC", name = "Bitcoin", symbol = "₿", decimals = 8 },
    ETH = { code = "ETH", name = "Ethereum", symbol = "Ξ", decimals = 8 },
}

-- Money metatable for OOP behavior.
local Money = {}
Money.__index = Money

--- Create a new Money object from minor units.
--- @param minor_units number Amount in minor units (cents, satoshis, etc.)
--- @param currency_code string ISO 4217 currency code
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if creation failed
function Money.new(minor_units, currency_code)
    if type(minor_units) ~= "number" then
        return nil, "minor_units must be a number"
    end
    if type(currency_code) ~= "string" then
        return nil, "currency_code must be a string"
    end

    local currency_info = safe_currency.currency_codes[currency_code:upper()]
    if not currency_info then
        return nil, "Unknown currency code: " .. currency_code
    end

    local self = setmetatable({}, Money)
    self._minor_units = math.floor(minor_units)
    self._currency = currency_info
    return self
end

--- Create a Money object from major units.
--- @param major_units number Amount in major units (dollars, euros, etc.)
--- @param currency_code string ISO 4217 currency code
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if creation failed
function Money.from_major(major_units, currency_code)
    if type(major_units) ~= "number" then
        return nil, "major_units must be a number"
    end

    local currency_info = safe_currency.currency_codes[currency_code:upper()]
    if not currency_info then
        return nil, "Unknown currency code: " .. currency_code
    end

    local multiplier = 10 ^ currency_info.decimals
    local minor_units = math.floor(major_units * multiplier + 0.5)

    return Money.new(minor_units, currency_code)
end

--- Create a zero Money object.
--- @param currency_code string ISO 4217 currency code
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if creation failed
function Money.zero(currency_code)
    return Money.new(0, currency_code)
end

--- Get the currency code.
--- @return string
function Money:currency_code()
    return self._currency.code
end

--- Get the currency info.
--- @return table
function Money:currency()
    return self._currency
end

--- Get minor units (cents, satoshis, etc.).
--- @return number
function Money:minor()
    return self._minor_units
end

--- Get major units (truncated).
--- @return number
function Money:major()
    local divisor = 10 ^ self._currency.decimals
    return math.floor(self._minor_units / divisor)
end

--- Add two monetary values.
--- @param other table Money object to add
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if operation failed
function Money:add(other)
    if getmetatable(other) ~= Money then
        return nil, "Cannot add non-Money value"
    end
    if self._currency.code ~= other._currency.code then
        return nil, "Currency mismatch: " .. self._currency.code .. " vs " .. other._currency.code
    end
    return Money.new(self._minor_units + other._minor_units, self._currency.code)
end

--- Subtract a monetary value.
--- @param other table Money object to subtract
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if operation failed
function Money:sub(other)
    if getmetatable(other) ~= Money then
        return nil, "Cannot subtract non-Money value"
    end
    if self._currency.code ~= other._currency.code then
        return nil, "Currency mismatch: " .. self._currency.code .. " vs " .. other._currency.code
    end
    return Money.new(self._minor_units - other._minor_units, self._currency.code)
end

--- Multiply by a scalar.
--- @param scalar number The multiplier
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if operation failed
function Money:mul(scalar)
    if type(scalar) ~= "number" then
        return nil, "Scalar must be a number"
    end
    return Money.new(math.floor(self._minor_units * scalar), self._currency.code)
end

--- Divide by a scalar.
--- @param scalar number The divisor
--- @return table|nil Money object or nil on error
--- @return string|nil Error message if operation failed
function Money:div(scalar)
    if type(scalar) ~= "number" then
        return nil, "Scalar must be a number"
    end
    if scalar == 0 then
        return nil, "Division by zero"
    end
    return Money.new(math.floor(self._minor_units / scalar), self._currency.code)
end

--- Check if zero.
--- @return boolean
function Money:is_zero()
    return self._minor_units == 0
end

--- Check if positive.
--- @return boolean
function Money:is_positive()
    return self._minor_units > 0
end

--- Check if negative.
--- @return boolean
function Money:is_negative()
    return self._minor_units < 0
end

--- Get absolute value.
--- @return table Money object
function Money:abs()
    return Money.new(math.abs(self._minor_units), self._currency.code)
end

--- Format as string.
--- @return string
function Money:format()
    local decimals = self._currency.decimals
    local divisor = 10 ^ decimals
    local abs_units = math.abs(self._minor_units)
    local major_part = math.floor(abs_units / divisor)
    local minor_part = abs_units % divisor
    local sign = self._minor_units < 0 and "-" or ""

    if decimals == 0 then
        return sign .. self._currency.symbol .. major_part
    else
        return string.format("%s%s%d.%0" .. decimals .. "d",
            sign, self._currency.symbol, major_part, minor_part)
    end
end

--- String representation.
function Money:__tostring()
    return self:format()
end

--- Equality comparison.
function Money:__eq(other)
    if getmetatable(other) ~= Money then
        return false
    end
    return self._minor_units == other._minor_units and
           self._currency.code == other._currency.code
end

--- Less than comparison (same currency only).
function Money:__lt(other)
    if getmetatable(other) ~= Money then
        return false
    end
    if self._currency.code ~= other._currency.code then
        return false
    end
    return self._minor_units < other._minor_units
end

--- Less than or equal comparison (same currency only).
function Money:__le(other)
    if getmetatable(other) ~= Money then
        return false
    end
    if self._currency.code ~= other._currency.code then
        return false
    end
    return self._minor_units <= other._minor_units
end

-- Expose the Money class
safe_currency.Money = Money

--- Parse currency code from string.
--- @param code_string string Currency code string
--- @return table|nil Currency info or nil if invalid
--- @return string|nil Error message if parsing failed
function safe_currency.parse_code(code_string)
    if type(code_string) ~= "string" then
        return nil, "Currency code must be a string"
    end
    local currency_info = safe_currency.currency_codes[code_string:upper()]
    if not currency_info then
        return nil, "Unknown currency code: " .. code_string
    end
    return currency_info
end

--- Check if valid currency code.
--- @param code_string string Currency code string
--- @return boolean
function safe_currency.is_valid_code(code_string)
    if type(code_string) ~= "string" then
        return false
    end
    return safe_currency.currency_codes[code_string:upper()] ~= nil
end

--- Get all available currency codes.
--- @return table Array of currency code strings
function safe_currency.available_codes()
    local codes = {}
    for code, _ in pairs(safe_currency.currency_codes) do
        table.insert(codes, code)
    end
    table.sort(codes)
    return codes
end

return safe_currency
