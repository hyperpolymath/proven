-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe color manipulation and conversion.
--- @module proven.safe_color

local safe_color = {}

--- Color structure.
--- @class Color
--- @field r number Red component (0-255)
--- @field g number Green component (0-255)
--- @field b number Blue component (0-255)
--- @field a number Alpha component (0-255)

local Color = {}
Color.__index = Color

--- Clamp a value to 0-255.
--- @param value number The value to clamp
--- @return number clamped The clamped value
local function clamp255(value)
    return math.max(0, math.min(255, math.floor(value + 0.5)))
end

--- Clamp a value to 0-1.
--- @param value number The value to clamp
--- @return number clamped The clamped value
local function clamp01(value)
    return math.max(0, math.min(1, value))
end

--- Create a new Color from RGB values.
--- @param r number Red (0-255)
--- @param g number Green (0-255)
--- @param b number Blue (0-255)
--- @param a number|nil Alpha (0-255), default 255
--- @return Color color The new color
function safe_color.rgb(r, g, b, a)
    local self = setmetatable({}, Color)
    self.r = clamp255(r or 0)
    self.g = clamp255(g or 0)
    self.b = clamp255(b or 0)
    self.a = clamp255(a or 255)
    return self
end

--- Create a new Color from normalized RGB values (0-1).
--- @param r number Red (0-1)
--- @param g number Green (0-1)
--- @param b number Blue (0-1)
--- @param a number|nil Alpha (0-1), default 1
--- @return Color color The new color
function safe_color.rgb_float(r, g, b, a)
    return safe_color.rgb(
        clamp01(r or 0) * 255,
        clamp01(g or 0) * 255,
        clamp01(b or 0) * 255,
        clamp01(a or 1) * 255
    )
end

--- Parse a hex color string.
--- @param hex_string string Hex color (e.g., "#FF0000", "FF0000", "#F00")
--- @return Color|nil color The parsed color or nil if invalid
--- @return string|nil error Error message if parsing failed
function safe_color.from_hex(hex_string)
    if type(hex_string) ~= "string" then
        return nil, "Input must be a string"
    end

    -- Remove # if present
    local hex = hex_string:match("^#?(%x+)$")
    if not hex then
        return nil, "Invalid hex format"
    end

    local r, g, b, a

    if #hex == 3 then
        r = tonumber(hex:sub(1, 1), 16) * 17
        g = tonumber(hex:sub(2, 2), 16) * 17
        b = tonumber(hex:sub(3, 3), 16) * 17
        a = 255
    elseif #hex == 4 then
        r = tonumber(hex:sub(1, 1), 16) * 17
        g = tonumber(hex:sub(2, 2), 16) * 17
        b = tonumber(hex:sub(3, 3), 16) * 17
        a = tonumber(hex:sub(4, 4), 16) * 17
    elseif #hex == 6 then
        r = tonumber(hex:sub(1, 2), 16)
        g = tonumber(hex:sub(3, 4), 16)
        b = tonumber(hex:sub(5, 6), 16)
        a = 255
    elseif #hex == 8 then
        r = tonumber(hex:sub(1, 2), 16)
        g = tonumber(hex:sub(3, 4), 16)
        b = tonumber(hex:sub(5, 6), 16)
        a = tonumber(hex:sub(7, 8), 16)
    else
        return nil, "Invalid hex length"
    end

    if not r or not g or not b or not a then
        return nil, "Invalid hex values"
    end

    return safe_color.rgb(r, g, b, a)
end

--- Convert Color to hex string.
--- @param include_alpha boolean|nil Include alpha in output
--- @return string hex The hex string (e.g., "#FF0000")
function Color:to_hex(include_alpha)
    if include_alpha then
        return string.format("#%02X%02X%02X%02X", self.r, self.g, self.b, self.a)
    else
        return string.format("#%02X%02X%02X", self.r, self.g, self.b)
    end
end

--- Convert Color to HSL.
--- @return number h Hue (0-360)
--- @return number s Saturation (0-1)
--- @return number l Lightness (0-1)
function Color:to_hsl()
    local r = self.r / 255
    local g = self.g / 255
    local b = self.b / 255

    local max_val = math.max(r, g, b)
    local min_val = math.min(r, g, b)
    local l = (max_val + min_val) / 2

    local h, s

    if max_val == min_val then
        h = 0
        s = 0
    else
        local d = max_val - min_val
        s = l > 0.5 and d / (2 - max_val - min_val) or d / (max_val + min_val)

        if max_val == r then
            h = ((g - b) / d + (g < b and 6 or 0)) / 6
        elseif max_val == g then
            h = ((b - r) / d + 2) / 6
        else
            h = ((r - g) / d + 4) / 6
        end
    end

    return h * 360, s, l
end

--- Create Color from HSL.
--- @param h number Hue (0-360)
--- @param s number Saturation (0-1)
--- @param l number Lightness (0-1)
--- @param a number|nil Alpha (0-255), default 255
--- @return Color color The new color
function safe_color.from_hsl(h, s, l, a)
    h = ((h % 360) + 360) % 360 / 360
    s = clamp01(s)
    l = clamp01(l)

    local function hue_to_rgb(p, q, t)
        if t < 0 then t = t + 1 end
        if t > 1 then t = t - 1 end
        if t < 1/6 then return p + (q - p) * 6 * t end
        if t < 1/2 then return q end
        if t < 2/3 then return p + (q - p) * (2/3 - t) * 6 end
        return p
    end

    local r, g, b

    if s == 0 then
        r, g, b = l, l, l
    else
        local q = l < 0.5 and l * (1 + s) or l + s - l * s
        local p = 2 * l - q
        r = hue_to_rgb(p, q, h + 1/3)
        g = hue_to_rgb(p, q, h)
        b = hue_to_rgb(p, q, h - 1/3)
    end

    return safe_color.rgb(r * 255, g * 255, b * 255, a or 255)
end

--- Convert Color to HSV.
--- @return number h Hue (0-360)
--- @return number s Saturation (0-1)
--- @return number v Value (0-1)
function Color:to_hsv()
    local r = self.r / 255
    local g = self.g / 255
    local b = self.b / 255

    local max_val = math.max(r, g, b)
    local min_val = math.min(r, g, b)
    local v = max_val
    local d = max_val - min_val

    local h, s

    if max_val == 0 then
        s = 0
    else
        s = d / max_val
    end

    if max_val == min_val then
        h = 0
    else
        if max_val == r then
            h = ((g - b) / d + (g < b and 6 or 0)) / 6
        elseif max_val == g then
            h = ((b - r) / d + 2) / 6
        else
            h = ((r - g) / d + 4) / 6
        end
    end

    return h * 360, s, v
end

--- Create Color from HSV.
--- @param h number Hue (0-360)
--- @param s number Saturation (0-1)
--- @param v number Value (0-1)
--- @param a number|nil Alpha (0-255), default 255
--- @return Color color The new color
function safe_color.from_hsv(h, s, v, a)
    h = ((h % 360) + 360) % 360 / 60
    s = clamp01(s)
    v = clamp01(v)

    local c = v * s
    local x = c * (1 - math.abs(h % 2 - 1))
    local m = v - c

    local r, g, b

    if h < 1 then
        r, g, b = c, x, 0
    elseif h < 2 then
        r, g, b = x, c, 0
    elseif h < 3 then
        r, g, b = 0, c, x
    elseif h < 4 then
        r, g, b = 0, x, c
    elseif h < 5 then
        r, g, b = x, 0, c
    else
        r, g, b = c, 0, x
    end

    return safe_color.rgb((r + m) * 255, (g + m) * 255, (b + m) * 255, a or 255)
end

--- Lighten a color.
--- @param amount number Amount to lighten (0-1)
--- @return Color color The lightened color
function Color:lighten(amount)
    local h, s, l = self:to_hsl()
    return safe_color.from_hsl(h, s, l + amount * (1 - l), self.a)
end

--- Darken a color.
--- @param amount number Amount to darken (0-1)
--- @return Color color The darkened color
function Color:darken(amount)
    local h, s, l = self:to_hsl()
    return safe_color.from_hsl(h, s, l * (1 - amount), self.a)
end

--- Saturate a color.
--- @param amount number Amount to saturate (0-1)
--- @return Color color The saturated color
function Color:saturate(amount)
    local h, s, l = self:to_hsl()
    return safe_color.from_hsl(h, s + amount * (1 - s), l, self.a)
end

--- Desaturate a color.
--- @param amount number Amount to desaturate (0-1)
--- @return Color color The desaturated color
function Color:desaturate(amount)
    local h, s, l = self:to_hsl()
    return safe_color.from_hsl(h, s * (1 - amount), l, self.a)
end

--- Get complementary color.
--- @return Color color The complementary color
function Color:complement()
    local h, s, l = self:to_hsl()
    return safe_color.from_hsl((h + 180) % 360, s, l, self.a)
end

--- Mix two colors.
--- @param other Color The other color
--- @param amount number|nil Mix amount (0-1), default 0.5
--- @return Color color The mixed color
function Color:mix(other, amount)
    amount = clamp01(amount or 0.5)
    local inv = 1 - amount

    return safe_color.rgb(
        self.r * inv + other.r * amount,
        self.g * inv + other.g * amount,
        self.b * inv + other.b * amount,
        self.a * inv + other.a * amount
    )
end

--- Get grayscale version of color.
--- @return Color color The grayscale color
function Color:grayscale()
    local gray = self.r * 0.299 + self.g * 0.587 + self.b * 0.114
    return safe_color.rgb(gray, gray, gray, self.a)
end

--- Get color luminance (perceived brightness).
--- @return number luminance Luminance (0-1)
function Color:luminance()
    local r = self.r / 255
    local g = self.g / 255
    local b = self.b / 255

    -- Apply gamma correction
    r = r <= 0.03928 and r / 12.92 or ((r + 0.055) / 1.055) ^ 2.4
    g = g <= 0.03928 and g / 12.92 or ((g + 0.055) / 1.055) ^ 2.4
    b = b <= 0.03928 and b / 12.92 or ((b + 0.055) / 1.055) ^ 2.4

    return 0.2126 * r + 0.7152 * g + 0.0722 * b
end

--- Calculate contrast ratio between two colors.
--- @param other Color The other color
--- @return number ratio The contrast ratio (1-21)
function Color:contrast_ratio(other)
    local l1 = self:luminance()
    local l2 = other:luminance()

    local lighter = math.max(l1, l2)
    local darker = math.min(l1, l2)

    return (lighter + 0.05) / (darker + 0.05)
end

--- Check if color is readable against a background (WCAG AA).
--- @param background Color The background color
--- @param large_text boolean|nil Whether text is large (>= 18pt)
--- @return boolean readable Whether the contrast is sufficient
function Color:is_readable(background, large_text)
    local ratio = self:contrast_ratio(background)
    local min_ratio = large_text and 3 or 4.5
    return ratio >= min_ratio
end

--- Convert to CSS rgb() string.
--- @return string css The CSS color string
function Color:to_css()
    if self.a < 255 then
        return string.format("rgba(%d, %d, %d, %.3f)", self.r, self.g, self.b, self.a / 255)
    else
        return string.format("rgb(%d, %d, %d)", self.r, self.g, self.b)
    end
end

safe_color.Color = Color

-- Preset colors
safe_color.WHITE = safe_color.rgb(255, 255, 255)
safe_color.BLACK = safe_color.rgb(0, 0, 0)
safe_color.RED = safe_color.rgb(255, 0, 0)
safe_color.GREEN = safe_color.rgb(0, 255, 0)
safe_color.BLUE = safe_color.rgb(0, 0, 255)
safe_color.YELLOW = safe_color.rgb(255, 255, 0)
safe_color.CYAN = safe_color.rgb(0, 255, 255)
safe_color.MAGENTA = safe_color.rgb(255, 0, 255)
safe_color.TRANSPARENT = safe_color.rgb(0, 0, 0, 0)

return safe_color
