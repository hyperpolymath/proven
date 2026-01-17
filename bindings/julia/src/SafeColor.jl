# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeColor

Safe color operations with validated color space conversions.
"""
module SafeColor

export Rgb, Rgba, Hsl, Hsla
export parse_hex, parse_rgb, parse_hsl
export rgb_to_hex, rgba_to_hex, rgb_to_hsl, hsl_to_rgb
export format_rgb, format_rgba, format_hsl, format_hsla
export blend, lighten, darken, saturate, desaturate
export luminance, contrast_ratio, is_light, is_dark

"""
    Rgb

RGB color with values 0-255.
"""
struct Rgb
    r::UInt8
    g::UInt8
    b::UInt8
end

"""
    Rgba

RGBA color with alpha 0.0-1.0.
"""
struct Rgba
    r::UInt8
    g::UInt8
    b::UInt8
    a::Float64

    function Rgba(r::UInt8, g::UInt8, b::UInt8, a::Float64)
        new(r, g, b, clamp(a, 0.0, 1.0))
    end
end

Rgba(r, g, b, a) = Rgba(UInt8(r), UInt8(g), UInt8(b), Float64(a))
Rgba(rgb::Rgb, a::Float64=1.0) = Rgba(rgb.r, rgb.g, rgb.b, a)

"""
    Hsl

HSL color with h: 0-360, s: 0-100, l: 0-100.
"""
struct Hsl
    h::Float64  # 0-360
    s::Float64  # 0-100
    l::Float64  # 0-100

    function Hsl(h::Float64, s::Float64, l::Float64)
        new(mod(h, 360.0), clamp(s, 0.0, 100.0), clamp(l, 0.0, 100.0))
    end
end

Hsl(h, s, l) = Hsl(Float64(h), Float64(s), Float64(l))

"""
    Hsla

HSLA color with alpha 0.0-1.0.
"""
struct Hsla
    h::Float64
    s::Float64
    l::Float64
    a::Float64

    function Hsla(h::Float64, s::Float64, l::Float64, a::Float64)
        new(mod(h, 360.0), clamp(s, 0.0, 100.0), clamp(l, 0.0, 100.0), clamp(a, 0.0, 1.0))
    end
end

Hsla(h, s, l, a) = Hsla(Float64(h), Float64(s), Float64(l), Float64(a))
Hsla(hsl::Hsl, a::Float64=1.0) = Hsla(hsl.h, hsl.s, hsl.l, a)

"""
    parse_hex(s::AbstractString) -> Union{Rgb, Nothing}

Parse hex color string (#RGB, #RRGGBB).
"""
function parse_hex(s::AbstractString)::Union{Rgb, Nothing}
    str = strip(s)
    startswith(str, "#") && (str = str[2:end])

    if length(str) == 3
        # Short form #RGB
        r = tryparse(UInt8, string(str[1], str[1]); base=16)
        g = tryparse(UInt8, string(str[2], str[2]); base=16)
        b = tryparse(UInt8, string(str[3], str[3]); base=16)
    elseif length(str) == 6
        # Long form #RRGGBB
        r = tryparse(UInt8, str[1:2]; base=16)
        g = tryparse(UInt8, str[3:4]; base=16)
        b = tryparse(UInt8, str[5:6]; base=16)
    else
        return nothing
    end

    (r === nothing || g === nothing || b === nothing) && return nothing
    Rgb(r, g, b)
end

"""
    parse_rgb(s::AbstractString) -> Union{Rgb, Nothing}

Parse CSS rgb() format.
"""
function parse_rgb(s::AbstractString)::Union{Rgb, Nothing}
    str = lowercase(strip(s))

    m = match(r"^rgb\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)$", str)
    m === nothing && return nothing

    r = tryparse(Int, m.captures[1])
    g = tryparse(Int, m.captures[2])
    b = tryparse(Int, m.captures[3])

    (r === nothing || g === nothing || b === nothing) && return nothing
    (r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255) && return nothing

    Rgb(UInt8(r), UInt8(g), UInt8(b))
end

"""
    parse_hsl(s::AbstractString) -> Union{Hsl, Nothing}

Parse CSS hsl() format.
"""
function parse_hsl(s::AbstractString)::Union{Hsl, Nothing}
    str = lowercase(strip(s))

    m = match(r"^hsl\s*\(\s*([\d.]+)\s*,\s*([\d.]+)%?\s*,\s*([\d.]+)%?\s*\)$", str)
    m === nothing && return nothing

    h = tryparse(Float64, m.captures[1])
    s_val = tryparse(Float64, m.captures[2])
    l = tryparse(Float64, m.captures[3])

    (h === nothing || s_val === nothing || l === nothing) && return nothing

    Hsl(h, s_val, l)
end

"""
    rgb_to_hex(c::Rgb) -> String

Convert RGB to hex string (#RRGGBB).
"""
function rgb_to_hex(c::Rgb)::String
    @sprintf("#%02x%02x%02x", c.r, c.g, c.b)
end

"""
    rgba_to_hex(c::Rgba) -> String

Convert RGBA to hex string (#RRGGBBAA).
"""
function rgba_to_hex(c::Rgba)::String
    alpha_byte = round(UInt8, c.a * 255)
    @sprintf("#%02x%02x%02x%02x", c.r, c.g, c.b, alpha_byte)
end

"""
    rgb_to_hsl(c::Rgb) -> Hsl

Convert RGB to HSL.
"""
function rgb_to_hsl(c::Rgb)::Hsl
    r = c.r / 255.0
    g = c.g / 255.0
    b = c.b / 255.0

    max_c = max(r, g, b)
    min_c = min(r, g, b)
    delta = max_c - min_c

    l = (max_c + min_c) / 2.0

    if delta == 0.0
        return Hsl(0.0, 0.0, l * 100.0)
    end

    s = if l < 0.5
        delta / (max_c + min_c)
    else
        delta / (2.0 - max_c - min_c)
    end

    h = if max_c == r
        60.0 * (((g - b) / delta) % 6)
    elseif max_c == g
        60.0 * (((b - r) / delta) + 2)
    else
        60.0 * (((r - g) / delta) + 4)
    end

    h < 0 && (h += 360.0)

    Hsl(h, s * 100.0, l * 100.0)
end

"""
    hsl_to_rgb(c::Hsl) -> Rgb

Convert HSL to RGB.
"""
function hsl_to_rgb(c::Hsl)::Rgb
    h = c.h / 360.0
    s = c.s / 100.0
    l = c.l / 100.0

    if s == 0.0
        v = round(UInt8, l * 255)
        return Rgb(v, v, v)
    end

    q = if l < 0.5
        l * (1.0 + s)
    else
        l + s - l * s
    end
    p = 2.0 * l - q

    function hue_to_rgb(t::Float64)::Float64
        t < 0 && (t += 1.0)
        t > 1 && (t -= 1.0)

        if t < 1/6
            p + (q - p) * 6.0 * t
        elseif t < 1/2
            q
        elseif t < 2/3
            p + (q - p) * (2/3 - t) * 6.0
        else
            p
        end
    end

    r = round(UInt8, hue_to_rgb(h + 1/3) * 255)
    g = round(UInt8, hue_to_rgb(h) * 255)
    b = round(UInt8, hue_to_rgb(h - 1/3) * 255)

    Rgb(r, g, b)
end

"""
    format_rgb(c::Rgb) -> String

Format as CSS rgb().
"""
format_rgb(c::Rgb)::String = "rgb($(c.r), $(c.g), $(c.b))"

"""
    format_rgba(c::Rgba) -> String

Format as CSS rgba().
"""
format_rgba(c::Rgba)::String = "rgba($(c.r), $(c.g), $(c.b), $(round(c.a, digits=2)))"

"""
    format_hsl(c::Hsl) -> String

Format as CSS hsl().
"""
format_hsl(c::Hsl)::String = "hsl($(round(c.h, digits=1)), $(round(c.s, digits=1))%, $(round(c.l, digits=1))%)"

"""
    format_hsla(c::Hsla) -> String

Format as CSS hsla().
"""
format_hsla(c::Hsla)::String = "hsla($(round(c.h, digits=1)), $(round(c.s, digits=1))%, $(round(c.l, digits=1))%, $(round(c.a, digits=2)))"

"""
    blend(c1::Rgb, c2::Rgb, factor::Float64) -> Rgb

Blend two colors. Factor 0.0 = c1, 1.0 = c2.
"""
function blend(c1::Rgb, c2::Rgb, factor::Float64)::Rgb
    f = clamp(factor, 0.0, 1.0)
    r = round(UInt8, c1.r * (1.0 - f) + c2.r * f)
    g = round(UInt8, c1.g * (1.0 - f) + c2.g * f)
    b = round(UInt8, c1.b * (1.0 - f) + c2.b * f)
    Rgb(r, g, b)
end

"""
    lighten(c::Rgb, amount::Float64) -> Rgb

Lighten a color by amount (0.0-1.0).
"""
function lighten(c::Rgb, amount::Float64)::Rgb
    hsl = rgb_to_hsl(c)
    new_l = min(100.0, hsl.l + amount * 100.0)
    hsl_to_rgb(Hsl(hsl.h, hsl.s, new_l))
end

"""
    darken(c::Rgb, amount::Float64) -> Rgb

Darken a color by amount (0.0-1.0).
"""
function darken(c::Rgb, amount::Float64)::Rgb
    hsl = rgb_to_hsl(c)
    new_l = max(0.0, hsl.l - amount * 100.0)
    hsl_to_rgb(Hsl(hsl.h, hsl.s, new_l))
end

"""
    saturate(c::Rgb, amount::Float64) -> Rgb

Increase saturation by amount (0.0-1.0).
"""
function saturate(c::Rgb, amount::Float64)::Rgb
    hsl = rgb_to_hsl(c)
    new_s = min(100.0, hsl.s + amount * 100.0)
    hsl_to_rgb(Hsl(hsl.h, new_s, hsl.l))
end

"""
    desaturate(c::Rgb, amount::Float64) -> Rgb

Decrease saturation by amount (0.0-1.0).
"""
function desaturate(c::Rgb, amount::Float64)::Rgb
    hsl = rgb_to_hsl(c)
    new_s = max(0.0, hsl.s - amount * 100.0)
    hsl_to_rgb(Hsl(hsl.h, new_s, hsl.l))
end

"""
    luminance(c::Rgb) -> Float64

Calculate relative luminance (0.0-1.0) per WCAG 2.1.
"""
function luminance(c::Rgb)::Float64
    function channel_luminance(v::UInt8)::Float64
        val = v / 255.0
        if val <= 0.03928
            val / 12.92
        else
            ((val + 0.055) / 1.055)^2.4
        end
    end

    0.2126 * channel_luminance(c.r) +
    0.7152 * channel_luminance(c.g) +
    0.0722 * channel_luminance(c.b)
end

"""
    contrast_ratio(c1::Rgb, c2::Rgb) -> Float64

Calculate WCAG contrast ratio between two colors.
"""
function contrast_ratio(c1::Rgb, c2::Rgb)::Float64
    l1 = luminance(c1)
    l2 = luminance(c2)

    lighter = max(l1, l2)
    darker = min(l1, l2)

    (lighter + 0.05) / (darker + 0.05)
end

"""
    is_light(c::Rgb) -> Bool

Check if color is light (luminance > 0.5).
"""
is_light(c::Rgb)::Bool = luminance(c) > 0.5

"""
    is_dark(c::Rgb) -> Bool

Check if color is dark (luminance <= 0.5).
"""
is_dark(c::Rgb)::Bool = luminance(c) <= 0.5

using Printf

# Display
Base.show(io::IO, c::Rgb) = print(io, rgb_to_hex(c))
Base.show(io::IO, c::Rgba) = print(io, format_rgba(c))
Base.show(io::IO, c::Hsl) = print(io, format_hsl(c))
Base.show(io::IO, c::Hsla) = print(io, format_hsla(c))

end # module
