#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_color.sh - Safe color parsing and manipulation for POSIX shells
# Source this file: . /path/to/safe_color.sh

# Parsed color components (0-255)
COLOR_R=""
COLOR_G=""
COLOR_B=""
COLOR_A=""

# Parse hex color (#RGB, #RRGGBB, #RRGGBBAA)
# Usage: color_parse_hex "#ff5500"
# Sets: COLOR_R, COLOR_G, COLOR_B, COLOR_A
# Returns: 0 on success, 1 on error
color_parse_hex() {
    input="$1"
    COLOR_R=""
    COLOR_G=""
    COLOR_B=""
    COLOR_A="255"
    PROVEN_ERROR=""

    # Strip # prefix
    hex="${input#\#}"

    # Validate hex characters
    case "$hex" in
        *[!0-9a-fA-F]*)
            PROVEN_ERROR="invalid_hex_characters"
            return 1
            ;;
    esac

    len=${#hex}

    case $len in
        3)
            # #RGB -> #RRGGBB
            r="${hex%??}"
            g="${hex#?}"
            g="${g%?}"
            b="${hex#??}"
            COLOR_R=$((16#$r$r))
            COLOR_G=$((16#$g$g))
            COLOR_B=$((16#$b$b))
            ;;
        4)
            # #RGBA -> #RRGGBBAA
            r="${hex%???}"
            g="${hex#?}"
            g="${g%??}"
            b="${hex#??}"
            b="${b%?}"
            a="${hex#???}"
            COLOR_R=$((16#$r$r))
            COLOR_G=$((16#$g$g))
            COLOR_B=$((16#$b$b))
            COLOR_A=$((16#$a$a))
            ;;
        6)
            # #RRGGBB
            COLOR_R=$((16#${hex%????}))
            mid="${hex#??}"
            COLOR_G=$((16#${mid%??}))
            COLOR_B=$((16#${hex#????}))
            ;;
        8)
            # #RRGGBBAA
            COLOR_R=$((16#${hex%??????}))
            hex2="${hex#??}"
            COLOR_G=$((16#${hex2%????}))
            hex3="${hex#????}"
            COLOR_B=$((16#${hex3%??}))
            COLOR_A=$((16#${hex#??????}))
            ;;
        *)
            PROVEN_ERROR="invalid_hex_length"
            return 1
            ;;
    esac

    return 0
}

# Parse RGB color (rgb(r, g, b) or r,g,b)
# Usage: color_parse_rgb "255, 128, 0"
# Sets: COLOR_R, COLOR_G, COLOR_B
# Returns: 0 on success, 1 on error
color_parse_rgb() {
    input="$1"
    COLOR_R=""
    COLOR_G=""
    COLOR_B=""
    COLOR_A="255"
    PROVEN_ERROR=""

    # Strip rgb() wrapper if present
    clean="$input"
    case "$clean" in
        rgb\(*\)|RGB\(*\))
            clean="${clean#rgb(}"
            clean="${clean#RGB(}"
            clean="${clean%)}"
            ;;
        rgba\(*\)|RGBA\(*\))
            clean="${clean#rgba(}"
            clean="${clean#RGBA(}"
            clean="${clean%)}"
            ;;
    esac

    # Split by comma or space
    clean=$(printf '%s' "$clean" | tr ',' ' ' | tr -s ' ')

    # Extract components
    set -- $clean
    if [ $# -lt 3 ]; then
        PROVEN_ERROR="insufficient_components"
        return 1
    fi

    COLOR_R="$1"
    COLOR_G="$2"
    COLOR_B="$3"
    [ $# -ge 4 ] && COLOR_A="$4"

    # Validate ranges
    for val in "$COLOR_R" "$COLOR_G" "$COLOR_B" "$COLOR_A"; do
        case "$val" in
            *[!0-9]*)
                PROVEN_ERROR="non_numeric_component"
                return 1
                ;;
        esac
        if [ "$val" -lt 0 ] || [ "$val" -gt 255 ]; then
            PROVEN_ERROR="component_out_of_range"
            return 1
        fi
    done

    return 0
}

# Check if hex color is valid
# Usage: color_is_valid_hex "#ff5500" && echo "valid"
# Returns: 0 if valid, 1 otherwise
color_is_valid_hex() {
    color_parse_hex "$1" 2>/dev/null
}

# Format color as hex
# Usage: hex=$(color_to_hex 255 128 0)
# Returns: #RRGGBB
color_to_hex() {
    r="$1"
    g="$2"
    b="$3"

    printf '#%02x%02x%02x' "$r" "$g" "$b"
}

# Format color as hex with alpha
# Usage: hex=$(color_to_hex_alpha 255 128 0 128)
# Returns: #RRGGBBAA
color_to_hex_alpha() {
    r="$1"
    g="$2"
    b="$3"
    a="$4"

    printf '#%02x%02x%02x%02x' "$r" "$g" "$b" "$a"
}

# Format color as rgb()
# Usage: rgb=$(color_to_rgb 255 128 0)
# Returns: rgb(255, 128, 0)
color_to_rgb() {
    printf 'rgb(%d, %d, %d)' "$1" "$2" "$3"
}

# Format color as rgba()
# Usage: rgba=$(color_to_rgba 255 128 0 0.5)
# Returns: rgba(255, 128, 0, 0.5)
color_to_rgba() {
    r="$1"
    g="$2"
    b="$3"
    a="$4"

    # If alpha is 0-255, convert to 0-1
    case "$a" in
        *.*)
            printf 'rgba(%d, %d, %d, %s)' "$r" "$g" "$b" "$a"
            ;;
        *)
            a_float=$(awk -v a="$a" 'BEGIN { printf "%.2f", a / 255 }')
            printf 'rgba(%d, %d, %d, %s)' "$r" "$g" "$b" "$a_float"
            ;;
    esac
}

# Convert RGB to HSL
# Usage: color_rgb_to_hsl 255 128 0
# Sets: COLOR_H, COLOR_S, COLOR_L (H: 0-360, S/L: 0-100)
COLOR_H=""
COLOR_S=""
COLOR_L=""

color_rgb_to_hsl() {
    r="$1"
    g="$2"
    b="$3"

    awk -v r="$r" -v g="$g" -v b="$b" 'BEGIN {
        r = r / 255
        g = g / 255
        b = b / 255

        max = r; min = r
        if (g > max) max = g
        if (b > max) max = b
        if (g < min) min = g
        if (b < min) min = b

        l = (max + min) / 2

        if (max == min) {
            h = s = 0
        } else {
            d = max - min
            s = l > 0.5 ? d / (2 - max - min) : d / (max + min)

            if (max == r) {
                h = (g - b) / d + (g < b ? 6 : 0)
            } else if (max == g) {
                h = (b - r) / d + 2
            } else {
                h = (r - g) / d + 4
            }
            h = h / 6
        }

        printf "%d %d %d", int(h * 360 + 0.5), int(s * 100 + 0.5), int(l * 100 + 0.5)
    }'
}

# Convert HSL to RGB
# Usage: color_hsl_to_rgb 30 100 50
# Returns: R G B
color_hsl_to_rgb() {
    h="$1"
    s="$2"
    l="$3"

    awk -v h="$h" -v s="$s" -v l="$l" 'BEGIN {
        h = h / 360
        s = s / 100
        l = l / 100

        if (s == 0) {
            r = g = b = l
        } else {
            q = l < 0.5 ? l * (1 + s) : l + s - l * s
            p = 2 * l - q

            r = hue2rgb(p, q, h + 1/3)
            g = hue2rgb(p, q, h)
            b = hue2rgb(p, q, h - 1/3)
        }

        printf "%d %d %d", int(r * 255 + 0.5), int(g * 255 + 0.5), int(b * 255 + 0.5)
    }

    function hue2rgb(p, q, t) {
        if (t < 0) t += 1
        if (t > 1) t -= 1
        if (t < 1/6) return p + (q - p) * 6 * t
        if (t < 1/2) return q
        if (t < 2/3) return p + (q - p) * (2/3 - t) * 6
        return p
    }'
}

# Lighten a color
# Usage: lighter=$(color_lighten "#ff5500" 20)
# Returns: Lightened hex color
color_lighten() {
    color="$1"
    percent="$2"

    if ! color_parse_hex "$color"; then
        return 1
    fi

    hsl=$(color_rgb_to_hsl "$COLOR_R" "$COLOR_G" "$COLOR_B")
    set -- $hsl
    h="$1"
    s="$2"
    l="$3"

    new_l=$((l + percent))
    [ $new_l -gt 100 ] && new_l=100

    rgb=$(color_hsl_to_rgb "$h" "$s" "$new_l")
    set -- $rgb

    color_to_hex "$1" "$2" "$3"
}

# Darken a color
# Usage: darker=$(color_darken "#ff5500" 20)
# Returns: Darkened hex color
color_darken() {
    color="$1"
    percent="$2"

    if ! color_parse_hex "$color"; then
        return 1
    fi

    hsl=$(color_rgb_to_hsl "$COLOR_R" "$COLOR_G" "$COLOR_B")
    set -- $hsl
    h="$1"
    s="$2"
    l="$3"

    new_l=$((l - percent))
    [ $new_l -lt 0 ] && new_l=0

    rgb=$(color_hsl_to_rgb "$h" "$s" "$new_l")
    set -- $rgb

    color_to_hex "$1" "$2" "$3"
}

# Calculate luminance (for contrast calculations)
# Usage: lum=$(color_luminance 255 128 0)
# Returns: Relative luminance (0.0-1.0)
color_luminance() {
    r="$1"
    g="$2"
    b="$3"

    awk -v r="$r" -v g="$g" -v b="$b" 'BEGIN {
        r = r / 255
        g = g / 255
        b = b / 255

        r = r <= 0.03928 ? r / 12.92 : ((r + 0.055) / 1.055) ^ 2.4
        g = g <= 0.03928 ? g / 12.92 : ((g + 0.055) / 1.055) ^ 2.4
        b = b <= 0.03928 ? b / 12.92 : ((b + 0.055) / 1.055) ^ 2.4

        printf "%.4f", 0.2126 * r + 0.7152 * g + 0.0722 * b
    }'
}

# Calculate contrast ratio between two colors
# Usage: ratio=$(color_contrast_ratio "#000000" "#ffffff")
# Returns: Contrast ratio (1-21)
color_contrast_ratio() {
    color1="$1"
    color2="$2"

    if ! color_parse_hex "$color1"; then
        return 1
    fi
    lum1=$(color_luminance "$COLOR_R" "$COLOR_G" "$COLOR_B")

    if ! color_parse_hex "$color2"; then
        return 1
    fi
    lum2=$(color_luminance "$COLOR_R" "$COLOR_G" "$COLOR_B")

    awk -v l1="$lum1" -v l2="$lum2" 'BEGIN {
        if (l1 > l2) {
            lighter = l1
            darker = l2
        } else {
            lighter = l2
            darker = l1
        }
        printf "%.2f", (lighter + 0.05) / (darker + 0.05)
    }'
}

# Check if color meets WCAG AA contrast with white
# Usage: color_is_dark "#000000" && echo "dark (good for white text)"
# Returns: 0 if dark (good for white text), 1 otherwise
color_is_dark() {
    color="$1"

    if ! color_parse_hex "$color"; then
        return 1
    fi

    lum=$(color_luminance "$COLOR_R" "$COLOR_G" "$COLOR_B")

    awk -v l="$lum" 'BEGIN { exit (l < 0.5) ? 0 : 1 }'
}

# Invert a color
# Usage: inverted=$(color_invert "#ff5500")
# Returns: Inverted hex color
color_invert() {
    color="$1"

    if ! color_parse_hex "$color"; then
        return 1
    fi

    r=$((255 - COLOR_R))
    g=$((255 - COLOR_G))
    b=$((255 - COLOR_B))

    color_to_hex "$r" "$g" "$b"
}

# Mix two colors
# Usage: mixed=$(color_mix "#ff0000" "#0000ff" 50)
# Returns: Mixed hex color
color_mix() {
    color1="$1"
    color2="$2"
    weight="${3:-50}"

    if ! color_parse_hex "$color1"; then
        return 1
    fi
    r1="$COLOR_R"
    g1="$COLOR_G"
    b1="$COLOR_B"

    if ! color_parse_hex "$color2"; then
        return 1
    fi
    r2="$COLOR_R"
    g2="$COLOR_G"
    b2="$COLOR_B"

    awk -v r1="$r1" -v g1="$g1" -v b1="$b1" \
        -v r2="$r2" -v g2="$g2" -v b2="$b2" \
        -v w="$weight" 'BEGIN {
        w = w / 100
        r = int(r1 * w + r2 * (1 - w) + 0.5)
        g = int(g1 * w + g2 * (1 - w) + 0.5)
        b = int(b1 * w + b2 * (1 - w) + 0.5)
        printf "#%02x%02x%02x", r, g, b
    }'
}

# Named color lookup
color_from_name() {
    name="$1"
    case $(printf '%s' "$name" | tr '[:upper:]' '[:lower:]') in
        black) printf '#000000' ;;
        white) printf '#ffffff' ;;
        red) printf '#ff0000' ;;
        green) printf '#00ff00' ;;
        blue) printf '#0000ff' ;;
        yellow) printf '#ffff00' ;;
        cyan) printf '#00ffff' ;;
        magenta) printf '#ff00ff' ;;
        orange) printf '#ffa500' ;;
        purple) printf '#800080' ;;
        pink) printf '#ffc0cb' ;;
        gray|grey) printf '#808080' ;;
        *) printf '' && return 1 ;;
    esac
}
