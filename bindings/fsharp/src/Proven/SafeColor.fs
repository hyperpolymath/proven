// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe color manipulation with various color space support.
module SafeColor =
    open System
    open System.Text.RegularExpressions

    /// Color parsing errors.
    type ColorError =
        | InvalidFormat of string
        | ValueOutOfRange of string
        | InvalidHex of string
        | InvalidComponent of string
        | EmptyInput

    /// RGB color (0-255 for each component).
    type RgbColor = {
        Red: int
        Green: int
        Blue: int
        Alpha: float  // 0.0 to 1.0
    }

    /// HSL color (Hue: 0-360, Saturation/Lightness: 0-100).
    type HslColor = {
        Hue: float
        Saturation: float
        Lightness: float
        Alpha: float
    }

    /// HSV/HSB color (Hue: 0-360, Saturation/Value: 0-100).
    type HsvColor = {
        Hue: float
        Saturation: float
        Value: float
        Alpha: float
    }

    /// CMYK color (0-100 for each component).
    type CmykColor = {
        Cyan: float
        Magenta: float
        Yellow: float
        Black: float
    }

    /// Clamp value to valid range.
    let private clampByte (value: int) : int =
        Math.Clamp(value, 0, 255)

    let private clampFloat (minVal: float) (maxVal: float) (value: float) : float =
        Math.Clamp(value, minVal, maxVal)

    /// Create RGB color.
    let createRgb (red: int) (green: int) (blue: int) : RgbColor =
        { Red = clampByte red; Green = clampByte green; Blue = clampByte blue; Alpha = 1.0 }

    /// Create RGBA color.
    let createRgba (red: int) (green: int) (blue: int) (alpha: float) : RgbColor =
        { Red = clampByte red; Green = clampByte green; Blue = clampByte blue; Alpha = clampFloat 0.0 1.0 alpha }

    /// Create HSL color.
    let createHsl (hue: float) (saturation: float) (lightness: float) : HslColor =
        { Hue = hue % 360.0; Saturation = clampFloat 0.0 100.0 saturation; Lightness = clampFloat 0.0 100.0 lightness; Alpha = 1.0 }

    /// Create HSLA color.
    let createHsla (hue: float) (saturation: float) (lightness: float) (alpha: float) : HslColor =
        { Hue = hue % 360.0; Saturation = clampFloat 0.0 100.0 saturation; Lightness = clampFloat 0.0 100.0 lightness; Alpha = clampFloat 0.0 1.0 alpha }

    /// Create HSV color.
    let createHsv (hue: float) (saturation: float) (value: float) : HsvColor =
        { Hue = hue % 360.0; Saturation = clampFloat 0.0 100.0 saturation; Value = clampFloat 0.0 100.0 value; Alpha = 1.0 }

    /// Create CMYK color.
    let createCmyk (cyan: float) (magenta: float) (yellow: float) (black: float) : CmykColor =
        { Cyan = clampFloat 0.0 100.0 cyan; Magenta = clampFloat 0.0 100.0 magenta; Yellow = clampFloat 0.0 100.0 yellow; Black = clampFloat 0.0 100.0 black }

    let private hexPattern = Regex(@"^#?([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$")

    /// Parse hex color string.
    let parseHex (input: string) : Result<RgbColor, ColorError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            let trimmed = input.Trim()
            if not (hexPattern.IsMatch(trimmed)) then
                Error(InvalidHex trimmed)
            else
                let hex = if trimmed.StartsWith("#") then trimmed.Substring(1) else trimmed
                let expandedHex =
                    if hex.Length = 3 then
                        sprintf "%c%c%c%c%c%c" hex.[0] hex.[0] hex.[1] hex.[1] hex.[2] hex.[2]
                    else hex
                try
                    let red = Convert.ToInt32(expandedHex.Substring(0, 2), 16)
                    let green = Convert.ToInt32(expandedHex.Substring(2, 2), 16)
                    let blue = Convert.ToInt32(expandedHex.Substring(4, 2), 16)
                    let alpha =
                        if expandedHex.Length = 8 then
                            float (Convert.ToInt32(expandedHex.Substring(6, 2), 16)) / 255.0
                        else 1.0
                    Ok { Red = red; Green = green; Blue = blue; Alpha = alpha }
                with
                | _ -> Error(InvalidHex input)

    /// Parse hex color, returning Option.
    let tryParseHex (input: string) : RgbColor option =
        match parseHex input with
        | Ok color -> Some color
        | Error _ -> None

    /// Format RGB as hex string.
    let toHex (color: RgbColor) : string =
        sprintf "#%02x%02x%02x" color.Red color.Green color.Blue

    /// Format RGBA as hex string with alpha.
    let toHexWithAlpha (color: RgbColor) : string =
        let alpha = int (color.Alpha * 255.0)
        sprintf "#%02x%02x%02x%02x" color.Red color.Green color.Blue alpha

    /// Format as CSS rgb() or rgba().
    let toCssRgb (color: RgbColor) : string =
        if color.Alpha = 1.0 then
            sprintf "rgb(%d, %d, %d)" color.Red color.Green color.Blue
        else
            sprintf "rgba(%d, %d, %d, %.2f)" color.Red color.Green color.Blue color.Alpha

    /// Format as CSS hsl() or hsla().
    let toCssHsl (color: HslColor) : string =
        if color.Alpha = 1.0 then
            sprintf "hsl(%.0f, %.0f%%, %.0f%%)" color.Hue color.Saturation color.Lightness
        else
            sprintf "hsla(%.0f, %.0f%%, %.0f%%, %.2f)" color.Hue color.Saturation color.Lightness color.Alpha

    /// Convert RGB to HSL.
    let rgbToHsl (color: RgbColor) : HslColor =
        let r = float color.Red / 255.0
        let g = float color.Green / 255.0
        let b = float color.Blue / 255.0

        let maxC = max (max r g) b
        let minC = min (min r g) b
        let l = (maxC + minC) / 2.0

        if maxC = minC then
            { Hue = 0.0; Saturation = 0.0; Lightness = l * 100.0; Alpha = color.Alpha }
        else
            let d = maxC - minC
            let s = if l > 0.5 then d / (2.0 - maxC - minC) else d / (maxC + minC)
            let h =
                if maxC = r then
                    let segment = (g - b) / d
                    let shift = if g < b then 6.0 else 0.0
                    segment + shift
                elif maxC = g then
                    (b - r) / d + 2.0
                else
                    (r - g) / d + 4.0
            { Hue = h * 60.0; Saturation = s * 100.0; Lightness = l * 100.0; Alpha = color.Alpha }

    /// Convert HSL to RGB.
    let hslToRgb (color: HslColor) : RgbColor =
        let h = color.Hue / 360.0
        let s = color.Saturation / 100.0
        let l = color.Lightness / 100.0

        if s = 0.0 then
            let gray = int (l * 255.0)
            { Red = gray; Green = gray; Blue = gray; Alpha = color.Alpha }
        else
            let hue2rgb p q t =
                let t = if t < 0.0 then t + 1.0 elif t > 1.0 then t - 1.0 else t
                if t < 1.0 / 6.0 then p + (q - p) * 6.0 * t
                elif t < 1.0 / 2.0 then q
                elif t < 2.0 / 3.0 then p + (q - p) * (2.0 / 3.0 - t) * 6.0
                else p

            let q = if l < 0.5 then l * (1.0 + s) else l + s - l * s
            let p = 2.0 * l - q
            let r = hue2rgb p q (h + 1.0 / 3.0)
            let g = hue2rgb p q h
            let b = hue2rgb p q (h - 1.0 / 3.0)
            { Red = int (r * 255.0); Green = int (g * 255.0); Blue = int (b * 255.0); Alpha = color.Alpha }

    /// Convert RGB to HSV.
    let rgbToHsv (color: RgbColor) : HsvColor =
        let r = float color.Red / 255.0
        let g = float color.Green / 255.0
        let b = float color.Blue / 255.0

        let maxC = max (max r g) b
        let minC = min (min r g) b
        let d = maxC - minC

        let h =
            if d = 0.0 then 0.0
            elif maxC = r then ((g - b) / d) % 6.0
            elif maxC = g then (b - r) / d + 2.0
            else (r - g) / d + 4.0

        let s = if maxC = 0.0 then 0.0 else d / maxC

        { Hue = h * 60.0; Saturation = s * 100.0; Value = maxC * 100.0; Alpha = color.Alpha }

    /// Convert HSV to RGB.
    let hsvToRgb (color: HsvColor) : RgbColor =
        let h = color.Hue / 60.0
        let s = color.Saturation / 100.0
        let v = color.Value / 100.0

        let i = int (floor h) % 6
        let f = h - floor h
        let p = v * (1.0 - s)
        let q = v * (1.0 - f * s)
        let t = v * (1.0 - (1.0 - f) * s)

        let r, g, b =
            match i with
            | 0 -> v, t, p
            | 1 -> q, v, p
            | 2 -> p, v, t
            | 3 -> p, q, v
            | 4 -> t, p, v
            | _ -> v, p, q

        { Red = int (r * 255.0); Green = int (g * 255.0); Blue = int (b * 255.0); Alpha = color.Alpha }

    /// Convert RGB to CMYK.
    let rgbToCmyk (color: RgbColor) : CmykColor =
        let r = float color.Red / 255.0
        let g = float color.Green / 255.0
        let b = float color.Blue / 255.0

        let k = 1.0 - max (max r g) b
        if k = 1.0 then
            { Cyan = 0.0; Magenta = 0.0; Yellow = 0.0; Black = 100.0 }
        else
            let c = (1.0 - r - k) / (1.0 - k)
            let m = (1.0 - g - k) / (1.0 - k)
            let y = (1.0 - b - k) / (1.0 - k)
            { Cyan = c * 100.0; Magenta = m * 100.0; Yellow = y * 100.0; Black = k * 100.0 }

    /// Convert CMYK to RGB.
    let cmykToRgb (color: CmykColor) : RgbColor =
        let c = color.Cyan / 100.0
        let m = color.Magenta / 100.0
        let y = color.Yellow / 100.0
        let k = color.Black / 100.0

        let r = 255.0 * (1.0 - c) * (1.0 - k)
        let g = 255.0 * (1.0 - m) * (1.0 - k)
        let b = 255.0 * (1.0 - y) * (1.0 - k)
        { Red = int r; Green = int g; Blue = int b; Alpha = 1.0 }

    /// Lighten color by percentage.
    let lighten (percent: float) (color: RgbColor) : RgbColor =
        let hsl = rgbToHsl color
        let newLightness = min 100.0 (hsl.Lightness + percent)
        hslToRgb { hsl with Lightness = newLightness }

    /// Darken color by percentage.
    let darken (percent: float) (color: RgbColor) : RgbColor =
        let hsl = rgbToHsl color
        let newLightness = max 0.0 (hsl.Lightness - percent)
        hslToRgb { hsl with Lightness = newLightness }

    /// Saturate color by percentage.
    let saturate (percent: float) (color: RgbColor) : RgbColor =
        let hsl = rgbToHsl color
        let newSaturation = min 100.0 (hsl.Saturation + percent)
        hslToRgb { hsl with Saturation = newSaturation }

    /// Desaturate color by percentage.
    let desaturate (percent: float) (color: RgbColor) : RgbColor =
        let hsl = rgbToHsl color
        let newSaturation = max 0.0 (hsl.Saturation - percent)
        hslToRgb { hsl with Saturation = newSaturation }

    /// Invert color.
    let invert (color: RgbColor) : RgbColor =
        { Red = 255 - color.Red; Green = 255 - color.Green; Blue = 255 - color.Blue; Alpha = color.Alpha }

    /// Convert to grayscale.
    let grayscale (color: RgbColor) : RgbColor =
        let gray = int (0.299 * float color.Red + 0.587 * float color.Green + 0.114 * float color.Blue)
        { Red = gray; Green = gray; Blue = gray; Alpha = color.Alpha }

    /// Get complementary color.
    let complementary (color: RgbColor) : RgbColor =
        let hsl = rgbToHsl color
        let newHue = (hsl.Hue + 180.0) % 360.0
        hslToRgb { hsl with Hue = newHue }

    /// Mix two colors.
    let mix (weight: float) (color1: RgbColor) (color2: RgbColor) : RgbColor =
        let w = clampFloat 0.0 1.0 weight
        let r = int (float color1.Red * w + float color2.Red * (1.0 - w))
        let g = int (float color1.Green * w + float color2.Green * (1.0 - w))
        let b = int (float color1.Blue * w + float color2.Blue * (1.0 - w))
        let a = color1.Alpha * w + color2.Alpha * (1.0 - w)
        { Red = r; Green = g; Blue = b; Alpha = a }

    /// Set alpha.
    let setAlpha (alpha: float) (color: RgbColor) : RgbColor =
        { color with Alpha = clampFloat 0.0 1.0 alpha }

    /// Get luminance (relative brightness).
    let luminance (color: RgbColor) : float =
        let gammaCorrect c =
            let cs = float c / 255.0
            if cs <= 0.03928 then cs / 12.92 else Math.Pow((cs + 0.055) / 1.055, 2.4)
        0.2126 * gammaCorrect color.Red + 0.7152 * gammaCorrect color.Green + 0.0722 * gammaCorrect color.Blue

    /// Get contrast ratio between two colors (WCAG).
    let contrastRatio (color1: RgbColor) (color2: RgbColor) : float =
        let l1 = luminance color1
        let l2 = luminance color2
        let lighter = max l1 l2
        let darker = min l1 l2
        (lighter + 0.05) / (darker + 0.05)

    /// Check if contrast meets WCAG AA for normal text (4.5:1).
    let meetsWcagAA (color1: RgbColor) (color2: RgbColor) : bool =
        contrastRatio color1 color2 >= 4.5

    /// Check if contrast meets WCAG AAA for normal text (7:1).
    let meetsWcagAAA (color1: RgbColor) (color2: RgbColor) : bool =
        contrastRatio color1 color2 >= 7.0

    /// Common colors.
    let black : RgbColor = createRgb 0 0 0
    let white : RgbColor = createRgb 255 255 255
    let red : RgbColor = createRgb 255 0 0
    let green : RgbColor = createRgb 0 255 0
    let blue : RgbColor = createRgb 0 0 255
    let yellow : RgbColor = createRgb 255 255 0
    let cyan : RgbColor = createRgb 0 255 255
    let magenta : RgbColor = createRgb 255 0 255
    let transparent : RgbColor = createRgba 0 0 0 0.0
