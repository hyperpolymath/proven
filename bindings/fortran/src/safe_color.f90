! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeColor - Color validation and conversion for Fortran
!

module safe_color
    use, intrinsic :: iso_fortran_env, only: int64, int32, real64
    implicit none
    private

    !> RGB color type (0-255 per channel)
    type, public :: rgb_color_type
        integer :: red = 0
        integer :: green = 0
        integer :: blue = 0
        integer :: alpha = 255
        logical :: valid = .false.
    contains
        procedure :: to_hex => rgb_to_hex
        procedure :: to_hsl => rgb_to_hsl
        procedure :: luminance => rgb_luminance
    end type rgb_color_type

    !> HSL color type (hue 0-360, saturation/lightness 0-100)
    type, public :: hsl_color_type
        real(real64) :: hue = 0.0_real64        ! 0-360
        real(real64) :: saturation = 0.0_real64  ! 0-100
        real(real64) :: lightness = 0.0_real64   ! 0-100
        real(real64) :: alpha = 1.0_real64       ! 0-1
        logical :: valid = .false.
    contains
        procedure :: to_rgb => hsl_to_rgb
    end type hsl_color_type

    !> Color result type
    type, public :: ColorResult
        type(rgb_color_type) :: color
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type ColorResult

    !> Named color constants
    integer, parameter, public :: COLOR_BLACK = 1
    integer, parameter, public :: COLOR_WHITE = 2
    integer, parameter, public :: COLOR_RED = 3
    integer, parameter, public :: COLOR_GREEN = 4
    integer, parameter, public :: COLOR_BLUE = 5
    integer, parameter, public :: COLOR_YELLOW = 6
    integer, parameter, public :: COLOR_CYAN = 7
    integer, parameter, public :: COLOR_MAGENTA = 8

    public :: parse_hex_color, parse_rgb_color, parse_hsl_color
    public :: rgb_from_values, hsl_from_values
    public :: color_blend, color_contrast_ratio, meets_wcag_aa, meets_wcag_aaa
    public :: lighten, darken, saturate, desaturate
    public :: get_named_color, color_distance

contains

    !> Parse hex color string (#RGB, #RGBA, #RRGGBB, #RRGGBBAA)
    function parse_hex_color(hex_string) result(res)
        character(len=*), intent(in) :: hex_string
        type(ColorResult) :: res
        integer :: str_len, start_pos
        character(len=8) :: hex_clean
        integer :: r, g, b, a

        str_len = len_trim(hex_string)
        start_pos = 1

        ! Skip # prefix
        if (hex_string(1:1) == '#') then
            start_pos = 2
            str_len = str_len - 1
        end if

        hex_clean = hex_string(start_pos:)

        select case (str_len)
            case (3)  ! RGB
                r = hex_digit(hex_clean(1:1)) * 17
                g = hex_digit(hex_clean(2:2)) * 17
                b = hex_digit(hex_clean(3:3)) * 17
                a = 255
            case (4)  ! RGBA
                r = hex_digit(hex_clean(1:1)) * 17
                g = hex_digit(hex_clean(2:2)) * 17
                b = hex_digit(hex_clean(3:3)) * 17
                a = hex_digit(hex_clean(4:4)) * 17
            case (6)  ! RRGGBB
                r = hex_digit(hex_clean(1:1)) * 16 + hex_digit(hex_clean(2:2))
                g = hex_digit(hex_clean(3:3)) * 16 + hex_digit(hex_clean(4:4))
                b = hex_digit(hex_clean(5:5)) * 16 + hex_digit(hex_clean(6:6))
                a = 255
            case (8)  ! RRGGBBAA
                r = hex_digit(hex_clean(1:1)) * 16 + hex_digit(hex_clean(2:2))
                g = hex_digit(hex_clean(3:3)) * 16 + hex_digit(hex_clean(4:4))
                b = hex_digit(hex_clean(5:5)) * 16 + hex_digit(hex_clean(6:6))
                a = hex_digit(hex_clean(7:7)) * 16 + hex_digit(hex_clean(8:8))
            case default
                res%error = 'Invalid hex color length'
                return
        end select

        ! Validate values
        if (r < 0 .or. r > 255 .or. g < 0 .or. g > 255 .or. &
            b < 0 .or. b > 255 .or. a < 0 .or. a > 255) then
            res%error = 'Invalid hex digit'
            return
        end if

        res%color%red = r
        res%color%green = g
        res%color%blue = b
        res%color%alpha = a
        res%color%valid = .true.
        res%ok = .true.
    end function parse_hex_color

    !> Convert hex character to integer
    pure function hex_digit(c) result(value)
        character(len=1), intent(in) :: c
        integer :: value

        select case (c)
            case ('0':'9')
                value = iachar(c) - iachar('0')
            case ('a':'f')
                value = iachar(c) - iachar('a') + 10
            case ('A':'F')
                value = iachar(c) - iachar('A') + 10
            case default
                value = -1
        end select
    end function hex_digit

    !> Parse RGB color string (rgb(r,g,b) or rgba(r,g,b,a))
    function parse_rgb_color(rgb_string) result(res)
        character(len=*), intent(in) :: rgb_string
        type(ColorResult) :: res
        integer :: paren_open, paren_close, comma1, comma2, comma3
        integer :: r, g, b
        real(real64) :: a
        integer :: io_stat
        character(len=64) :: working_str

        working_str = adjustl(rgb_string)

        paren_open = index(working_str, '(')
        paren_close = index(working_str, ')')

        if (paren_open == 0 .or. paren_close == 0) then
            res%error = 'Invalid RGB format'
            return
        end if

        ! Find commas
        comma1 = index(working_str(paren_open+1:paren_close-1), ',') + paren_open
        comma2 = index(working_str(comma1+1:paren_close-1), ',') + comma1

        ! Parse values
        read(working_str(paren_open+1:comma1-1), *, iostat=io_stat) r
        if (io_stat /= 0) then
            res%error = 'Invalid red value'
            return
        end if

        read(working_str(comma1+1:comma2-1), *, iostat=io_stat) g
        if (io_stat /= 0) then
            res%error = 'Invalid green value'
            return
        end if

        ! Check for alpha
        comma3 = index(working_str(comma2+1:paren_close-1), ',') + comma2

        if (comma3 > comma2) then
            read(working_str(comma2+1:comma3-1), *, iostat=io_stat) b
            read(working_str(comma3+1:paren_close-1), *, iostat=io_stat) a
            res%color%alpha = nint(a * 255.0_real64)
        else
            read(working_str(comma2+1:paren_close-1), *, iostat=io_stat) b
            res%color%alpha = 255
        end if

        if (io_stat /= 0) then
            res%error = 'Invalid blue value'
            return
        end if

        ! Clamp values
        res%color%red = max(0, min(255, r))
        res%color%green = max(0, min(255, g))
        res%color%blue = max(0, min(255, b))
        res%color%valid = .true.
        res%ok = .true.
    end function parse_rgb_color

    !> Create RGB color from values
    pure function rgb_from_values(r, g, b, a) result(color)
        integer, intent(in) :: r, g, b
        integer, intent(in), optional :: a
        type(rgb_color_type) :: color

        color%red = max(0, min(255, r))
        color%green = max(0, min(255, g))
        color%blue = max(0, min(255, b))
        if (present(a)) then
            color%alpha = max(0, min(255, a))
        else
            color%alpha = 255
        end if
        color%valid = .true.
    end function rgb_from_values

    !> Create HSL color from values
    pure function hsl_from_values(h, s, l, a) result(color)
        real(real64), intent(in) :: h, s, l
        real(real64), intent(in), optional :: a
        type(hsl_color_type) :: color

        color%hue = mod(h, 360.0_real64)
        if (color%hue < 0.0_real64) color%hue = color%hue + 360.0_real64
        color%saturation = max(0.0_real64, min(100.0_real64, s))
        color%lightness = max(0.0_real64, min(100.0_real64, l))
        if (present(a)) then
            color%alpha = max(0.0_real64, min(1.0_real64, a))
        else
            color%alpha = 1.0_real64
        end if
        color%valid = .true.
    end function hsl_from_values

    !> Parse HSL color string
    function parse_hsl_color(hsl_string) result(color)
        character(len=*), intent(in) :: hsl_string
        type(hsl_color_type) :: color
        integer :: paren_open, paren_close, comma1, comma2
        integer :: io_stat
        real(real64) :: h, s, l
        character(len=64) :: working_str

        working_str = adjustl(hsl_string)

        paren_open = index(working_str, '(')
        paren_close = index(working_str, ')')

        if (paren_open == 0 .or. paren_close == 0) return

        comma1 = index(working_str(paren_open+1:paren_close-1), ',') + paren_open
        comma2 = index(working_str(comma1+1:paren_close-1), ',') + comma1

        read(working_str(paren_open+1:comma1-1), *, iostat=io_stat) h
        if (io_stat /= 0) return

        read(working_str(comma1+1:comma2-1), *, iostat=io_stat) s
        if (io_stat /= 0) return

        read(working_str(comma2+1:paren_close-1), *, iostat=io_stat) l
        if (io_stat /= 0) return

        color = hsl_from_values(h, s, l)
    end function parse_hsl_color

    !> Convert RGB to hex string
    function rgb_to_hex(self) result(hex_string)
        class(rgb_color_type), intent(in) :: self
        character(len=9) :: hex_string
        character(len=16), parameter :: HEX_CHARS = '0123456789abcdef'

        if (.not. self%valid) then
            hex_string = ''
            return
        end if

        hex_string(1:1) = '#'
        hex_string(2:2) = HEX_CHARS(self%red / 16 + 1:self%red / 16 + 1)
        hex_string(3:3) = HEX_CHARS(mod(self%red, 16) + 1:mod(self%red, 16) + 1)
        hex_string(4:4) = HEX_CHARS(self%green / 16 + 1:self%green / 16 + 1)
        hex_string(5:5) = HEX_CHARS(mod(self%green, 16) + 1:mod(self%green, 16) + 1)
        hex_string(6:6) = HEX_CHARS(self%blue / 16 + 1:self%blue / 16 + 1)
        hex_string(7:7) = HEX_CHARS(mod(self%blue, 16) + 1:mod(self%blue, 16) + 1)

        if (self%alpha < 255) then
            hex_string(8:8) = HEX_CHARS(self%alpha / 16 + 1:self%alpha / 16 + 1)
            hex_string(9:9) = HEX_CHARS(mod(self%alpha, 16) + 1:mod(self%alpha, 16) + 1)
        else
            hex_string(8:9) = ''
        end if
    end function rgb_to_hex

    !> Convert RGB to HSL
    function rgb_to_hsl(self) result(hsl)
        class(rgb_color_type), intent(in) :: self
        type(hsl_color_type) :: hsl
        real(real64) :: r, g, b, max_c, min_c, delta, l, s, h

        if (.not. self%valid) return

        r = real(self%red, real64) / 255.0_real64
        g = real(self%green, real64) / 255.0_real64
        b = real(self%blue, real64) / 255.0_real64

        max_c = max(r, g, b)
        min_c = min(r, g, b)
        delta = max_c - min_c

        l = (max_c + min_c) / 2.0_real64

        if (delta < 1.0e-10_real64) then
            h = 0.0_real64
            s = 0.0_real64
        else
            if (l < 0.5_real64) then
                s = delta / (max_c + min_c)
            else
                s = delta / (2.0_real64 - max_c - min_c)
            end if

            if (abs(max_c - r) < 1.0e-10_real64) then
                h = (g - b) / delta
                if (g < b) h = h + 6.0_real64
            else if (abs(max_c - g) < 1.0e-10_real64) then
                h = (b - r) / delta + 2.0_real64
            else
                h = (r - g) / delta + 4.0_real64
            end if

            h = h * 60.0_real64
        end if

        hsl%hue = h
        hsl%saturation = s * 100.0_real64
        hsl%lightness = l * 100.0_real64
        hsl%alpha = real(self%alpha, real64) / 255.0_real64
        hsl%valid = .true.
    end function rgb_to_hsl

    !> Convert HSL to RGB
    function hsl_to_rgb(self) result(rgb)
        class(hsl_color_type), intent(in) :: self
        type(rgb_color_type) :: rgb
        real(real64) :: h, s, l, c, x, m, r, g, b

        if (.not. self%valid) return

        h = self%hue
        s = self%saturation / 100.0_real64
        l = self%lightness / 100.0_real64

        c = (1.0_real64 - abs(2.0_real64 * l - 1.0_real64)) * s
        x = c * (1.0_real64 - abs(mod(h / 60.0_real64, 2.0_real64) - 1.0_real64))
        m = l - c / 2.0_real64

        if (h < 60.0_real64) then
            r = c; g = x; b = 0.0_real64
        else if (h < 120.0_real64) then
            r = x; g = c; b = 0.0_real64
        else if (h < 180.0_real64) then
            r = 0.0_real64; g = c; b = x
        else if (h < 240.0_real64) then
            r = 0.0_real64; g = x; b = c
        else if (h < 300.0_real64) then
            r = x; g = 0.0_real64; b = c
        else
            r = c; g = 0.0_real64; b = x
        end if

        rgb%red = nint((r + m) * 255.0_real64)
        rgb%green = nint((g + m) * 255.0_real64)
        rgb%blue = nint((b + m) * 255.0_real64)
        rgb%alpha = nint(self%alpha * 255.0_real64)
        rgb%valid = .true.
    end function hsl_to_rgb

    !> Calculate relative luminance (WCAG)
    pure function rgb_luminance(self) result(lum)
        class(rgb_color_type), intent(in) :: self
        real(real64) :: lum
        real(real64) :: r, g, b

        r = srgb_to_linear(real(self%red, real64) / 255.0_real64)
        g = srgb_to_linear(real(self%green, real64) / 255.0_real64)
        b = srgb_to_linear(real(self%blue, real64) / 255.0_real64)

        lum = 0.2126_real64 * r + 0.7152_real64 * g + 0.0722_real64 * b
    end function rgb_luminance

    !> Convert sRGB to linear RGB
    pure function srgb_to_linear(c) result(linear)
        real(real64), intent(in) :: c
        real(real64) :: linear

        if (c <= 0.04045_real64) then
            linear = c / 12.92_real64
        else
            linear = ((c + 0.055_real64) / 1.055_real64) ** 2.4_real64
        end if
    end function srgb_to_linear

    !> Calculate contrast ratio between two colors
    function color_contrast_ratio(color1, color2) result(ratio)
        type(rgb_color_type), intent(in) :: color1, color2
        real(real64) :: ratio
        real(real64) :: lum1, lum2, lighter, darker

        lum1 = color1%luminance()
        lum2 = color2%luminance()

        lighter = max(lum1, lum2)
        darker = min(lum1, lum2)

        ratio = (lighter + 0.05_real64) / (darker + 0.05_real64)
    end function color_contrast_ratio

    !> Check if contrast meets WCAG AA (4.5:1 for normal text)
    function meets_wcag_aa(color1, color2, large_text) result(meets)
        type(rgb_color_type), intent(in) :: color1, color2
        logical, intent(in), optional :: large_text
        logical :: meets
        real(real64) :: ratio, threshold

        ratio = color_contrast_ratio(color1, color2)

        if (present(large_text) .and. large_text) then
            threshold = 3.0_real64
        else
            threshold = 4.5_real64
        end if

        meets = ratio >= threshold
    end function meets_wcag_aa

    !> Check if contrast meets WCAG AAA (7:1 for normal text)
    function meets_wcag_aaa(color1, color2, large_text) result(meets)
        type(rgb_color_type), intent(in) :: color1, color2
        logical, intent(in), optional :: large_text
        logical :: meets
        real(real64) :: ratio, threshold

        ratio = color_contrast_ratio(color1, color2)

        if (present(large_text) .and. large_text) then
            threshold = 4.5_real64
        else
            threshold = 7.0_real64
        end if

        meets = ratio >= threshold
    end function meets_wcag_aaa

    !> Blend two colors
    function color_blend(color1, color2, factor) result(blended)
        type(rgb_color_type), intent(in) :: color1, color2
        real(real64), intent(in) :: factor
        type(rgb_color_type) :: blended
        real(real64) :: f

        f = max(0.0_real64, min(1.0_real64, factor))

        blended%red = nint(real(color1%red, real64) * (1.0_real64 - f) + &
                          real(color2%red, real64) * f)
        blended%green = nint(real(color1%green, real64) * (1.0_real64 - f) + &
                            real(color2%green, real64) * f)
        blended%blue = nint(real(color1%blue, real64) * (1.0_real64 - f) + &
                           real(color2%blue, real64) * f)
        blended%alpha = nint(real(color1%alpha, real64) * (1.0_real64 - f) + &
                            real(color2%alpha, real64) * f)
        blended%valid = .true.
    end function color_blend

    !> Lighten a color by percentage
    function lighten(color, amount) result(lightened)
        type(rgb_color_type), intent(in) :: color
        real(real64), intent(in) :: amount
        type(rgb_color_type) :: lightened
        type(hsl_color_type) :: hsl

        hsl = color%to_hsl()
        hsl%lightness = min(100.0_real64, hsl%lightness + amount)
        lightened = hsl%to_rgb()
    end function lighten

    !> Darken a color by percentage
    function darken(color, amount) result(darkened)
        type(rgb_color_type), intent(in) :: color
        real(real64), intent(in) :: amount
        type(rgb_color_type) :: darkened
        type(hsl_color_type) :: hsl

        hsl = color%to_hsl()
        hsl%lightness = max(0.0_real64, hsl%lightness - amount)
        darkened = hsl%to_rgb()
    end function darken

    !> Increase saturation
    function saturate(color, amount) result(saturated)
        type(rgb_color_type), intent(in) :: color
        real(real64), intent(in) :: amount
        type(rgb_color_type) :: saturated
        type(hsl_color_type) :: hsl

        hsl = color%to_hsl()
        hsl%saturation = min(100.0_real64, hsl%saturation + amount)
        saturated = hsl%to_rgb()
    end function saturate

    !> Decrease saturation
    function desaturate(color, amount) result(desaturated)
        type(rgb_color_type), intent(in) :: color
        real(real64), intent(in) :: amount
        type(rgb_color_type) :: desaturated
        type(hsl_color_type) :: hsl

        hsl = color%to_hsl()
        hsl%saturation = max(0.0_real64, hsl%saturation - amount)
        desaturated = hsl%to_rgb()
    end function desaturate

    !> Get named color
    pure function get_named_color(color_id) result(color)
        integer, intent(in) :: color_id
        type(rgb_color_type) :: color

        select case (color_id)
            case (COLOR_BLACK)
                color = rgb_from_values(0, 0, 0)
            case (COLOR_WHITE)
                color = rgb_from_values(255, 255, 255)
            case (COLOR_RED)
                color = rgb_from_values(255, 0, 0)
            case (COLOR_GREEN)
                color = rgb_from_values(0, 255, 0)
            case (COLOR_BLUE)
                color = rgb_from_values(0, 0, 255)
            case (COLOR_YELLOW)
                color = rgb_from_values(255, 255, 0)
            case (COLOR_CYAN)
                color = rgb_from_values(0, 255, 255)
            case (COLOR_MAGENTA)
                color = rgb_from_values(255, 0, 255)
            case default
                color%valid = .false.
        end select
    end function get_named_color

    !> Calculate Euclidean distance between colors
    pure function color_distance(color1, color2) result(distance)
        type(rgb_color_type), intent(in) :: color1, color2
        real(real64) :: distance
        real(real64) :: dr, dg, db

        dr = real(color1%red - color2%red, real64)
        dg = real(color1%green - color2%green, real64)
        db = real(color1%blue - color2%blue, real64)

        distance = sqrt(dr*dr + dg*dg + db*db)
    end function color_distance

end module safe_color
