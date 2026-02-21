\ SPDX-License-Identifier: PMPL-1.0-or-later
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-COLOR - Safe color representation for Forth
\ Stack-based color operations with validation

\ ============================================================
\ Color Storage
\ ============================================================

\ RGB components (0-255)
variable color-red
variable color-green
variable color-blue
variable color-alpha    \ 0-255 (255 = fully opaque)

\ HSL components (scaled: H=0-3600, S=0-1000, L=0-1000)
variable color-hue
variable color-sat
variable color-light

\ Output buffer
32 constant COLOR-OUT-MAX
create color-out-buffer COLOR-OUT-MAX allot
variable color-out-pos

: color-out-reset ( -- ) 0 color-out-pos ! ;
: color-out-emit ( c -- ) color-out-buffer color-out-pos @ + c! 1 color-out-pos +! ;
: color-out-result ( -- addr len ) color-out-buffer color-out-pos @ ;

\ ============================================================
\ Named Colors (Web Colors)
\ ============================================================

\ Common colors as RGB values (packed as R*65536 + G*256 + B)
16711680 constant COLOR-RED        \ #FF0000
65280 constant COLOR-GREEN         \ #00FF00
255 constant COLOR-BLUE            \ #0000FF
16777215 constant COLOR-WHITE      \ #FFFFFF
0 constant COLOR-BLACK             \ #000000
16776960 constant COLOR-YELLOW     \ #FFFF00
16711935 constant COLOR-MAGENTA    \ #FF00FF
65535 constant COLOR-CYAN          \ #00FFFF
8421504 constant COLOR-GRAY        \ #808080
16753920 constant COLOR-ORANGE     \ #FFA500

\ ============================================================
\ RGB Operations
\ ============================================================

\ Set RGB components
\ ( r g b -- )
: color-set-rgb ( r g b -- )
    0 255 clamp color-blue !
    0 255 clamp color-green !
    0 255 clamp color-red !
    255 color-alpha !
;

\ Set RGBA components
\ ( r g b a -- )
: color-set-rgba ( r g b a -- )
    0 255 clamp color-alpha !
    0 255 clamp color-blue !
    0 255 clamp color-green !
    0 255 clamp color-red !
;

\ Get RGB components
\ ( -- r g b )
: color-get-rgb ( -- r g b )
    color-red @ color-green @ color-blue @
;

\ Get RGBA components
\ ( -- r g b a )
: color-get-rgba ( -- r g b a )
    color-red @ color-green @ color-blue @ color-alpha @
;

\ Set from packed RGB value
\ ( packed -- )
: color-from-packed ( packed -- )
    dup 255 and color-blue !
    dup 8 rshift 255 and color-green !
    16 rshift 255 and color-red !
    255 color-alpha !
;

\ Get packed RGB value
\ ( -- packed )
: color-to-packed ( -- packed )
    color-red @ 16 lshift
    color-green @ 8 lshift or
    color-blue @ or
;

\ ============================================================
\ RGB Validation
\ ============================================================

\ Check if RGB values are valid
\ ( r g b -- flag )
: valid-rgb? ( r g b -- flag )
    0 255 in-range? -rot
    0 255 in-range? -rot
    0 255 in-range?
    and and
;

\ ============================================================
\ Color Mixing
\ ============================================================

\ Mix two colors with weight (weight 0-100 for first color)
\ ( r1 g1 b1 r2 g2 b2 weight -- r g b )
: color-mix ( r1 g1 b1 r2 g2 b2 weight -- r g b )
    dup 100 > if drop 100 then
    dup 0< if drop 0 then
    >r
    \ Blue
    swap r@ 100 swap - * swap r@ * + 100 /
    >r
    \ Green
    swap r@ 100 swap - * swap r@ * + 100 /
    >r
    \ Red
    r> r> r> swap
    3 roll r@ 100 swap - * 3 roll r> * + 100 /
    -rot
;

\ Blend with alpha
\ ( r1 g1 b1 a1 r2 g2 b2 -- r g b )
: color-blend ( r1 g1 b1 a1 r2 g2 b2 -- r g b )
    \ Simplified alpha blend using first color's alpha
    4 pick 100 * 255 / color-mix
;

\ ============================================================
\ Color Transformations
\ ============================================================

\ Lighten color by percentage
\ ( percent -- )
: color-lighten ( percent -- )
    100 min 0 max
    255 color-red @ - over * 100 / color-red @ + color-red !
    255 color-green @ - over * 100 / color-green @ + color-green !
    255 color-blue @ - swap * 100 / color-blue @ + color-blue !
;

\ Darken color by percentage
\ ( percent -- )
: color-darken ( percent -- )
    100 min 0 max
    color-red @ over * 100 / color-red @ swap - color-red !
    color-green @ over * 100 / color-green @ swap - color-green !
    color-blue @ swap * 100 / color-blue @ swap - color-blue !
;

\ Invert color
: color-invert ( -- )
    255 color-red @ - color-red !
    255 color-green @ - color-green !
    255 color-blue @ - color-blue !
;

\ Convert to grayscale (luminance method)
: color-grayscale ( -- )
    color-red @ 299 *
    color-green @ 587 * +
    color-blue @ 114 * +
    1000 /
    dup color-red !
    dup color-green !
    color-blue !
;

\ ============================================================
\ Hex Parsing and Formatting
\ ============================================================

\ Parse hex digit
: color-hex-digit ( c -- n flag )
    dup [char] 0 >= over [char] 9 <= and if
        [char] 0 - true exit
    then
    dup [char] a >= over [char] f <= and if
        [char] a - 10 + true exit
    then
    dup [char] A >= over [char] F <= and if
        [char] A - 10 + true exit
    then
    drop 0 false
;

\ Emit hex digit
: color-emit-hex ( n -- )
    15 and
    dup 10 < if [char] 0 + else 10 - [char] A + then
    color-out-emit
;

\ Parse hex color string (#RGB or #RRGGBB)
\ ( addr len -- flag )
: color-parse-hex ( addr len -- flag )
    dup 4 = if                  \ #RGB format
        over c@ [char] # <> if 2drop false exit then
        over 1+ c@ color-hex-digit 0= if 2drop false exit then
        dup 16 * over + color-red !
        over 2 + c@ color-hex-digit 0= if 2drop false exit then
        dup 16 * over + color-green !
        over 3 + c@ color-hex-digit 0= if 2drop false exit then
        dup 16 * over + color-blue !
        2drop 255 color-alpha ! true exit
    then
    dup 7 = if                  \ #RRGGBB format
        over c@ [char] # <> if 2drop false exit then
        over 1+ c@ color-hex-digit 0= if 2drop false exit then
        16 * over 2 + c@ color-hex-digit 0= if 2drop false exit then
        + color-red !
        over 3 + c@ color-hex-digit 0= if 2drop false exit then
        16 * over 4 + c@ color-hex-digit 0= if 2drop false exit then
        + color-green !
        over 5 + c@ color-hex-digit 0= if 2drop false exit then
        16 * over 6 + c@ color-hex-digit 0= if 2drop false exit then
        + color-blue !
        2drop 255 color-alpha ! true exit
    then
    2drop false
;

\ Format color as #RRGGBB
\ ( -- addr len )
: color-format-hex ( -- addr len )
    color-out-reset
    [char] # color-out-emit
    color-red @ dup 4 rshift color-emit-hex 15 and color-emit-hex
    color-green @ dup 4 rshift color-emit-hex 15 and color-emit-hex
    color-blue @ dup 4 rshift color-emit-hex 15 and color-emit-hex
    color-out-result
;

\ ============================================================
\ Color Properties
\ ============================================================

\ Calculate luminance (0-255)
\ ( -- luminance )
: color-luminance ( -- luminance )
    color-red @ 299 *
    color-green @ 587 * +
    color-blue @ 114 * +
    1000 /
;

\ Check if color is light (luminance > 127)
\ ( -- flag )
: color-light? ( -- flag )
    color-luminance 127 >
;

\ Check if color is dark
\ ( -- flag )
: color-dark? ( -- flag )
    color-luminance 128 <
;

\ Get contrasting color (black or white)
\ ( -- r g b )
: color-contrast ( -- r g b )
    color-light? if 0 0 0 else 255 255 255 then
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-color-version ( -- ) ." SAFE-COLOR for Forth v0.4.0" cr ;

: safe-color-help ( -- )
    cr
    ." SAFE-COLOR - Color Representation" cr
    ." ==================================" cr
    cr
    ." RGB Operations:" cr
    ."   color-set-rgb   ( r g b -- )" cr
    ."   color-set-rgba  ( r g b a -- )" cr
    ."   color-get-rgb   ( -- r g b )" cr
    ."   color-get-rgba  ( -- r g b a )" cr
    ."   valid-rgb?      ( r g b -- flag )" cr
    cr
    ." Packed RGB:" cr
    ."   color-from-packed ( packed -- )" cr
    ."   color-to-packed   ( -- packed )" cr
    cr
    ." Named colors: COLOR-RED COLOR-GREEN COLOR-BLUE COLOR-WHITE" cr
    ."               COLOR-BLACK COLOR-YELLOW COLOR-MAGENTA COLOR-CYAN" cr
    cr
    ." Mixing:" cr
    ."   color-mix   ( r1 g1 b1 r2 g2 b2 weight -- r g b )" cr
    ."   color-blend ( r1 g1 b1 a1 r2 g2 b2 -- r g b )" cr
    cr
    ." Transformations:" cr
    ."   color-lighten color-darken color-invert color-grayscale" cr
    cr
    ." Hex Format:" cr
    ."   color-parse-hex  ( addr len -- flag )" cr
    ."   color-format-hex ( -- addr len )" cr
    cr
    ." Properties:" cr
    ."   color-luminance color-light? color-dark? color-contrast" cr
    cr
;
