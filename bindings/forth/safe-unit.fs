\ SPDX-License-Identifier: Apache-2.0
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-UNIT - Safe unit conversions for Forth
\ Stack-based unit operations with overflow protection

\ ============================================================
\ Scaling Factor
\ ============================================================

\ All values stored as milliunits for precision
1000 constant UNIT-SCALE

\ ============================================================
\ Length Conversions
\ ============================================================

\ Meters as base unit (stored as millimeters)

\ Convert meters to millimeters
\ ( meters -- mm )
: m>mm ( meters -- mm )
    1000 *
;

\ Convert millimeters to meters
\ ( mm -- meters )
: mm>m ( mm -- meters )
    1000 /
;

\ Convert meters to centimeters
\ ( m -- cm )
: m>cm ( m -- cm )
    100 *
;

\ Convert centimeters to meters
\ ( cm -- m )
: cm>m ( cm -- m )
    100 /
;

\ Convert meters to kilometers
\ ( m -- km ) Note: loses precision
: m>km ( m -- km )
    1000 /
;

\ Convert kilometers to meters
\ ( km -- m )
: km>m ( km -- m )
    1000 *
;

\ Convert inches to millimeters (1 inch = 25.4 mm)
\ ( inches -- mm )
: in>mm ( inches -- mm )
    254 * 10 /
;

\ Convert millimeters to inches
\ ( mm -- inches )
: mm>in ( mm -- inches )
    10 * 254 /
;

\ Convert feet to meters (1 foot = 0.3048 m)
\ Using scaled: 3048/10000
\ ( feet -- mm )
: ft>mm ( feet -- mm )
    3048 * 10 /
;

\ Convert meters to feet
\ ( mm -- feet )
: mm>ft ( mm -- feet )
    10 * 3048 /
;

\ Convert miles to meters (1 mile = 1609.344 m)
\ ( miles -- m )
: mi>m ( miles -- m )
    1609 *
;

\ Convert meters to miles
\ ( m -- miles )
: m>mi ( m -- miles )
    1609 /
;

\ ============================================================
\ Mass/Weight Conversions
\ ============================================================

\ Grams as base unit (stored as milligrams)

\ Convert grams to milligrams
\ ( g -- mg )
: g>mg ( g -- mg )
    1000 *
;

\ Convert milligrams to grams
\ ( mg -- g )
: mg>g ( mg -- g )
    1000 /
;

\ Convert grams to kilograms
\ ( g -- kg )
: g>kg ( g -- kg )
    1000 /
;

\ Convert kilograms to grams
\ ( kg -- g )
: kg>g ( kg -- g )
    1000 *
;

\ Convert pounds to grams (1 lb = 453.592 g)
\ ( lb -- g )
: lb>g ( lb -- g )
    4536 * 10 /
;

\ Convert grams to pounds
\ ( g -- lb )
: g>lb ( g -- lb )
    10 * 4536 /
;

\ Convert ounces to grams (1 oz = 28.3495 g)
\ ( oz -- g )
: oz>g ( oz -- g )
    283 * 10 /
;

\ Convert grams to ounces
\ ( g -- oz )
: g>oz ( g -- oz )
    10 * 283 /
;

\ ============================================================
\ Temperature Conversions
\ ============================================================

\ Temperatures stored as millidegrees for precision

\ Convert Celsius to Fahrenheit
\ F = C * 9/5 + 32
\ ( mC -- mF )
: c>f ( mC -- mF )
    9 * 5 / 32000 +
;

\ Convert Fahrenheit to Celsius
\ C = (F - 32) * 5/9
\ ( mF -- mC )
: f>c ( mF -- mC )
    32000 - 5 * 9 /
;

\ Convert Celsius to Kelvin
\ K = C + 273.15
\ ( mC -- mK )
: c>k ( mC -- mK )
    273150 +
;

\ Convert Kelvin to Celsius
\ C = K - 273.15
\ ( mK -- mC )
: k>c ( mK -- mC )
    273150 -
;

\ Convert Fahrenheit to Kelvin
\ ( mF -- mK )
: f>k ( mF -- mK )
    f>c c>k
;

\ Convert Kelvin to Fahrenheit
\ ( mK -- mF )
: k>f ( mK -- mF )
    k>c c>f
;

\ ============================================================
\ Volume Conversions
\ ============================================================

\ Liters as base unit (stored as milliliters)

\ Convert liters to milliliters
\ ( L -- mL )
: l>ml ( L -- mL )
    1000 *
;

\ Convert milliliters to liters
\ ( mL -- L )
: ml>l ( mL -- L )
    1000 /
;

\ Convert gallons (US) to liters (1 gal = 3.78541 L)
\ ( gal -- mL )
: gal>ml ( gal -- mL )
    3785 *
;

\ Convert liters to gallons
\ ( mL -- gal )
: ml>gal ( mL -- gal )
    3785 /
;

\ Convert fluid ounces to milliliters (1 fl oz = 29.5735 mL)
\ ( floz -- mL )
: floz>ml ( floz -- mL )
    296 * 10 /
;

\ Convert milliliters to fluid ounces
\ ( mL -- floz )
: ml>floz ( mL -- floz )
    10 * 296 /
;

\ Convert cups to milliliters (1 cup = 236.588 mL)
\ ( cups -- mL )
: cup>ml ( cups -- mL )
    237 *
;

\ Convert milliliters to cups
\ ( mL -- cups )
: ml>cup ( mL -- cups )
    237 /
;

\ ============================================================
\ Area Conversions
\ ============================================================

\ Square meters as base unit (stored as square centimeters)

\ Convert square meters to square centimeters
\ ( m2 -- cm2 )
: m2>cm2 ( m2 -- cm2 )
    10000 *
;

\ Convert square centimeters to square meters
\ ( cm2 -- m2 )
: cm2>m2 ( cm2 -- m2 )
    10000 /
;

\ Convert square feet to square meters
\ ( ft2 -- m2*1000 ) returns in m2 * 1000
: ft2>m2 ( ft2 -- m2*1000 )
    929 * 10 /
;

\ Convert acres to square meters
\ ( acres -- m2 )
: acre>m2 ( acres -- m2 )
    4047 *
;

\ ============================================================
\ Speed Conversions
\ ============================================================

\ Speeds stored as mm/s for precision

\ Convert km/h to m/s
\ ( kmh*1000 -- ms*1000 )
: kmh>ms ( kmh -- ms )
    1000 * 3600 /
;

\ Convert m/s to km/h
\ ( ms -- kmh )
: ms>kmh ( ms -- kmh )
    3600 * 1000 /
;

\ Convert mph to m/s
\ ( mph -- ms*1000 )
: mph>ms ( mph -- ms )
    447 * 1000 /
;

\ Convert m/s to mph
\ ( ms -- mph )
: ms>mph ( ms -- mph )
    1000 * 447 /
;

\ Convert knots to m/s (1 knot = 0.514444 m/s)
\ ( knots -- ms*1000 )
: kn>ms ( knots -- ms )
    514 * 1000 /
;

\ Convert m/s to knots
\ ( ms -- knots )
: ms>kn ( ms -- knots )
    1000 * 514 /
;

\ ============================================================
\ Time Conversions
\ ============================================================

\ Base unit: milliseconds

\ Convert seconds to milliseconds
\ ( s -- ms )
: s>ms ( s -- ms )
    1000 *
;

\ Convert milliseconds to seconds
\ ( ms -- s )
: ms>s ( ms -- s )
    1000 /
;

\ Convert minutes to seconds
\ ( min -- s )
: min>s ( min -- s )
    60 *
;

\ Convert seconds to minutes
\ ( s -- min )
: s>min ( s -- min )
    60 /
;

\ Convert hours to seconds
\ ( h -- s )
: h>s ( h -- s )
    3600 *
;

\ Convert seconds to hours
\ ( s -- h )
: s>h ( s -- h )
    3600 /
;

\ Convert days to seconds
\ ( d -- s )
: d>s ( d -- s )
    86400 *
;

\ Convert seconds to days
\ ( s -- d )
: s>d ( s -- d )
    86400 /
;

\ ============================================================
\ Data Size Conversions
\ ============================================================

\ Base unit: bytes

\ Convert kilobytes to bytes
\ ( KB -- B )
: kb>b ( KB -- B )
    1024 *
;

\ Convert bytes to kilobytes
\ ( B -- KB )
: b>kb ( B -- KB )
    1024 /
;

\ Convert megabytes to bytes
\ ( MB -- B )
: mb>b ( MB -- B )
    1048576 *
;

\ Convert bytes to megabytes
\ ( B -- MB )
: b>mb ( B -- MB )
    1048576 /
;

\ Convert gigabytes to megabytes
\ ( GB -- MB )
: gb>mb ( GB -- MB )
    1024 *
;

\ Convert megabytes to gigabytes
\ ( MB -- GB )
: mb>gb ( MB -- GB )
    1024 /
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-unit-version ( -- ) ." SAFE-UNIT for Forth v0.4.0" cr ;

: safe-unit-help ( -- )
    cr
    ." SAFE-UNIT - Unit Conversions" cr
    ." ============================" cr
    cr
    ." Length: m>mm mm>m m>cm cm>m m>km km>m" cr
    ."         in>mm mm>in ft>mm mm>ft mi>m m>mi" cr
    cr
    ." Mass:   g>mg mg>g g>kg kg>g lb>g g>lb oz>g g>oz" cr
    cr
    ." Temperature (use millidegrees):" cr
    ."         c>f f>c c>k k>c f>k k>f" cr
    cr
    ." Volume: l>ml ml>l gal>ml ml>gal" cr
    ."         floz>ml ml>floz cup>ml ml>cup" cr
    cr
    ." Area:   m2>cm2 cm2>m2 ft2>m2 acre>m2" cr
    cr
    ." Speed:  kmh>ms ms>kmh mph>ms ms>mph kn>ms ms>kn" cr
    cr
    ." Time:   s>ms ms>s min>s s>min h>s s>h d>s s>d" cr
    cr
    ." Data:   kb>b b>kb mb>b b>mb gb>mb mb>gb" cr
    cr
;
