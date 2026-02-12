\ SPDX-License-Identifier: Apache-2.0
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-ANGLE - Safe angular measurement operations for Forth
\ Stack-based angle operations with normalization

\ ============================================================
\ Constants
\ ============================================================

\ Use scaled integers for precision (angles * 1000)
1000 constant ANGLE-SCALE

\ Full circle values (scaled)
360000 constant DEGREES-FULL    \ 360 * 1000
6283185 constant RADIANS-FULL   \ 2*PI * 1000000 (using different scale)
400000 constant GRADIANS-FULL   \ 400 * 1000

\ Pi approximations (scaled by 1000000)
3141592 constant PI-SCALED
1570796 constant HALF-PI-SCALED
6283185 constant TWO-PI-SCALED

\ ============================================================
\ Angle Storage
\ ============================================================

variable angle-value    \ Stored in millidegrees
variable angle-unit     \ 0=degrees, 1=radians, 2=gradians

\ Unit constants
0 constant UNIT-DEGREES
1 constant UNIT-RADIANS
2 constant UNIT-GRADIANS

\ ============================================================
\ Normalization
\ ============================================================

\ Normalize angle to [0, 360) degrees (scaled)
\ ( millidegrees -- normalized )
: angle-normalize-deg ( millidegrees -- normalized )
    begin dup 0< while DEGREES-FULL + repeat
    begin dup DEGREES-FULL >= while DEGREES-FULL - repeat
;

\ Normalize angle to [-180, 180) degrees (scaled)
\ ( millidegrees -- normalized )
: angle-normalize-signed ( millidegrees -- normalized )
    angle-normalize-deg
    dup 180000 >= if DEGREES-FULL - then
;

\ ============================================================
\ Conversion Between Units
\ ============================================================

\ Convert degrees to radians (both scaled)
\ degrees * PI / 180 (using integer math)
\ ( millidegrees -- milliradians )
: deg>rad ( millidegrees -- milliradians )
    PI-SCALED * 180000 /
;

\ Convert radians to degrees
\ radians * 180 / PI
\ ( milliradians -- millidegrees )
: rad>deg ( milliradians -- millidegrees )
    180000 * PI-SCALED /
;

\ Convert degrees to gradians
\ degrees * 400 / 360
\ ( millidegrees -- milligradians )
: deg>grad ( millidegrees -- milligradians )
    10 * 9 /
;

\ Convert gradians to degrees
\ gradians * 360 / 400
\ ( milligradians -- millidegrees )
: grad>deg ( milligradians -- millidegrees )
    9 * 10 /
;

\ Convert radians to gradians
\ ( milliradians -- milligradians )
: rad>grad ( milliradians -- milligradians )
    rad>deg deg>grad
;

\ Convert gradians to radians
\ ( milligradians -- milliradians )
: grad>rad ( milligradians -- milliradians )
    grad>deg deg>rad
;

\ ============================================================
\ Angle Operations
\ ============================================================

\ Set angle in degrees
\ ( degrees -- )
: angle-set-deg ( degrees -- )
    ANGLE-SCALE * angle-value !
    UNIT-DEGREES angle-unit !
;

\ Set angle in millidegrees
\ ( millidegrees -- )
: angle-set-mdeg ( millidegrees -- )
    angle-value !
    UNIT-DEGREES angle-unit !
;

\ Get angle in degrees
\ ( -- degrees )
: angle-get-deg ( -- degrees )
    angle-value @ ANGLE-SCALE /
;

\ Get angle in millidegrees
\ ( -- millidegrees )
: angle-get-mdeg ( -- millidegrees )
    angle-value @
;

\ Normalize stored angle
: angle-normalize ( -- )
    angle-value @ angle-normalize-deg angle-value !
;

\ ============================================================
\ Trigonometry (Lookup Tables)
\ ============================================================

\ Sine table for 0-90 degrees (scaled by 1000)
\ Values at 0, 10, 20, 30, 40, 50, 60, 70, 80, 90 degrees
create sine-table
    0 , 174 , 342 , 500 , 643 , 766 , 866 , 940 , 985 , 1000 ,

\ Get sine approximation (linear interpolation)
\ ( millidegrees -- sine*1000 )
: angle-sin ( millidegrees -- sin*1000 )
    angle-normalize-deg
    \ Handle quadrants
    dup 180000 >= if
        180000 - recurse negate exit
    then
    dup 90000 > if
        180000 swap - recurse exit
    then
    \ Now in [0, 90000]
    10000 /                     \ Convert to 0-9 index
    dup 9 > if drop 9 then
    cells sine-table + @
;

\ Get cosine (cos(x) = sin(90-x))
\ ( millidegrees -- cos*1000 )
: angle-cos ( millidegrees -- cos*1000 )
    90000 swap - angle-sin
;

\ Get tangent (sin/cos)
\ ( millidegrees -- tan*1000 flag ) flag false if undefined
: angle-tan ( millidegrees -- tan*1000 flag )
    dup angle-cos
    dup 0= if drop drop 0 false exit then
    swap angle-sin swap
    1000 swap */
    true
;

\ ============================================================
\ Angle Arithmetic
\ ============================================================

\ Add two angles
\ ( angle1 angle2 -- sum )
: angle+ ( angle1 angle2 -- sum )
    + angle-normalize-deg
;

\ Subtract angles
\ ( angle1 angle2 -- diff )
: angle- ( angle1 angle2 -- diff )
    - angle-normalize-deg
;

\ Multiply angle by scalar
\ ( angle n -- result )
: angle* ( angle n -- result )
    * angle-normalize-deg
;

\ Divide angle by scalar
\ ( angle n -- result flag )
: angle/ ( angle n -- result flag )
    dup 0= if drop false exit then
    / angle-normalize-deg true
;

\ ============================================================
\ Angular Distance
\ ============================================================

\ Calculate shortest angular distance
\ ( angle1 angle2 -- distance )
: angle-distance ( angle1 angle2 -- distance )
    -
    dup 180000 > if 360000 swap - then
    dup -180000 < if 360000 + then
    abs
;

\ Linear interpolation between angles (shortest path)
\ ( angle1 angle2 t -- result ) t is 0-1000 (0-100%)
: angle-lerp ( angle1 angle2 t -- result )
    >r
    2dup angle-distance         \ a1 a2 dist
    -rot - angle-normalize-signed
    dup 180000 > if 360000 - then
    dup -180000 < if 360000 + then
    r> * 1000 /
    swap +
    angle-normalize-deg
;

\ ============================================================
\ Compass Directions
\ ============================================================

\ Convert angle to compass direction index (0-7 for N,NE,E,SE,S,SW,W,NW)
\ ( millidegrees -- direction )
: angle-compass ( millidegrees -- direction )
    22500 +                     \ Offset by half segment
    angle-normalize-deg
    45000 /
    7 and
;

\ Compass direction names
create compass-names
    s" N  " , s" NE " , s" E  " , s" SE " , s" S  " , s" SW " , s" W  " , s" NW " ,

\ ============================================================
\ Validation
\ ============================================================

\ Check if angle is in range
\ ( angle min max -- flag )
: angle-in-range? ( angle min max -- flag )
    rot angle-normalize-deg
    -rot 2dup <= if             \ min <= max (simple case)
        rot dup 3 pick >= swap 3 pick <= and
        nip nip
    else                        \ Wraps around (max < min)
        rot dup 3 pick >= swap 3 pick <= or
        nip nip
    then
;

\ ============================================================
\ Output Formatting
\ ============================================================

32 constant ANG-OUT-MAX
create ang-out-buffer ANG-OUT-MAX allot
variable ang-out-pos

: ang-out-reset ( -- ) 0 ang-out-pos ! ;
: ang-out-emit ( c -- ) ang-out-buffer ang-out-pos @ + c! 1 ang-out-pos +! ;
: ang-out-result ( -- addr len ) ang-out-buffer ang-out-pos @ ;

\ Emit number to angle buffer
: ang-emit-num ( n -- )
    dup 0< if [char] - ang-out-emit negate then
    dup 0= if drop [char] 0 ang-out-emit exit then
    0 swap
    begin dup 0> while 10 /mod swap rot 1+ -rot repeat drop
    0 ?do [char] 0 + ang-out-emit loop
;

\ Format angle as degrees
\ ( millidegrees -- addr len )
: angle-format-deg ( millidegrees -- addr len )
    ang-out-reset
    ANGLE-SCALE /mod
    swap                        \ int frac
    ang-emit-num
    [char] . ang-out-emit
    abs ang-emit-num
    [char] ^ ang-out-emit       \ degree symbol substitute
    ang-out-result
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-angle-version ( -- ) ." SAFE-ANGLE for Forth v0.4.0" cr ;

: safe-angle-help ( -- )
    cr
    ." SAFE-ANGLE - Angular Measurements" cr
    ." ==================================" cr
    cr
    ." All angles stored in millidegrees (degrees * 1000)" cr
    cr
    ." Setting/Getting:" cr
    ."   angle-set-deg  ( degrees -- )" cr
    ."   angle-set-mdeg ( millidegrees -- )" cr
    ."   angle-get-deg  ( -- degrees )" cr
    ."   angle-get-mdeg ( -- millidegrees )" cr
    cr
    ." Normalization:" cr
    ."   angle-normalize        \ Normalize to [0, 360)" cr
    ."   angle-normalize-deg    ( mdeg -- mdeg )" cr
    ."   angle-normalize-signed ( mdeg -- mdeg ) \ [-180, 180)" cr
    cr
    ." Conversions:" cr
    ."   deg>rad rad>deg deg>grad grad>deg rad>grad grad>rad" cr
    cr
    ." Trigonometry (approximate):" cr
    ."   angle-sin angle-cos ( mdeg -- value*1000 )" cr
    ."   angle-tan ( mdeg -- value*1000 flag )" cr
    cr
    ." Arithmetic:" cr
    ."   angle+ angle- angle* angle/" cr
    cr
    ." Distance/Interpolation:" cr
    ."   angle-distance ( a1 a2 -- dist )" cr
    ."   angle-lerp     ( a1 a2 t -- result )" cr
    cr
    ." Compass:" cr
    ."   angle-compass ( mdeg -- direction ) \ 0-7 for N,NE,E..." cr
    cr
;
