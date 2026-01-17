\ SPDX-License-Identifier: PMPL-1.0
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-DATETIME - Safe date and time operations for Forth
\ Stack-based temporal operations with validation

\ ============================================================
\ Constants
\ ============================================================

\ Days in months (non-leap year)
create days-in-month
    31 , 28 , 31 , 30 , 31 , 30 ,
    31 , 31 , 30 , 31 , 30 , 31 ,

\ Month names
create month-names
    s" Jan" , s" Feb" , s" Mar" , s" Apr" ,
    s" May" , s" Jun" , s" Jul" , s" Aug" ,
    s" Sep" , s" Oct" , s" Nov" , s" Dec" ,

\ Day names
create day-names
    s" Sun" , s" Mon" , s" Tue" , s" Wed" ,
    s" Thu" , s" Fri" , s" Sat" ,

\ Time constants
60 constant SECONDS-PER-MINUTE
3600 constant SECONDS-PER-HOUR
86400 constant SECONDS-PER-DAY
604800 constant SECONDS-PER-WEEK

\ ============================================================
\ Date Variables
\ ============================================================

variable date-year
variable date-month
variable date-day
variable date-hour
variable date-minute
variable date-second

\ ============================================================
\ Leap Year and Month Days
\ ============================================================

\ Check if year is a leap year
\ ( year -- flag )
: leap-year? ( year -- flag )
    dup 400 mod 0= if drop true exit then
    dup 100 mod 0= if drop false exit then
    4 mod 0=
;

\ Get days in a specific month
\ ( month year -- days )
: month-days ( month year -- days )
    swap 1-                     \ 0-indexed month
    dup 1 = if                  \ February
        drop leap-year? if 29 else 28 then
    else
        cells days-in-month + @
        nip
    then
;

\ ============================================================
\ Date Validation
\ ============================================================

\ Check if date is valid
\ ( year month day -- flag )
: valid-date? ( year month day -- flag )
    rot dup 1 9999 in-range? 0= if 2drop drop false exit then
    rot dup 1 12 in-range? 0= if 2drop drop false exit then
    rot dup 1 <= if 2drop drop false exit then
    -rot 2dup month-days          \ day year month days
    rot drop                      \ day days
    <=
;

\ Check if time is valid
\ ( hour minute second -- flag )
: valid-time? ( hour minute second -- flag )
    rot 0 23 in-range? 0= if 2drop false exit then
    swap 0 59 in-range? 0= if drop false exit then
    0 59 in-range?
;

\ ============================================================
\ Date Components
\ ============================================================

\ Store date components
\ ( year month day -- )
: set-date ( year month day -- )
    date-day ! date-month ! date-year !
;

\ Store time components
\ ( hour minute second -- )
: set-time ( hour minute second -- )
    date-second ! date-minute ! date-hour !
;

\ Get date components
\ ( -- year month day )
: get-date ( -- year month day )
    date-year @ date-month @ date-day @
;

\ Get time components
\ ( -- hour minute second )
: get-time ( -- hour minute second )
    date-hour @ date-minute @ date-second @
;

\ ============================================================
\ Day of Week (Zeller's Congruence)
\ ============================================================

\ Calculate day of week (0=Sunday, 6=Saturday)
\ ( year month day -- dow )
: day-of-week ( year month day -- dow )
    -rot                        \ day year month
    dup 3 < if
        swap 1- swap
        12 +
    then
    swap dup                    \ day month year year
    100 / swap 100 mod          \ day month century year-of-century
    dup 4 / +                   \ ... + y/4
    swap dup 4 / swap           \ ... + c/4
    2 * - +                     \ ... - 2*c
    3 pick 1+ 13 * 5 / +        \ ... + 13*(m+1)/5
    4 pick +                    \ ... + d
    7 mod
    dup 0< if 7 + then
    nip nip nip
;

\ ============================================================
\ Date Arithmetic
\ ============================================================

\ Add days to a date (simple version)
\ ( year month day n -- year' month' day' )
: add-days ( year month day n -- year' month' day' )
    -rot 2>r                    \ n year | month day
    swap 2r> rot                \ year month day n
    + \ Add to day
    begin
        2over month-days over < while
        2over month-days -      \ subtract month days
        rot 1+ dup 12 > if
            drop 1
            rot 1+ -rot
        then
        rot
    repeat
    begin
        dup 0<= while
        rot 1- dup 0= if
            drop 12
            rot 1- -rot
        then
        rot
        2over month-days +
    repeat
;

\ Subtract days from a date
\ ( year month day n -- year' month' day' )
: sub-days ( year month day n -- year' month' day' )
    negate add-days
;

\ ============================================================
\ Unix Timestamp (Simplified)
\ ============================================================

\ Days from year 1 to year Y (simplified)
\ ( year -- days )
: days-to-year ( year -- days )
    1-
    dup 365 *                   \ days from normal years
    swap dup 4 / swap           \ + leap years
    dup 100 / swap              \ - centuries
    400 /                       \ + quad-centuries
    - + +
;

\ Convert date to day number from epoch (Jan 1, 1970)
\ ( year month day -- day-number )
: date-to-days ( year month day -- day-number )
    -rot                        \ day year month
    swap dup days-to-year       \ day month year year-days
    swap 2swap                  \ year-days day month
    1 ?do
        i over month-days +
    loop
    drop
    + 1-
    swap 1970 days-to-year -
;

\ Convert date/time to Unix timestamp (seconds since epoch)
\ ( year month day hour minute second -- timestamp )
: datetime-to-unix ( year month day hour minute second -- timestamp )
    -rot                        \ year month day second hour minute
    60 * + 60 * +               \ seconds of day
    -rot 2swap rot              \ day year month seconds
    -rot date-to-days           \ seconds day-number
    SECONDS-PER-DAY * +
;

\ ============================================================
\ ISO 8601 Formatting
\ ============================================================

64 constant DT-OUT-MAX
create dt-out-buffer DT-OUT-MAX allot
variable dt-out-pos

: dt-out-reset ( -- ) 0 dt-out-pos ! ;
: dt-out-emit ( c -- ) dt-out-buffer dt-out-pos @ + c! 1 dt-out-pos +! ;
: dt-out-result ( -- addr len ) dt-out-buffer dt-out-pos @ ;

\ Emit number with padding
\ ( n width -- )
: dt-emit-padded ( n width -- )
    swap abs
    dup 0= if
        drop 0 do [char] 0 dt-out-emit loop
        exit
    then
    \ Count digits
    dup 0 begin over 0> while swap 10 / swap 1+ repeat nip
    \ Emit padding
    rot over - 0 max 0 ?do [char] 0 dt-out-emit loop
    \ Emit digits (reversed)
    swap
    begin dup 0> while
        dup 10 mod [char] 0 + swap
        10 /
    repeat
    drop
    0 ?do dt-out-emit loop
;

\ Format date as ISO 8601 (YYYY-MM-DD)
\ ( year month day -- addr len )
: format-date-iso ( year month day -- addr len )
    dt-out-reset
    rot 4 dt-emit-padded
    [char] - dt-out-emit
    swap 2 dt-emit-padded
    [char] - dt-out-emit
    2 dt-emit-padded
    dt-out-result
;

\ Format time as ISO 8601 (HH:MM:SS)
\ ( hour minute second -- addr len )
: format-time-iso ( hour minute second -- addr len )
    dt-out-reset
    rot 2 dt-emit-padded
    [char] : dt-out-emit
    swap 2 dt-emit-padded
    [char] : dt-out-emit
    2 dt-emit-padded
    dt-out-result
;

\ Format datetime as ISO 8601 (YYYY-MM-DDTHH:MM:SS)
\ ( year month day hour minute second -- addr len )
: format-datetime-iso ( year month day hour minute second -- addr len )
    dt-out-reset
    >r >r >r                    \ save time
    rot 4 dt-emit-padded
    [char] - dt-out-emit
    swap 2 dt-emit-padded
    [char] - dt-out-emit
    2 dt-emit-padded
    [char] T dt-out-emit
    r> 2 dt-emit-padded
    [char] : dt-out-emit
    r> 2 dt-emit-padded
    [char] : dt-out-emit
    r> 2 dt-emit-padded
    dt-out-result
;

\ ============================================================
\ Duration
\ ============================================================

\ Convert seconds to hours:minutes:seconds
\ ( seconds -- hours minutes seconds )
: seconds-to-hms ( seconds -- hours minutes seconds )
    dup SECONDS-PER-HOUR /      \ hours
    swap SECONDS-PER-HOUR mod   \ remaining seconds
    dup 60 /                    \ minutes
    swap 60 mod                 \ seconds
;

\ Convert hours:minutes:seconds to seconds
\ ( hours minutes seconds -- total-seconds )
: hms-to-seconds ( hours minutes seconds -- total-seconds )
    rot SECONDS-PER-HOUR *
    rot 60 * +
    +
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-datetime-version ( -- ) ." SAFE-DATETIME for Forth v0.4.0" cr ;

: safe-datetime-help ( -- )
    cr
    ." SAFE-DATETIME - Date and Time Operations" cr
    ." =========================================" cr
    cr
    ." Validation:" cr
    ."   valid-date?   ( year month day -- flag )" cr
    ."   valid-time?   ( hour minute second -- flag )" cr
    ."   leap-year?    ( year -- flag )" cr
    cr
    ." Date Arithmetic:" cr
    ."   add-days      ( year month day n -- year' month' day' )" cr
    ."   sub-days      ( year month day n -- year' month' day' )" cr
    ."   day-of-week   ( year month day -- dow ) \ 0=Sun, 6=Sat" cr
    cr
    ." Unix Timestamps:" cr
    ."   date-to-days     ( year month day -- days )" cr
    ."   datetime-to-unix ( y m d h mi s -- timestamp )" cr
    cr
    ." Formatting:" cr
    ."   format-date-iso     ( year month day -- addr len )" cr
    ."   format-time-iso     ( hour minute second -- addr len )" cr
    ."   format-datetime-iso ( y m d h mi s -- addr len )" cr
    cr
    ." Duration:" cr
    ."   seconds-to-hms ( seconds -- h m s )" cr
    ."   hms-to-seconds ( h m s -- seconds )" cr
    cr
;
