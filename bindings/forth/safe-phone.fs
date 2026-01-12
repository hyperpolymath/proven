\ SPDX-License-Identifier: AGPL-3.0-or-later
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SafePhone - Phone number validation and formatting for Forth
\ ITU-T E.164 compliant phone number handling
\ Supports international dialing codes and basic validation

\ ============================================================
\ Country Calling Code Constants
\ ============================================================

\ Major country codes (ITU-T E.164)
1 constant CC-1                 \ USA, Canada, Caribbean
7 constant CC-7                 \ Russia, Kazakhstan
20 constant CC-20               \ Egypt
27 constant CC-27               \ South Africa
30 constant CC-30               \ Greece
31 constant CC-31               \ Netherlands
32 constant CC-32               \ Belgium
33 constant CC-33               \ France
34 constant CC-34               \ Spain
36 constant CC-36               \ Hungary
39 constant CC-39               \ Italy
40 constant CC-40               \ Romania
41 constant CC-41               \ Switzerland
43 constant CC-43               \ Austria
44 constant CC-44               \ United Kingdom
45 constant CC-45               \ Denmark
46 constant CC-46               \ Sweden
47 constant CC-47               \ Norway
48 constant CC-48               \ Poland
49 constant CC-49               \ Germany
51 constant CC-51               \ Peru
52 constant CC-52               \ Mexico
53 constant CC-53               \ Cuba
54 constant CC-54               \ Argentina
55 constant CC-55               \ Brazil
56 constant CC-56               \ Chile
57 constant CC-57               \ Colombia
58 constant CC-58               \ Venezuela
60 constant CC-60               \ Malaysia
61 constant CC-61               \ Australia
62 constant CC-62               \ Indonesia
63 constant CC-63               \ Philippines
64 constant CC-64               \ New Zealand
65 constant CC-65               \ Singapore
66 constant CC-66               \ Thailand
81 constant CC-81               \ Japan
82 constant CC-82               \ South Korea
84 constant CC-84               \ Vietnam
86 constant CC-86               \ China
90 constant CC-90               \ Turkey
91 constant CC-91               \ India
92 constant CC-92               \ Pakistan
93 constant CC-93               \ Afghanistan
94 constant CC-94               \ Sri Lanka
95 constant CC-95               \ Myanmar
98 constant CC-98               \ Iran
212 constant CC-212             \ Morocco
213 constant CC-213             \ Algeria
216 constant CC-216             \ Tunisia
218 constant CC-218             \ Libya
234 constant CC-234             \ Nigeria
254 constant CC-254             \ Kenya
255 constant CC-255             \ Tanzania
256 constant CC-256             \ Uganda
351 constant CC-351             \ Portugal
352 constant CC-352             \ Luxembourg
353 constant CC-353             \ Ireland
354 constant CC-354             \ Iceland
358 constant CC-358             \ Finland
380 constant CC-380             \ Ukraine
381 constant CC-381             \ Serbia
385 constant CC-385             \ Croatia
386 constant CC-386             \ Slovenia
420 constant CC-420             \ Czech Republic
421 constant CC-421             \ Slovakia
852 constant CC-852             \ Hong Kong
853 constant CC-853             \ Macau
855 constant CC-855             \ Cambodia
856 constant CC-856             \ Laos
880 constant CC-880             \ Bangladesh
886 constant CC-886             \ Taiwan
960 constant CC-960             \ Maldives
961 constant CC-961             \ Lebanon
962 constant CC-962             \ Jordan
963 constant CC-963             \ Syria
964 constant CC-964             \ Iraq
965 constant CC-965             \ Kuwait
966 constant CC-966             \ Saudi Arabia
967 constant CC-967             \ Yemen
968 constant CC-968             \ Oman
971 constant CC-971             \ UAE
972 constant CC-972             \ Israel
973 constant CC-973             \ Bahrain
974 constant CC-974             \ Qatar
975 constant CC-975             \ Bhutan
976 constant CC-976             \ Mongolia
977 constant CC-977             \ Nepal
0 constant CC-UNKNOWN           \ Unknown

\ ============================================================
\ Phone Number Storage
\ ============================================================

\ Maximum phone number length (E.164 max is 15 digits)
15 constant PHONE-MAX-DIGITS
7 constant PHONE-MIN-DIGITS

\ Phone number buffer: country-code (cell) + national number (string)
create phone-cc variable
32 constant PHONE-NAT-MAX
create phone-national PHONE-NAT-MAX allot
variable phone-national-len

\ Output buffer for formatting
64 constant PHONE-OUT-MAX
create phone-out-buffer PHONE-OUT-MAX allot
variable phone-out-pos

: phone-out-reset ( -- ) 0 phone-out-pos ! ;
: phone-out-emit ( c -- ) phone-out-buffer phone-out-pos @ + c! 1 phone-out-pos +! ;
: phone-out-result ( -- addr len ) phone-out-buffer phone-out-pos @ ;

: phone-out-string ( addr len -- )
    0 do
        dup i + c@ phone-out-emit
    loop
    drop
;

\ ============================================================
\ Character Utilities
\ ============================================================

\ Check if character is a digit
\ ( c -- flag )
: digit-char? ( c -- flag )
    dup [char] 0 >= swap [char] 9 <= and
;

\ Convert digit character to value
\ ( c -- n )
: digit-char>value ( c -- n )
    [char] 0 -
;

\ ============================================================
\ Country Code Recognition
\ ============================================================

\ Check if number is a valid country code
\ ( n -- flag )
: valid-country-code? ( n -- flag )
    case
        1 of true endof
        7 of true endof
        20 of true endof
        27 of true endof
        30 of true endof  31 of true endof  32 of true endof
        33 of true endof  34 of true endof  36 of true endof
        39 of true endof  40 of true endof  41 of true endof
        43 of true endof  44 of true endof  45 of true endof
        46 of true endof  47 of true endof  48 of true endof
        49 of true endof  51 of true endof  52 of true endof
        53 of true endof  54 of true endof  55 of true endof
        56 of true endof  57 of true endof  58 of true endof
        60 of true endof  61 of true endof  62 of true endof
        63 of true endof  64 of true endof  65 of true endof
        66 of true endof  81 of true endof  82 of true endof
        84 of true endof  86 of true endof  90 of true endof
        91 of true endof  92 of true endof  93 of true endof
        94 of true endof  95 of true endof  98 of true endof
        212 of true endof  213 of true endof  216 of true endof
        218 of true endof  234 of true endof  254 of true endof
        255 of true endof  256 of true endof  351 of true endof
        352 of true endof  353 of true endof  354 of true endof
        358 of true endof  380 of true endof  381 of true endof
        385 of true endof  386 of true endof  420 of true endof
        421 of true endof  852 of true endof  853 of true endof
        855 of true endof  856 of true endof  880 of true endof
        886 of true endof  960 of true endof  961 of true endof
        962 of true endof  963 of true endof  964 of true endof
        965 of true endof  966 of true endof  967 of true endof
        968 of true endof  971 of true endof  972 of true endof
        973 of true endof  974 of true endof  975 of true endof
        976 of true endof  977 of true endof
        false swap
    endcase
;

\ Get country name from code
\ ( cc -- addr len )
: country-code>name ( cc -- addr len )
    case
        1 of s" North America" endof
        7 of s" Russia/Kazakhstan" endof
        33 of s" France" endof
        34 of s" Spain" endof
        39 of s" Italy" endof
        44 of s" United Kingdom" endof
        49 of s" Germany" endof
        52 of s" Mexico" endof
        55 of s" Brazil" endof
        61 of s" Australia" endof
        81 of s" Japan" endof
        82 of s" South Korea" endof
        86 of s" China" endof
        91 of s" India" endof
        s" Unknown" rot
    endcase
;

\ ============================================================
\ Phone Number Parsing
\ ============================================================

\ Extract digits only from string into phone-national buffer
\ ( addr len -- digit-count )
: extract-digits ( addr len -- digit-count )
    0 phone-national-len !
    0 do
        dup i + c@
        dup digit-char? if
            phone-national phone-national-len @ + c!
            1 phone-national-len +!
        else
            drop
        then
    loop
    drop
    phone-national-len @
;

\ Try to parse country code from start of digit string
\ Returns: country-code and number of digits consumed
\ ( -- cc digits-consumed flag )
: try-parse-cc ( -- cc digits-consumed flag )
    \ Try 3-digit code first
    phone-national-len @ 3 >= if
        phone-national c@ digit-char>value 100 *
        phone-national 1+ c@ digit-char>value 10 * +
        phone-national 2 + c@ digit-char>value +
        dup valid-country-code? if
            3 true exit
        then
        drop
    then
    \ Try 2-digit code
    phone-national-len @ 2 >= if
        phone-national c@ digit-char>value 10 *
        phone-national 1+ c@ digit-char>value +
        dup valid-country-code? if
            2 true exit
        then
        drop
    then
    \ Try 1-digit code
    phone-national-len @ 1 >= if
        phone-national c@ digit-char>value
        dup valid-country-code? if
            1 true exit
        then
        drop
    then
    0 0 false
;

\ Parse phone number from E.164 format (+CCNNNNN...)
\ ( addr len -- flag ) stores result in phone-cc and phone-national
: phone-parse ( addr len -- flag )
    dup 0= if 2drop false exit then

    \ Check for + prefix
    over c@ [char] + = if
        1- swap 1+ swap             \ skip the +
    then

    \ Extract digits
    extract-digits

    \ Validate length
    dup PHONE-MIN-DIGITS < if drop false exit then
    dup PHONE-MAX-DIGITS > if drop false exit then
    drop

    \ Try to parse country code
    try-parse-cc 0= if 2drop false exit then

    \ Store country code
    swap phone-cc !

    \ Shift national number (remove country code digits)
    phone-national over + phone-national
    phone-national-len @ rot - dup phone-national-len !
    cmove

    \ Validate national number length
    phone-national-len @ 4 < if false exit then

    true
;

\ ============================================================
\ Phone Number Formatting
\ ============================================================

\ Format phone number in E.164 format (+CCNNNNN...)
\ ( -- addr len ) uses stored phone number
: phone-format-e164 ( -- addr len )
    phone-out-reset
    [char] + phone-out-emit
    \ Emit country code
    phone-cc @
    dup 100 >= if
        dup 100 / [char] 0 + phone-out-emit
        10 mod dup 10 >= if
            10 / [char] 0 + phone-out-emit
            10 mod
        then
    else dup 10 >= if
        dup 10 / [char] 0 + phone-out-emit
        10 mod
    then then
    [char] 0 + phone-out-emit
    \ Emit national number
    phone-national phone-national-len @ phone-out-string
    phone-out-result
;

\ Format phone number with spaces (international style)
\ ( -- addr len )
: phone-format-intl ( -- addr len )
    phone-out-reset
    [char] + phone-out-emit
    \ Emit country code
    phone-cc @
    dup 100 >= if
        dup 100 / [char] 0 + phone-out-emit
        10 mod dup 10 >= if
            10 / [char] 0 + phone-out-emit
            10 mod
        then
    else dup 10 >= if
        dup 10 / [char] 0 + phone-out-emit
        10 mod
    then then
    [char] 0 + phone-out-emit
    [char] space phone-out-emit
    \ Format national number with spaces
    phone-national-len @ dup 7 <= if
        \ Short: XXX XXXX
        3 min 0 do phone-national i + c@ phone-out-emit loop
        [char] space phone-out-emit
        phone-national-len @ 3 - 0 max 0 do
            phone-national 3 i + + c@ phone-out-emit
        loop
    else dup 10 <= if
        \ Medium: XXX XXX XXXX
        drop
        3 0 do phone-national i + c@ phone-out-emit loop
        [char] space phone-out-emit
        3 0 do phone-national 3 i + + c@ phone-out-emit loop
        [char] space phone-out-emit
        phone-national-len @ 6 - 0 do
            phone-national 6 i + + c@ phone-out-emit
        loop
    else
        \ Long: just space every 4 digits
        drop
        phone-national-len @ 0 do
            phone-national i + c@ phone-out-emit
            i 1+ 4 mod 0= i 1+ phone-national-len @ < and if
                [char] space phone-out-emit
            then
        loop
    then then
    phone-out-result
;

\ Format phone number as stored (digits only, no +)
\ ( -- addr len )
: phone-format-digits ( -- addr len )
    phone-out-reset
    \ Emit country code
    phone-cc @
    dup 100 >= if
        dup 100 / [char] 0 + phone-out-emit
        10 mod dup 10 >= if
            10 / [char] 0 + phone-out-emit
            10 mod
        then
    else dup 10 >= if
        dup 10 / [char] 0 + phone-out-emit
        10 mod
    then then
    [char] 0 + phone-out-emit
    phone-national phone-national-len @ phone-out-string
    phone-out-result
;

\ ============================================================
\ Phone Number Validation
\ ============================================================

\ Check if string is valid phone number
\ ( addr len -- flag )
: phone-valid? ( addr len -- flag )
    phone-parse
;

\ Quick check if string looks like phone number
\ ( addr len -- flag )
: phone? ( addr len -- flag )
    dup PHONE-MIN-DIGITS < if 2drop false exit then
    dup PHONE-MAX-DIGITS 5 + > if 2drop false exit then  \ allow for formatting
    \ Check for digits (allow +, spaces, hyphens, parens)
    true -rot                       \ flag addr len
    0 do
        over i + c@
        dup digit-char? swap
        dup [char] + = swap
        dup [char] space = swap
        dup [char] - = swap
        dup [char] ( = swap
        [char] ) =
        or or or or or 0= if
            rot drop false -rot
            leave
        then
    loop
    2drop
;

\ ============================================================
\ Phone Number Properties
\ ============================================================

\ Get country code from parsed phone number
\ ( -- cc )
: phone-get-cc ( -- cc )
    phone-cc @
;

\ Get national number from parsed phone number
\ ( -- addr len )
: phone-get-national ( -- addr len )
    phone-national phone-national-len @
;

\ Get total length of phone number in digits
\ ( -- n )
: phone-digit-count ( -- n )
    phone-cc @
    dup 100 >= if 3 else dup 10 >= if 2 else 1 then then
    nip
    phone-national-len @ +
;

\ ============================================================
\ Phone Number Type Detection (basic)
\ ============================================================

0 constant PHONE-TYPE-UNKNOWN
1 constant PHONE-TYPE-MOBILE
2 constant PHONE-TYPE-FIXED
3 constant PHONE-TYPE-TOLLFREE
4 constant PHONE-TYPE-PREMIUM

\ Basic type detection (very simplified, country-specific rules needed)
\ ( -- type )
: phone-detect-type ( -- type )
    phone-national-len @ 0= if PHONE-TYPE-UNKNOWN exit then
    phone-national c@
    \ US/Canada toll-free starts with 8
    phone-cc @ 1 = if
        dup [char] 8 = if drop PHONE-TYPE-TOLLFREE exit then
        drop PHONE-TYPE-UNKNOWN exit
    then
    \ UK mobile starts with 7
    phone-cc @ 44 = if
        dup [char] 7 = if drop PHONE-TYPE-MOBILE exit then
        dup [char] 2 = if drop PHONE-TYPE-FIXED exit then
        drop PHONE-TYPE-UNKNOWN exit
    then
    drop PHONE-TYPE-UNKNOWN
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-phone-version ( -- ) ." SafePhone for Forth v0.1.0" cr ;

: safe-phone-help ( -- )
    cr
    ." SafePhone - Phone Number Operations" cr
    ." ====================================" cr
    cr
    ." Country Codes:" cr
    ."   CC-1 (N.America) CC-44 (UK) CC-49 (Germany) CC-86 (China) ..." cr
    cr
    ." Parsing:" cr
    ."   phone-parse   ( addr len -- flag )  Parse E.164 phone number" cr
    ."   phone-valid?  ( addr len -- flag )  Check if valid phone" cr
    ."   phone?        ( addr len -- flag )  Quick format check" cr
    cr
    ." Formatting:" cr
    ."   phone-format-e164    ( -- addr len )  Format as +CCNNN..." cr
    ."   phone-format-intl    ( -- addr len )  Format with spaces" cr
    ."   phone-format-digits  ( -- addr len )  Digits only" cr
    cr
    ." Properties:" cr
    ."   phone-get-cc        ( -- cc )        Get country code" cr
    ."   phone-get-national  ( -- addr len )  Get national number" cr
    ."   phone-digit-count   ( -- n )         Total digit count" cr
    ."   phone-detect-type   ( -- type )      Detect number type" cr
    cr
    ." Type Constants:" cr
    ."   PHONE-TYPE-UNKNOWN PHONE-TYPE-MOBILE PHONE-TYPE-FIXED" cr
    ."   PHONE-TYPE-TOLLFREE PHONE-TYPE-PREMIUM" cr
    cr
;
