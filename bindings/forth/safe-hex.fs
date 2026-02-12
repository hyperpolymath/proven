\ SPDX-License-Identifier: Apache-2.0
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SafeHex - Hexadecimal encoding and decoding for Forth
\ Provides validated hex operations with constant-time comparison
\ for cryptographic safety

\ ============================================================
\ Constants and Buffers
\ ============================================================

256 constant HEX-BUFFER-MAX
create hex-encode-buffer HEX-BUFFER-MAX allot
create hex-decode-buffer HEX-BUFFER-MAX 2 / allot
variable hex-encode-pos
variable hex-decode-pos

\ ============================================================
\ Hex Character Utilities
\ ============================================================

\ Check if character is valid hex digit (0-9, a-f, A-F)
\ ( c -- flag )
: hex-char? ( c -- flag )
    dup [char] 0 >= over [char] 9 <= and if drop true exit then
    dup [char] a >= over [char] f <= and if drop true exit then
    dup [char] A >= over [char] F <= and if drop true exit then
    drop false
;

\ Convert hex character to nibble value (0-15)
\ ( c -- n flag ) flag is true if valid
: hex-char>nibble ( c -- n flag )
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

\ Convert nibble (0-15) to lowercase hex character
\ ( n -- c )
: nibble>hex ( n -- c )
    dup 10 < if
        [char] 0 +
    else
        10 - [char] a +
    then
;

\ Convert nibble (0-15) to uppercase hex character
\ ( n -- c )
: nibble>hex-upper ( n -- c )
    dup 10 < if
        [char] 0 +
    else
        10 - [char] A +
    then
;

\ ============================================================
\ Hex Encoding
\ ============================================================

\ Reset encode buffer
: hex-encode-reset ( -- ) 0 hex-encode-pos ! ;

\ Emit character to encode buffer
: hex-encode-emit ( c -- )
    hex-encode-buffer hex-encode-pos @ + c!
    1 hex-encode-pos +!
;

\ Get encode result
: hex-encode-result ( -- addr len )
    hex-encode-buffer hex-encode-pos @
;

\ Encode single byte to two hex characters (lowercase)
\ ( byte -- )
: hex-encode-byte ( byte -- )
    dup 4 rshift $0F and nibble>hex hex-encode-emit
    $0F and nibble>hex hex-encode-emit
;

\ Encode single byte to two hex characters (uppercase)
\ ( byte -- )
: hex-encode-byte-upper ( byte -- )
    dup 4 rshift $0F and nibble>hex-upper hex-encode-emit
    $0F and nibble>hex-upper hex-encode-emit
;

\ Encode byte array to hex string (lowercase)
\ ( addr len -- result-addr result-len )
: hex-encode ( addr len -- result-addr result-len )
    hex-encode-reset
    0 do
        dup i + c@ hex-encode-byte
    loop
    drop
    hex-encode-result
;

\ Encode byte array to hex string (uppercase)
\ ( addr len -- result-addr result-len )
: hex-encode-upper ( addr len -- result-addr result-len )
    hex-encode-reset
    0 do
        dup i + c@ hex-encode-byte-upper
    loop
    drop
    hex-encode-result
;

\ ============================================================
\ Hex Decoding
\ ============================================================

\ Reset decode buffer
: hex-decode-reset ( -- ) 0 hex-decode-pos ! ;

\ Emit byte to decode buffer
: hex-decode-emit ( byte -- )
    hex-decode-buffer hex-decode-pos @ + c!
    1 hex-decode-pos +!
;

\ Get decode result
: hex-decode-result ( -- addr len )
    hex-decode-buffer hex-decode-pos @
;

\ Decode two hex characters to a byte
\ ( c1 c2 -- byte flag ) flag is true if valid
: hex-decode-pair ( c1 c2 -- byte flag )
    hex-char>nibble if              \ c1 n2
        swap hex-char>nibble if     \ n2 n1
            4 lshift or true        \ byte true
        else
            2drop 0 false
        then
    else
        drop drop 0 false
    then
;

\ Decode hex string to bytes
\ ( addr len -- result-addr result-len flag ) flag is true if valid
: hex-decode ( addr len -- result-addr result-len flag )
    hex-decode-reset
    \ Check for even length
    dup 2 mod 0<> if
        2drop hex-decode-result false exit
    then
    \ Decode pairs
    2 / 0 do
        over i 2 * + c@             \ first char
        2 pick i 2 * 1+ + c@        \ second char
        hex-decode-pair 0= if
            2drop hex-decode-result false unloop exit
        then
        hex-decode-emit
    loop
    drop
    hex-decode-result true
;

\ ============================================================
\ Hex String Validation
\ ============================================================

\ Check if string contains only valid hex characters
\ ( addr len -- flag )
: hex-valid? ( addr len -- flag )
    dup 0= if 2drop false exit then
    true -rot                       \ flag addr len
    0 do
        over i + c@ hex-char? 0= if
            rot drop false -rot
            leave
        then
    loop
    2drop
;

\ Check if string is valid hex with even length (can decode to bytes)
\ ( addr len -- flag )
: hex-bytes-valid? ( addr len -- flag )
    dup 2 mod 0<> if 2drop false exit then
    hex-valid?
;

\ ============================================================
\ Constant-Time Comparison
\ ============================================================

\ Constant-time comparison of two byte arrays
\ Resistant to timing attacks - always compares all bytes
\ ( addr1 addr2 len -- flag )
: constant-time-equal ( addr1 addr2 len -- flag )
    0 -rot                          \ diff addr1 addr2 len
    0 do
        2dup i + c@ swap i + c@     \ diff addr1 addr2 byte1 byte2
        xor                         \ diff addr1 addr2 xor-result
        3 roll or                   \ addr1 addr2 new-diff
        -rot                        \ diff addr1 addr2
    loop
    2drop 0=
;

\ Constant-time comparison of two hex strings
\ ( addr1 len1 addr2 len2 -- flag )
: hex-constant-time-equal ( addr1 len1 addr2 len2 -- flag )
    \ First check lengths match (not constant-time, but length is usually not secret)
    2 pick <> if 2drop drop false exit then
    \ Now compare bytes
    swap drop                       \ addr1 len1 addr2
    -rot constant-time-equal
;

\ ============================================================
\ Hex Formatting Utilities
\ ============================================================

\ Format hex with spaces between bytes (e.g., "01 02 03")
\ ( addr len -- result-addr result-len )
: hex-format-spaced ( addr len -- result-addr result-len )
    hex-encode-reset
    0 do
        dup i + c@ hex-encode-byte
        i 1+ dup 3 pick < swap 0<> and if
            [char] space hex-encode-emit
        then
    loop
    drop
    hex-encode-result
;

\ Format hex with colons between bytes (e.g., "01:02:03")
\ ( addr len -- result-addr result-len )
: hex-format-colons ( addr len -- result-addr result-len )
    hex-encode-reset
    0 do
        dup i + c@ hex-encode-byte
        i 1+ dup 3 pick < swap 0<> and if
            [char] : hex-encode-emit
        then
    loop
    drop
    hex-encode-result
;

\ Format hex with 0x prefix
\ ( addr len -- result-addr result-len )
: hex-format-0x ( addr len -- result-addr result-len )
    hex-encode-reset
    s" 0x" 0 do dup i + c@ hex-encode-emit loop drop
    0 do
        dup i + c@ hex-encode-byte
    loop
    drop
    hex-encode-result
;

\ ============================================================
\ Hex String Operations
\ ============================================================

\ Convert hex string to lowercase (in place)
\ ( addr len -- )
: hex-to-lower ( addr len -- )
    0 do
        dup i + c@
        dup [char] A >= over [char] F <= and if
            32 +                    \ convert to lowercase
            over i + c!
        else
            drop
        then
    loop
    drop
;

\ Convert hex string to uppercase (in place)
\ ( addr len -- )
: hex-to-upper ( addr len -- )
    0 do
        dup i + c@
        dup [char] a >= over [char] f <= and if
            32 -                    \ convert to uppercase
            over i + c!
        else
            drop
        then
    loop
    drop
;

\ Compare two hex strings (case-insensitive)
\ ( addr1 len1 addr2 len2 -- flag )
: hex-equal? ( addr1 len1 addr2 len2 -- flag )
    rot over <> if 2drop drop false exit then
    \ Same length, compare
    swap 0 do                       \ addr1 addr2
        over i + c@ dup [char] a >= over [char] f <= and if 32 - then
        over i + c@ dup [char] a >= over [char] f <= and if 32 - then
        <> if 2drop false unloop exit then
    loop
    2drop true
;

\ ============================================================
\ Integer Conversion
\ ============================================================

\ Convert unsigned integer to hex string
\ ( u -- addr len )
: int>hex ( u -- addr len )
    hex-encode-reset
    dup 0= if
        drop
        [char] 0 hex-encode-emit
        hex-encode-result exit
    then
    \ Count hex digits needed
    dup 0                           \ u u count
    begin over 0> while
        swap 16 / swap 1+
    repeat
    nip                             \ u count
    \ Emit in reverse order to buffer, then reverse
    swap                            \ count u
    begin dup 0> while
        dup $F and nibble>hex hex-encode-emit
        16 /
    repeat
    drop
    \ Reverse the buffer
    hex-encode-buffer hex-encode-pos @ 2 / 0 do
        hex-encode-buffer i + c@
        hex-encode-buffer hex-encode-pos @ 1- i - + c@
        hex-encode-buffer i + c!
        hex-encode-buffer hex-encode-pos @ 1- i - + c!
    loop
    hex-encode-result
;

\ Convert hex string to unsigned integer
\ ( addr len -- u flag ) flag is true if valid
: hex>int ( addr len -- u flag )
    dup 0= if 2drop 0 false exit then
    0 -rot                          \ result addr len
    0 do
        over i + c@ hex-char>nibble 0= if
            2drop drop 0 false unloop exit
        then
        rot 16 * + -rot             \ accumulate
    loop
    2drop true
;

\ ============================================================
\ XOR Operations
\ ============================================================

\ XOR two byte arrays of same length
\ ( addr1 addr2 len dest -- )
: hex-xor-bytes ( addr1 addr2 len dest -- )
    swap 0 do                       \ addr1 addr2 dest
        2 pick i + c@               \ get byte from addr1
        2 pick i + c@               \ get byte from addr2
        xor                         \ xor them
        over i + c!                 \ store in dest
    loop
    drop 2drop
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-hex-version ( -- ) ." SafeHex for Forth v0.1.0" cr ;

: safe-hex-help ( -- )
    cr
    ." SafeHex - Hexadecimal Operations" cr
    ." =================================" cr
    cr
    ." Encoding:" cr
    ."   hex-encode       ( addr len -- addr len )  Encode bytes to hex" cr
    ."   hex-encode-upper ( addr len -- addr len )  Encode to uppercase" cr
    cr
    ." Decoding:" cr
    ."   hex-decode       ( addr len -- addr len flag )  Decode hex to bytes" cr
    cr
    ." Validation:" cr
    ."   hex-valid?       ( addr len -- flag )  Check valid hex chars" cr
    ."   hex-bytes-valid? ( addr len -- flag )  Check valid + even length" cr
    ."   hex-char?        ( c -- flag )         Check single hex char" cr
    cr
    ." Constant-Time Comparison:" cr
    ."   constant-time-equal     ( a1 a2 len -- flag )     Compare bytes" cr
    ."   hex-constant-time-equal ( a1 l1 a2 l2 -- flag )   Compare hex strings" cr
    cr
    ." Formatting:" cr
    ."   hex-format-spaced  ( addr len -- addr len )  With spaces" cr
    ."   hex-format-colons  ( addr len -- addr len )  With colons" cr
    ."   hex-format-0x      ( addr len -- addr len )  With 0x prefix" cr
    cr
    ." String Operations:" cr
    ."   hex-to-lower   ( addr len -- )           Convert to lowercase" cr
    ."   hex-to-upper   ( addr len -- )           Convert to uppercase" cr
    ."   hex-equal?     ( a1 l1 a2 l2 -- flag )   Case-insensitive compare" cr
    cr
    ." Integer Conversion:" cr
    ."   int>hex   ( u -- addr len )        Unsigned to hex" cr
    ."   hex>int   ( addr len -- u flag )   Hex to unsigned" cr
    cr
    ." Low-level:" cr
    ."   hex-char>nibble   ( c -- n flag )  Char to 0-15" cr
    ."   nibble>hex        ( n -- c )       0-15 to lowercase char" cr
    ."   nibble>hex-upper  ( n -- c )       0-15 to uppercase char" cr
    ."   hex-xor-bytes     ( a1 a2 len dest -- )  XOR byte arrays" cr
    cr
;
