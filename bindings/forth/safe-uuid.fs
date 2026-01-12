\ SPDX-License-Identifier: AGPL-3.0-or-later
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SafeUUID - UUID generation and validation for Forth
\ RFC 4122 compliant UUID parsing, formatting, and validation
\ Uses 16-byte buffer for UUID storage

\ ============================================================
\ UUID Storage and Constants
\ ============================================================

16 constant UUID-BYTES
36 constant UUID-STRING-LEN
45 constant UUID-URN-LEN       \ urn:uuid: + 36 chars

\ UUID buffer for current operations
create uuid-buffer UUID-BYTES allot

\ Output buffer for formatting
64 constant UUID-OUT-MAX
create uuid-out-buffer UUID-OUT-MAX allot
variable uuid-out-pos

\ Nil UUID (all zeros)
create nil-uuid UUID-BYTES allot
nil-uuid UUID-BYTES 0 fill

\ DNS namespace UUID (6ba7b810-9dad-11d1-80b4-00c04fd430c8)
create namespace-dns UUID-BYTES allot
$6b namespace-dns 0 + c!  $a7 namespace-dns 1 + c!
$b8 namespace-dns 2 + c!  $10 namespace-dns 3 + c!
$9d namespace-dns 4 + c!  $ad namespace-dns 5 + c!
$11 namespace-dns 6 + c!  $d1 namespace-dns 7 + c!
$80 namespace-dns 8 + c!  $b4 namespace-dns 9 + c!
$00 namespace-dns 10 + c!  $c0 namespace-dns 11 + c!
$4f namespace-dns 12 + c!  $d4 namespace-dns 13 + c!
$30 namespace-dns 14 + c!  $c8 namespace-dns 15 + c!

\ URL namespace UUID (6ba7b811-9dad-11d1-80b4-00c04fd430c8)
create namespace-url UUID-BYTES allot
$6b namespace-url 0 + c!  $a7 namespace-url 1 + c!
$b8 namespace-url 2 + c!  $11 namespace-url 3 + c!
$9d namespace-url 4 + c!  $ad namespace-url 5 + c!
$11 namespace-url 6 + c!  $d1 namespace-url 7 + c!
$80 namespace-url 8 + c!  $b4 namespace-url 9 + c!
$00 namespace-url 10 + c!  $c0 namespace-url 11 + c!
$4f namespace-url 12 + c!  $d4 namespace-url 13 + c!
$30 namespace-url 14 + c!  $c8 namespace-url 15 + c!

\ ============================================================
\ Hex Character Utilities
\ ============================================================

\ Check if character is valid hex digit
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
: nibble>hex-char ( n -- c )
    dup 10 < if
        [char] 0 +
    else
        10 - [char] a +
    then
;

\ Convert nibble (0-15) to uppercase hex character
\ ( n -- c )
: nibble>hex-char-upper ( n -- c )
    dup 10 < if
        [char] 0 +
    else
        10 - [char] A +
    then
;

\ ============================================================
\ UUID Output Buffer Management
\ ============================================================

: uuid-out-reset ( -- ) 0 uuid-out-pos ! ;
: uuid-out-emit ( c -- ) uuid-out-buffer uuid-out-pos @ + c! 1 uuid-out-pos +! ;
: uuid-out-result ( -- addr len ) uuid-out-buffer uuid-out-pos @ ;

\ Emit a byte as two hex characters (lowercase)
\ ( byte -- )
: uuid-emit-byte ( byte -- )
    dup 4 rshift $0F and nibble>hex-char uuid-out-emit
    $0F and nibble>hex-char uuid-out-emit
;

\ ============================================================
\ UUID Parsing
\ ============================================================

\ Parse two hex characters to a byte
\ ( addr -- byte flag ) flag is true if valid
: parse-hex-byte ( addr -- byte flag )
    dup c@ hex-char>nibble if
        4 lshift swap 1+ c@ hex-char>nibble if
            or true
        else
            drop false
        then
    else
        drop drop false
    then
;

\ Parse UUID from canonical format (8-4-4-4-12)
\ ( addr len dest -- flag ) flag is true if valid
: uuid-parse ( addr len dest -- flag )
    >r                              \ save destination
    \ Check length
    dup UUID-STRING-LEN <> if
        2drop r> drop false exit
    then
    \ Check hyphens at positions 8, 13, 18, 23
    over 8 + c@ [char] - <> if 2drop r> drop false exit then
    over 13 + c@ [char] - <> if 2drop r> drop false exit then
    over 18 + c@ [char] - <> if 2drop r> drop false exit then
    over 23 + c@ [char] - <> if 2drop r> drop false exit then
    \ Parse hex bytes: 0-7, 9-12, 14-17, 19-22, 24-35
    r>                              \ get destination back
    swap drop                       \ drop length, keep addr
    \ Parse first 4 bytes (positions 0-7)
    over 0 + parse-hex-byte 0= if 2drop false exit then over 0 + c!
    over 2 + parse-hex-byte 0= if 2drop false exit then over 1 + c!
    over 4 + parse-hex-byte 0= if 2drop false exit then over 2 + c!
    over 6 + parse-hex-byte 0= if 2drop false exit then over 3 + c!
    \ Parse bytes 4-5 (positions 9-12)
    over 9 + parse-hex-byte 0= if 2drop false exit then over 4 + c!
    over 11 + parse-hex-byte 0= if 2drop false exit then over 5 + c!
    \ Parse bytes 6-7 (positions 14-17)
    over 14 + parse-hex-byte 0= if 2drop false exit then over 6 + c!
    over 16 + parse-hex-byte 0= if 2drop false exit then over 7 + c!
    \ Parse bytes 8-9 (positions 19-22)
    over 19 + parse-hex-byte 0= if 2drop false exit then over 8 + c!
    over 21 + parse-hex-byte 0= if 2drop false exit then over 9 + c!
    \ Parse bytes 10-15 (positions 24-35)
    over 24 + parse-hex-byte 0= if 2drop false exit then over 10 + c!
    over 26 + parse-hex-byte 0= if 2drop false exit then over 11 + c!
    over 28 + parse-hex-byte 0= if 2drop false exit then over 12 + c!
    over 30 + parse-hex-byte 0= if 2drop false exit then over 13 + c!
    over 32 + parse-hex-byte 0= if 2drop false exit then over 14 + c!
    over 34 + parse-hex-byte 0= if 2drop false exit then over 15 + c!
    2drop true
;

\ Parse UUID into the default uuid-buffer
\ ( addr len -- flag )
: uuid-parse-default ( addr len -- flag )
    uuid-buffer uuid-parse
;

\ ============================================================
\ UUID Formatting
\ ============================================================

\ Format UUID bytes to canonical string (8-4-4-4-12)
\ ( src-addr -- result-addr result-len )
: uuid-format ( src-addr -- result-addr result-len )
    uuid-out-reset
    \ Bytes 0-3
    dup 0 + c@ uuid-emit-byte
    dup 1 + c@ uuid-emit-byte
    dup 2 + c@ uuid-emit-byte
    dup 3 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    \ Bytes 4-5
    dup 4 + c@ uuid-emit-byte
    dup 5 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    \ Bytes 6-7
    dup 6 + c@ uuid-emit-byte
    dup 7 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    \ Bytes 8-9
    dup 8 + c@ uuid-emit-byte
    dup 9 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    \ Bytes 10-15
    dup 10 + c@ uuid-emit-byte
    dup 11 + c@ uuid-emit-byte
    dup 12 + c@ uuid-emit-byte
    dup 13 + c@ uuid-emit-byte
    dup 14 + c@ uuid-emit-byte
    15 + c@ uuid-emit-byte
    uuid-out-result
;

\ Format UUID as URN (urn:uuid:...)
\ ( src-addr -- result-addr result-len )
: uuid-format-urn ( src-addr -- result-addr result-len )
    uuid-out-reset
    s" urn:uuid:" uuid-out-buffer swap cmove
    9 uuid-out-pos !
    \ Now format the UUID part
    \ Bytes 0-3
    dup 0 + c@ uuid-emit-byte
    dup 1 + c@ uuid-emit-byte
    dup 2 + c@ uuid-emit-byte
    dup 3 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    dup 4 + c@ uuid-emit-byte
    dup 5 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    dup 6 + c@ uuid-emit-byte
    dup 7 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    dup 8 + c@ uuid-emit-byte
    dup 9 + c@ uuid-emit-byte
    [char] - uuid-out-emit
    dup 10 + c@ uuid-emit-byte
    dup 11 + c@ uuid-emit-byte
    dup 12 + c@ uuid-emit-byte
    dup 13 + c@ uuid-emit-byte
    dup 14 + c@ uuid-emit-byte
    15 + c@ uuid-emit-byte
    uuid-out-result
;

\ Format UUID without hyphens (compact)
\ ( src-addr -- result-addr result-len )
: uuid-format-compact ( src-addr -- result-addr result-len )
    uuid-out-reset
    UUID-BYTES 0 do
        dup i + c@ uuid-emit-byte
    loop
    drop
    uuid-out-result
;

\ ============================================================
\ UUID Validation
\ ============================================================

\ Check if string looks like a UUID (quick format check)
\ ( addr len -- flag )
: uuid? ( addr len -- flag )
    dup UUID-STRING-LEN <> if 2drop false exit then
    over 8 + c@ [char] - <> if 2drop false exit then
    over 13 + c@ [char] - <> if 2drop false exit then
    over 18 + c@ [char] - <> if 2drop false exit then
    over 23 + c@ [char] - = swap drop
;

\ Full UUID validation (parse to verify)
\ ( addr len -- flag )
: uuid-valid? ( addr len -- flag )
    uuid-buffer uuid-parse
;

\ Check if UUID is nil (all zeros)
\ ( uuid-addr -- flag )
: uuid-nil? ( uuid-addr -- flag )
    true swap
    UUID-BYTES 0 do
        dup i + c@ 0<> if swap drop false swap leave then
    loop
    drop
;

\ ============================================================
\ UUID Version and Variant
\ ============================================================

\ UUID versions
1 constant UUID-V1              \ Time-based
2 constant UUID-V2              \ DCE Security
3 constant UUID-V3              \ Name-based (MD5)
4 constant UUID-V4              \ Random
5 constant UUID-V5              \ Name-based (SHA-1)
0 constant UUID-VNIL            \ Nil

\ Get UUID version (0-5)
\ ( uuid-addr -- version )
: uuid-version ( uuid-addr -- version )
    6 + c@ 4 rshift $0F and
;

\ Get UUID variant (0=NCS, 1=RFC4122, 2=Microsoft, 3=Future)
\ ( uuid-addr -- variant )
: uuid-variant ( uuid-addr -- variant )
    8 + c@
    dup $80 and 0= if drop 0 exit then      \ NCS: 0xxx
    dup $C0 and $80 = if drop 1 exit then   \ RFC4122: 10xx
    dup $E0 and $C0 = if drop 2 exit then   \ Microsoft: 110x
    drop 3                                   \ Future: 111x
;

\ ============================================================
\ UUID Comparison
\ ============================================================

\ Compare two UUIDs for equality
\ ( uuid1-addr uuid2-addr -- flag )
: uuid= ( uuid1-addr uuid2-addr -- flag )
    UUID-BYTES 0 do
        2dup i + c@ swap i + c@ <> if
            2drop false unloop exit
        then
    loop
    2drop true
;

\ Copy UUID from source to destination
\ ( src-addr dest-addr -- )
: uuid-copy ( src-addr dest-addr -- )
    UUID-BYTES cmove
;

\ ============================================================
\ UUID v4 Preparation (requires external random source)
\ ============================================================

\ Set version bits in UUID buffer (call after filling with random)
\ ( uuid-addr version -- )
: uuid-set-version ( uuid-addr version -- )
    swap 6 + dup c@
    $0F and                         \ clear version bits
    rot 4 lshift or                 \ set new version
    swap c!
;

\ Set variant bits to RFC4122 (call after filling with random)
\ ( uuid-addr -- )
: uuid-set-variant-rfc4122 ( uuid-addr -- )
    8 + dup c@
    $3F and                         \ clear variant bits
    $80 or                          \ set RFC4122 variant (10xx)
    swap c!
;

\ Prepare UUID buffer as v4 (assumes 16 random bytes already present)
\ ( uuid-addr -- )
: uuid-prepare-v4 ( uuid-addr -- )
    dup UUID-V4 uuid-set-version
    uuid-set-variant-rfc4122
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-uuid-version ( -- ) ." SafeUUID for Forth v0.1.0" cr ;

: safe-uuid-help ( -- )
    cr
    ." SafeUUID - UUID Operations" cr
    ." ===========================" cr
    cr
    ." Parsing:" cr
    ."   uuid-parse     ( addr len dest -- flag )  Parse UUID string to bytes" cr
    ."   uuid?          ( addr len -- flag )       Quick format check" cr
    ."   uuid-valid?    ( addr len -- flag )       Full validation" cr
    cr
    ." Formatting:" cr
    ."   uuid-format        ( addr -- addr len )   Format as 8-4-4-4-12" cr
    ."   uuid-format-urn    ( addr -- addr len )   Format as urn:uuid:..." cr
    ."   uuid-format-compact ( addr -- addr len )  Format without hyphens" cr
    cr
    ." Properties:" cr
    ."   uuid-version   ( addr -- n )              Get version (1-5, 0=nil)" cr
    ."   uuid-variant   ( addr -- n )              Get variant (0-3)" cr
    ."   uuid-nil?      ( addr -- flag )           Check if nil UUID" cr
    cr
    ." Comparison:" cr
    ."   uuid=          ( addr1 addr2 -- flag )    Compare two UUIDs" cr
    ."   uuid-copy      ( src dest -- )            Copy UUID" cr
    cr
    ." Constants:" cr
    ."   nil-uuid, namespace-dns, namespace-url" cr
    ."   UUID-V1 UUID-V2 UUID-V3 UUID-V4 UUID-V5" cr
    cr
;
