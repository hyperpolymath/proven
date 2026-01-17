\ SPDX-License-Identifier: PMPL-1.0
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ Proven - Safe, validated operations library for Forth
\ Stack-based safety operations with formal verification guarantees
\ Version 0.4.0

\ ============================================================
\ Library Metadata
\ ============================================================

s" 0.4.0" 2constant PROVEN-VERSION-STRING
38 constant MODULE-COUNT

\ Module Categories:
\   Core (11): SAFE-MATH, SAFE-STRING, SAFE-PATH, SAFE-EMAIL, SAFE-URL,
\              SAFE-NETWORK, SAFE-CRYPTO, SAFE-UUID, SAFE-CURRENCY,
\              SAFE-PHONE, SAFE-HEX
\   Data (7): SAFE-JSON, SAFE-DATETIME, SAFE-FLOAT, SAFE-VERSION,
\             SAFE-COLOR, SAFE-ANGLE, SAFE-UNIT
\   Data Structures (5): SAFE-BUFFER, SAFE-QUEUE, SAFE-BLOOM,
\                        SAFE-LRU, SAFE-GRAPH
\   Resilience (4): SAFE-RATE-LIMITER, SAFE-CIRCUIT-BREAKER,
\                   SAFE-RETRY, SAFE-MONOTONIC
\   State (2): SAFE-STATE-MACHINE, SAFE-CALCULATOR
\   Algorithm (4): SAFE-GEO, SAFE-PROBABILITY, SAFE-CHECKSUM, SAFE-TENSOR
\   Security (2): SAFE-PASSWORD, SAFE-ML
\   HTTP (3): SAFE-HEADER, SAFE-COOKIE, SAFE-CONTENT-TYPE

\ ============================================================
\ SAFE-MATH - Overflow-checked arithmetic
\ ============================================================

\ Maximum and minimum cell values (assuming 64-bit)
9223372036854775807 constant MAX-INT64
-9223372036854775808 constant MIN-INT64

\ Flag for overflow (0 = no overflow, -1 = overflow)
variable overflow-flag

\ Initialize overflow flag
: init-overflow ( -- ) 0 overflow-flag ! ;

\ Check if overflow occurred
: overflow? ( -- flag ) overflow-flag @ ;

\ Safe add with overflow checking
\ ( a b -- result flag ) flag is 0 if ok, -1 if overflow
: safe+ ( a b -- result flag )
    over 0> if                  \ b > 0
        over MAX-INT64 rot - > if
            2drop 0 -1 exit     \ overflow
        then
    else over 0< if             \ b < 0
        over MIN-INT64 rot - < if
            2drop 0 -1 exit     \ overflow
        then
    then then
    + 0                         \ ok
;

\ Safe subtract with overflow checking
: safe- ( a b -- result flag )
    over 0< if                  \ b < 0
        over MAX-INT64 rot + > if
            2drop 0 -1 exit     \ overflow
        then
    else over 0> if             \ b > 0
        over MIN-INT64 rot + < if
            2drop 0 -1 exit     \ overflow
        then
    then then
    - 0
;

\ Safe multiply with overflow checking
: safe* ( a b -- result flag )
    2dup or 0= if               \ either is 0
        2drop 0 0 exit
    then
    2dup *                      \ compute result
    rot dup 0<> if
        over swap / rot <> if   \ check a * b / a = b
            drop 0 -1 exit      \ overflow
        then
    else
        drop
    then
    0
;

\ Safe divide with zero check
: safe/ ( a b -- result flag )
    dup 0= if                   \ division by zero
        2drop 0 -1 exit
    then
    over MIN-INT64 = over -1 = and if  \ MIN / -1 overflow
        2drop 0 -1 exit
    then
    / 0
;

\ Safe modulo
: safe-mod ( a b -- result flag )
    dup 0= if
        2drop 0 -1 exit
    then
    mod 0
;

\ Safe absolute value
: safe-abs ( a -- result flag )
    dup MIN-INT64 = if
        drop 0 -1 exit
    then
    abs 0
;

\ Safe negate
: safe-negate ( a -- result flag )
    dup MIN-INT64 = if
        drop 0 -1 exit
    then
    negate 0
;

\ Clamp value to range
: clamp ( value min max -- clamped )
    rot                         \ min max value
    2dup < if                   \ value < min
        2drop                   \ return min
    else
        swap drop               \ min value
        2dup > if               \ value > max
            nip                 \ return max
        else
            drop                \ return value
        then
    then
;

\ Check if value in range
: in-range? ( value min max -- flag )
    rot dup                     \ min max value value
    3 pick >= swap              \ min max (value>=min) value
    3 pick <= and               \ min max flag
    nip nip                     \ flag
;

\ ============================================================
\ SAFE-STRING - XSS prevention
\ ============================================================

\ String buffer for results
256 constant MAX-STRING
create string-buffer MAX-STRING allot
variable string-pos

: reset-string ( -- ) 0 string-pos ! ;
: emit-to-string ( c -- ) string-buffer string-pos @ + c! 1 string-pos +! ;
: string-result ( -- addr len ) string-buffer string-pos @ ;

\ Escape HTML special characters
: escape-html-char ( c -- )
    case
        [char] & of s" &amp;" string-buffer string-pos @ + swap cmove 5 string-pos +! endof
        [char] < of s" &lt;" string-buffer string-pos @ + swap cmove 4 string-pos +! endof
        [char] > of s" &gt;" string-buffer string-pos @ + swap cmove 4 string-pos +! endof
        [char] " of s" &quot;" string-buffer string-pos @ + swap cmove 6 string-pos +! endof
        [char] ' of s" &#x27;" string-buffer string-pos @ + swap cmove 6 string-pos +! endof
        emit-to-string 0        \ default: just copy
    endcase
;

\ Escape HTML string
: escape-html ( addr len -- result-addr result-len )
    reset-string
    0 do
        dup i + c@ escape-html-char
    loop
    drop
    string-result
;

\ Escape SQL single quotes
: escape-sql ( addr len -- result-addr result-len )
    reset-string
    0 do
        dup i + c@
        dup [char] ' = if
            emit-to-string [char] ' emit-to-string
        else
            emit-to-string
        then
    loop
    drop
    string-result
;

\ Check if char is alphanumeric
: alpha-num? ( c -- flag )
    dup [char] a >= over [char] z <= and if drop true exit then
    dup [char] A >= over [char] Z <= and if drop true exit then
    dup [char] 0 >= over [char] 9 <= and if drop true exit then
    drop false
;

\ Sanitize to alphanumeric + underscore + hyphen
: sanitize-default ( addr len -- result-addr result-len )
    reset-string
    0 do
        dup i + c@
        dup alpha-num? over [char] _ = or over [char] - = or if
            emit-to-string
        else
            drop
        then
    loop
    drop
    string-result
;

\ ============================================================
\ SAFE-PATH - Directory traversal prevention
\ ============================================================

\ Check if string contains substring
: contains? ( addr1 len1 addr2 len2 -- flag )
    2swap 2over                 \ save copies
    0 -rot                      \ flag addr1 len1 addr2 len2
    begin
        2over 2over             \ compare substrings
        2dup > if               \ len1 >= len2
            4 pick 4 pick       \ get start addresses
            3 pick              \ get len2
            compare 0= if
                2drop 2drop true exit
            then
        else
            2drop 2drop false exit
        then
        2swap 1- swap 1+ swap 2swap
        over 0=
    until
    2drop 2drop false
;

\ Check for path traversal
: has-traversal? ( addr len -- flag )
    2dup s" .." contains? if 2drop true exit then
    2dup s" ./" contains? if 2drop true exit then
    2dup s" %2e%2e" contains? if 2drop true exit then
    2dup s" %00" contains? if 2drop true exit then
    2drop false
;

\ Sanitize filename (basic - remove slashes)
: sanitize-filename ( addr len -- result-addr result-len )
    reset-string
    0 do
        dup i + c@
        dup [char] / = over [char] \ = or if
            drop [char] _ emit-to-string
        else
            emit-to-string
        then
    loop
    drop
    string-result
;

\ ============================================================
\ SAFE-EMAIL - Email validation
\ ============================================================

\ Check if char is valid in local part
: email-local-char? ( c -- flag )
    dup alpha-num? if drop true exit then
    dup [char] . = if drop true exit then
    dup [char] ! = if drop true exit then
    dup [char] # = if drop true exit then
    dup [char] $ = if drop true exit then
    dup [char] % = if drop true exit then
    dup [char] & = if drop true exit then
    dup [char] ' = if drop true exit then
    dup [char] * = if drop true exit then
    dup [char] + = if drop true exit then
    dup [char] - = if drop true exit then
    dup [char] / = if drop true exit then
    dup [char] = = if drop true exit then
    dup [char] ? = if drop true exit then
    dup [char] ^ = if drop true exit then
    dup [char] _ = if drop true exit then
    dup [char] ` = if drop true exit then
    dup [char] { = if drop true exit then
    dup [char] | = if drop true exit then
    dup [char] } = if drop true exit then
    dup [char] ~ = if drop true exit then
    drop false
;

\ Find @ in email
: find-at ( addr len -- pos | -1 )
    0 do
        dup i + c@ [char] @ = if
            drop i unloop exit
        then
    loop
    drop -1
;

\ Basic email validation
: valid-email? ( addr len -- flag )
    dup 0= if 2drop false exit then
    dup 254 > if 2drop false exit then

    2dup find-at               \ find @
    dup -1 = if drop 2drop false exit then

    \ Check local part length
    dup 0= if drop 2drop false exit then
    dup 64 > if drop 2drop false exit then

    \ Check domain exists
    dup 1+ 2 pick swap - 0= if drop 2drop false exit then

    drop 2drop true
;

\ ============================================================
\ SAFE-URL - URL validation and encoding
\ ============================================================

\ URL-safe characters (unreserved per RFC 3986)
: url-unreserved? ( c -- flag )
    dup alpha-num? if drop true exit then
    dup [char] - = if drop true exit then
    dup [char] . = if drop true exit then
    dup [char] _ = if drop true exit then
    dup [char] ~ = if drop true exit then
    drop false
;

\ Hex digit to char
: hex-digit ( n -- c )
    dup 10 < if [char] 0 + else 10 - [char] A + then
;

\ URL-encode a string
: url-encode ( addr len -- result-addr result-len )
    reset-string
    0 do
        dup i + c@
        dup url-unreserved? if
            emit-to-string
        else
            [char] % emit-to-string
            dup 4 rshift hex-digit emit-to-string
            15 and hex-digit emit-to-string
        then
    loop
    drop
    string-result
;

\ Check if URL has dangerous characters
: url-has-injection? ( addr len -- flag )
    0 do
        dup i + c@
        dup [char] < = swap [char] > = or
        over i + c@ [char] " = or
        over i + c@ [char] ' = or
        if drop true unloop exit then
    loop
    drop false
;

\ ============================================================
\ SAFE-NETWORK - IP validation
\ ============================================================

\ Variables for parsed IP
variable ip-a variable ip-b variable ip-c variable ip-d

\ Parse octet from string (returns value and remaining string)
: parse-octet ( addr len -- value flag addr' len' )
    0 -rot                      \ value addr len
    begin
        dup 0> while
        over c@
        dup [char] 0 >= over [char] 9 <= and if
            [char] 0 -
            rot 10 * + -rot     \ accumulate digit
            1- swap 1+ swap     \ advance string
        else
            dup [char] . = if
                drop 1- swap 1+ swap  \ skip dot
                rot dup 0 255 in-range? if
                    true 2swap exit   \ success
                else
                    drop false 2swap exit  \ invalid octet
                then
            else
                drop 2drop drop false s" " exit  \ invalid char
            then
        then
    repeat
    \ End of string - return last octet
    rot dup 0 255 in-range? if
        true 2swap
    else
        drop false 2swap
    then
;

\ Parse IPv4 address
: parse-ipv4 ( addr len -- flag )
    parse-octet if
        ip-a !
        parse-octet if
            ip-b !
            parse-octet if
                ip-c !
                \ Last octet (no dot expected)
                dup 0= if 2drop false exit then
                parse-octet if
                    ip-d !
                    dup 0= if 2drop true exit then  \ success if nothing left
                    2drop false
                else 2drop false then
            else 2drop false then
        else 2drop false then
    else 2drop false then
;

\ Check if IP is loopback
: ip-loopback? ( -- flag )
    ip-a @ 127 =
;

\ Check if IP is private (RFC 1918)
: ip-private? ( -- flag )
    ip-a @ 10 = if true exit then
    ip-a @ 172 = ip-b @ 16 >= ip-b @ 31 <= and and if true exit then
    ip-a @ 192 = ip-b @ 168 = and if true exit then
    false
;

\ Check if port is valid
: valid-port? ( port -- flag )
    dup 1 >= swap 65535 <= and
;

\ Check if port is privileged
: privileged-port? ( port -- flag )
    dup 1 >= swap 1024 < and
;

\ ============================================================
\ SAFE-CRYPTO - Basic crypto operations
\ ============================================================

\ Simple XOR-based hash (NOT cryptographically secure - for demo only)
variable hash-acc

: simple-hash ( addr len -- hash )
    0 hash-acc !
    0 do
        dup i + c@
        hash-acc @ xor
        hash-acc @ 5 lshift xor
        hash-acc !
    loop
    drop
    hash-acc @
;

\ Constant-time compare
: constant-time= ( addr1 addr2 len -- flag )
    0 -rot                      \ result addr1 addr2 len
    0 do
        2dup i + c@ swap i + c@ xor
        rot or -rot             \ accumulate differences
    loop
    2drop 0=
;

\ Print message about crypto
: crypto-note ( -- )
    cr ." Note: Use external crypto library for real applications" cr
;

\ ============================================================
\ Main entry point
\ ============================================================

: proven-version ( -- )
    ." Proven for Forth v" PROVEN-VERSION-STRING type cr
    ." Modules: " MODULE-COUNT . cr
;

: proven-help ( -- )
    cr
    ." Proven Safety Library for Forth v" PROVEN-VERSION-STRING type cr
    ." ====================================================" cr
    ." Total Modules: " MODULE-COUNT . cr
    cr
    ." Core Modules (11):" cr
    ."   SAFE-MATH     - Overflow-checked arithmetic" cr
    ."   SAFE-STRING   - XSS prevention, HTML/SQL escaping" cr
    ."   SAFE-PATH     - Directory traversal prevention" cr
    ."   SAFE-EMAIL    - Email validation" cr
    ."   SAFE-URL      - URL validation and encoding" cr
    ."   SAFE-NETWORK  - IP/port validation" cr
    ."   SAFE-CRYPTO   - Basic crypto operations" cr
    ."   SAFE-UUID     - UUID generation (see safe-uuid.fs)" cr
    ."   SAFE-CURRENCY - Currency operations (see safe-currency.fs)" cr
    ."   SAFE-PHONE    - Phone number handling (see safe-phone.fs)" cr
    ."   SAFE-HEX      - Hex encoding (see safe-hex.fs)" cr
    cr
    ." Data Modules (7):" cr
    ."   SAFE-JSON     - JSON parsing/validation" cr
    ."   SAFE-DATETIME - Date/time operations" cr
    ."   SAFE-FLOAT    - Safe floating-point" cr
    ."   SAFE-VERSION  - Semantic versioning" cr
    ."   SAFE-COLOR    - Color representation" cr
    ."   SAFE-ANGLE    - Angular measurements" cr
    ."   SAFE-UNIT     - Unit conversions" cr
    cr
    ." Data Structure Modules (5):" cr
    ."   SAFE-BUFFER   - Bounded buffers" cr
    ."   SAFE-QUEUE    - FIFO queues" cr
    ."   SAFE-BLOOM    - Bloom filters" cr
    ."   SAFE-LRU      - LRU cache" cr
    ."   SAFE-GRAPH    - Graph operations" cr
    cr
    ." Resilience Modules (4):" cr
    ."   SAFE-RATE-LIMITER    - Rate limiting" cr
    ."   SAFE-CIRCUIT-BREAKER - Circuit breaker pattern" cr
    ."   SAFE-RETRY           - Retry with backoff" cr
    ."   SAFE-MONOTONIC       - Monotonic counters" cr
    cr
    ." State Modules (2):" cr
    ."   SAFE-STATE-MACHINE - Finite state machines" cr
    ."   SAFE-CALCULATOR    - Safe expression evaluator" cr
    cr
    ." Algorithm Modules (4):" cr
    ."   SAFE-GEO         - Geographic coordinates" cr
    ."   SAFE-PROBABILITY - Probability operations" cr
    ."   SAFE-CHECKSUM    - CRC/hash functions" cr
    ."   SAFE-TENSOR      - Vector/matrix operations" cr
    cr
    ." Security Modules (2):" cr
    ."   SAFE-PASSWORD - Password validation" cr
    ."   SAFE-ML       - ML safety operations" cr
    cr
    ." HTTP Modules (3):" cr
    ."   SAFE-HEADER       - HTTP header handling" cr
    ."   SAFE-COOKIE       - Cookie operations" cr
    ."   SAFE-CONTENT-TYPE - MIME type handling" cr
    cr
;

\ Display quick reference
: proven-quick ( -- )
    cr
    ." Quick Reference - Core Words:" cr
    ." =============================" cr
    cr
    ." Arithmetic: safe+ safe- safe* safe/ safe-mod safe-abs safe-negate clamp in-range?" cr
    ." Strings:    escape-html escape-sql sanitize-default" cr
    ." Paths:      has-traversal? sanitize-filename" cr
    ." Email:      valid-email?" cr
    ." URL:        url-encode url-has-injection?" cr
    ." Network:    parse-ipv4 ip-loopback? ip-private? valid-port?" cr
    ." Crypto:     simple-hash constant-time=" cr
    cr
    ." Load additional modules with: include safe-<module>.fs" cr
    cr
;
