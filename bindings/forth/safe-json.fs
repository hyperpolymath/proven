\ SPDX-License-Identifier: PMPL-1.0
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-JSON - Safe JSON parsing and validation for Forth
\ Stack-based JSON operations with bounds checking

\ ============================================================
\ Constants and Buffers
\ ============================================================

256 constant JSON-MAX-DEPTH
1024 constant JSON-MAX-STRING
create json-string-buffer JSON-MAX-STRING allot
variable json-string-pos
variable json-depth
variable json-error

\ JSON token types
0 constant JSON-NULL
1 constant JSON-BOOL
2 constant JSON-NUMBER
3 constant JSON-STRING
4 constant JSON-ARRAY
5 constant JSON-OBJECT
6 constant JSON-INVALID

\ ============================================================
\ Character Classification
\ ============================================================

\ Check if whitespace
: json-ws? ( c -- flag )
    dup 32 = swap          \ space
    dup 9 = swap           \ tab
    dup 10 = swap          \ newline
    13 = or or or          \ carriage return
;

\ Check if digit
: json-digit? ( c -- flag )
    dup [char] 0 >= swap [char] 9 <= and
;

\ Check if hex digit
: json-hex? ( c -- flag )
    dup json-digit? if drop true exit then
    dup [char] a >= over [char] f <= and if drop true exit then
    dup [char] A >= over [char] F <= and if drop true exit then
    drop false
;

\ ============================================================
\ Basic Parsing Helpers
\ ============================================================

\ Skip whitespace
\ ( addr len -- addr' len' )
: json-skip-ws ( addr len -- addr' len' )
    begin
        dup 0> while
        over c@ json-ws? while
        1- swap 1+ swap
    repeat then
;

\ Match literal string
\ ( addr len literal-addr literal-len -- addr' len' flag )
: json-match ( addr len lit-addr lit-len -- addr' len' flag )
    2swap 2over                 \ lit-addr lit-len addr len lit-addr lit-len
    2dup 2>r                    \ save lit-addr lit-len
    2swap                       \ lit-addr lit-len lit-addr lit-len addr len
    2r>                         \ lit-addr lit-len addr len lit-addr lit-len
    rot over < if               \ not enough input
        2drop 2drop 2drop
        s" " false exit
    then
    drop                        \ lit-addr lit-len addr lit-addr lit-len
    3 pick swap compare 0= if
        \ Match succeeded
        2drop                   \ lit-addr lit-len
        swap over + swap -      \ advance input
        true
    else
        2drop 2drop
        s" " false
    then
;

\ ============================================================
\ JSON Value Detection
\ ============================================================

\ Peek at next non-whitespace character
\ ( addr len -- c flag )
: json-peek ( addr len -- c flag )
    json-skip-ws
    dup 0> if
        over c@ true
    else
        2drop 0 false
    then
;

\ Detect JSON value type from first character
\ ( c -- type )
: json-detect-type ( c -- type )
    case
        [char] n of JSON-NULL endof
        [char] t of JSON-BOOL endof
        [char] f of JSON-BOOL endof
        [char] " of JSON-STRING endof
        [char] [ of JSON-ARRAY endof
        [char] { of JSON-OBJECT endof
        [char] - of JSON-NUMBER endof
        dup json-digit? if JSON-NUMBER swap then
        JSON-INVALID swap
    endcase
;

\ ============================================================
\ JSON String Parsing
\ ============================================================

\ Reset string buffer
: json-string-reset ( -- )
    0 json-string-pos !
;

\ Emit to string buffer with bounds check
: json-string-emit ( c -- flag )
    json-string-pos @ JSON-MAX-STRING < if
        json-string-buffer json-string-pos @ + c!
        1 json-string-pos +!
        true
    else
        false
    then
;

\ Get string result
: json-string-result ( -- addr len )
    json-string-buffer json-string-pos @
;

\ Parse hex digit
: json-hex-value ( c -- n )
    dup json-digit? if [char] 0 - exit then
    dup [char] a >= over [char] f <= and if [char] a - 10 + exit then
    dup [char] A >= over [char] F <= and if [char] A - 10 + exit then
    drop 0
;

\ Parse JSON string (starting after opening quote)
\ ( addr len -- addr' len' flag )
: json-parse-string ( addr len -- addr' len' flag )
    json-string-reset
    begin
        dup 0> while
        over c@
        case
            [char] " of                 \ end of string
                1- swap 1+ swap true exit
            endof
            [char] \ of                 \ escape sequence
                1- swap 1+ swap
                dup 0= if 2drop false exit then
                over c@
                case
                    [char] " of [char] " json-string-emit drop endof
                    [char] \ of [char] \ json-string-emit drop endof
                    [char] / of [char] / json-string-emit drop endof
                    [char] b of 8 json-string-emit drop endof
                    [char] f of 12 json-string-emit drop endof
                    [char] n of 10 json-string-emit drop endof
                    [char] r of 13 json-string-emit drop endof
                    [char] t of 9 json-string-emit drop endof
                    [char] u of
                        \ Unicode escape - simplified, just skip 4 hex digits
                        1- swap 1+ swap
                        4 0 do
                            dup 0= if 2drop false unloop exit then
                            over c@ json-hex? 0= if 2drop false unloop exit then
                            1- swap 1+ swap
                        loop
                        [char] ? json-string-emit drop  \ Placeholder
                        0
                    endof
                    2drop 2drop false exit
                endcase
                1- swap 1+ swap
            endof
            \ Regular character
            dup json-string-emit 0= if
                drop 2drop false exit
            then
            1- swap 1+ swap
            0
        endcase
    repeat
    2drop false
;

\ ============================================================
\ JSON Number Parsing
\ ============================================================

\ Parse JSON number (returns value and remaining string)
\ ( addr len -- value addr' len' flag )
: json-parse-number ( addr len -- value addr' len' flag )
    0 -rot                      \ accumulator addr len
    \ Handle negative
    over c@ [char] - = if
        1- swap 1+ swap
        true -rot               \ negative flag
    else
        false -rot
    then
    \ Parse digits
    dup 0= if rot drop 2drop 0 s" " false exit then
    over c@ json-digit? 0= if rot drop 2drop 0 s" " false exit then
    begin
        dup 0> while
        over c@ json-digit? while
        over c@ [char] 0 -
        rot 10 * + -rot
        1- swap 1+ swap
    repeat then
    \ Skip fractional part if present
    dup 0> if
        over c@ [char] . = if
            1- swap 1+ swap
            begin
                dup 0> while
                over c@ json-digit? while
                1- swap 1+ swap
            repeat then
        then
    then
    \ Skip exponent if present
    dup 0> if
        over c@ dup [char] e = swap [char] E = or if
            1- swap 1+ swap
            dup 0> if
                over c@ dup [char] + = swap [char] - = or if
                    1- swap 1+ swap
                then
            then
            begin
                dup 0> while
                over c@ json-digit? while
                1- swap 1+ swap
            repeat then
        then
    then
    \ Apply sign
    rot if rot negate -rot then
    rot -rot true
;

\ ============================================================
\ JSON Validation (Simplified)
\ ============================================================

\ Validate JSON null
\ ( addr len -- addr' len' flag )
: json-validate-null ( addr len -- addr' len' flag )
    s" null" json-match
;

\ Validate JSON boolean
\ ( addr len -- addr' len' flag )
: json-validate-bool ( addr len -- addr' len' flag )
    over c@ [char] t = if
        s" true" json-match
    else
        s" false" json-match
    then
;

\ Forward declaration for recursive validation
defer json-validate-value

\ Validate JSON array elements
\ ( addr len -- addr' len' flag )
: json-validate-array ( addr len -- addr' len' flag )
    1 json-depth +!
    json-depth @ JSON-MAX-DEPTH > if
        2drop s" " false exit
    then
    1- swap 1+ swap             \ skip [
    json-skip-ws
    \ Empty array?
    dup 0> if
        over c@ [char] ] = if
            1- swap 1+ swap
            -1 json-depth +!
            true exit
        then
    then
    \ Parse elements
    begin
        json-validate-value 0= if
            -1 json-depth +!
            false exit
        then
        json-skip-ws
        dup 0= if 2drop -1 json-depth +! false exit then
        over c@
        dup [char] ] = if
            drop 1- swap 1+ swap
            -1 json-depth +!
            true exit
        then
        [char] , <> if
            2drop -1 json-depth +!
            false exit
        then
        1- swap 1+ swap         \ skip comma
        json-skip-ws
    again
;

\ Validate JSON object
\ ( addr len -- addr' len' flag )
: json-validate-object ( addr len -- addr' len' flag )
    1 json-depth +!
    json-depth @ JSON-MAX-DEPTH > if
        2drop s" " false exit
    then
    1- swap 1+ swap             \ skip {
    json-skip-ws
    \ Empty object?
    dup 0> if
        over c@ [char] } = if
            1- swap 1+ swap
            -1 json-depth +!
            true exit
        then
    then
    \ Parse members
    begin
        \ Expect string key
        dup 0= if 2drop -1 json-depth +! false exit then
        over c@ [char] " <> if
            2drop -1 json-depth +!
            false exit
        then
        1- swap 1+ swap         \ skip opening quote
        json-parse-string 0= if
            -1 json-depth +!
            false exit
        then
        json-skip-ws
        \ Expect colon
        dup 0= if 2drop -1 json-depth +! false exit then
        over c@ [char] : <> if
            2drop -1 json-depth +!
            false exit
        then
        1- swap 1+ swap         \ skip colon
        json-skip-ws
        \ Validate value
        json-validate-value 0= if
            -1 json-depth +!
            false exit
        then
        json-skip-ws
        dup 0= if 2drop -1 json-depth +! false exit then
        over c@
        dup [char] } = if
            drop 1- swap 1+ swap
            -1 json-depth +!
            true exit
        then
        [char] , <> if
            2drop -1 json-depth +!
            false exit
        then
        1- swap 1+ swap         \ skip comma
        json-skip-ws
    again
;

\ Validate any JSON value
\ ( addr len -- addr' len' flag )
:noname ( addr len -- addr' len' flag )
    json-skip-ws
    dup 0= if 2drop s" " false exit then
    over c@ json-detect-type
    case
        JSON-NULL of json-validate-null endof
        JSON-BOOL of json-validate-bool endof
        JSON-NUMBER of json-parse-number if 2swap 2drop true else false then endof
        JSON-STRING of
            1- swap 1+ swap     \ skip opening quote
            json-parse-string
        endof
        JSON-ARRAY of json-validate-array endof
        JSON-OBJECT of json-validate-object endof
        2drop s" " false swap
    endcase
; is json-validate-value

\ Main validation entry point
\ ( addr len -- flag )
: json-valid? ( addr len -- flag )
    0 json-depth !
    0 json-error !
    json-validate-value if
        json-skip-ws
        nip 0=                  \ valid if no remaining input
    else
        2drop false
    then
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-json-version ( -- ) ." SAFE-JSON for Forth v0.4.0" cr ;

: safe-json-help ( -- )
    cr
    ." SAFE-JSON - JSON Parsing and Validation" cr
    ." ========================================" cr
    cr
    ." Validation:" cr
    ."   json-valid?     ( addr len -- flag )" cr
    cr
    ." Type Detection:" cr
    ."   json-detect-type ( c -- type )" cr
    ."   Types: JSON-NULL JSON-BOOL JSON-NUMBER JSON-STRING JSON-ARRAY JSON-OBJECT" cr
    cr
    ." Parsing:" cr
    ."   json-parse-string ( addr len -- addr' len' flag )" cr
    ."   json-parse-number ( addr len -- value addr' len' flag )" cr
    cr
    ." Utilities:" cr
    ."   json-skip-ws    ( addr len -- addr' len' )" cr
    ."   json-peek       ( addr len -- c flag )" cr
    cr
;
