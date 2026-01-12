\ SPDX-License-Identifier: AGPL-3.0-or-later
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SafeCurrency - Type-safe currency operations for Forth
\ ISO 4217 currency codes and safe monetary arithmetic
\ All monetary values stored in minor units (cents, pence, etc.)

\ ============================================================
\ ISO 4217 Currency Code Constants
\ ============================================================

\ Major world currencies
0 constant CUR-USD              \ US Dollar
1 constant CUR-EUR              \ Euro
2 constant CUR-GBP              \ British Pound
3 constant CUR-JPY              \ Japanese Yen
4 constant CUR-CHF              \ Swiss Franc
5 constant CUR-CAD              \ Canadian Dollar
6 constant CUR-AUD              \ Australian Dollar
7 constant CUR-NZD              \ New Zealand Dollar
8 constant CUR-CNY              \ Chinese Yuan
9 constant CUR-INR              \ Indian Rupee
10 constant CUR-BRL             \ Brazilian Real
11 constant CUR-MXN             \ Mexican Peso
12 constant CUR-KRW             \ South Korean Won
13 constant CUR-SGD             \ Singapore Dollar
14 constant CUR-HKD             \ Hong Kong Dollar
15 constant CUR-SEK             \ Swedish Krona
16 constant CUR-NOK             \ Norwegian Krone
17 constant CUR-DKK             \ Danish Krone
18 constant CUR-PLN             \ Polish Zloty
19 constant CUR-RUB             \ Russian Ruble
20 constant CUR-ZAR             \ South African Rand
21 constant CUR-TRY             \ Turkish Lira
22 constant CUR-THB             \ Thai Baht
23 constant CUR-MYR             \ Malaysian Ringgit
24 constant CUR-IDR             \ Indonesian Rupiah
25 constant CUR-PHP             \ Philippine Peso
26 constant CUR-VND             \ Vietnamese Dong
27 constant CUR-AED             \ UAE Dirham
28 constant CUR-SAR             \ Saudi Riyal
29 constant CUR-ILS             \ Israeli Shekel
30 constant CUR-CZK             \ Czech Koruna
31 constant CUR-HUF             \ Hungarian Forint
32 constant CUR-RON             \ Romanian Leu
33 constant CUR-BGN             \ Bulgarian Lev
34 constant CUR-HRK             \ Croatian Kuna
35 constant CUR-ISK             \ Icelandic Krona
36 constant CUR-CLP             \ Chilean Peso
37 constant CUR-COP             \ Colombian Peso
38 constant CUR-PEN             \ Peruvian Sol
39 constant CUR-ARS             \ Argentine Peso
40 constant CUR-BTC             \ Bitcoin
41 constant CUR-ETH             \ Ethereum
42 constant CUR-UNKNOWN         \ Unknown currency

\ ============================================================
\ Currency Properties
\ ============================================================

\ Get decimal places for a currency
\ ( currency-code -- decimals )
: currency-decimals ( currency-code -- decimals )
    case
        CUR-JPY of 0 endof
        CUR-KRW of 0 endof
        CUR-VND of 0 endof
        CUR-BTC of 8 endof
        CUR-ETH of 8 endof         \ Capped at 8 for practicality
        2 swap                      \ Default: 2 decimal places
    endcase
;

\ Currency code string table
create currency-codes
    s" USD" , s" EUR" , s" GBP" , s" JPY" , s" CHF" ,
    s" CAD" , s" AUD" , s" NZD" , s" CNY" , s" INR" ,
    s" BRL" , s" MXN" , s" KRW" , s" SGD" , s" HKD" ,
    s" SEK" , s" NOK" , s" DKK" , s" PLN" , s" RUB" ,
    s" ZAR" , s" TRY" , s" THB" , s" MYR" , s" IDR" ,
    s" PHP" , s" VND" , s" AED" , s" SAR" , s" ILS" ,
    s" CZK" , s" HUF" , s" RON" , s" BGN" , s" HRK" ,
    s" ISK" , s" CLP" , s" COP" , s" PEN" , s" ARS" ,
    s" BTC" , s" ETH" , s" ???" ,

\ Output buffer for currency formatting
64 constant MONEY-OUT-MAX
create money-out-buffer MONEY-OUT-MAX allot
variable money-out-pos

: money-out-reset ( -- ) 0 money-out-pos ! ;
: money-out-emit ( c -- ) money-out-buffer money-out-pos @ + c! 1 money-out-pos +! ;
: money-out-result ( -- addr len ) money-out-buffer money-out-pos @ ;

\ Emit string to money output buffer
\ ( addr len -- )
: money-out-string ( addr len -- )
    0 do
        dup i + c@ money-out-emit
    loop
    drop
;

\ ============================================================
\ Currency Code String Conversion
\ ============================================================

\ Get currency code string
\ ( currency-code -- addr len )
: currency-code>string ( currency-code -- addr len )
    case
        CUR-USD of s" USD" endof
        CUR-EUR of s" EUR" endof
        CUR-GBP of s" GBP" endof
        CUR-JPY of s" JPY" endof
        CUR-CHF of s" CHF" endof
        CUR-CAD of s" CAD" endof
        CUR-AUD of s" AUD" endof
        CUR-NZD of s" NZD" endof
        CUR-CNY of s" CNY" endof
        CUR-INR of s" INR" endof
        CUR-BRL of s" BRL" endof
        CUR-MXN of s" MXN" endof
        CUR-KRW of s" KRW" endof
        CUR-SGD of s" SGD" endof
        CUR-HKD of s" HKD" endof
        CUR-SEK of s" SEK" endof
        CUR-NOK of s" NOK" endof
        CUR-DKK of s" DKK" endof
        CUR-PLN of s" PLN" endof
        CUR-RUB of s" RUB" endof
        CUR-ZAR of s" ZAR" endof
        CUR-TRY of s" TRY" endof
        CUR-THB of s" THB" endof
        CUR-MYR of s" MYR" endof
        CUR-IDR of s" IDR" endof
        CUR-PHP of s" PHP" endof
        CUR-VND of s" VND" endof
        CUR-AED of s" AED" endof
        CUR-SAR of s" SAR" endof
        CUR-ILS of s" ILS" endof
        CUR-CZK of s" CZK" endof
        CUR-HUF of s" HUF" endof
        CUR-RON of s" RON" endof
        CUR-BGN of s" BGN" endof
        CUR-HRK of s" HRK" endof
        CUR-ISK of s" ISK" endof
        CUR-CLP of s" CLP" endof
        CUR-COP of s" COP" endof
        CUR-PEN of s" PEN" endof
        CUR-ARS of s" ARS" endof
        CUR-BTC of s" BTC" endof
        CUR-ETH of s" ETH" endof
        s" ???" rot
    endcase
;

\ Get currency symbol
\ ( currency-code -- addr len )
: currency-symbol ( currency-code -- addr len )
    case
        CUR-USD of s" $" endof
        CUR-EUR of s" E" endof       \ Euro symbol may not render in all Forths
        CUR-GBP of s" L" endof       \ Pound symbol
        CUR-JPY of s" Y" endof       \ Yen symbol
        CUR-CHF of s" Fr" endof
        CUR-CAD of s" C$" endof
        CUR-AUD of s" A$" endof
        CUR-NZD of s" NZ$" endof
        CUR-CNY of s" Y" endof
        CUR-INR of s" Rs" endof
        CUR-BRL of s" R$" endof
        CUR-MXN of s" Mex$" endof
        CUR-KRW of s" W" endof
        CUR-SGD of s" S$" endof
        CUR-HKD of s" HK$" endof
        CUR-BTC of s" B" endof
        CUR-ETH of s" E" endof
        s" " rot
    endcase
;

\ Parse currency code string to constant
\ ( addr len -- currency-code flag ) flag true if valid
: string>currency-code ( addr len -- currency-code flag )
    dup 3 <> if 2drop CUR-UNKNOWN false exit then
    2dup s" USD" compare 0= if 2drop CUR-USD true exit then
    2dup s" EUR" compare 0= if 2drop CUR-EUR true exit then
    2dup s" GBP" compare 0= if 2drop CUR-GBP true exit then
    2dup s" JPY" compare 0= if 2drop CUR-JPY true exit then
    2dup s" CHF" compare 0= if 2drop CUR-CHF true exit then
    2dup s" CAD" compare 0= if 2drop CUR-CAD true exit then
    2dup s" AUD" compare 0= if 2drop CUR-AUD true exit then
    2dup s" NZD" compare 0= if 2drop CUR-NZD true exit then
    2dup s" CNY" compare 0= if 2drop CUR-CNY true exit then
    2dup s" INR" compare 0= if 2drop CUR-INR true exit then
    2dup s" BRL" compare 0= if 2drop CUR-BRL true exit then
    2dup s" MXN" compare 0= if 2drop CUR-MXN true exit then
    2dup s" KRW" compare 0= if 2drop CUR-KRW true exit then
    2dup s" SGD" compare 0= if 2drop CUR-SGD true exit then
    2dup s" HKD" compare 0= if 2drop CUR-HKD true exit then
    2dup s" SEK" compare 0= if 2drop CUR-SEK true exit then
    2dup s" NOK" compare 0= if 2drop CUR-NOK true exit then
    2dup s" DKK" compare 0= if 2drop CUR-DKK true exit then
    2dup s" PLN" compare 0= if 2drop CUR-PLN true exit then
    2dup s" ZAR" compare 0= if 2drop CUR-ZAR true exit then
    2dup s" BTC" compare 0= if 2drop CUR-BTC true exit then
    2dup s" ETH" compare 0= if 2drop CUR-ETH true exit then
    2drop CUR-UNKNOWN false
;

\ ============================================================
\ Money Structure
\ ============================================================

\ Money is represented as: ( minor-units currency-code )
\ Operations expect this pair on the stack

\ Create money from major units (dollars, euros, etc.)
\ ( major currency-code -- minor-units currency-code )
: money-from-major ( major currency-code -- minor-units currency-code )
    dup currency-decimals           \ major code decimals
    1 swap 0 ?do 10 * loop          \ major code multiplier
    rot * swap                      \ minor-units code
;

\ Create money from minor units (cents, pence, etc.)
\ ( minor currency-code -- minor currency-code )
: money-from-minor ( minor currency-code -- minor currency-code )
    \ Already in correct form
;

\ Zero amount for a currency
\ ( currency-code -- 0 currency-code )
: money-zero ( currency-code -- 0 currency-code )
    0 swap
;

\ ============================================================
\ Safe Money Arithmetic
\ ============================================================

\ Add two money values (must be same currency)
\ ( m1 c1 m2 c2 -- result currency flag ) flag false if currency mismatch
: money+ ( m1 c1 m2 c2 -- result currency flag )
    rot over = if                   \ check currencies match
        drop rot +                  \ m2 + m1
        swap true                   \ result currency true
    else
        2drop 2drop 0 0 false       \ currency mismatch
    then
;

\ Subtract two money values (must be same currency)
\ ( m1 c1 m2 c2 -- result currency flag )
: money- ( m1 c1 m2 c2 -- result currency flag )
    rot over = if
        drop rot swap -             \ m1 - m2
        swap true
    else
        2drop 2drop 0 0 false
    then
;

\ Multiply money by scalar
\ ( minor currency n -- result currency )
: money* ( minor currency n -- result currency )
    rot * swap
;

\ Divide money by scalar (with zero check)
\ ( minor currency n -- result currency flag ) flag false if divide by zero
: money/ ( minor currency n -- result currency flag )
    dup 0= if
        drop 0 swap false
    else
        rot swap / swap true
    then
;

\ Negate money value
\ ( minor currency -- result currency )
: money-negate ( minor currency -- result currency )
    swap negate swap
;

\ Absolute value of money
\ ( minor currency -- result currency )
: money-abs ( minor currency -- result currency )
    swap abs swap
;

\ ============================================================
\ Money Comparison
\ ============================================================

\ Compare money values (same currency required)
\ ( m1 c1 m2 c2 -- flag ) true if equal
: money= ( m1 c1 m2 c2 -- flag )
    rot = if                        \ currencies equal?
        =                           \ compare amounts
    else
        2drop false
    then
;

\ Check if money is zero
\ ( minor currency -- flag )
: money-zero? ( minor currency -- flag )
    drop 0=
;

\ Check if money is positive
\ ( minor currency -- flag )
: money-positive? ( minor currency -- flag )
    drop 0>
;

\ Check if money is negative
\ ( minor currency -- flag )
: money-negative? ( minor currency -- flag )
    drop 0<
;

\ ============================================================
\ Money Formatting
\ ============================================================

\ Helper: emit number to output buffer
\ ( n -- )
: money-emit-number ( n -- )
    dup 0< if
        [char] - money-out-emit
        negate
    then
    dup 0= if
        drop [char] 0 money-out-emit
    else
        \ Convert to digits (reverse order)
        0 swap                      \ digit-count n
        begin dup 0> while
            10 /mod swap            \ n/10 n%10
            [char] 0 + swap         \ digit n/10
            rot 1+ -rot             \ increment count
        repeat
        drop                        \ drop zero
        \ Now emit digits in correct order
        0 do money-out-emit loop
    then
;

\ Helper: emit number with leading zeros
\ ( n width -- )
: money-emit-padded ( n width -- )
    swap abs                        \ width n
    dup 0= if
        drop 0 do [char] 0 money-out-emit loop
        exit
    then
    \ Count digits
    dup 0                           \ width n n count
    begin over 0> while
        swap 10 / swap 1+
    repeat
    nip                             \ width n count
    \ Emit leading zeros
    rot over - 0 max 0 ?do [char] 0 money-out-emit loop
    \ Emit number
    swap money-emit-number
;

\ Format money value
\ ( minor currency -- addr len )
: money-format ( minor currency -- addr len )
    money-out-reset
    dup currency-decimals           \ minor currency decimals
    dup 0= if
        \ No decimals (JPY, KRW, etc.)
        drop swap                   \ currency minor
        dup 0< if [char] - money-out-emit negate then
        money-emit-number
        [char] space money-out-emit
        currency-code>string money-out-string
    else
        \ Has decimals
        1 swap 0 ?do 10 * loop      \ minor currency divisor
        rot dup 0<                  \ currency divisor minor negative?
        if [char] - money-out-emit negate then
        over /mod                   \ currency divisor remainder major
        money-emit-number
        [char] . money-out-emit
        \ Emit fractional part with leading zeros
        swap                        \ currency remainder divisor
        \ Calculate width needed
        dup 1 begin over 10 >= while swap 1+ swap 10 / repeat drop swap drop
        swap money-emit-padded
        drop                        \ drop currency
        [char] space money-out-emit
        currency-code>string money-out-string
    then
    money-out-result
;

\ Format money with symbol
\ ( minor currency -- addr len )
: money-format-symbol ( minor currency -- addr len )
    money-out-reset
    dup currency-symbol money-out-string
    dup currency-decimals
    dup 0= if
        drop swap
        dup 0< if [char] - money-out-emit negate then
        money-emit-number
    else
        1 swap 0 ?do 10 * loop
        rot dup 0<
        if [char] - money-out-emit negate then
        over /mod
        money-emit-number
        [char] . money-out-emit
        swap
        dup 1 begin over 10 >= while swap 1+ swap 10 / repeat drop swap drop
        swap money-emit-padded
        drop
    then
    money-out-result
;

\ ============================================================
\ Exchange Rate Operations
\ ============================================================

\ Exchange rate stored as integer with 6 decimal places
\ 1000000 = 1.0 rate

1000000 constant RATE-SCALE

\ Apply exchange rate to money
\ ( minor from-currency rate to-currency -- result to-currency )
: money-convert ( minor from-currency rate to-currency -- result to-currency )
    >r >r                           \ save to-currency and rate
    drop                            \ drop from-currency
    r> * RATE-SCALE /               \ apply rate
    r>                              \ restore to-currency
;

\ ============================================================
\ Validation
\ ============================================================

\ Check if currency code is valid
\ ( currency-code -- flag )
: currency-valid? ( currency-code -- flag )
    dup 0 >= swap CUR-ETH <= and
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-currency-version ( -- ) ." SafeCurrency for Forth v0.1.0" cr ;

: safe-currency-help ( -- )
    cr
    ." SafeCurrency - Currency Operations" cr
    ." ===================================" cr
    cr
    ." Currency Codes:" cr
    ."   CUR-USD CUR-EUR CUR-GBP CUR-JPY CUR-CHF CUR-CAD ..." cr
    ."   CUR-BTC CUR-ETH (crypto)" cr
    cr
    ." Construction:" cr
    ."   money-from-major  ( major code -- minor code )" cr
    ."   money-from-minor  ( minor code -- minor code )" cr
    ."   money-zero        ( code -- 0 code )" cr
    cr
    ." Arithmetic:" cr
    ."   money+   ( m1 c1 m2 c2 -- result code flag )" cr
    ."   money-   ( m1 c1 m2 c2 -- result code flag )" cr
    ."   money*   ( minor code n -- result code )" cr
    ."   money/   ( minor code n -- result code flag )" cr
    ."   money-negate money-abs" cr
    cr
    ." Comparison:" cr
    ."   money=        ( m1 c1 m2 c2 -- flag )" cr
    ."   money-zero?   money-positive?   money-negative?" cr
    cr
    ." Formatting:" cr
    ."   money-format        ( minor code -- addr len )" cr
    ."   money-format-symbol ( minor code -- addr len )" cr
    cr
    ." Properties:" cr
    ."   currency-decimals   ( code -- n )" cr
    ."   currency-code>string ( code -- addr len )" cr
    ."   currency-symbol     ( code -- addr len )" cr
    ."   string>currency-code ( addr len -- code flag )" cr
    cr
;
