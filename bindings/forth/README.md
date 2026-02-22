# Proven for Forth

Safe, validated operations library for Forth applications.

## Installation

Include the library in your Forth program:

```forth
include proven.fs
```

## Usage

```forth
include proven.fs

\ Safe math with overflow checking
1000000000 2000000000 safe+ .  \ prints result
\ flag on stack: 0 = ok, -1 = overflow

\ XSS prevention
s" <script>alert('xss')</script>" escape-html type
\ prints: &lt;script&gt;alert('xss')&lt;/script&gt;

\ Path safety
s" ../../../etc/passwd" has-traversal? if
    ." Dangerous path!" cr
then

\ Email validation
s" user@example.com" valid-email? if
    ." Valid email" cr
then

\ IP parsing
s" 192.168.1.1" parse-ipv4 if
    ip-private? if
        ." Private network" cr
    then
then

\ Port validation
8080 valid-port? if
    ." Valid port" cr
then
```

## Stack Effects

All words document their stack effects in standard Forth notation.

### SafeMath

Overflow-checked arithmetic operations:

```forth
\ All operations return ( result flag ) where flag is 0 for ok, -1 for overflow

safe+        ( a b -- result flag )       \ Add with overflow check
safe-        ( a b -- result flag )       \ Subtract with overflow check
safe*        ( a b -- result flag )       \ Multiply with overflow check
safe/        ( a b -- result flag )       \ Divide with zero check
safe-mod     ( a b -- result flag )       \ Modulo with zero check
safe-abs     ( a -- result flag )         \ Absolute value
safe-negate  ( a -- result flag )         \ Negate

clamp        ( value min max -- clamped ) \ Clamp to range
in-range?    ( value min max -- flag )    \ Check if in range
```

### SafeString

XSS prevention and string sanitization:

```forth
escape-html      ( addr len -- addr' len' )  \ Escape HTML entities
escape-sql       ( addr len -- addr' len' )  \ Escape SQL quotes
sanitize-default ( addr len -- addr' len' )  \ Keep only alphanumeric + _-
```

### SafePath

Directory traversal protection:

```forth
has-traversal?    ( addr len -- flag )       \ Check for traversal
sanitize-filename ( addr len -- addr' len' ) \ Remove dangerous chars
```

### SafeEmail

Email validation:

```forth
valid-email? ( addr len -- flag )  \ Basic email format check
find-at      ( addr len -- pos )   \ Find @ position (-1 if not found)
```

### SafeNetwork

IP address validation and classification:

```forth
parse-ipv4      ( addr len -- flag )  \ Parse IP, store in ip-a/b/c/d
ip-loopback?    ( -- flag )           \ Check if parsed IP is loopback
ip-private?     ( -- flag )           \ Check if parsed IP is private
valid-port?     ( port -- flag )      \ Check port in range 1-65535
privileged-port? ( port -- flag )     \ Check port < 1024
```

### SafeCrypto

Basic cryptographic operations:

```forth
simple-hash     ( addr len -- hash )        \ Simple XOR hash (demo only)
constant-time=  ( addr1 addr2 len -- flag ) \ Timing-safe compare
```

**Note:** The crypto operations are simplified for demonstration. Use a proper cryptographic library for production applications.

## Examples

### Overflow-Safe Calculation

```forth
\ Calculate factorial with overflow detection
: safe-factorial ( n -- result flag )
    dup 0= if drop 1 0 exit then
    1 0                         \ result flag
    rot 1+ 1 do
        swap i safe*
        dup if                  \ overflow?
            nip nip unloop exit
        then
        drop swap
    loop
    swap drop
;

20 safe-factorial .s  \ Should show large result and 0 (ok)
21 safe-factorial .s  \ May overflow
```

### Input Sanitization

```forth
\ Sanitize user input for display
: safe-display ( addr len -- )
    escape-html type
;

s" User said: <script>evil()</script>" safe-display
\ Output: User said: &lt;script&gt;evil()&lt;/script&gt;
```

### Path Validation

```forth
\ Safe file access
: safe-open ( addr len -- fid flag )
    2dup has-traversal? if
        2drop 0 -1              \ return error
    else
        r/o open-file           \ open normally
    then
;
```

## Compatibility

This library uses standard ANS Forth words and should work with:
- Gforth
- SwiftForth
- VFX Forth
- Other ANS-compliant implementations

## Additional Modules

### SafeUUID

UUID parsing, formatting, and validation (RFC 4122):

```forth
include safe-uuid.fs

\ Parse UUID
s" 550e8400-e29b-41d4-a716-446655440000" uuid-buffer uuid-parse if
    uuid-buffer uuid-format type cr   \ Canonical format
    uuid-buffer uuid-format-urn type cr  \ urn:uuid:...
then

\ Quick validation
s" 550e8400-e29b-41d4-a716-446655440000" uuid? if
    ." Looks like a UUID" cr
then

\ Properties
uuid-buffer uuid-version .    \ 4 for v4 UUID
uuid-buffer uuid-nil? .       \ Check if nil UUID
```

### SafeCurrency

ISO 4217 currency codes and monetary arithmetic:

```forth
include safe-currency.fs

\ Create money (100 dollars = 10000 cents)
100 CUR-USD money-from-major  ( -- 10000 CUR-USD )

\ Arithmetic with currency safety
100 CUR-USD money-from-major
50 CUR-USD money-from-major money+ if
    money-format type cr    \ 150.00 USD
then

\ Formatting
5099 CUR-USD money-format-symbol type cr  \ $50.99

\ Currency properties
CUR-JPY currency-decimals .  \ 0 (yen has no decimals)
CUR-BTC currency-decimals .  \ 8 (satoshis)
```

### SafePhone

Phone number validation and E.164 formatting:

```forth
include safe-phone.fs

\ Parse phone number
s" +1 555 123 4567" phone-parse if
    phone-format-e164 type cr     \ +15551234567
    phone-format-intl type cr     \ +1 555 123 4567
    phone-get-cc .                \ 1
then

\ Validation
s" +44 20 7946 0958" phone-valid? if
    ." Valid UK number" cr
then

\ Country codes
CC-44 country-code>name type cr   \ United Kingdom
```

### SafeHex

Hexadecimal encoding/decoding with constant-time comparison:

```forth
include safe-hex.fs

\ Encode bytes to hex
s" ABC" hex-encode type cr        \ 414243

\ Decode hex to bytes
s" 48454c4c4f" hex-decode if
    type cr                       \ HELLO
then

\ Constant-time comparison (for crypto)
s" abc123" s" ABC123" hex-constant-time-equal .  \ Compare safely

\ Formatting
s" AABBCC" hex-decode if
    hex-format-spaced type cr     \ aa bb cc
    hex-format-colons type cr     \ aa:bb:cc
    hex-format-0x type cr         \ 0xaabbcc
then

\ Integer conversion
255 int>hex type cr               \ ff
s" ff" hex>int if . then cr       \ 255
```

## License

PMPL-1.0-or-later
