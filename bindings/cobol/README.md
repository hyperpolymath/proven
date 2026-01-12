# Proven for COBOL

Safe, validated operations library for COBOL applications.

## Installation

Copy the COBOL source files to your project and compile with GnuCOBOL or your COBOL compiler:

```bash
# Compile all modules with GnuCOBOL
cobc -c SAFE-MATH.cob
cobc -c SAFE-STRING.cob
cobc -c SAFE-PATH.cob
cobc -c SAFE-EMAIL.cob
cobc -c SAFE-NETWORK.cob
cobc -c SAFE-CRYPTO.cob
cobc -c SAFE-UUID.cob
cobc -c SAFE-CURRENCY.cob
cobc -c SAFE-PHONE.cob
cobc -c SAFE-HEX.cob
```

## Usage

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAMPLE-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-A                  PIC S9(18) VALUE 1000000000.
       01 WS-B                  PIC S9(18) VALUE 2000000000.
       01 WS-RESULT             PIC S9(18).
       01 WS-STATUS             PIC 9.

       01 WS-INPUT              PIC X(100).
       01 WS-INPUT-LEN          PIC 9(4).
       01 WS-OUTPUT             PIC X(200).
       01 WS-OUTPUT-LEN         PIC 9(4).

       01 WS-EMAIL              PIC X(254).
       01 WS-EMAIL-LEN          PIC 9(4).
       01 WS-LOCAL              PIC X(64).
       01 WS-DOMAIN             PIC X(255).
       01 WS-ERROR              PIC X(100).

       01 WS-IP                 PIC X(15).
       01 WS-IP-LEN             PIC 9(2).
       01 WS-OCTET-1            PIC 9(3).
       01 WS-OCTET-2            PIC 9(3).
       01 WS-OCTET-3            PIC 9(3).
       01 WS-OCTET-4            PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.

      *    Safe math with overflow checking
           CALL "SAFE-ADD" USING WS-A WS-B WS-RESULT WS-STATUS.
           IF WS-STATUS = 1
              DISPLAY "Sum: " WS-RESULT
           ELSE
              DISPLAY "Overflow!"
           END-IF.

      *    XSS prevention
           MOVE "<script>alert('xss')</script>" TO WS-INPUT.
           MOVE 30 TO WS-INPUT-LEN.
           CALL "ESCAPE-HTML" USING WS-INPUT WS-INPUT-LEN
                                    WS-OUTPUT WS-OUTPUT-LEN.
           DISPLAY "Escaped: " WS-OUTPUT(1:WS-OUTPUT-LEN).

      *    Path safety
           MOVE "../../../etc/passwd" TO WS-INPUT.
           MOVE 20 TO WS-INPUT-LEN.
           CALL "HAS-TRAVERSAL" USING WS-INPUT WS-INPUT-LEN WS-STATUS.
           IF WS-STATUS = 1
              DISPLAY "Dangerous path!"
           END-IF.

      *    Email validation
           MOVE "user@example.com" TO WS-EMAIL.
           MOVE 16 TO WS-EMAIL-LEN.
           CALL "PARSE-EMAIL" USING WS-EMAIL WS-EMAIL-LEN WS-STATUS
                                    WS-LOCAL WS-DOMAIN WS-ERROR.
           IF WS-STATUS = 1
              DISPLAY "Local: " WS-LOCAL
              DISPLAY "Domain: " WS-DOMAIN
           ELSE
              DISPLAY "Invalid: " WS-ERROR
           END-IF.

      *    IP classification
           MOVE "192.168.1.1" TO WS-IP.
           MOVE 11 TO WS-IP-LEN.
           CALL "PARSE-IPV4" USING WS-IP WS-IP-LEN WS-STATUS
                                   WS-OCTET-1 WS-OCTET-2
                                   WS-OCTET-3 WS-OCTET-4.
           IF WS-STATUS = 1
              CALL "IS-PRIVATE-IP" USING WS-OCTET-1 WS-OCTET-2
                                         WS-STATUS
              IF WS-STATUS = 1
                 DISPLAY "Private network"
              END-IF
           END-IF.

           STOP RUN.
```

## Modules

### SAFE-MATH

Overflow-checked arithmetic operations:

```cobol
      * All operations use linkage section parameters
      * Status: 1 = OK, 0 = overflow/error

       CALL "SAFE-ADD" USING A B RESULT STATUS.
       CALL "SAFE-SUBTRACT" USING A B RESULT STATUS.
       CALL "SAFE-MULTIPLY" USING A B RESULT STATUS.
       CALL "SAFE-DIVIDE" USING A B RESULT STATUS.
       CALL "SAFE-MODULO" USING A B RESULT STATUS.
       CALL "CLAMP-VALUE" USING VALUE MIN-VAL MAX-VAL RESULT.
       CALL "IN-RANGE" USING VALUE MIN-VAL MAX-VAL STATUS.
```

### SAFE-STRING

XSS prevention and string sanitization:

```cobol
      * Escape HTML special characters
       CALL "ESCAPE-HTML" USING INPUT-STR INPUT-LEN
                                OUTPUT-STR OUTPUT-LEN.

      * Escape SQL single quotes
       CALL "ESCAPE-SQL" USING INPUT-STR INPUT-LEN
                               OUTPUT-STR OUTPUT-LEN.

      * Keep only alphanumeric + underscore + hyphen
       CALL "SANITIZE-DEFAULT" USING INPUT-STR INPUT-LEN
                                     OUTPUT-STR OUTPUT-LEN.

      * Convert to URL-safe slug
       CALL "SLUGIFY" USING INPUT-STR INPUT-LEN
                            OUTPUT-STR OUTPUT-LEN.
```

### SAFE-PATH

Directory traversal protection:

```cobol
      * Check for traversal patterns (returns 1 if dangerous)
       CALL "HAS-TRAVERSAL" USING PATH PATH-LEN RESULT.

      * Remove dangerous characters from filename
       CALL "SANITIZE-FILENAME" USING INPUT-PATH INPUT-LEN
                                      OUTPUT-PATH OUTPUT-LEN.

      * Safely join paths
       CALL "SAFE-PATH-JOIN" USING BASE-PATH BASE-LEN
                                   FILE-NAME FILE-LEN
                                   RESULT ERROR-MSG.
```

### SAFE-EMAIL

Email validation:

```cobol
      * Basic format validation
       CALL "IS-VALID-EMAIL" USING EMAIL EMAIL-LEN RESULT.

      * Parse into local part and domain
       CALL "PARSE-EMAIL" USING EMAIL EMAIL-LEN RESULT
                                LOCAL-PART DOMAIN ERROR-MSG.

      * Check for disposable email domains
       CALL "IS-DISPOSABLE-EMAIL" USING DOMAIN RESULT.

      * Normalize email (lowercase domain)
       CALL "NORMALIZE-EMAIL" USING EMAIL EMAIL-LEN RESULT.
```

### SAFE-NETWORK

IP address validation and classification:

```cobol
      * Parse IPv4 address
       CALL "PARSE-IPV4" USING IP-STRING IP-LEN RESULT
                               OCTET-1 OCTET-2 OCTET-3 OCTET-4.

      * Check IP type
       CALL "IS-LOOPBACK" USING OCTET-1 RESULT.
       CALL "IS-PRIVATE-IP" USING OCTET-1 OCTET-2 RESULT.
       CALL "IS-RESERVED-IP" USING OCTET-1 OCTET-2 OCTET-3 RESULT.

      * Get classification (0=invalid, 1=loopback, 2=private,
      *                     3=reserved, 4=public)
       CALL "CLASSIFY-IP" USING OCTET-1 OCTET-2 OCTET-3 CLASS.

      * Port validation
       CALL "IS-VALID-PORT" USING PORT RESULT.
       CALL "IS-PRIVILEGED-PORT" USING PORT RESULT.
```

### SAFE-CRYPTO

Basic cryptographic operations:

```cobol
      * Timing-safe comparison
       CALL "CONSTANT-TIME-EQUALS" USING DATA1 DATA2 LENGTH RESULT.

      * Simple hash (demo only - use platform crypto for production)
       CALL "SIMPLE-HASH" USING INPUT INPUT-LEN OUTPUT OUTPUT-LEN.

      * Convert bytes to hex
       CALL "BYTES-TO-HEX" USING INPUT INPUT-LEN OUTPUT OUTPUT-LEN.

      * Generate random token
       CALL "GENERATE-TOKEN" USING TOKEN-LEN OUTPUT OUTPUT-LEN.

      * Securely wipe memory
       CALL "SECURE-WIPE" USING DATA DATA-LEN.
```

**Note:** The crypto operations use basic algorithms for demonstration. For production applications, integrate with platform-specific cryptographic APIs (IBM ICSF, OpenSSL via C interop, etc.).

### SAFE-UUID

UUID parsing and validation:

```cobol
      * Parse and validate UUID string (36-char canonical format)
       CALL "PARSE-UUID" USING UUID-STRING RESULT ERROR-MSG.

      * Format UUID bytes to canonical string
       CALL "FORMAT-UUID" USING UUID-BYTES USE-UPPERCASE
                                UUID-STRING RESULT.

      * Get UUID version (1-7)
       CALL "GET-UUID-VERSION" USING UUID-STRING VERSION RESULT.

      * Get UUID variant (0=NCS, 1=RFC4122, 2=Microsoft, 3=Future)
       CALL "GET-UUID-VARIANT" USING UUID-STRING VARIANT RESULT.

      * Check if UUID is nil (all zeros)
       CALL "IS-NIL-UUID" USING UUID-STRING RESULT.

      * Normalize UUID to lowercase canonical form
       CALL "NORMALIZE-UUID" USING UUID-STRING RESULT.
```

**Copybook:** Include `SAFEUUID.cpy` for UUID data structures:

```cobol
       COPY SAFEUUID.
      * Provides: UUID-RECORD, UUID-BYTES, UUID-VERSION-INFO, etc.
```

### SAFE-CURRENCY

Currency arithmetic with overflow protection:

```cobol
      * Add two money amounts (minor units)
       CALL "MONEY-ADD" USING CURRENCY-A MINOR-A
                              CURRENCY-B MINOR-B
                              RESULT-MINOR STATUS ERROR-MSG.

      * Subtract money amounts
       CALL "MONEY-SUBTRACT" USING CURRENCY-A MINOR-A
                                   CURRENCY-B MINOR-B
                                   RESULT-MINOR STATUS ERROR-MSG.

      * Multiply money by integer
       CALL "MONEY-MULTIPLY" USING MINOR-A MULTIPLIER
                                   RESULT-MINOR STATUS ERROR-MSG.

      * Divide money by integer
       CALL "MONEY-DIVIDE" USING MINOR-A DIVISOR
                                 RESULT-MINOR STATUS ERROR-MSG.

      * Get decimal places for currency (USD=2, JPY=0, BTC=8)
       CALL "GET-DECIMAL-PLACES" USING CURRENCY-CODE
                                       DECIMAL-PLACES STATUS.

      * Convert minor units to major (cents to dollars)
       CALL "MINOR-TO-MAJOR" USING MINOR-UNITS DECIMAL-PLACES
                                   MAJOR-AMOUNT.

      * Convert major to minor (dollars to cents)
       CALL "MAJOR-TO-MINOR" USING MAJOR-AMOUNT DECIMAL-PLACES
                                   MINOR-UNITS STATUS.

      * Validate currency code
       CALL "IS-VALID-CURRENCY" USING CURRENCY-CODE STATUS.

      * Format money for display
       CALL "FORMAT-MONEY" USING MINOR-AMOUNT CURRENCY-CODE
                                 USE-THOUSANDS-SEP
                                 FORMATTED-OUTPUT OUTPUT-LEN.
```

**Copybook:** Include `SAFECURR.cpy` for currency data structures:

```cobol
       COPY SAFECURR.
      * Provides: CURRENCY-CODE (with 88 levels for USD, EUR, etc.),
      *           MONEY-RECORD, MONEY-RESULT, EXCHANGE-RATE-RECORD
```

### SAFE-PHONE

Phone number parsing and formatting (E.164):

```cobol
      * Parse phone number string
       CALL "PARSE-PHONE" USING PHONE-INPUT INPUT-LENGTH
                                DEFAULT-COUNTRY
                                COUNTRY-CODE NATIONAL-NUMBER EXTENSION
                                RESULT ERROR-MSG.

      * Format phone number for display
      * FORMAT-TYPE: 0=E164, 1=International, 2=National, 3=RFC3966
       CALL "FORMAT-PHONE" USING COUNTRY-CODE NATIONAL-NUMBER
                                 FORMAT-TYPE
                                 FORMATTED-OUTPUT OUTPUT-LENGTH.

      * Convert to E.164 format (+1XXXXXXXXXX)
       CALL "TO-E164" USING COUNTRY-CODE NATIONAL-NUMBER
                            E164-OUTPUT OUTPUT-LENGTH RESULT.

      * Basic phone validation (7-15 digits)
       CALL "IS-VALID-PHONE" USING PHONE-INPUT INPUT-LENGTH RESULT.
```

**Copybook:** Include `SAFEPHONE.cpy` for phone data structures:

```cobol
       COPY SAFEPHONE.
      * Provides: COUNTRY-CODE (with 88 levels for US, UK, etc.),
      *           PHONE-NUMBER-RECORD, PHONE-NUMBER-TYPE,
      *           PHONE-PARSE-RESULT, PHONE-FORMAT-OPTIONS
```

### SAFE-HEX

Hexadecimal encoding and decoding:

```cobol
      * Encode bytes to hex string
       CALL "HEX-ENCODE" USING INPUT-BYTES INPUT-LENGTH
                               USE-UPPERCASE ADD-PREFIX
                               OUTPUT-STRING OUTPUT-LENGTH RESULT.

      * Encode with byte separator (for MAC addresses, etc.)
       CALL "HEX-ENCODE-SEP" USING INPUT-BYTES INPUT-LENGTH
                                   USE-UPPERCASE SEPARATOR
                                   OUTPUT-STRING OUTPUT-LENGTH RESULT.

      * Decode hex string to bytes
       CALL "HEX-DECODE" USING HEX-STRING HEX-LENGTH
                               OUTPUT-BYTES OUTPUT-LENGTH
                               RESULT ERROR-MSG.

      * Validate hex string
       CALL "IS-VALID-HEX-STRING" USING HEX-STRING HEX-LENGTH RESULT.

      * Convert single byte to hex
       CALL "BYTE-TO-HEX" USING INPUT-BYTE USE-UPPERCASE
                                OUTPUT-HEX.

      * Convert hex pair to single byte
       CALL "HEX-TO-BYTE" USING HEX-PAIR OUTPUT-BYTE RESULT.

      * Normalize hex (lowercase, strip prefix)
       CALL "NORMALIZE-HEX" USING HEX-STRING HEX-LENGTH
                                  OUTPUT-STRING OUTPUT-LENGTH.
```

**Copybook:** Include `SAFEHEX.cpy` for hex data structures:

```cobol
       COPY SAFEHEX.
      * Provides: HEX-ENCODE-OPTIONS, HEX-ENCODE-RESULT,
      *           HEX-DECODE-RESULT, HEX-RGB-COLOR, HEX-MAC-ADDRESS,
      *           pre-sized fields for MD5, SHA1, SHA256, SHA512
```

## Data Types

### Numeric Ranges

```cobol
      * Safe math uses 18-digit signed integers
       01 WS-VALUE              PIC S9(18).

      * Maximum: +999999999999999999
      * Minimum: -999999999999999999
```

### Status Codes

```cobol
      * Return status values
       01 WS-STATUS             PIC 9.
          88 WS-SUCCESS         VALUE 1.
          88 WS-FAILURE         VALUE 0.
```

### IP Classification Codes

```cobol
      * IP classification values
       01 WS-IP-CLASS           PIC 9.
          88 WS-IP-INVALID      VALUE 0.
          88 WS-IP-LOOPBACK     VALUE 1.
          88 WS-IP-PRIVATE      VALUE 2.
          88 WS-IP-RESERVED     VALUE 3.
          88 WS-IP-PUBLIC       VALUE 4.
```

## Examples

### Safe Financial Calculation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINANCIAL-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL          PIC S9(18) VALUE 1000000.
       01 WS-RATE               PIC S9(18) VALUE 5.
       01 WS-INTEREST           PIC S9(18).
       01 WS-TEMP               PIC S9(18).
       01 WS-STATUS             PIC 9.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      *    Calculate interest = principal * rate / 100
           CALL "SAFE-MULTIPLY" USING WS-PRINCIPAL WS-RATE
                                      WS-TEMP WS-STATUS.
           IF WS-STATUS = 0
              DISPLAY "Overflow in multiplication"
              STOP RUN
           END-IF.

           MOVE 100 TO WS-RATE.
           CALL "SAFE-DIVIDE" USING WS-TEMP WS-RATE
                                    WS-INTEREST WS-STATUS.
           IF WS-STATUS = 0
              DISPLAY "Division error"
              STOP RUN
           END-IF.

           DISPLAY "Interest: " WS-INTEREST.
           STOP RUN.
```

### Input Sanitization for Reports

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT-SANITIZER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USER-INPUT         PIC X(200).
       01 WS-INPUT-LEN          PIC 9(4).
       01 WS-SAFE-OUTPUT        PIC X(400).
       01 WS-OUTPUT-LEN         PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      *    User input might contain dangerous characters
           MOVE "<b>Bold</b> & 'quoted'" TO WS-USER-INPUT.
           MOVE 23 TO WS-INPUT-LEN.

      *    Escape for HTML output
           CALL "ESCAPE-HTML" USING WS-USER-INPUT WS-INPUT-LEN
                                    WS-SAFE-OUTPUT WS-OUTPUT-LEN.

           DISPLAY "Safe HTML: " WS-SAFE-OUTPUT(1:WS-OUTPUT-LEN).
      *    Output: &lt;b&gt;Bold&lt;/b&gt; &amp; &#x27;quoted&#x27;

           STOP RUN.
```

## Compatibility

This library is designed for:
- GnuCOBOL 3.x+
- IBM Enterprise COBOL
- Micro Focus COBOL
- Other COBOL-85/2002 compliant compilers

Note: Some intrinsic functions (LOWER-CASE, ORD, MOD) require COBOL-85 or later.

## License

PMPL-1.0
