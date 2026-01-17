      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeHex - Hexadecimal data structures for COBOL
      *

      * Hex encoding lookup table
       01 HEX-CHARS-UPPER             PIC X(16) VALUE "0123456789ABCDEF".
       01 HEX-CHARS-LOWER             PIC X(16) VALUE "0123456789abcdef".

      * Hex encoding options
       01 HEX-ENCODE-OPTIONS.
          05 HEX-USE-UPPERCASE        PIC 9 VALUE 1.
             88 HEX-UPPER             VALUE 1.
             88 HEX-LOWER             VALUE 0.
          05 HEX-ADD-PREFIX           PIC 9 VALUE 0.
             88 HEX-WITH-PREFIX       VALUE 1.
             88 HEX-NO-PREFIX         VALUE 0.
          05 HEX-PREFIX-STYLE         PIC 9 VALUE 0.
             88 HEX-PREFIX-0X         VALUE 0.
             88 HEX-PREFIX-HASH       VALUE 1.
             88 HEX-PREFIX-X          VALUE 2.
          05 HEX-BYTE-SEPARATOR       PIC X VALUE SPACE.
          05 HEX-GROUP-SIZE           PIC 9 VALUE 0.

      * Hex encoding result
       01 HEX-ENCODE-RESULT.
          05 HEX-ENCODE-STATUS        PIC 9 VALUE 0.
             88 HEX-ENCODE-OK         VALUE 1.
             88 HEX-ENCODE-FAILED     VALUE 0.
          05 HEX-ENCODE-OUTPUT-LEN    PIC 9(6).
          05 HEX-ENCODE-ERROR         PIC X(50).

      * Hex decoding result
       01 HEX-DECODE-RESULT.
          05 HEX-DECODE-STATUS        PIC 9 VALUE 0.
             88 HEX-DECODE-OK         VALUE 1.
             88 HEX-DECODE-FAILED     VALUE 0.
          05 HEX-DECODE-OUTPUT-LEN    PIC 9(6).
          05 HEX-DECODE-ERROR         PIC X(50).

      * Common hex-encoded data types
       01 HEX-BYTE-VALUE              PIC X(2).
       01 HEX-WORD-VALUE              PIC X(4).
       01 HEX-DWORD-VALUE             PIC X(8).
       01 HEX-QWORD-VALUE             PIC X(16).

      * Hash/digest sizes in hex
       01 HEX-MD5-VALUE               PIC X(32).
       01 HEX-SHA1-VALUE              PIC X(40).
       01 HEX-SHA256-VALUE            PIC X(64).
       01 HEX-SHA512-VALUE            PIC X(128).

      * Color representation (RGB hex)
       01 HEX-RGB-COLOR.
          05 HEX-COLOR-PREFIX         PIC X VALUE "#".
          05 HEX-COLOR-RED            PIC X(2).
          05 HEX-COLOR-GREEN          PIC X(2).
          05 HEX-COLOR-BLUE           PIC X(2).
       01 HEX-RGB-STRING REDEFINES HEX-RGB-COLOR PIC X(7).

      * MAC address (hex with colons)
       01 HEX-MAC-ADDRESS.
          05 HEX-MAC-OCTET-1          PIC X(2).
          05 HEX-MAC-SEP-1            PIC X VALUE ":".
          05 HEX-MAC-OCTET-2          PIC X(2).
          05 HEX-MAC-SEP-2            PIC X VALUE ":".
          05 HEX-MAC-OCTET-3          PIC X(2).
          05 HEX-MAC-SEP-3            PIC X VALUE ":".
          05 HEX-MAC-OCTET-4          PIC X(2).
          05 HEX-MAC-SEP-4            PIC X VALUE ":".
          05 HEX-MAC-OCTET-5          PIC X(2).
          05 HEX-MAC-SEP-5            PIC X VALUE ":".
          05 HEX-MAC-OCTET-6          PIC X(2).
       01 HEX-MAC-STRING REDEFINES HEX-MAC-ADDRESS PIC X(17).

      * Byte array for encoding/decoding
       01 HEX-BYTE-ARRAY.
          05 HEX-BYTE-COUNT           PIC 9(6).
          05 HEX-BYTE-DATA            PIC X OCCURS 1024 TIMES.

      * Hex string buffer
       01 HEX-STRING-BUFFER.
          05 HEX-STRING-LENGTH        PIC 9(6).
          05 HEX-STRING-DATA          PIC X(2048).

