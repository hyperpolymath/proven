      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeUUID - UUID data structures for COBOL
      *

      * UUID record structure (36-char canonical format)
      * Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
       01 UUID-RECORD.
          05 UUID-SEGMENT-1           PIC X(8).
          05 UUID-DASH-1              PIC X VALUE "-".
          05 UUID-SEGMENT-2           PIC X(4).
          05 UUID-DASH-2              PIC X VALUE "-".
          05 UUID-SEGMENT-3           PIC X(4).
          05 UUID-DASH-3              PIC X VALUE "-".
          05 UUID-SEGMENT-4           PIC X(4).
          05 UUID-DASH-4              PIC X VALUE "-".
          05 UUID-SEGMENT-5           PIC X(12).
       01 UUID-STRING REDEFINES UUID-RECORD PIC X(36).

      * UUID as raw bytes (16 bytes / 128 bits)
       01 UUID-BYTES.
          05 UUID-BYTE                OCCURS 16 TIMES PIC X.

      * UUID parsing result
       01 UUID-PARSE-RESULT.
          05 UUID-PARSE-STATUS        PIC 9 VALUE 0.
             88 UUID-PARSE-OK         VALUE 1.
             88 UUID-PARSE-FAILED     VALUE 0.
          05 UUID-PARSE-ERROR         PIC X(50).

      * UUID version information
       01 UUID-VERSION-INFO.
          05 UUID-VERSION             PIC 9 VALUE 0.
             88 UUID-V1-TIME          VALUE 1.
             88 UUID-V2-DCE           VALUE 2.
             88 UUID-V3-MD5           VALUE 3.
             88 UUID-V4-RANDOM        VALUE 4.
             88 UUID-V5-SHA1          VALUE 5.
             88 UUID-V6-SORTABLE      VALUE 6.
             88 UUID-V7-UNIX-TS       VALUE 7.
          05 UUID-VARIANT             PIC 9 VALUE 0.
             88 UUID-VARIANT-NCS      VALUE 0.
             88 UUID-VARIANT-RFC4122  VALUE 1.
             88 UUID-VARIANT-MS       VALUE 2.
             88 UUID-VARIANT-FUTURE   VALUE 3.

      * Nil UUID constant
       01 UUID-NIL                    PIC X(36)
          VALUE "00000000-0000-0000-0000-000000000000".

      * Max UUID constant
       01 UUID-MAX                    PIC X(36)
          VALUE "ffffffff-ffff-ffff-ffff-ffffffffffff".

