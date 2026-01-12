      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven - Common data types for COBOL bindings
      *

      * Return status codes
       01 WS-RETURN-STATUS.
          05 WS-SUCCESS               PIC 9 VALUE 0.
          05 WS-ERROR                 PIC 9 VALUE 1.
          05 WS-OVERFLOW              PIC 9 VALUE 2.
          05 WS-INVALID               PIC 9 VALUE 3.

      * Boolean values
       01 WS-BOOLEAN.
          05 WS-TRUE                  PIC 9 VALUE 1.
          05 WS-FALSE                 PIC 9 VALUE 0.

      * Maximum string lengths
       01 WS-MAX-LENGTHS.
          05 WS-MAX-STRING            PIC 9(4) VALUE 4096.
          05 WS-MAX-EMAIL             PIC 9(3) VALUE 254.
          05 WS-MAX-PATH              PIC 9(4) VALUE 1024.
          05 WS-MAX-HOSTNAME          PIC 9(3) VALUE 253.

      * IPv4 address structure
       01 WS-IPV4-ADDRESS.
          05 WS-IP-OCTET-1            PIC 9(3).
          05 WS-IP-OCTET-2            PIC 9(3).
          05 WS-IP-OCTET-3            PIC 9(3).
          05 WS-IP-OCTET-4            PIC 9(3).
          05 WS-IP-STRING             PIC X(15).
          05 WS-IP-VALID              PIC 9 VALUE 0.

      * IP classification codes
       01 WS-IP-CLASSIFICATIONS.
          05 WS-IP-CLASS-INVALID      PIC 9 VALUE 0.
          05 WS-IP-CLASS-LOOPBACK     PIC 9 VALUE 1.
          05 WS-IP-CLASS-PRIVATE      PIC 9 VALUE 2.
          05 WS-IP-CLASS-RESERVED     PIC 9 VALUE 3.
          05 WS-IP-CLASS-PUBLIC       PIC 9 VALUE 4.

      * Email result structure
       01 WS-EMAIL-RESULT.
          05 WS-EMAIL-VALID           PIC 9 VALUE 0.
          05 WS-EMAIL-LOCAL           PIC X(64).
          05 WS-EMAIL-DOMAIN          PIC X(255).
          05 WS-EMAIL-ERROR           PIC X(100).

      * Path result structure
       01 WS-PATH-RESULT.
          05 WS-PATH-VALID            PIC 9 VALUE 0.
          05 WS-PATH-VALUE            PIC X(1024).
          05 WS-PATH-ERROR            PIC X(100).
