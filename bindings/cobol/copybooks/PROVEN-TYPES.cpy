      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *
      * Proven FFI - Common data types for COBOL bindings.
      *
      * These data items map to the C types declared in proven.h.
      * All computation delegates to libproven (Idris 2 + Zig)
      * via the CALL verb.
      *

      * ProvenStatus codes (matching proven.h)
       01 WS-PROVEN-STATUS.
          05 PROVEN-OK                  PIC S9(9) COMP VALUE 0.
          05 PROVEN-ERR-NULL-PTR        PIC S9(9) COMP VALUE -1.
          05 PROVEN-ERR-INVALID-ARG     PIC S9(9) COMP VALUE -2.
          05 PROVEN-ERR-OVERFLOW        PIC S9(9) COMP VALUE -3.
          05 PROVEN-ERR-UNDERFLOW       PIC S9(9) COMP VALUE -4.
          05 PROVEN-ERR-DIV-BY-ZERO     PIC S9(9) COMP VALUE -5.
          05 PROVEN-ERR-PARSE-FAIL      PIC S9(9) COMP VALUE -6.
          05 PROVEN-ERR-VALIDATION      PIC S9(9) COMP VALUE -7.
          05 PROVEN-ERR-OUT-OF-BOUNDS   PIC S9(9) COMP VALUE -8.
          05 PROVEN-ERR-ENCODING        PIC S9(9) COMP VALUE -9.
          05 PROVEN-ERR-ALLOC           PIC S9(9) COMP VALUE -10.

      * ProvenIntResult (matching C struct)
       01 WS-INT-RESULT.
          05 WS-INT-STATUS              PIC S9(9) COMP.
          05 WS-INT-VALUE               PIC S9(18) COMP.

      * ProvenBoolResult (matching C struct)
       01 WS-BOOL-RESULT.
          05 WS-BOOL-STATUS             PIC S9(9) COMP.
          05 WS-BOOL-VALUE              PIC 9 COMP.

      * ProvenFloatResult (matching C struct)
       01 WS-FLOAT-RESULT.
          05 WS-FLOAT-STATUS            PIC S9(9) COMP.
          05 WS-FLOAT-VALUE             COMP-2.

      * ProvenStringResult (matching C struct)
       01 WS-STRING-RESULT.
          05 WS-STR-STATUS              PIC S9(9) COMP.
          05 WS-STR-PTR                 USAGE POINTER.
          05 WS-STR-LENGTH              PIC 9(9) COMP.

      * Working buffer for string parameters
       01 WS-INPUT-BUFFER               PIC X(4096).
       01 WS-INPUT-LENGTH               PIC 9(9) COMP.

      * Lifecycle status
       01 WS-INIT-STATUS                PIC S9(9) COMP.
