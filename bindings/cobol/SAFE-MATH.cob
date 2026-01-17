      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeMath - Overflow-checked arithmetic for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-MATH.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Maximum and minimum values for signed 18-digit integers
       01 WS-INT-LIMITS.
          05 WS-MAX-INT        PIC S9(18) VALUE 999999999999999999.
          05 WS-MIN-INT        PIC S9(18) VALUE -999999999999999999.

      * Working variables for calculations
       01 WS-CALC-VARS.
          05 WS-TEMP-A         PIC S9(18).
          05 WS-TEMP-B         PIC S9(18).
          05 WS-TEMP-RESULT    PIC S9(18).
          05 WS-CHECK-LIMIT    PIC S9(18).

      * Safe operation result
       01 WS-SAFE-RESULT.
          05 WS-RESULT-VALUE   PIC S9(18).
          05 WS-RESULT-OK      PIC 9 VALUE 1.
             88 WS-OP-SUCCESS  VALUE 1.
             88 WS-OP-OVERFLOW VALUE 0.

       LINKAGE SECTION.
       01 LS-OPERAND-A         PIC S9(18).
       01 LS-OPERAND-B         PIC S9(18).
       01 LS-RESULT            PIC S9(18).
       01 LS-STATUS            PIC 9.
       01 LS-MIN-VALUE         PIC S9(18).
       01 LS-MAX-VALUE         PIC S9(18).

       PROCEDURE DIVISION.

      *================================================================
      * SAFE-ADD: Add two numbers with overflow checking
      * Input:  LS-OPERAND-A, LS-OPERAND-B
      * Output: LS-RESULT, LS-STATUS (1=OK, 0=OVERFLOW)
      *================================================================
       SAFE-ADD SECTION.
           ENTRY "SAFE-ADD" USING LS-OPERAND-A LS-OPERAND-B
                                  LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE LS-OPERAND-A TO WS-TEMP-A
           MOVE LS-OPERAND-B TO WS-TEMP-B

      *    Check for positive overflow: b > 0 and a > MAX - b
           IF WS-TEMP-B > 0
              COMPUTE WS-CHECK-LIMIT = WS-MAX-INT - WS-TEMP-B
              IF WS-TEMP-A > WS-CHECK-LIMIT
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
                 GOBACK
              END-IF
           END-IF

      *    Check for negative overflow: b < 0 and a < MIN - b
           IF WS-TEMP-B < 0
              COMPUTE WS-CHECK-LIMIT = WS-MIN-INT - WS-TEMP-B
              IF WS-TEMP-A < WS-CHECK-LIMIT
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
                 GOBACK
              END-IF
           END-IF

      *    Safe to add
           COMPUTE LS-RESULT = WS-TEMP-A + WS-TEMP-B
           GOBACK.

       SAFE-ADD-EXIT.
           EXIT.

      *================================================================
      * SAFE-SUBTRACT: Subtract with overflow checking
      * Input:  LS-OPERAND-A, LS-OPERAND-B
      * Output: LS-RESULT (A - B), LS-STATUS
      *================================================================
       SAFE-SUBTRACT SECTION.
           ENTRY "SAFE-SUBTRACT" USING LS-OPERAND-A LS-OPERAND-B
                                       LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE LS-OPERAND-A TO WS-TEMP-A
           MOVE LS-OPERAND-B TO WS-TEMP-B

      *    Check for positive overflow: b < 0 and a > MAX + b
           IF WS-TEMP-B < 0
              COMPUTE WS-CHECK-LIMIT = WS-MAX-INT + WS-TEMP-B
              IF WS-TEMP-A > WS-CHECK-LIMIT
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
                 GOBACK
              END-IF
           END-IF

      *    Check for negative overflow: b > 0 and a < MIN + b
           IF WS-TEMP-B > 0
              COMPUTE WS-CHECK-LIMIT = WS-MIN-INT + WS-TEMP-B
              IF WS-TEMP-A < WS-CHECK-LIMIT
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
                 GOBACK
              END-IF
           END-IF

      *    Safe to subtract
           COMPUTE LS-RESULT = WS-TEMP-A - WS-TEMP-B
           GOBACK.

       SAFE-SUBTRACT-EXIT.
           EXIT.

      *================================================================
      * SAFE-MULTIPLY: Multiply with overflow checking
      * Input:  LS-OPERAND-A, LS-OPERAND-B
      * Output: LS-RESULT, LS-STATUS
      *================================================================
       SAFE-MULTIPLY SECTION.
           ENTRY "SAFE-MULTIPLY" USING LS-OPERAND-A LS-OPERAND-B
                                       LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE LS-OPERAND-A TO WS-TEMP-A
           MOVE LS-OPERAND-B TO WS-TEMP-B

      *    Handle zero cases
           IF WS-TEMP-A = 0 OR WS-TEMP-B = 0
              MOVE 0 TO LS-RESULT
              GOBACK
           END-IF

      *    Check overflow by computing and verifying
           COMPUTE WS-TEMP-RESULT = WS-TEMP-A * WS-TEMP-B
               ON SIZE ERROR
                  MOVE 0 TO LS-RESULT
                  MOVE 0 TO LS-STATUS
                  GOBACK
           END-COMPUTE

      *    Verify by division
           IF WS-TEMP-A NOT = 0
              COMPUTE WS-CHECK-LIMIT = WS-TEMP-RESULT / WS-TEMP-A
              IF WS-CHECK-LIMIT NOT = WS-TEMP-B
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
                 GOBACK
              END-IF
           END-IF

           MOVE WS-TEMP-RESULT TO LS-RESULT
           GOBACK.

       SAFE-MULTIPLY-EXIT.
           EXIT.

      *================================================================
      * SAFE-DIVIDE: Divide with zero check
      * Input:  LS-OPERAND-A, LS-OPERAND-B
      * Output: LS-RESULT (A / B), LS-STATUS
      *================================================================
       SAFE-DIVIDE SECTION.
           ENTRY "SAFE-DIVIDE" USING LS-OPERAND-A LS-OPERAND-B
                                     LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE LS-OPERAND-A TO WS-TEMP-A
           MOVE LS-OPERAND-B TO WS-TEMP-B

      *    Check for division by zero
           IF WS-TEMP-B = 0
              MOVE 0 TO LS-RESULT
              MOVE 0 TO LS-STATUS
              GOBACK
           END-IF

      *    Check for MIN / -1 overflow
           IF WS-TEMP-A = WS-MIN-INT AND WS-TEMP-B = -1
              MOVE 0 TO LS-RESULT
              MOVE 0 TO LS-STATUS
              GOBACK
           END-IF

      *    Safe to divide
           COMPUTE LS-RESULT = WS-TEMP-A / WS-TEMP-B
           GOBACK.

       SAFE-DIVIDE-EXIT.
           EXIT.

      *================================================================
      * SAFE-MODULO: Modulo with zero check
      * Input:  LS-OPERAND-A, LS-OPERAND-B
      * Output: LS-RESULT, LS-STATUS
      *================================================================
       SAFE-MODULO SECTION.
           ENTRY "SAFE-MODULO" USING LS-OPERAND-A LS-OPERAND-B
                                     LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS

      *    Check for division by zero
           IF LS-OPERAND-B = 0
              MOVE 0 TO LS-RESULT
              MOVE 0 TO LS-STATUS
              GOBACK
           END-IF

      *    Compute modulo using FUNCTION MOD
           COMPUTE LS-RESULT = FUNCTION MOD(LS-OPERAND-A LS-OPERAND-B)
           GOBACK.

       SAFE-MODULO-EXIT.
           EXIT.

      *================================================================
      * CLAMP-VALUE: Clamp a value to a range
      * Input:  LS-OPERAND-A (value), LS-MIN-VALUE, LS-MAX-VALUE
      * Output: LS-RESULT
      *================================================================
       CLAMP-VALUE SECTION.
           ENTRY "CLAMP-VALUE" USING LS-OPERAND-A LS-MIN-VALUE
                                     LS-MAX-VALUE LS-RESULT.

           IF LS-OPERAND-A < LS-MIN-VALUE
              MOVE LS-MIN-VALUE TO LS-RESULT
           ELSE IF LS-OPERAND-A > LS-MAX-VALUE
              MOVE LS-MAX-VALUE TO LS-RESULT
           ELSE
              MOVE LS-OPERAND-A TO LS-RESULT
           END-IF
           END-IF

           GOBACK.

       CLAMP-VALUE-EXIT.
           EXIT.

      *================================================================
      * IN-RANGE: Check if value is in range
      * Input:  LS-OPERAND-A (value), LS-MIN-VALUE, LS-MAX-VALUE
      * Output: LS-STATUS (1=in range, 0=out of range)
      *================================================================
       IN-RANGE SECTION.
           ENTRY "IN-RANGE" USING LS-OPERAND-A LS-MIN-VALUE
                                  LS-MAX-VALUE LS-STATUS.

           IF LS-OPERAND-A >= LS-MIN-VALUE
              AND LS-OPERAND-A <= LS-MAX-VALUE
              MOVE 1 TO LS-STATUS
           ELSE
              MOVE 0 TO LS-STATUS
           END-IF

           GOBACK.

       IN-RANGE-EXIT.
           EXIT.

       END PROGRAM SAFE-MATH.
