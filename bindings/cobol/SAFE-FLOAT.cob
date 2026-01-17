      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *>
      *> Proven SafeFloat - Safe floating-point operations for COBOL
      *>

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-FLOAT.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(4).
       01 WS-TEMP-A                   COMP-2.
       01 WS-TEMP-B                   COMP-2.
       01 WS-TEMP-RESULT              COMP-2.
       01 WS-TEMP-ABS                 COMP-2.
       01 WS-DIFF                     COMP-2.
       01 WS-MAX-VAL                  COMP-2.

      * Epsilon for floating-point comparison
       01 WS-DEFAULT-EPSILON          COMP-2 VALUE 1.0E-9.
       01 WS-RELATIVE-EPSILON         COMP-2 VALUE 1.0E-14.

      * Special value indicators
       01 WS-IS-SPECIAL               PIC 9 VALUE 0.

      * Maximum and minimum COMP-2 values (approximations)
       01 WS-MAX-COMP2                COMP-2 VALUE 1.7976931348623157E+308.
       01 WS-MIN-COMP2                COMP-2 VALUE 2.2250738585072014E-308.

       LINKAGE SECTION.
       01 LS-VALUE-A                  COMP-2.
       01 LS-VALUE-B                  COMP-2.
       01 LS-RESULT                   COMP-2.
       01 LS-STATUS                   PIC 9.
       01 LS-EPSILON                  COMP-2.
       01 LS-MIN-VALUE                COMP-2.
       01 LS-MAX-VALUE                COMP-2.
       01 LS-TOLERANCE                COMP-2.
       01 LS-ULPS                     PIC S9(9) COMP-3.
       01 LS-PRECISION                PIC 9(2).
       01 LS-OUTPUT-STRING            PIC X(30).
       01 LS-OUTPUT-LENGTH            PIC 9(2).

       PROCEDURE DIVISION.

      *================================================================
      * FLOAT-EQUALS: Compare two floats for equality with epsilon
      * Input:  LS-VALUE-A, LS-VALUE-B, LS-EPSILON
      * Output: LS-STATUS (1=equal, 0=not equal)
      *================================================================
       FLOAT-EQUALS SECTION.
           ENTRY "FLOAT-EQUALS" USING LS-VALUE-A LS-VALUE-B
                 LS-EPSILON LS-STATUS.

           MOVE 0 TO LS-STATUS
           MOVE LS-VALUE-A TO WS-TEMP-A
           MOVE LS-VALUE-B TO WS-TEMP-B
           MOVE LS-EPSILON TO WS-DIFF

      *    If epsilon is zero, use default
           IF WS-DIFF = 0
              MOVE WS-DEFAULT-EPSILON TO WS-DIFF
           END-IF

      *    Calculate absolute difference
           COMPUTE WS-TEMP-RESULT = WS-TEMP-A - WS-TEMP-B
           IF WS-TEMP-RESULT < 0
              COMPUTE WS-TEMP-RESULT = 0 - WS-TEMP-RESULT
           END-IF

      *    Compare with epsilon
           IF WS-TEMP-RESULT <= WS-DIFF
              MOVE 1 TO LS-STATUS
           END-IF

           GOBACK.

       FLOAT-EQUALS-EXIT.
           EXIT.

      *================================================================
      * FLOAT-RELATIVE-EQUALS: Compare with relative tolerance
      * Input:  LS-VALUE-A, LS-VALUE-B, LS-TOLERANCE
      * Output: LS-STATUS (1=equal, 0=not equal)
      *================================================================
       FLOAT-RELATIVE-EQUALS SECTION.
           ENTRY "FLOAT-RELATIVE-EQUALS" USING LS-VALUE-A LS-VALUE-B
                 LS-TOLERANCE LS-STATUS.

           MOVE 0 TO LS-STATUS
           MOVE LS-VALUE-A TO WS-TEMP-A
           MOVE LS-VALUE-B TO WS-TEMP-B

      *    Get absolute values for scaling
           IF WS-TEMP-A < 0
              COMPUTE WS-TEMP-ABS = 0 - WS-TEMP-A
           ELSE
              MOVE WS-TEMP-A TO WS-TEMP-ABS
           END-IF

           IF WS-TEMP-B < 0
              COMPUTE WS-DIFF = 0 - WS-TEMP-B
           ELSE
              MOVE WS-TEMP-B TO WS-DIFF
           END-IF

      *    Get max absolute value
           IF WS-TEMP-ABS > WS-DIFF
              MOVE WS-TEMP-ABS TO WS-MAX-VAL
           ELSE
              MOVE WS-DIFF TO WS-MAX-VAL
           END-IF

      *    Calculate absolute difference
           COMPUTE WS-TEMP-RESULT = WS-TEMP-A - WS-TEMP-B
           IF WS-TEMP-RESULT < 0
              COMPUTE WS-TEMP-RESULT = 0 - WS-TEMP-RESULT
           END-IF

      *    Compare with relative tolerance
           IF WS-MAX-VAL = 0
              IF WS-TEMP-RESULT = 0
                 MOVE 1 TO LS-STATUS
              END-IF
           ELSE
              COMPUTE WS-DIFF = WS-MAX-VAL * LS-TOLERANCE
              IF WS-TEMP-RESULT <= WS-DIFF
                 MOVE 1 TO LS-STATUS
              END-IF
           END-IF

           GOBACK.

       FLOAT-RELATIVE-EQUALS-EXIT.
           EXIT.

      *================================================================
      * SAFE-FLOAT-ADD: Add with overflow checking
      * Input:  LS-VALUE-A, LS-VALUE-B
      * Output: LS-RESULT, LS-STATUS (1=OK, 0=overflow)
      *================================================================
       SAFE-FLOAT-ADD SECTION.
           ENTRY "SAFE-FLOAT-ADD" USING LS-VALUE-A LS-VALUE-B
                 LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE 0 TO LS-RESULT

           COMPUTE LS-RESULT = LS-VALUE-A + LS-VALUE-B
              ON SIZE ERROR
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
           END-COMPUTE

           GOBACK.

       SAFE-FLOAT-ADD-EXIT.
           EXIT.

      *================================================================
      * SAFE-FLOAT-SUBTRACT: Subtract with overflow checking
      * Input:  LS-VALUE-A, LS-VALUE-B
      * Output: LS-RESULT (A - B), LS-STATUS
      *================================================================
       SAFE-FLOAT-SUBTRACT SECTION.
           ENTRY "SAFE-FLOAT-SUBTRACT" USING LS-VALUE-A LS-VALUE-B
                 LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE 0 TO LS-RESULT

           COMPUTE LS-RESULT = LS-VALUE-A - LS-VALUE-B
              ON SIZE ERROR
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
           END-COMPUTE

           GOBACK.

       SAFE-FLOAT-SUBTRACT-EXIT.
           EXIT.

      *================================================================
      * SAFE-FLOAT-MULTIPLY: Multiply with overflow checking
      * Input:  LS-VALUE-A, LS-VALUE-B
      * Output: LS-RESULT, LS-STATUS
      *================================================================
       SAFE-FLOAT-MULTIPLY SECTION.
           ENTRY "SAFE-FLOAT-MULTIPLY" USING LS-VALUE-A LS-VALUE-B
                 LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE 0 TO LS-RESULT

      *    Handle zero cases
           IF LS-VALUE-A = 0 OR LS-VALUE-B = 0
              MOVE 0 TO LS-RESULT
              GOBACK
           END-IF

           COMPUTE LS-RESULT = LS-VALUE-A * LS-VALUE-B
              ON SIZE ERROR
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
           END-COMPUTE

           GOBACK.

       SAFE-FLOAT-MULTIPLY-EXIT.
           EXIT.

      *================================================================
      * SAFE-FLOAT-DIVIDE: Divide with zero check
      * Input:  LS-VALUE-A, LS-VALUE-B
      * Output: LS-RESULT (A / B), LS-STATUS
      *================================================================
       SAFE-FLOAT-DIVIDE SECTION.
           ENTRY "SAFE-FLOAT-DIVIDE" USING LS-VALUE-A LS-VALUE-B
                 LS-RESULT LS-STATUS.

           MOVE 1 TO LS-STATUS
           MOVE 0 TO LS-RESULT

      *    Check for division by zero
           IF LS-VALUE-B = 0
              MOVE 0 TO LS-STATUS
              GOBACK
           END-IF

           COMPUTE LS-RESULT = LS-VALUE-A / LS-VALUE-B
              ON SIZE ERROR
                 MOVE 0 TO LS-RESULT
                 MOVE 0 TO LS-STATUS
           END-COMPUTE

           GOBACK.

       SAFE-FLOAT-DIVIDE-EXIT.
           EXIT.

      *================================================================
      * FLOAT-CLAMP: Clamp value to range
      * Input:  LS-VALUE-A, LS-MIN-VALUE, LS-MAX-VALUE
      * Output: LS-RESULT
      *================================================================
       FLOAT-CLAMP SECTION.
           ENTRY "FLOAT-CLAMP" USING LS-VALUE-A LS-MIN-VALUE
                 LS-MAX-VALUE LS-RESULT.

           IF LS-VALUE-A < LS-MIN-VALUE
              MOVE LS-MIN-VALUE TO LS-RESULT
           ELSE IF LS-VALUE-A > LS-MAX-VALUE
              MOVE LS-MAX-VALUE TO LS-RESULT
           ELSE
              MOVE LS-VALUE-A TO LS-RESULT
           END-IF
           END-IF

           GOBACK.

       FLOAT-CLAMP-EXIT.
           EXIT.

      *================================================================
      * FLOAT-IN-RANGE: Check if value is in range
      * Input:  LS-VALUE-A, LS-MIN-VALUE, LS-MAX-VALUE
      * Output: LS-STATUS (1=in range, 0=out of range)
      *================================================================
       FLOAT-IN-RANGE SECTION.
           ENTRY "FLOAT-IN-RANGE" USING LS-VALUE-A LS-MIN-VALUE
                 LS-MAX-VALUE LS-STATUS.

           IF LS-VALUE-A >= LS-MIN-VALUE
              AND LS-VALUE-A <= LS-MAX-VALUE
              MOVE 1 TO LS-STATUS
           ELSE
              MOVE 0 TO LS-STATUS
           END-IF

           GOBACK.

       FLOAT-IN-RANGE-EXIT.
           EXIT.

      *================================================================
      * FLOAT-ABS: Absolute value
      * Input:  LS-VALUE-A
      * Output: LS-RESULT
      *================================================================
       FLOAT-ABS SECTION.
           ENTRY "FLOAT-ABS" USING LS-VALUE-A LS-RESULT.

           IF LS-VALUE-A < 0
              COMPUTE LS-RESULT = 0 - LS-VALUE-A
           ELSE
              MOVE LS-VALUE-A TO LS-RESULT
           END-IF

           GOBACK.

       FLOAT-ABS-EXIT.
           EXIT.

      *================================================================
      * FLOAT-SIGN: Get sign of value (-1, 0, or 1)
      * Input:  LS-VALUE-A
      * Output: LS-RESULT (-1, 0, or 1)
      *================================================================
       FLOAT-SIGN SECTION.
           ENTRY "FLOAT-SIGN" USING LS-VALUE-A LS-RESULT.

           IF LS-VALUE-A < 0
              MOVE -1 TO LS-RESULT
           ELSE IF LS-VALUE-A > 0
              MOVE 1 TO LS-RESULT
           ELSE
              MOVE 0 TO LS-RESULT
           END-IF
           END-IF

           GOBACK.

       FLOAT-SIGN-EXIT.
           EXIT.

      *================================================================
      * FLOAT-MIN: Return minimum of two values
      * Input:  LS-VALUE-A, LS-VALUE-B
      * Output: LS-RESULT
      *================================================================
       FLOAT-MIN SECTION.
           ENTRY "FLOAT-MIN" USING LS-VALUE-A LS-VALUE-B LS-RESULT.

           IF LS-VALUE-A < LS-VALUE-B
              MOVE LS-VALUE-A TO LS-RESULT
           ELSE
              MOVE LS-VALUE-B TO LS-RESULT
           END-IF

           GOBACK.

       FLOAT-MIN-EXIT.
           EXIT.

      *================================================================
      * FLOAT-MAX: Return maximum of two values
      * Input:  LS-VALUE-A, LS-VALUE-B
      * Output: LS-RESULT
      *================================================================
       FLOAT-MAX SECTION.
           ENTRY "FLOAT-MAX" USING LS-VALUE-A LS-VALUE-B LS-RESULT.

           IF LS-VALUE-A > LS-VALUE-B
              MOVE LS-VALUE-A TO LS-RESULT
           ELSE
              MOVE LS-VALUE-B TO LS-RESULT
           END-IF

           GOBACK.

       FLOAT-MAX-EXIT.
           EXIT.

      *================================================================
      * FLOAT-LERP: Linear interpolation
      * Input:  LS-VALUE-A (start), LS-VALUE-B (end), LS-TOLERANCE (t)
      * Output: LS-RESULT (A + (B - A) * t)
      *================================================================
       FLOAT-LERP SECTION.
           ENTRY "FLOAT-LERP" USING LS-VALUE-A LS-VALUE-B
                 LS-TOLERANCE LS-RESULT.

           COMPUTE LS-RESULT =
              LS-VALUE-A + (LS-VALUE-B - LS-VALUE-A) * LS-TOLERANCE

           GOBACK.

       FLOAT-LERP-EXIT.
           EXIT.

      *================================================================
      * ROUND-TO-PRECISION: Round to N decimal places
      * Input:  LS-VALUE-A, LS-PRECISION
      * Output: LS-RESULT
      *================================================================
       ROUND-TO-PRECISION SECTION.
           ENTRY "ROUND-TO-PRECISION" USING LS-VALUE-A LS-PRECISION
                 LS-RESULT.

           MOVE LS-VALUE-A TO WS-TEMP-A

      *    Calculate multiplier (10^precision)
           COMPUTE WS-TEMP-B = 10 ** LS-PRECISION

      *    Multiply, round, divide
           COMPUTE WS-TEMP-RESULT = WS-TEMP-A * WS-TEMP-B
           COMPUTE WS-TEMP-RESULT =
              FUNCTION INTEGER(WS-TEMP-RESULT + 0.5)
           COMPUTE LS-RESULT = WS-TEMP-RESULT / WS-TEMP-B

           GOBACK.

       ROUND-TO-PRECISION-EXIT.
           EXIT.

      *================================================================
      * IS-POSITIVE: Check if value is positive
      * Input:  LS-VALUE-A
      * Output: LS-STATUS (1=positive, 0=not positive)
      *================================================================
       IS-POSITIVE SECTION.
           ENTRY "IS-POSITIVE" USING LS-VALUE-A LS-STATUS.

           IF LS-VALUE-A > 0
              MOVE 1 TO LS-STATUS
           ELSE
              MOVE 0 TO LS-STATUS
           END-IF

           GOBACK.

       IS-POSITIVE-EXIT.
           EXIT.

      *================================================================
      * IS-NEGATIVE: Check if value is negative
      * Input:  LS-VALUE-A
      * Output: LS-STATUS (1=negative, 0=not negative)
      *================================================================
       IS-NEGATIVE SECTION.
           ENTRY "IS-NEGATIVE" USING LS-VALUE-A LS-STATUS.

           IF LS-VALUE-A < 0
              MOVE 1 TO LS-STATUS
           ELSE
              MOVE 0 TO LS-STATUS
           END-IF

           GOBACK.

       IS-NEGATIVE-EXIT.
           EXIT.

      *================================================================
      * IS-ZERO: Check if value is zero (within epsilon)
      * Input:  LS-VALUE-A, LS-EPSILON
      * Output: LS-STATUS (1=zero, 0=not zero)
      *================================================================
       IS-ZERO SECTION.
           ENTRY "IS-ZERO" USING LS-VALUE-A LS-EPSILON LS-STATUS.

           MOVE LS-VALUE-A TO WS-TEMP-A
           IF WS-TEMP-A < 0
              COMPUTE WS-TEMP-A = 0 - WS-TEMP-A
           END-IF

           MOVE LS-EPSILON TO WS-DIFF
           IF WS-DIFF = 0
              MOVE WS-DEFAULT-EPSILON TO WS-DIFF
           END-IF

           IF WS-TEMP-A <= WS-DIFF
              MOVE 1 TO LS-STATUS
           ELSE
              MOVE 0 TO LS-STATUS
           END-IF

           GOBACK.

       IS-ZERO-EXIT.
           EXIT.

       END PROGRAM SAFE-FLOAT.
