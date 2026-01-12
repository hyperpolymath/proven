      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeCurrency - Currency arithmetic for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-CURRENCY.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(4).
       01 WS-OUTPUT-POS               PIC 9(4).
       01 WS-TEMP-AMOUNT              PIC S9(15)V9(4) COMP-3.
       01 WS-TEMP-MINOR               PIC S9(18) COMP-3.
       01 WS-DECIMAL-FACTOR           PIC 9(5) VALUE 1.
       01 WS-CHECK-OVERFLOW           PIC S9(18) COMP-3.

      * Maximum values for overflow checking
       01 WS-MAX-AMOUNT               PIC S9(15)V9(4) COMP-3
          VALUE 99999999999999.9999.
       01 WS-MIN-AMOUNT               PIC S9(15)V9(4) COMP-3
          VALUE -99999999999999.9999.
       01 WS-MAX-MINOR                PIC S9(18) COMP-3
          VALUE 999999999999999999.
       01 WS-MIN-MINOR                PIC S9(18) COMP-3
          VALUE -999999999999999999.

      * Currency decimal places table
       01 WS-CURRENCY-DECIMALS-TABLE.
          05 FILLER                   PIC X(4) VALUE "USD2".
          05 FILLER                   PIC X(4) VALUE "EUR2".
          05 FILLER                   PIC X(4) VALUE "GBP2".
          05 FILLER                   PIC X(4) VALUE "JPY0".
          05 FILLER                   PIC X(4) VALUE "CHF2".
          05 FILLER                   PIC X(4) VALUE "CAD2".
          05 FILLER                   PIC X(4) VALUE "AUD2".
          05 FILLER                   PIC X(4) VALUE "NZD2".
          05 FILLER                   PIC X(4) VALUE "CNY2".
          05 FILLER                   PIC X(4) VALUE "HKD2".
          05 FILLER                   PIC X(4) VALUE "SGD2".
          05 FILLER                   PIC X(4) VALUE "SEK2".
          05 FILLER                   PIC X(4) VALUE "NOK2".
          05 FILLER                   PIC X(4) VALUE "DKK2".
          05 FILLER                   PIC X(4) VALUE "KRW0".
          05 FILLER                   PIC X(4) VALUE "INR2".
          05 FILLER                   PIC X(4) VALUE "BRL2".
          05 FILLER                   PIC X(4) VALUE "MXN2".
          05 FILLER                   PIC X(4) VALUE "ZAR2".
          05 FILLER                   PIC X(4) VALUE "RUB2".
          05 FILLER                   PIC X(4) VALUE "BTC8".
          05 FILLER                   PIC X(4) VALUE "ETH8".
          05 FILLER                   PIC X(4) VALUE "KWD3".
          05 FILLER                   PIC X(4) VALUE "BHD3".
          05 FILLER                   PIC X(4) VALUE "OMR3".
       01 WS-CURRENCY-TABLE REDEFINES WS-CURRENCY-DECIMALS-TABLE.
          05 WS-CURR-ENTRY            OCCURS 25 TIMES.
             10 WS-CURR-CODE          PIC X(3).
             10 WS-CURR-DEC           PIC 9.

      * Formatting work areas
       01 WS-FORMAT-WORK              PIC X(30).
       01 WS-SIGN-CHAR                PIC X VALUE SPACE.
       01 WS-INTEGER-PART             PIC 9(15).
       01 WS-DECIMAL-PART             PIC 9(4).
       01 WS-FORMATTED-INT            PIC X(20).
       01 WS-FORMATTED-LEN            PIC 9(4).

       LINKAGE SECTION.
       01 LS-CURRENCY-A               PIC X(3).
       01 LS-AMOUNT-A                 PIC S9(15)V9(4) COMP-3.
       01 LS-MINOR-A                  PIC S9(18) COMP-3.
       01 LS-CURRENCY-B               PIC X(3).
       01 LS-AMOUNT-B                 PIC S9(15)V9(4) COMP-3.
       01 LS-MINOR-B                  PIC S9(18) COMP-3.
       01 LS-RESULT-AMOUNT            PIC S9(15)V9(4) COMP-3.
       01 LS-RESULT-MINOR             PIC S9(18) COMP-3.
       01 LS-RESULT-STATUS            PIC 9.
       01 LS-ERROR-MSG                PIC X(50).
       01 LS-DECIMAL-PLACES           PIC 9.
       01 LS-MULTIPLIER               PIC S9(9) COMP-3.
       01 LS-DIVISOR                  PIC S9(9) COMP-3.
       01 LS-FORMATTED-OUTPUT         PIC X(30).
       01 LS-OUTPUT-LENGTH            PIC 9(4).
       01 LS-USE-THOUSANDS-SEP        PIC 9.
       01 LS-CURRENCY-CODE            PIC X(3).

       PROCEDURE DIVISION.

      *================================================================
      * MONEY-ADD: Add two money amounts with overflow checking
      * Input:  LS-CURRENCY-A, LS-MINOR-A, LS-CURRENCY-B, LS-MINOR-B
      * Output: LS-RESULT-MINOR, LS-RESULT-STATUS (1=OK, 0=overflow,
      *         3=currency mismatch), LS-ERROR-MSG
      *================================================================
       MONEY-ADD SECTION.
           ENTRY "MONEY-ADD" USING LS-CURRENCY-A LS-MINOR-A
                 LS-CURRENCY-B LS-MINOR-B
                 LS-RESULT-MINOR LS-RESULT-STATUS LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT-MINOR
           MOVE 1 TO LS-RESULT-STATUS
           INITIALIZE LS-ERROR-MSG

      *    Check currency match
           IF LS-CURRENCY-A NOT = LS-CURRENCY-B
              MOVE 3 TO LS-RESULT-STATUS
              MOVE "Currency mismatch in addition" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Check for positive overflow: b > 0 and a > MAX - b
           IF LS-MINOR-B > 0
              COMPUTE WS-CHECK-OVERFLOW = WS-MAX-MINOR - LS-MINOR-B
              IF LS-MINOR-A > WS-CHECK-OVERFLOW
                 MOVE 0 TO LS-RESULT-STATUS
                 MOVE "Overflow in addition" TO LS-ERROR-MSG
                 GOBACK
              END-IF
           END-IF

      *    Check for negative overflow: b < 0 and a < MIN - b
           IF LS-MINOR-B < 0
              COMPUTE WS-CHECK-OVERFLOW = WS-MIN-MINOR - LS-MINOR-B
              IF LS-MINOR-A < WS-CHECK-OVERFLOW
                 MOVE 0 TO LS-RESULT-STATUS
                 MOVE "Underflow in addition" TO LS-ERROR-MSG
                 GOBACK
              END-IF
           END-IF

      *    Safe to add
           COMPUTE LS-RESULT-MINOR = LS-MINOR-A + LS-MINOR-B
           GOBACK.

       MONEY-ADD-EXIT.
           EXIT.

      *================================================================
      * MONEY-SUBTRACT: Subtract money amounts with overflow checking
      * Input:  LS-CURRENCY-A, LS-MINOR-A, LS-CURRENCY-B, LS-MINOR-B
      * Output: LS-RESULT-MINOR (A - B), LS-RESULT-STATUS, LS-ERROR-MSG
      *================================================================
       MONEY-SUBTRACT SECTION.
           ENTRY "MONEY-SUBTRACT" USING LS-CURRENCY-A LS-MINOR-A
                 LS-CURRENCY-B LS-MINOR-B
                 LS-RESULT-MINOR LS-RESULT-STATUS LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT-MINOR
           MOVE 1 TO LS-RESULT-STATUS
           INITIALIZE LS-ERROR-MSG

      *    Check currency match
           IF LS-CURRENCY-A NOT = LS-CURRENCY-B
              MOVE 3 TO LS-RESULT-STATUS
              MOVE "Currency mismatch in subtraction" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Check for positive overflow: b < 0 and a > MAX + b
           IF LS-MINOR-B < 0
              COMPUTE WS-CHECK-OVERFLOW = WS-MAX-MINOR + LS-MINOR-B
              IF LS-MINOR-A > WS-CHECK-OVERFLOW
                 MOVE 0 TO LS-RESULT-STATUS
                 MOVE "Overflow in subtraction" TO LS-ERROR-MSG
                 GOBACK
              END-IF
           END-IF

      *    Check for negative overflow: b > 0 and a < MIN + b
           IF LS-MINOR-B > 0
              COMPUTE WS-CHECK-OVERFLOW = WS-MIN-MINOR + LS-MINOR-B
              IF LS-MINOR-A < WS-CHECK-OVERFLOW
                 MOVE 0 TO LS-RESULT-STATUS
                 MOVE "Underflow in subtraction" TO LS-ERROR-MSG
                 GOBACK
              END-IF
           END-IF

      *    Safe to subtract
           COMPUTE LS-RESULT-MINOR = LS-MINOR-A - LS-MINOR-B
           GOBACK.

       MONEY-SUBTRACT-EXIT.
           EXIT.

      *================================================================
      * MONEY-MULTIPLY: Multiply money by integer with overflow check
      * Input:  LS-MINOR-A (amount), LS-MULTIPLIER
      * Output: LS-RESULT-MINOR, LS-RESULT-STATUS, LS-ERROR-MSG
      *================================================================
       MONEY-MULTIPLY SECTION.
           ENTRY "MONEY-MULTIPLY" USING LS-MINOR-A LS-MULTIPLIER
                 LS-RESULT-MINOR LS-RESULT-STATUS LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT-MINOR
           MOVE 1 TO LS-RESULT-STATUS
           INITIALIZE LS-ERROR-MSG

      *    Handle zero cases
           IF LS-MINOR-A = 0 OR LS-MULTIPLIER = 0
              MOVE 0 TO LS-RESULT-MINOR
              GOBACK
           END-IF

      *    Check overflow using ON SIZE ERROR
           COMPUTE LS-RESULT-MINOR = LS-MINOR-A * LS-MULTIPLIER
              ON SIZE ERROR
                 MOVE 0 TO LS-RESULT-MINOR
                 MOVE 0 TO LS-RESULT-STATUS
                 MOVE "Overflow in multiplication" TO LS-ERROR-MSG
                 GOBACK
           END-COMPUTE

      *    Verify by division
           IF LS-MINOR-A NOT = 0
              COMPUTE WS-CHECK-OVERFLOW = LS-RESULT-MINOR / LS-MINOR-A
              IF WS-CHECK-OVERFLOW NOT = LS-MULTIPLIER
                 MOVE 0 TO LS-RESULT-MINOR
                 MOVE 0 TO LS-RESULT-STATUS
                 MOVE "Overflow in multiplication" TO LS-ERROR-MSG
                 GOBACK
              END-IF
           END-IF

           GOBACK.

       MONEY-MULTIPLY-EXIT.
           EXIT.

      *================================================================
      * MONEY-DIVIDE: Divide money by integer with zero check
      * Input:  LS-MINOR-A (amount), LS-DIVISOR
      * Output: LS-RESULT-MINOR, LS-RESULT-STATUS, LS-ERROR-MSG
      *================================================================
       MONEY-DIVIDE SECTION.
           ENTRY "MONEY-DIVIDE" USING LS-MINOR-A LS-DIVISOR
                 LS-RESULT-MINOR LS-RESULT-STATUS LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT-MINOR
           MOVE 1 TO LS-RESULT-STATUS
           INITIALIZE LS-ERROR-MSG

      *    Check for division by zero
           IF LS-DIVISOR = 0
              MOVE 0 TO LS-RESULT-STATUS
              MOVE "Division by zero" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Safe to divide
           COMPUTE LS-RESULT-MINOR = LS-MINOR-A / LS-DIVISOR
           GOBACK.

       MONEY-DIVIDE-EXIT.
           EXIT.

      *================================================================
      * GET-DECIMAL-PLACES: Get decimal places for a currency
      * Input:  LS-CURRENCY-CODE
      * Output: LS-DECIMAL-PLACES (0-8), LS-RESULT-STATUS
      *================================================================
       GET-DECIMAL-PLACES SECTION.
           ENTRY "GET-DECIMAL-PLACES" USING LS-CURRENCY-CODE
                 LS-DECIMAL-PLACES LS-RESULT-STATUS.

           MOVE 2 TO LS-DECIMAL-PLACES
           MOVE 1 TO LS-RESULT-STATUS

      *    Search currency table
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 25

              IF WS-CURR-CODE(WS-INDEX) = LS-CURRENCY-CODE
                 MOVE WS-CURR-DEC(WS-INDEX) TO LS-DECIMAL-PLACES
                 GOBACK
              END-IF
           END-PERFORM

      *    Not found - use default of 2
           GOBACK.

       GET-DECIMAL-PLACES-EXIT.
           EXIT.

      *================================================================
      * MINOR-TO-MAJOR: Convert minor units to major units
      * Input:  LS-MINOR-A, LS-DECIMAL-PLACES
      * Output: LS-RESULT-AMOUNT
      *================================================================
       MINOR-TO-MAJOR SECTION.
           ENTRY "MINOR-TO-MAJOR" USING LS-MINOR-A LS-DECIMAL-PLACES
                 LS-RESULT-AMOUNT.

      *    Calculate divisor based on decimal places
           EVALUATE LS-DECIMAL-PLACES
              WHEN 0
                 MOVE 1 TO WS-DECIMAL-FACTOR
              WHEN 1
                 MOVE 10 TO WS-DECIMAL-FACTOR
              WHEN 2
                 MOVE 100 TO WS-DECIMAL-FACTOR
              WHEN 3
                 MOVE 1000 TO WS-DECIMAL-FACTOR
              WHEN 4
                 MOVE 10000 TO WS-DECIMAL-FACTOR
              WHEN OTHER
                 COMPUTE WS-DECIMAL-FACTOR = 10 ** LS-DECIMAL-PLACES
           END-EVALUATE

           COMPUTE LS-RESULT-AMOUNT = LS-MINOR-A / WS-DECIMAL-FACTOR
           GOBACK.

       MINOR-TO-MAJOR-EXIT.
           EXIT.

      *================================================================
      * MAJOR-TO-MINOR: Convert major units to minor units
      * Input:  LS-AMOUNT-A, LS-DECIMAL-PLACES
      * Output: LS-RESULT-MINOR, LS-RESULT-STATUS
      *================================================================
       MAJOR-TO-MINOR SECTION.
           ENTRY "MAJOR-TO-MINOR" USING LS-AMOUNT-A LS-DECIMAL-PLACES
                 LS-RESULT-MINOR LS-RESULT-STATUS.

           MOVE 1 TO LS-RESULT-STATUS

      *    Calculate multiplier based on decimal places
           EVALUATE LS-DECIMAL-PLACES
              WHEN 0
                 MOVE 1 TO WS-DECIMAL-FACTOR
              WHEN 1
                 MOVE 10 TO WS-DECIMAL-FACTOR
              WHEN 2
                 MOVE 100 TO WS-DECIMAL-FACTOR
              WHEN 3
                 MOVE 1000 TO WS-DECIMAL-FACTOR
              WHEN 4
                 MOVE 10000 TO WS-DECIMAL-FACTOR
              WHEN OTHER
                 COMPUTE WS-DECIMAL-FACTOR = 10 ** LS-DECIMAL-PLACES
           END-EVALUATE

           COMPUTE LS-RESULT-MINOR = LS-AMOUNT-A * WS-DECIMAL-FACTOR
              ON SIZE ERROR
                 MOVE 0 TO LS-RESULT-MINOR
                 MOVE 0 TO LS-RESULT-STATUS
           END-COMPUTE

           GOBACK.

       MAJOR-TO-MINOR-EXIT.
           EXIT.

      *================================================================
      * IS-VALID-CURRENCY: Check if currency code is valid
      * Input:  LS-CURRENCY-CODE
      * Output: LS-RESULT-STATUS (1=valid, 0=invalid)
      *================================================================
       IS-VALID-CURRENCY SECTION.
           ENTRY "IS-VALID-CURRENCY" USING LS-CURRENCY-CODE
                 LS-RESULT-STATUS.

           MOVE 0 TO LS-RESULT-STATUS

      *    Check against known currencies
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 25

              IF WS-CURR-CODE(WS-INDEX) = LS-CURRENCY-CODE
                 MOVE 1 TO LS-RESULT-STATUS
                 GOBACK
              END-IF
           END-PERFORM

           GOBACK.

       IS-VALID-CURRENCY-EXIT.
           EXIT.

      *================================================================
      * FORMAT-MONEY: Format money for display
      * Input:  LS-MINOR-A, LS-CURRENCY-CODE, LS-USE-THOUSANDS-SEP
      * Output: LS-FORMATTED-OUTPUT, LS-OUTPUT-LENGTH
      *================================================================
       FORMAT-MONEY SECTION.
           ENTRY "FORMAT-MONEY" USING LS-MINOR-A LS-CURRENCY-CODE
                 LS-USE-THOUSANDS-SEP
                 LS-FORMATTED-OUTPUT LS-OUTPUT-LENGTH.

           INITIALIZE LS-FORMATTED-OUTPUT
           MOVE 0 TO LS-OUTPUT-LENGTH
           MOVE 1 TO WS-OUTPUT-POS

      *    Get decimal places for this currency
           MOVE 2 TO LS-DECIMAL-PLACES
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 25

              IF WS-CURR-CODE(WS-INDEX) = LS-CURRENCY-CODE
                 MOVE WS-CURR-DEC(WS-INDEX) TO LS-DECIMAL-PLACES
                 EXIT PERFORM
              END-IF
           END-PERFORM

      *    Calculate divisor
           EVALUATE LS-DECIMAL-PLACES
              WHEN 0
                 MOVE 1 TO WS-DECIMAL-FACTOR
              WHEN 2
                 MOVE 100 TO WS-DECIMAL-FACTOR
              WHEN 3
                 MOVE 1000 TO WS-DECIMAL-FACTOR
              WHEN 8
                 MOVE 100000000 TO WS-DECIMAL-FACTOR
              WHEN OTHER
                 COMPUTE WS-DECIMAL-FACTOR = 10 ** LS-DECIMAL-PLACES
           END-EVALUATE

      *    Handle sign
           IF LS-MINOR-A < 0
              MOVE "-" TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
              ADD 1 TO WS-OUTPUT-POS
              COMPUTE WS-TEMP-MINOR = 0 - LS-MINOR-A
           ELSE
              MOVE LS-MINOR-A TO WS-TEMP-MINOR
           END-IF

      *    Add currency code
           MOVE LS-CURRENCY-CODE TO
              LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:3)
           ADD 3 TO WS-OUTPUT-POS
           MOVE " " TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
           ADD 1 TO WS-OUTPUT-POS

      *    Calculate integer and decimal parts
           COMPUTE WS-INTEGER-PART = WS-TEMP-MINOR / WS-DECIMAL-FACTOR
           COMPUTE WS-DECIMAL-PART =
              FUNCTION MOD(WS-TEMP-MINOR WS-DECIMAL-FACTOR)

      *    Format integer part
           MOVE WS-INTEGER-PART TO WS-FORMATTED-INT
           INSPECT WS-FORMATTED-INT REPLACING LEADING "0" BY SPACE

           MOVE FUNCTION TRIM(WS-FORMATTED-INT) TO
              LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:15)
           ADD FUNCTION LENGTH(FUNCTION TRIM(WS-FORMATTED-INT))
              TO WS-OUTPUT-POS

      *    Add decimal part if needed
           IF LS-DECIMAL-PLACES > 0
              MOVE "." TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
              ADD 1 TO WS-OUTPUT-POS
              MOVE WS-DECIMAL-PART TO WS-FORMAT-WORK
              MOVE WS-FORMAT-WORK(1:LS-DECIMAL-PLACES) TO
                 LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:LS-DECIMAL-PLACES)
              ADD LS-DECIMAL-PLACES TO WS-OUTPUT-POS
           END-IF

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       FORMAT-MONEY-EXIT.
           EXIT.

       END PROGRAM SAFE-CURRENCY.
