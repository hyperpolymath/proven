      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *>
      *> Proven SafeDateTime - Date and time validation for COBOL
      *>

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-DATETIME.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(4).
       01 WS-TEMP-VALUE               PIC 9(8).
       01 WS-CURRENT-CHAR             PIC X(1).

      * Date components
       01 WS-DATE-PARTS.
          05 WS-YEAR                  PIC 9(4).
          05 WS-MONTH                 PIC 9(2).
          05 WS-DAY                   PIC 9(2).

      * Time components
       01 WS-TIME-PARTS.
          05 WS-HOUR                  PIC 9(2).
          05 WS-MINUTE                PIC 9(2).
          05 WS-SECOND                PIC 9(2).
          05 WS-MILLISECOND           PIC 9(3).

      * Days in month table
       01 WS-DAYS-IN-MONTH-TABLE.
          05 FILLER                   PIC 9(2) VALUE 31.
          05 FILLER                   PIC 9(2) VALUE 28.
          05 FILLER                   PIC 9(2) VALUE 31.
          05 FILLER                   PIC 9(2) VALUE 30.
          05 FILLER                   PIC 9(2) VALUE 31.
          05 FILLER                   PIC 9(2) VALUE 30.
          05 FILLER                   PIC 9(2) VALUE 31.
          05 FILLER                   PIC 9(2) VALUE 31.
          05 FILLER                   PIC 9(2) VALUE 30.
          05 FILLER                   PIC 9(2) VALUE 31.
          05 FILLER                   PIC 9(2) VALUE 30.
          05 FILLER                   PIC 9(2) VALUE 31.
       01 WS-DAYS-TABLE REDEFINES WS-DAYS-IN-MONTH-TABLE.
          05 WS-MAX-DAYS              OCCURS 12 TIMES PIC 9(2).

      * Leap year check
       01 WS-IS-LEAP                  PIC 9 VALUE 0.

      * Julian day calculation
       01 WS-JULIAN-DAY               PIC 9(7).
       01 WS-JULIAN-A                 PIC S9(8) COMP-3.
       01 WS-JULIAN-Y                 PIC S9(8) COMP-3.
       01 WS-JULIAN-M                 PIC S9(8) COMP-3.

       LINKAGE SECTION.
       01 LS-DATE-STRING              PIC X(30).
       01 LS-DATE-LENGTH              PIC 9(2).
       01 LS-YEAR                     PIC 9(4).
       01 LS-MONTH                    PIC 9(2).
       01 LS-DAY                      PIC 9(2).
       01 LS-HOUR                     PIC 9(2).
       01 LS-MINUTE                   PIC 9(2).
       01 LS-SECOND                   PIC 9(2).
       01 LS-RESULT                   PIC 9.
       01 LS-ERROR-MSG                PIC X(50).
       01 LS-OUTPUT-STRING            PIC X(30).
       01 LS-OUTPUT-LENGTH            PIC 9(2).
       01 LS-DAYS-DIFF                PIC S9(8) COMP-3.
       01 LS-YEAR2                    PIC 9(4).
       01 LS-MONTH2                   PIC 9(2).
       01 LS-DAY2                     PIC 9(2).
       01 LS-DAY-OF-WEEK              PIC 9.

       PROCEDURE DIVISION.

      *================================================================
      * VALIDATE-DATE: Validate year, month, day values
      * Input:  LS-YEAR, LS-MONTH, LS-DAY
      * Output: LS-RESULT (1=valid, 0=invalid), LS-ERROR-MSG
      *================================================================
       VALIDATE-DATE SECTION.
           ENTRY "VALIDATE-DATE" USING LS-YEAR LS-MONTH LS-DAY
                 LS-RESULT LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-ERROR-MSG
           MOVE LS-YEAR TO WS-YEAR
           MOVE LS-MONTH TO WS-MONTH
           MOVE LS-DAY TO WS-DAY

      *    Validate year range
           IF WS-YEAR < 1 OR WS-YEAR > 9999
              MOVE "Year must be between 1 and 9999" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Validate month range
           IF WS-MONTH < 1 OR WS-MONTH > 12
              MOVE "Month must be between 1 and 12" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Check leap year
           PERFORM CHECK-LEAP-YEAR

      *    Get max days for this month
           MOVE WS-MAX-DAYS(WS-MONTH) TO WS-TEMP-VALUE

      *    February in leap year
           IF WS-MONTH = 2 AND WS-IS-LEAP = 1
              MOVE 29 TO WS-TEMP-VALUE
           END-IF

      *    Validate day range
           IF WS-DAY < 1 OR WS-DAY > WS-TEMP-VALUE
              STRING "Day must be between 1 and " WS-TEMP-VALUE
                 DELIMITED BY SIZE INTO LS-ERROR-MSG
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       VALIDATE-DATE-EXIT.
           EXIT.

      *================================================================
      * CHECK-LEAP-YEAR: Determine if WS-YEAR is a leap year
      *================================================================
       CHECK-LEAP-YEAR SECTION.
           MOVE 0 TO WS-IS-LEAP

           IF FUNCTION MOD(WS-YEAR 4) = 0
              IF FUNCTION MOD(WS-YEAR 100) = 0
                 IF FUNCTION MOD(WS-YEAR 400) = 0
                    MOVE 1 TO WS-IS-LEAP
                 END-IF
              ELSE
                 MOVE 1 TO WS-IS-LEAP
              END-IF
           END-IF.

       CHECK-LEAP-YEAR-EXIT.
           EXIT.

      *================================================================
      * VALIDATE-TIME: Validate hour, minute, second values
      * Input:  LS-HOUR, LS-MINUTE, LS-SECOND
      * Output: LS-RESULT (1=valid, 0=invalid), LS-ERROR-MSG
      *================================================================
       VALIDATE-TIME SECTION.
           ENTRY "VALIDATE-TIME" USING LS-HOUR LS-MINUTE LS-SECOND
                 LS-RESULT LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-ERROR-MSG

      *    Validate hour
           IF LS-HOUR > 23
              MOVE "Hour must be between 0 and 23" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Validate minute
           IF LS-MINUTE > 59
              MOVE "Minute must be between 0 and 59" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Validate second
           IF LS-SECOND > 59
              MOVE "Second must be between 0 and 59" TO LS-ERROR-MSG
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       VALIDATE-TIME-EXIT.
           EXIT.

      *================================================================
      * PARSE-ISO8601: Parse ISO 8601 date string (YYYY-MM-DD)
      * Input:  LS-DATE-STRING, LS-DATE-LENGTH
      * Output: LS-YEAR, LS-MONTH, LS-DAY, LS-RESULT, LS-ERROR-MSG
      *================================================================
       PARSE-ISO8601 SECTION.
           ENTRY "PARSE-ISO8601" USING LS-DATE-STRING LS-DATE-LENGTH
                 LS-YEAR LS-MONTH LS-DAY LS-RESULT LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-YEAR
           MOVE 0 TO LS-MONTH
           MOVE 0 TO LS-DAY
           INITIALIZE LS-ERROR-MSG

      *    Must be at least 10 characters (YYYY-MM-DD)
           IF LS-DATE-LENGTH < 10
              MOVE "Date too short (need YYYY-MM-DD)" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Check dashes at positions 5 and 8
           IF LS-DATE-STRING(5:1) NOT = "-"
              MOVE "Missing dash at position 5" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF LS-DATE-STRING(8:1) NOT = "-"
              MOVE "Missing dash at position 8" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Extract year
           IF LS-DATE-STRING(1:4) IS NUMERIC
              MOVE LS-DATE-STRING(1:4) TO LS-YEAR
           ELSE
              MOVE "Invalid year" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Extract month
           IF LS-DATE-STRING(6:2) IS NUMERIC
              MOVE LS-DATE-STRING(6:2) TO LS-MONTH
           ELSE
              MOVE "Invalid month" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Extract day
           IF LS-DATE-STRING(9:2) IS NUMERIC
              MOVE LS-DATE-STRING(9:2) TO LS-DAY
           ELSE
              MOVE "Invalid day" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Validate the date
           MOVE LS-YEAR TO WS-YEAR
           MOVE LS-MONTH TO WS-MONTH
           MOVE LS-DAY TO WS-DAY
           PERFORM CHECK-LEAP-YEAR
           MOVE WS-MAX-DAYS(WS-MONTH) TO WS-TEMP-VALUE
           IF WS-MONTH = 2 AND WS-IS-LEAP = 1
              MOVE 29 TO WS-TEMP-VALUE
           END-IF

           IF LS-MONTH < 1 OR LS-MONTH > 12
              MOVE "Month out of range" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF LS-DAY < 1 OR LS-DAY > WS-TEMP-VALUE
              MOVE "Day out of range for month" TO LS-ERROR-MSG
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       PARSE-ISO8601-EXIT.
           EXIT.

      *================================================================
      * FORMAT-ISO8601: Format date as ISO 8601 string
      * Input:  LS-YEAR, LS-MONTH, LS-DAY
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       FORMAT-ISO8601 SECTION.
           ENTRY "FORMAT-ISO8601" USING LS-YEAR LS-MONTH LS-DAY
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 10 TO LS-OUTPUT-LENGTH

      *    Format: YYYY-MM-DD
           MOVE LS-YEAR TO LS-OUTPUT-STRING(1:4)
           MOVE "-" TO LS-OUTPUT-STRING(5:1)
           MOVE LS-MONTH TO LS-OUTPUT-STRING(6:2)
           MOVE "-" TO LS-OUTPUT-STRING(8:1)
           MOVE LS-DAY TO LS-OUTPUT-STRING(9:2)

           GOBACK.

       FORMAT-ISO8601-EXIT.
           EXIT.

      *================================================================
      * IS-LEAP-YEAR: Check if year is a leap year
      * Input:  LS-YEAR
      * Output: LS-RESULT (1=leap year, 0=not leap year)
      *================================================================
       IS-LEAP-YEAR SECTION.
           ENTRY "IS-LEAP-YEAR" USING LS-YEAR LS-RESULT.

           MOVE LS-YEAR TO WS-YEAR
           PERFORM CHECK-LEAP-YEAR
           MOVE WS-IS-LEAP TO LS-RESULT
           GOBACK.

       IS-LEAP-YEAR-EXIT.
           EXIT.

      *================================================================
      * DAYS-IN-MONTH: Get number of days in a month
      * Input:  LS-YEAR, LS-MONTH
      * Output: LS-DAY (days in month), LS-RESULT
      *================================================================
       DAYS-IN-MONTH SECTION.
           ENTRY "DAYS-IN-MONTH" USING LS-YEAR LS-MONTH
                 LS-DAY LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-DAY

           IF LS-MONTH < 1 OR LS-MONTH > 12
              GOBACK
           END-IF

           MOVE LS-YEAR TO WS-YEAR
           PERFORM CHECK-LEAP-YEAR

           MOVE WS-MAX-DAYS(LS-MONTH) TO LS-DAY

           IF LS-MONTH = 2 AND WS-IS-LEAP = 1
              MOVE 29 TO LS-DAY
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       DAYS-IN-MONTH-EXIT.
           EXIT.

      *================================================================
      * DAY-OF-WEEK: Calculate day of week (0=Sunday, 6=Saturday)
      * Input:  LS-YEAR, LS-MONTH, LS-DAY
      * Output: LS-DAY-OF-WEEK (0-6), LS-RESULT
      *================================================================
       DAY-OF-WEEK SECTION.
           ENTRY "DAY-OF-WEEK" USING LS-YEAR LS-MONTH LS-DAY
                 LS-DAY-OF-WEEK LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-DAY-OF-WEEK
           MOVE LS-YEAR TO WS-YEAR
           MOVE LS-MONTH TO WS-MONTH
           MOVE LS-DAY TO WS-DAY

      *    Adjust month and year for algorithm
           IF WS-MONTH < 3
              ADD 12 TO WS-MONTH
              SUBTRACT 1 FROM WS-YEAR
           END-IF

      *    Zeller's congruence
           COMPUTE WS-TEMP-VALUE =
              WS-DAY +
              ((13 * (WS-MONTH + 1)) / 5) +
              WS-YEAR +
              (WS-YEAR / 4) -
              (WS-YEAR / 100) +
              (WS-YEAR / 400)

           COMPUTE LS-DAY-OF-WEEK =
              FUNCTION MOD((WS-TEMP-VALUE + 6) 7)

           MOVE 1 TO LS-RESULT
           GOBACK.

       DAY-OF-WEEK-EXIT.
           EXIT.

      *================================================================
      * DAYS-BETWEEN: Calculate days between two dates
      * Input:  LS-YEAR, LS-MONTH, LS-DAY (date1)
      *         LS-YEAR2, LS-MONTH2, LS-DAY2 (date2)
      * Output: LS-DAYS-DIFF (date2 - date1), LS-RESULT
      *================================================================
       DAYS-BETWEEN SECTION.
           ENTRY "DAYS-BETWEEN" USING LS-YEAR LS-MONTH LS-DAY
                 LS-YEAR2 LS-MONTH2 LS-DAY2
                 LS-DAYS-DIFF LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-DAYS-DIFF

      *    Calculate Julian day for first date
           MOVE LS-YEAR TO WS-YEAR
           MOVE LS-MONTH TO WS-MONTH
           MOVE LS-DAY TO WS-DAY
           PERFORM CALCULATE-JULIAN-DAY
           MOVE WS-JULIAN-DAY TO WS-JULIAN-A

      *    Calculate Julian day for second date
           MOVE LS-YEAR2 TO WS-YEAR
           MOVE LS-MONTH2 TO WS-MONTH
           MOVE LS-DAY2 TO WS-DAY
           PERFORM CALCULATE-JULIAN-DAY

      *    Difference
           COMPUTE LS-DAYS-DIFF = WS-JULIAN-DAY - WS-JULIAN-A

           MOVE 1 TO LS-RESULT
           GOBACK.

       DAYS-BETWEEN-EXIT.
           EXIT.

      *================================================================
      * CALCULATE-JULIAN-DAY: Convert date to Julian day number
      *================================================================
       CALCULATE-JULIAN-DAY SECTION.
           COMPUTE WS-JULIAN-A = (14 - WS-MONTH) / 12
           COMPUTE WS-JULIAN-Y = WS-YEAR + 4800 - WS-JULIAN-A
           COMPUTE WS-JULIAN-M = WS-MONTH + 12 * WS-JULIAN-A - 3

           COMPUTE WS-JULIAN-DAY =
              WS-DAY +
              (153 * WS-JULIAN-M + 2) / 5 +
              365 * WS-JULIAN-Y +
              WS-JULIAN-Y / 4 -
              WS-JULIAN-Y / 100 +
              WS-JULIAN-Y / 400 -
              32045.

       CALCULATE-JULIAN-DAY-EXIT.
           EXIT.

      *================================================================
      * ADD-DAYS: Add days to a date
      * Input:  LS-YEAR, LS-MONTH, LS-DAY, LS-DAYS-DIFF
      * Output: LS-YEAR, LS-MONTH, LS-DAY (updated), LS-RESULT
      *================================================================
       ADD-DAYS SECTION.
           ENTRY "ADD-DAYS" USING LS-YEAR LS-MONTH LS-DAY
                 LS-DAYS-DIFF LS-RESULT.

           MOVE 1 TO LS-RESULT
           MOVE LS-YEAR TO WS-YEAR
           MOVE LS-MONTH TO WS-MONTH
           MOVE LS-DAY TO WS-DAY

      *    Calculate current Julian day
           PERFORM CALCULATE-JULIAN-DAY

      *    Add days
           ADD LS-DAYS-DIFF TO WS-JULIAN-DAY

      *    Convert back to calendar date
           PERFORM JULIAN-TO-CALENDAR

           MOVE WS-YEAR TO LS-YEAR
           MOVE WS-MONTH TO LS-MONTH
           MOVE WS-DAY TO LS-DAY

           GOBACK.

       ADD-DAYS-EXIT.
           EXIT.

      *================================================================
      * JULIAN-TO-CALENDAR: Convert Julian day to calendar date
      *================================================================
       JULIAN-TO-CALENDAR SECTION.
           COMPUTE WS-JULIAN-A = WS-JULIAN-DAY + 32044
           COMPUTE WS-JULIAN-Y =
              (4 * WS-JULIAN-A + 3) / 1461
           COMPUTE WS-JULIAN-A =
              WS-JULIAN-A - (1461 * WS-JULIAN-Y) / 4
           COMPUTE WS-JULIAN-M =
              (4 * WS-JULIAN-A + 3) / 1461
           COMPUTE WS-JULIAN-A =
              WS-JULIAN-A - (1461 * WS-JULIAN-M) / 4
           COMPUTE WS-DAY = WS-JULIAN-A + 1

           COMPUTE WS-JULIAN-A = WS-JULIAN-DAY + 32044
           COMPUTE WS-JULIAN-Y =
              (4 * WS-JULIAN-A + 3) / 1461
           COMPUTE WS-JULIAN-A =
              WS-JULIAN-A - (1461 * WS-JULIAN-Y / 4)
           COMPUTE WS-JULIAN-M =
              (4 * WS-JULIAN-A + 3) / 1461
           COMPUTE WS-JULIAN-M =
              (5 * WS-JULIAN-A + 2) / 153
           COMPUTE WS-DAY =
              WS-JULIAN-A - (153 * WS-JULIAN-M + 2) / 5 + 1
           COMPUTE WS-MONTH =
              WS-JULIAN-M + 3 - 12 * (WS-JULIAN-M / 10)
           COMPUTE WS-YEAR =
              100 * WS-JULIAN-Y + WS-JULIAN-M / 10 - 4800 +
              (WS-MONTH - 3) / 10.

       JULIAN-TO-CALENDAR-EXIT.
           EXIT.

       END PROGRAM SAFE-DATETIME.
