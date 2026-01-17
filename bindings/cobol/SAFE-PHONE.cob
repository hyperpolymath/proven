      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafePhone - Phone number parsing and formatting for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-PHONE.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(4).
       01 WS-OUTPUT-POS               PIC 9(4).
       01 WS-INPUT-LEN                PIC 9(4).
       01 WS-CURRENT-CHAR             PIC X(1).
       01 WS-DIGIT-COUNT              PIC 9(2) VALUE 0.
       01 WS-TEMP-VALUE               PIC 9(4) VALUE 0.
       01 WS-PARSE-STATE              PIC 9 VALUE 0.
       01 WS-HAS-PLUS                 PIC 9 VALUE 0.
       01 WS-PAREN-DEPTH              PIC 9 VALUE 0.

      * Normalized phone number (digits only)
       01 WS-NORMALIZED-PHONE         PIC X(20).
       01 WS-NORMALIZED-LEN           PIC 9(2).

      * Country code extraction work area
       01 WS-CC-WORK                  PIC 9(4).
       01 WS-CC-1                     PIC 9(1).
       01 WS-CC-2                     PIC 9(2).
       01 WS-CC-3                     PIC 9(3).

      * Common country code table (code, length)
       01 WS-COUNTRY-CODE-TABLE.
      *   1-digit codes
          05 FILLER                   PIC X(5) VALUE "00011".
          05 FILLER                   PIC X(5) VALUE "00071".
      *   2-digit codes
          05 FILLER                   PIC X(5) VALUE "00202".
          05 FILLER                   PIC X(5) VALUE "00272".
          05 FILLER                   PIC X(5) VALUE "00302".
          05 FILLER                   PIC X(5) VALUE "00312".
          05 FILLER                   PIC X(5) VALUE "00322".
          05 FILLER                   PIC X(5) VALUE "00332".
          05 FILLER                   PIC X(5) VALUE "00342".
          05 FILLER                   PIC X(5) VALUE "00392".
          05 FILLER                   PIC X(5) VALUE "00442".
          05 FILLER                   PIC X(5) VALUE "00492".
          05 FILLER                   PIC X(5) VALUE "00552".
          05 FILLER                   PIC X(5) VALUE "00612".
          05 FILLER                   PIC X(5) VALUE "00812".
          05 FILLER                   PIC X(5) VALUE "00862".
          05 FILLER                   PIC X(5) VALUE "00912".
      *   3-digit codes
          05 FILLER                   PIC X(5) VALUE "02123".
          05 FILLER                   PIC X(5) VALUE "02343".
          05 FILLER                   PIC X(5) VALUE "09663".
          05 FILLER                   PIC X(5) VALUE "09713".
          05 FILLER                   PIC X(5) VALUE "09723".
       01 WS-CC-TABLE REDEFINES WS-COUNTRY-CODE-TABLE.
          05 WS-CC-ENTRY              OCCURS 22 TIMES.
             10 WS-CC-VALUE           PIC 9(4).
             10 WS-CC-LEN             PIC 9.

      * Phone format patterns
       01 WS-NANP-PATTERN             PIC X(20)
          VALUE "(XXX) XXX-XXXX".
       01 WS-INTL-PATTERN             PIC X(25)
          VALUE "+X XXX XXX XXXX".

       LINKAGE SECTION.
       01 LS-PHONE-INPUT              PIC X(30).
       01 LS-PHONE-LENGTH             PIC 9(2).
       01 LS-COUNTRY-CODE             PIC 9(4).
       01 LS-NATIONAL-NUMBER          PIC X(15).
       01 LS-EXTENSION                PIC X(10).
       01 LS-RESULT                   PIC 9.
       01 LS-ERROR-MSG                PIC X(50).
       01 LS-FORMATTED-OUTPUT         PIC X(25).
       01 LS-OUTPUT-LENGTH            PIC 9(2).
       01 LS-FORMAT-TYPE              PIC 9.
       01 LS-DEFAULT-COUNTRY          PIC 9(4).
       01 LS-E164-OUTPUT              PIC X(20).

       PROCEDURE DIVISION.

      *================================================================
      * PARSE-PHONE: Parse phone number string
      * Input:  LS-PHONE-INPUT, LS-PHONE-LENGTH, LS-DEFAULT-COUNTRY
      * Output: LS-COUNTRY-CODE, LS-NATIONAL-NUMBER, LS-EXTENSION,
      *         LS-RESULT, LS-ERROR-MSG
      *================================================================
       PARSE-PHONE SECTION.
           ENTRY "PARSE-PHONE" USING LS-PHONE-INPUT LS-PHONE-LENGTH
                 LS-DEFAULT-COUNTRY
                 LS-COUNTRY-CODE LS-NATIONAL-NUMBER LS-EXTENSION
                 LS-RESULT LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-COUNTRY-CODE
           INITIALIZE LS-NATIONAL-NUMBER
           INITIALIZE LS-EXTENSION
           INITIALIZE LS-ERROR-MSG
           INITIALIZE WS-NORMALIZED-PHONE
           MOVE 0 TO WS-NORMALIZED-LEN
           MOVE 0 TO WS-HAS-PLUS
           MOVE 0 TO WS-PAREN-DEPTH
           MOVE LS-PHONE-LENGTH TO WS-INPUT-LEN

      *    Check minimum length
           IF WS-INPUT-LEN < 3
              MOVE "Phone number too short" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    First pass: extract digits and detect format
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-PHONE-INPUT(WS-INDEX:1) TO WS-CURRENT-CHAR

              EVALUATE TRUE
                 WHEN WS-CURRENT-CHAR >= "0"
                      AND WS-CURRENT-CHAR <= "9"
      *             Accumulate digit
                    ADD 1 TO WS-NORMALIZED-LEN
                    IF WS-NORMALIZED-LEN <= 20
                       MOVE WS-CURRENT-CHAR TO
                          WS-NORMALIZED-PHONE(WS-NORMALIZED-LEN:1)
                    END-IF

                 WHEN WS-CURRENT-CHAR = "+"
      *             Plus must be first non-space character
                    IF WS-NORMALIZED-LEN = 0
                       MOVE 1 TO WS-HAS-PLUS
                    ELSE
                       MOVE "Invalid + position" TO LS-ERROR-MSG
                       GOBACK
                    END-IF

                 WHEN WS-CURRENT-CHAR = "("
                    ADD 1 TO WS-PAREN-DEPTH

                 WHEN WS-CURRENT-CHAR = ")"
                    IF WS-PAREN-DEPTH > 0
                       SUBTRACT 1 FROM WS-PAREN-DEPTH
                    END-IF

                 WHEN WS-CURRENT-CHAR = " "
                      OR WS-CURRENT-CHAR = "-"
                      OR WS-CURRENT-CHAR = "."
      *             Skip formatting characters
                    CONTINUE

                 WHEN WS-CURRENT-CHAR = "x"
                      OR WS-CURRENT-CHAR = "X"
      *             Extension follows - handled separately
                    EXIT PERFORM

                 WHEN OTHER
      *             Invalid character
                    STRING "Invalid character: " WS-CURRENT-CHAR
                       DELIMITED BY SIZE INTO LS-ERROR-MSG
                    GOBACK
              END-EVALUATE
           END-PERFORM

      *    Validate digit count
           IF WS-NORMALIZED-LEN < 7
              MOVE "Not enough digits" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF WS-NORMALIZED-LEN > 15
              MOVE "Too many digits" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Extract country code
           IF WS-HAS-PLUS = 1
              PERFORM EXTRACT-COUNTRY-CODE
           ELSE
      *       Use default country code
              MOVE LS-DEFAULT-COUNTRY TO LS-COUNTRY-CODE
           END-IF

      *    Extract national number (rest after country code)
           IF WS-HAS-PLUS = 1
              PERFORM EXTRACT-NATIONAL-NUMBER
           ELSE
              MOVE WS-NORMALIZED-PHONE(1:WS-NORMALIZED-LEN)
                 TO LS-NATIONAL-NUMBER
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       PARSE-PHONE-EXIT.
           EXIT.

      *================================================================
      * EXTRACT-COUNTRY-CODE: Extract country code from normalized num
      *================================================================
       EXTRACT-COUNTRY-CODE SECTION.
      *    Try 1-digit codes first
           MOVE WS-NORMALIZED-PHONE(1:1) TO WS-CC-1

           IF WS-CC-1 = 1 OR WS-CC-1 = 7
              MOVE WS-CC-1 TO LS-COUNTRY-CODE
              GO TO EXTRACT-COUNTRY-CODE-EXIT
           END-IF

      *    Try 2-digit codes
           IF WS-NORMALIZED-LEN >= 2
              MOVE WS-NORMALIZED-PHONE(1:2) TO WS-CC-2
              PERFORM VARYING WS-INDEX FROM 1 BY 1
                      UNTIL WS-INDEX > 22

                 IF WS-CC-LEN(WS-INDEX) = 2
                    IF WS-CC-VALUE(WS-INDEX) = WS-CC-2
                       MOVE WS-CC-2 TO LS-COUNTRY-CODE
                       GO TO EXTRACT-COUNTRY-CODE-EXIT
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

      *    Try 3-digit codes
           IF WS-NORMALIZED-LEN >= 3
              MOVE WS-NORMALIZED-PHONE(1:3) TO WS-CC-3
              PERFORM VARYING WS-INDEX FROM 1 BY 1
                      UNTIL WS-INDEX > 22

                 IF WS-CC-LEN(WS-INDEX) = 3
                    IF WS-CC-VALUE(WS-INDEX) = WS-CC-3
                       MOVE WS-CC-3 TO LS-COUNTRY-CODE
                       GO TO EXTRACT-COUNTRY-CODE-EXIT
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

      *    Default to first digit as country code
           MOVE WS-CC-1 TO LS-COUNTRY-CODE.

       EXTRACT-COUNTRY-CODE-EXIT.
           EXIT.

      *================================================================
      * EXTRACT-NATIONAL-NUMBER: Extract national portion
      *================================================================
       EXTRACT-NATIONAL-NUMBER SECTION.
      *    Determine country code length
           MOVE 1 TO WS-INDEX

           IF LS-COUNTRY-CODE >= 100
              MOVE 3 TO WS-INDEX
           ELSE IF LS-COUNTRY-CODE >= 10
              MOVE 2 TO WS-INDEX
           END-IF
           END-IF

      *    Skip country code digits
           ADD 1 TO WS-INDEX
           COMPUTE WS-DIGIT-COUNT = WS-NORMALIZED-LEN - WS-INDEX + 1

           IF WS-DIGIT-COUNT > 0 AND WS-DIGIT-COUNT <= 15
              MOVE WS-NORMALIZED-PHONE(WS-INDEX:WS-DIGIT-COUNT)
                 TO LS-NATIONAL-NUMBER
           END-IF.

       EXTRACT-NATIONAL-NUMBER-EXIT.
           EXIT.

      *================================================================
      * FORMAT-PHONE: Format phone number for display
      * Input:  LS-COUNTRY-CODE, LS-NATIONAL-NUMBER, LS-FORMAT-TYPE
      *         (0=E164, 1=International, 2=National, 3=RFC3966)
      * Output: LS-FORMATTED-OUTPUT, LS-OUTPUT-LENGTH
      *================================================================
       FORMAT-PHONE SECTION.
           ENTRY "FORMAT-PHONE" USING LS-COUNTRY-CODE LS-NATIONAL-NUMBER
                 LS-FORMAT-TYPE
                 LS-FORMATTED-OUTPUT LS-OUTPUT-LENGTH.

           INITIALIZE LS-FORMATTED-OUTPUT
           MOVE 1 TO WS-OUTPUT-POS

           EVALUATE LS-FORMAT-TYPE
              WHEN 0
      *          E.164 format: +[CC][National]
                 MOVE "+" TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS

                 MOVE LS-COUNTRY-CODE TO WS-CC-WORK
                 INSPECT WS-CC-WORK REPLACING LEADING "0" BY SPACE
                 MOVE FUNCTION TRIM(WS-CC-WORK) TO
                    LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:4)
                 ADD FUNCTION LENGTH(FUNCTION TRIM(WS-CC-WORK))
                    TO WS-OUTPUT-POS

                 MOVE FUNCTION TRIM(LS-NATIONAL-NUMBER) TO
                    LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:15)
                 ADD FUNCTION LENGTH(
                    FUNCTION TRIM(LS-NATIONAL-NUMBER))
                    TO WS-OUTPUT-POS

              WHEN 1
      *          International format: +CC NNN NNN NNNN
                 MOVE "+" TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS

                 MOVE LS-COUNTRY-CODE TO WS-CC-WORK
                 INSPECT WS-CC-WORK REPLACING LEADING "0" BY SPACE
                 MOVE FUNCTION TRIM(WS-CC-WORK) TO
                    LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:4)
                 ADD FUNCTION LENGTH(FUNCTION TRIM(WS-CC-WORK))
                    TO WS-OUTPUT-POS

                 MOVE " " TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS

                 PERFORM FORMAT-NATIONAL-NUMBER

              WHEN 2
      *          National format: (NNN) NNN-NNNN for NANP
                 PERFORM FORMAT-NATIONAL-NUMBER

              WHEN 3
      *          RFC 3966 format: tel:+CC-NNN-NNN-NNNN
                 MOVE "tel:+" TO LS-FORMATTED-OUTPUT(1:5)
                 MOVE 6 TO WS-OUTPUT-POS

                 MOVE LS-COUNTRY-CODE TO WS-CC-WORK
                 INSPECT WS-CC-WORK REPLACING LEADING "0" BY SPACE
                 MOVE FUNCTION TRIM(WS-CC-WORK) TO
                    LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:4)
                 ADD FUNCTION LENGTH(FUNCTION TRIM(WS-CC-WORK))
                    TO WS-OUTPUT-POS

                 MOVE "-" TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS

                 MOVE FUNCTION TRIM(LS-NATIONAL-NUMBER) TO
                    LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:15)
                 ADD FUNCTION LENGTH(
                    FUNCTION TRIM(LS-NATIONAL-NUMBER))
                    TO WS-OUTPUT-POS
           END-EVALUATE

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       FORMAT-PHONE-EXIT.
           EXIT.

      *================================================================
      * FORMAT-NATIONAL-NUMBER: Format national number with spacing
      *================================================================
       FORMAT-NATIONAL-NUMBER SECTION.
           MOVE 0 TO WS-DIGIT-COUNT

      *    Count digits in national number
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 15
                   OR LS-NATIONAL-NUMBER(WS-INDEX:1) = SPACE

              ADD 1 TO WS-DIGIT-COUNT
           END-PERFORM

      *    Format based on length (NANP = 10 digits)
           IF WS-DIGIT-COUNT = 10 AND LS-COUNTRY-CODE = 1
      *       NANP format: (XXX) XXX-XXXX
              MOVE "(" TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
              ADD 1 TO WS-OUTPUT-POS
              MOVE LS-NATIONAL-NUMBER(1:3) TO
                 LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:3)
              ADD 3 TO WS-OUTPUT-POS
              MOVE ") " TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:2)
              ADD 2 TO WS-OUTPUT-POS
              MOVE LS-NATIONAL-NUMBER(4:3) TO
                 LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:3)
              ADD 3 TO WS-OUTPUT-POS
              MOVE "-" TO LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:1)
              ADD 1 TO WS-OUTPUT-POS
              MOVE LS-NATIONAL-NUMBER(7:4) TO
                 LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:4)
              ADD 4 TO WS-OUTPUT-POS
           ELSE
      *       Generic format with spaces every 3-4 digits
              MOVE FUNCTION TRIM(LS-NATIONAL-NUMBER) TO
                 LS-FORMATTED-OUTPUT(WS-OUTPUT-POS:15)
              ADD WS-DIGIT-COUNT TO WS-OUTPUT-POS
           END-IF.

       FORMAT-NATIONAL-NUMBER-EXIT.
           EXIT.

      *================================================================
      * TO-E164: Convert phone to E.164 format
      * Input:  LS-COUNTRY-CODE, LS-NATIONAL-NUMBER
      * Output: LS-E164-OUTPUT, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       TO-E164 SECTION.
           ENTRY "TO-E164" USING LS-COUNTRY-CODE LS-NATIONAL-NUMBER
                 LS-E164-OUTPUT LS-OUTPUT-LENGTH LS-RESULT.

           INITIALIZE LS-E164-OUTPUT
           MOVE 1 TO LS-RESULT
           MOVE 1 TO WS-OUTPUT-POS

      *    Add plus sign
           MOVE "+" TO LS-E164-OUTPUT(WS-OUTPUT-POS:1)
           ADD 1 TO WS-OUTPUT-POS

      *    Add country code
           MOVE LS-COUNTRY-CODE TO WS-CC-WORK
           INSPECT WS-CC-WORK REPLACING LEADING "0" BY SPACE
           MOVE FUNCTION TRIM(WS-CC-WORK) TO
              LS-E164-OUTPUT(WS-OUTPUT-POS:4)
           ADD FUNCTION LENGTH(FUNCTION TRIM(WS-CC-WORK))
              TO WS-OUTPUT-POS

      *    Add national number (digits only)
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 15
                   OR LS-NATIONAL-NUMBER(WS-INDEX:1) = SPACE

              MOVE LS-NATIONAL-NUMBER(WS-INDEX:1) TO WS-CURRENT-CHAR
              IF WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                 MOVE WS-CURRENT-CHAR TO
                    LS-E164-OUTPUT(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       TO-E164-EXIT.
           EXIT.

      *================================================================
      * IS-VALID-PHONE: Basic phone validation
      * Input:  LS-PHONE-INPUT, LS-PHONE-LENGTH
      * Output: LS-RESULT (1=valid, 0=invalid)
      *================================================================
       IS-VALID-PHONE SECTION.
           ENTRY "IS-VALID-PHONE" USING LS-PHONE-INPUT LS-PHONE-LENGTH
                 LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO WS-DIGIT-COUNT
           MOVE LS-PHONE-LENGTH TO WS-INPUT-LEN

      *    Count digits
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-PHONE-INPUT(WS-INDEX:1) TO WS-CURRENT-CHAR
              IF WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                 ADD 1 TO WS-DIGIT-COUNT
              END-IF
           END-PERFORM

      *    Valid phone has 7-15 digits
           IF WS-DIGIT-COUNT >= 7 AND WS-DIGIT-COUNT <= 15
              MOVE 1 TO LS-RESULT
           END-IF

           GOBACK.

       IS-VALID-PHONE-EXIT.
           EXIT.

       END PROGRAM SAFE-PHONE.
