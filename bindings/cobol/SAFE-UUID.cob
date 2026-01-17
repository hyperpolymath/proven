      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeUUID - UUID parsing and formatting for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-UUID.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(4).
       01 WS-OUTPUT-POS               PIC 9(4).
       01 WS-INPUT-LEN                PIC 9(4).
       01 WS-CURRENT-CHAR             PIC X(1).
       01 WS-TEMP-VALUE               PIC 9(4).
       01 WS-BYTE-INDEX               PIC 9(4).
       01 WS-HEX-INDEX                PIC 9(4).
       01 WS-HIGH-NIBBLE              PIC 9(2).
       01 WS-LOW-NIBBLE               PIC 9(2).

      * Hex character lookup
       01 WS-HEX-CHARS                PIC X(16)
          VALUE "0123456789abcdef".
       01 WS-HEX-CHARS-UPPER          PIC X(16)
          VALUE "0123456789ABCDEF".

      * UUID segment lengths and positions
       01 WS-UUID-STRUCTURE.
          05 WS-SEG1-START            PIC 9(2) VALUE 1.
          05 WS-SEG1-LEN              PIC 9(2) VALUE 8.
          05 WS-SEG2-START            PIC 9(2) VALUE 10.
          05 WS-SEG2-LEN              PIC 9(2) VALUE 4.
          05 WS-SEG3-START            PIC 9(2) VALUE 15.
          05 WS-SEG3-LEN              PIC 9(2) VALUE 4.
          05 WS-SEG4-START            PIC 9(2) VALUE 20.
          05 WS-SEG4-LEN              PIC 9(2) VALUE 4.
          05 WS-SEG5-START            PIC 9(2) VALUE 25.
          05 WS-SEG5-LEN              PIC 9(2) VALUE 12.

      * Dash positions (1-indexed)
       01 WS-DASH-POSITIONS.
          05 FILLER                   PIC 9(2) VALUE 9.
          05 FILLER                   PIC 9(2) VALUE 14.
          05 FILLER                   PIC 9(2) VALUE 19.
          05 FILLER                   PIC 9(2) VALUE 24.
       01 WS-DASH-TABLE REDEFINES WS-DASH-POSITIONS.
          05 WS-DASH-POS              OCCURS 4 TIMES PIC 9(2).

      * UUID temporary storage
       01 WS-UUID-TEMP                PIC X(36).
       01 WS-UUID-BYTES-TEMP.
          05 WS-UUID-BYTE             OCCURS 16 TIMES PIC X.

      * Validation flags
       01 WS-IS-VALID-HEX             PIC 9 VALUE 0.
       01 WS-DASH-COUNT               PIC 9 VALUE 0.
       01 WS-HEX-COUNT                PIC 9(2) VALUE 0.

       LINKAGE SECTION.
       01 LS-UUID-STRING              PIC X(36).
       01 LS-UUID-LENGTH              PIC 9(2).
       01 LS-UUID-BYTES               PIC X(16).
       01 LS-RESULT                   PIC 9.
       01 LS-ERROR-MSG                PIC X(50).
       01 LS-UUID-VERSION             PIC 9.
       01 LS-UUID-VARIANT             PIC 9.
       01 LS-USE-UPPERCASE            PIC 9.

       PROCEDURE DIVISION.

      *================================================================
      * PARSE-UUID: Parse UUID string to validate format
      * Input:  LS-UUID-STRING (36-char canonical format)
      * Output: LS-RESULT (1=valid, 0=invalid), LS-ERROR-MSG
      *================================================================
       PARSE-UUID SECTION.
           ENTRY "PARSE-UUID" USING LS-UUID-STRING LS-RESULT
                                    LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-ERROR-MSG
           MOVE 0 TO WS-DASH-COUNT
           MOVE 0 TO WS-HEX-COUNT

      *    Check length
           IF FUNCTION LENGTH(FUNCTION TRIM(LS-UUID-STRING)) NOT = 36
              MOVE "UUID must be exactly 36 characters" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Validate each character position
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 36

              MOVE LS-UUID-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

      *       Check if this position should be a dash
              MOVE 0 TO WS-IS-VALID-HEX
              PERFORM VARYING WS-HEX-INDEX FROM 1 BY 1
                      UNTIL WS-HEX-INDEX > 4
                 IF WS-INDEX = WS-DASH-POS(WS-HEX-INDEX)
                    IF WS-CURRENT-CHAR NOT = "-"
                       STRING "Expected dash at position "
                              WS-INDEX DELIMITED BY SIZE
                              INTO LS-ERROR-MSG
                       GOBACK
                    END-IF
                    ADD 1 TO WS-DASH-COUNT
                    MOVE 1 TO WS-IS-VALID-HEX
                 END-IF
              END-PERFORM

      *       If not a dash position, must be valid hex
              IF WS-IS-VALID-HEX = 0
                 PERFORM CHECK-HEX-CHAR
                 IF WS-IS-VALID-HEX = 0
                    STRING "Invalid hex character at position "
                           WS-INDEX DELIMITED BY SIZE
                           INTO LS-ERROR-MSG
                    GOBACK
                 END-IF
                 ADD 1 TO WS-HEX-COUNT
              END-IF
           END-PERFORM

      *    Verify counts
           IF WS-DASH-COUNT NOT = 4
              MOVE "UUID must have exactly 4 dashes" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF WS-HEX-COUNT NOT = 32
              MOVE "UUID must have exactly 32 hex digits" TO LS-ERROR-MSG
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       PARSE-UUID-EXIT.
           EXIT.

      *================================================================
      * CHECK-HEX-CHAR: Check if current char is valid hexadecimal
      *================================================================
       CHECK-HEX-CHAR SECTION.
           MOVE 0 TO WS-IS-VALID-HEX

           IF (WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9")
              OR (WS-CURRENT-CHAR >= "a" AND WS-CURRENT-CHAR <= "f")
              OR (WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "F")
              MOVE 1 TO WS-IS-VALID-HEX
           END-IF.

       CHECK-HEX-CHAR-EXIT.
           EXIT.

      *================================================================
      * FORMAT-UUID: Format UUID bytes to canonical string
      * Input:  LS-UUID-BYTES (16 bytes), LS-USE-UPPERCASE (0/1)
      * Output: LS-UUID-STRING, LS-RESULT
      *================================================================
       FORMAT-UUID SECTION.
           ENTRY "FORMAT-UUID" USING LS-UUID-BYTES LS-USE-UPPERCASE
                                     LS-UUID-STRING LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-UUID-STRING
           MOVE 1 TO WS-OUTPUT-POS

      *    Convert each byte to two hex characters
           PERFORM VARYING WS-BYTE-INDEX FROM 1 BY 1
                   UNTIL WS-BYTE-INDEX > 16

      *       Insert dashes at appropriate positions
              IF WS-OUTPUT-POS = 9 OR WS-OUTPUT-POS = 14
                 OR WS-OUTPUT-POS = 19 OR WS-OUTPUT-POS = 24
                 MOVE "-" TO LS-UUID-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF

      *       Convert byte to hex
              COMPUTE WS-HIGH-NIBBLE =
                 FUNCTION ORD(LS-UUID-BYTES(WS-BYTE-INDEX:1)) / 16
              COMPUTE WS-LOW-NIBBLE =
                 FUNCTION MOD(
                    FUNCTION ORD(LS-UUID-BYTES(WS-BYTE-INDEX:1)) 16)

      *       Add 1 for 1-indexed COBOL strings
              ADD 1 TO WS-HIGH-NIBBLE
              ADD 1 TO WS-LOW-NIBBLE

              IF LS-USE-UPPERCASE = 1
                 MOVE WS-HEX-CHARS-UPPER(WS-HIGH-NIBBLE:1)
                    TO LS-UUID-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 MOVE WS-HEX-CHARS-UPPER(WS-LOW-NIBBLE:1)
                    TO LS-UUID-STRING(WS-OUTPUT-POS:1)
              ELSE
                 MOVE WS-HEX-CHARS(WS-HIGH-NIBBLE:1)
                    TO LS-UUID-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 MOVE WS-HEX-CHARS(WS-LOW-NIBBLE:1)
                    TO LS-UUID-STRING(WS-OUTPUT-POS:1)
              END-IF
              ADD 1 TO WS-OUTPUT-POS
           END-PERFORM

           GOBACK.

       FORMAT-UUID-EXIT.
           EXIT.

      *================================================================
      * GET-UUID-VERSION: Extract version from UUID string
      * Input:  LS-UUID-STRING
      * Output: LS-UUID-VERSION (1-7), LS-RESULT
      *================================================================
       GET-UUID-VERSION SECTION.
           ENTRY "GET-UUID-VERSION" USING LS-UUID-STRING
                                          LS-UUID-VERSION LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-UUID-VERSION

      *    Version is in position 15 (first char of segment 3)
           MOVE LS-UUID-STRING(15:1) TO WS-CURRENT-CHAR

           EVALUATE WS-CURRENT-CHAR
              WHEN "1"
                 MOVE 1 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN "2"
                 MOVE 2 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN "3"
                 MOVE 3 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN "4"
                 MOVE 4 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN "5"
                 MOVE 5 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN "6"
                 MOVE 6 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN "7"
                 MOVE 7 TO LS-UUID-VERSION
                 MOVE 1 TO LS-RESULT
              WHEN OTHER
                 MOVE 0 TO LS-RESULT
           END-EVALUATE

           GOBACK.

       GET-UUID-VERSION-EXIT.
           EXIT.

      *================================================================
      * GET-UUID-VARIANT: Extract variant from UUID string
      * Input:  LS-UUID-STRING
      * Output: LS-UUID-VARIANT (0-3), LS-RESULT
      * Variant: 0=NCS, 1=RFC4122, 2=Microsoft, 3=Future
      *================================================================
       GET-UUID-VARIANT SECTION.
           ENTRY "GET-UUID-VARIANT" USING LS-UUID-STRING
                                          LS-UUID-VARIANT LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-UUID-VARIANT

      *    Variant is encoded in position 20 (first char of segment 4)
           MOVE LS-UUID-STRING(20:1) TO WS-CURRENT-CHAR

      *    Convert to uppercase for comparison
           INSPECT WS-CURRENT-CHAR
              CONVERTING "abcdef" TO "ABCDEF"

           EVALUATE TRUE
      *       0-7 = NCS (variant 0)
              WHEN WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "7"
                 MOVE 0 TO LS-UUID-VARIANT
                 MOVE 1 TO LS-RESULT
      *       8-B = RFC 4122 (variant 1)
              WHEN WS-CURRENT-CHAR >= "8" AND WS-CURRENT-CHAR <= "9"
                 MOVE 1 TO LS-UUID-VARIANT
                 MOVE 1 TO LS-RESULT
              WHEN WS-CURRENT-CHAR = "A" OR WS-CURRENT-CHAR = "B"
                 MOVE 1 TO LS-UUID-VARIANT
                 MOVE 1 TO LS-RESULT
      *       C-D = Microsoft (variant 2)
              WHEN WS-CURRENT-CHAR = "C" OR WS-CURRENT-CHAR = "D"
                 MOVE 2 TO LS-UUID-VARIANT
                 MOVE 1 TO LS-RESULT
      *       E-F = Future (variant 3)
              WHEN WS-CURRENT-CHAR = "E" OR WS-CURRENT-CHAR = "F"
                 MOVE 3 TO LS-UUID-VARIANT
                 MOVE 1 TO LS-RESULT
              WHEN OTHER
                 MOVE 0 TO LS-RESULT
           END-EVALUATE

           GOBACK.

       GET-UUID-VARIANT-EXIT.
           EXIT.

      *================================================================
      * IS-NIL-UUID: Check if UUID is the nil UUID
      * Input:  LS-UUID-STRING
      * Output: LS-RESULT (1=nil, 0=not nil)
      *================================================================
       IS-NIL-UUID SECTION.
           ENTRY "IS-NIL-UUID" USING LS-UUID-STRING LS-RESULT.

           IF LS-UUID-STRING =
              "00000000-0000-0000-0000-000000000000"
              MOVE 1 TO LS-RESULT
           ELSE
              MOVE 0 TO LS-RESULT
           END-IF

           GOBACK.

       IS-NIL-UUID-EXIT.
           EXIT.

      *================================================================
      * NORMALIZE-UUID: Convert UUID to lowercase canonical form
      * Input:  LS-UUID-STRING
      * Output: LS-UUID-STRING (normalized), LS-RESULT
      *================================================================
       NORMALIZE-UUID SECTION.
           ENTRY "NORMALIZE-UUID" USING LS-UUID-STRING LS-RESULT.

           MOVE LS-UUID-STRING TO WS-UUID-TEMP
           MOVE FUNCTION LOWER-CASE(WS-UUID-TEMP) TO LS-UUID-STRING
           MOVE 1 TO LS-RESULT

           GOBACK.

       NORMALIZE-UUID-EXIT.
           EXIT.

       END PROGRAM SAFE-UUID.
