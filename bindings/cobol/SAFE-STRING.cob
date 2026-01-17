      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeString - XSS prevention for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-STRING.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX              PIC 9(4).
       01 WS-OUTPUT-POS         PIC 9(4).
       01 WS-INPUT-LEN          PIC 9(4).
       01 WS-CURRENT-CHAR       PIC X(1).
       01 WS-TEMP-STRING        PIC X(8192).

      * HTML entity replacements
       01 WS-HTML-ENTITIES.
          05 WS-AMP             PIC X(5) VALUE "&amp;".
          05 WS-LT              PIC X(4) VALUE "&lt;".
          05 WS-GT              PIC X(4) VALUE "&gt;".
          05 WS-QUOT            PIC X(6) VALUE "&quot;".
          05 WS-APOS            PIC X(6) VALUE "&#x27;".

      * SQL escape
       01 WS-SQL-QUOTE          PIC X(2) VALUE "''".

      * Character classifications
       01 WS-CHAR-FLAGS.
          05 WS-IS-ALPHA        PIC 9 VALUE 0.
          05 WS-IS-DIGIT        PIC 9 VALUE 0.
          05 WS-IS-ALNUM        PIC 9 VALUE 0.

       LINKAGE SECTION.
       01 LS-INPUT-STRING       PIC X(4096).
       01 LS-INPUT-LENGTH       PIC 9(4).
       01 LS-OUTPUT-STRING      PIC X(8192).
       01 LS-OUTPUT-LENGTH      PIC 9(4).

       PROCEDURE DIVISION.

      *================================================================
      * ESCAPE-HTML: Escape HTML special characters
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH
      *================================================================
       ESCAPE-HTML SECTION.
           ENTRY "ESCAPE-HTML" USING LS-INPUT-STRING LS-INPUT-LENGTH
                                     LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              EVALUATE WS-CURRENT-CHAR
                 WHEN "&"
                    MOVE WS-AMP TO
                         LS-OUTPUT-STRING(WS-OUTPUT-POS:5)
                    ADD 5 TO WS-OUTPUT-POS
                 WHEN "<"
                    MOVE WS-LT TO
                         LS-OUTPUT-STRING(WS-OUTPUT-POS:4)
                    ADD 4 TO WS-OUTPUT-POS
                 WHEN ">"
                    MOVE WS-GT TO
                         LS-OUTPUT-STRING(WS-OUTPUT-POS:4)
                    ADD 4 TO WS-OUTPUT-POS
                 WHEN '"'
                    MOVE WS-QUOT TO
                         LS-OUTPUT-STRING(WS-OUTPUT-POS:6)
                    ADD 6 TO WS-OUTPUT-POS
                 WHEN "'"
                    MOVE WS-APOS TO
                         LS-OUTPUT-STRING(WS-OUTPUT-POS:6)
                    ADD 6 TO WS-OUTPUT-POS
                 WHEN OTHER
                    MOVE WS-CURRENT-CHAR TO
                         LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    ADD 1 TO WS-OUTPUT-POS
              END-EVALUATE
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       ESCAPE-HTML-EXIT.
           EXIT.

      *================================================================
      * ESCAPE-SQL: Escape SQL single quotes
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH
      *================================================================
       ESCAPE-SQL SECTION.
           ENTRY "ESCAPE-SQL" USING LS-INPUT-STRING LS-INPUT-LENGTH
                                    LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              IF WS-CURRENT-CHAR = "'"
                 MOVE WS-SQL-QUOTE TO
                      LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                 ADD 2 TO WS-OUTPUT-POS
              ELSE
                 MOVE WS-CURRENT-CHAR TO
                      LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       ESCAPE-SQL-EXIT.
           EXIT.

      *================================================================
      * SANITIZE-DEFAULT: Keep only alphanumeric + underscore + hyphen
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH
      *================================================================
       SANITIZE-DEFAULT SECTION.
           ENTRY "SANITIZE-DEFAULT" USING LS-INPUT-STRING
                 LS-INPUT-LENGTH LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              PERFORM CHECK-ALNUM

              IF WS-IS-ALNUM = 1
                 OR WS-CURRENT-CHAR = "_"
                 OR WS-CURRENT-CHAR = "-"
                 MOVE WS-CURRENT-CHAR TO
                      LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       SANITIZE-DEFAULT-EXIT.
           EXIT.

      *================================================================
      * CHECK-ALNUM: Check if current char is alphanumeric
      *================================================================
       CHECK-ALNUM SECTION.
           MOVE 0 TO WS-IS-ALPHA
           MOVE 0 TO WS-IS-DIGIT
           MOVE 0 TO WS-IS-ALNUM

      *    Check if letter
           IF (WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "Z")
              OR (WS-CURRENT-CHAR >= "a" AND WS-CURRENT-CHAR <= "z")
              MOVE 1 TO WS-IS-ALPHA
              MOVE 1 TO WS-IS-ALNUM
           END-IF

      *    Check if digit
           IF WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
              MOVE 1 TO WS-IS-DIGIT
              MOVE 1 TO WS-IS-ALNUM
           END-IF.

       CHECK-ALNUM-EXIT.
           EXIT.

      *================================================================
      * SLUGIFY: Convert to URL-safe slug
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH
      *================================================================
       SLUGIFY SECTION.
           ENTRY "SLUGIFY" USING LS-INPUT-STRING LS-INPUT-LENGTH
                                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              PERFORM CHECK-ALNUM

      *       Convert uppercase to lowercase
              IF WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "Z"
                 INSPECT WS-CURRENT-CHAR
                    CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    TO         "abcdefghijklmnopqrstuvwxyz"
              END-IF

      *       Keep alphanumeric and convert spaces/special to hyphen
              IF WS-IS-ALNUM = 1
                 MOVE WS-CURRENT-CHAR TO
                      LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              ELSE IF WS-CURRENT-CHAR = " "
                 OR WS-CURRENT-CHAR = "_"
      *          Only add hyphen if not already last char
                 IF WS-OUTPUT-POS > 1
                    IF LS-OUTPUT-STRING(WS-OUTPUT-POS - 1:1) NOT = "-"
                       MOVE "-" TO
                            LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                       ADD 1 TO WS-OUTPUT-POS
                    END-IF
                 END-IF
              END-IF
              END-IF
           END-PERFORM

      *    Remove trailing hyphen
           IF WS-OUTPUT-POS > 1
              IF LS-OUTPUT-STRING(WS-OUTPUT-POS - 1:1) = "-"
                 SUBTRACT 1 FROM WS-OUTPUT-POS
              END-IF
           END-IF

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       SLUGIFY-EXIT.
           EXIT.

       END PROGRAM SAFE-STRING.
