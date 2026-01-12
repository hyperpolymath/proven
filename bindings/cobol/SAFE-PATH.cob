      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafePath - Directory traversal prevention for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-PATH.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX              PIC 9(4).
       01 WS-OUTPUT-POS         PIC 9(4).
       01 WS-INPUT-LEN          PIC 9(4).
       01 WS-CURRENT-CHAR       PIC X(1).
       01 WS-FOUND              PIC 9 VALUE 0.
       01 WS-SEARCH-POS         PIC 9(4).
       01 WS-REMAINING          PIC 9(4).

      * Dangerous patterns
       01 WS-PATTERNS.
          05 WS-DOTDOT          PIC X(2) VALUE "..".
          05 WS-DOTSLASH        PIC X(2) VALUE "./".
          05 WS-ENCODED-DOT     PIC X(6) VALUE "%2e%2e".
          05 WS-NULL-BYTE       PIC X(3) VALUE "%00".
          05 WS-BACKSLASH       PIC X(1) VALUE "\".

       LINKAGE SECTION.
       01 LS-INPUT-PATH         PIC X(1024).
       01 LS-INPUT-LENGTH       PIC 9(4).
       01 LS-OUTPUT-PATH        PIC X(1024).
       01 LS-OUTPUT-LENGTH      PIC 9(4).
       01 LS-RESULT             PIC 9.
       01 LS-ERROR-MSG          PIC X(100).

       PROCEDURE DIVISION.

      *================================================================
      * HAS-TRAVERSAL: Check for path traversal patterns
      * Input:  LS-INPUT-PATH, LS-INPUT-LENGTH
      * Output: LS-RESULT (1=has traversal, 0=safe)
      *================================================================
       HAS-TRAVERSAL SECTION.
           ENTRY "HAS-TRAVERSAL" USING LS-INPUT-PATH LS-INPUT-LENGTH
                                       LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

      *    Check for ".."
           PERFORM CHECK-DOTDOT
           IF WS-FOUND = 1
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    Check for "./"
           PERFORM CHECK-DOTSLASH
           IF WS-FOUND = 1
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    Check for URL-encoded ".." (%2e%2e)
           PERFORM CHECK-ENCODED-DOT
           IF WS-FOUND = 1
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    Check for null byte (%00)
           PERFORM CHECK-NULL-BYTE
           IF WS-FOUND = 1
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

           GOBACK.

       HAS-TRAVERSAL-EXIT.
           EXIT.

      *================================================================
      * CHECK-DOTDOT: Search for ".." pattern
      *================================================================
       CHECK-DOTDOT SECTION.
           MOVE 0 TO WS-FOUND

           IF WS-INPUT-LEN < 2
              EXIT SECTION
           END-IF

           COMPUTE WS-REMAINING = WS-INPUT-LEN - 1

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-REMAINING

              IF LS-INPUT-PATH(WS-INDEX:2) = WS-DOTDOT
                 MOVE 1 TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM.

       CHECK-DOTDOT-EXIT.
           EXIT.

      *================================================================
      * CHECK-DOTSLASH: Search for "./" pattern
      *================================================================
       CHECK-DOTSLASH SECTION.
           MOVE 0 TO WS-FOUND

           IF WS-INPUT-LEN < 2
              EXIT SECTION
           END-IF

           COMPUTE WS-REMAINING = WS-INPUT-LEN - 1

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-REMAINING

              IF LS-INPUT-PATH(WS-INDEX:2) = WS-DOTSLASH
                 MOVE 1 TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM.

       CHECK-DOTSLASH-EXIT.
           EXIT.

      *================================================================
      * CHECK-ENCODED-DOT: Search for "%2e%2e" pattern
      *================================================================
       CHECK-ENCODED-DOT SECTION.
           MOVE 0 TO WS-FOUND

           IF WS-INPUT-LEN < 6
              EXIT SECTION
           END-IF

           COMPUTE WS-REMAINING = WS-INPUT-LEN - 5

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-REMAINING

      *       Case-insensitive check
              IF FUNCTION LOWER-CASE(LS-INPUT-PATH(WS-INDEX:6))
                 = WS-ENCODED-DOT
                 MOVE 1 TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM.

       CHECK-ENCODED-DOT-EXIT.
           EXIT.

      *================================================================
      * CHECK-NULL-BYTE: Search for "%00" pattern
      *================================================================
       CHECK-NULL-BYTE SECTION.
           MOVE 0 TO WS-FOUND

           IF WS-INPUT-LEN < 3
              EXIT SECTION
           END-IF

           COMPUTE WS-REMAINING = WS-INPUT-LEN - 2

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-REMAINING

              IF FUNCTION LOWER-CASE(LS-INPUT-PATH(WS-INDEX:3))
                 = WS-NULL-BYTE
                 MOVE 1 TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM.

       CHECK-NULL-BYTE-EXIT.
           EXIT.

      *================================================================
      * SANITIZE-FILENAME: Remove dangerous characters from filename
      * Input:  LS-INPUT-PATH, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-PATH, LS-OUTPUT-LENGTH
      *================================================================
       SANITIZE-FILENAME SECTION.
           ENTRY "SANITIZE-FILENAME" USING LS-INPUT-PATH
                 LS-INPUT-LENGTH LS-OUTPUT-PATH LS-OUTPUT-LENGTH.

           INITIALIZE LS-OUTPUT-PATH
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-PATH(WS-INDEX:1) TO WS-CURRENT-CHAR

      *       Replace dangerous chars with underscore
              EVALUATE TRUE
                 WHEN WS-CURRENT-CHAR = "/"
                 WHEN WS-CURRENT-CHAR = "\"
                 WHEN WS-CURRENT-CHAR = ":"
                 WHEN WS-CURRENT-CHAR = "*"
                 WHEN WS-CURRENT-CHAR = "?"
                 WHEN WS-CURRENT-CHAR = '"'
                 WHEN WS-CURRENT-CHAR = "<"
                 WHEN WS-CURRENT-CHAR = ">"
                 WHEN WS-CURRENT-CHAR = "|"
                    MOVE "_" TO
                         LS-OUTPUT-PATH(WS-OUTPUT-POS:1)
                    ADD 1 TO WS-OUTPUT-POS
                 WHEN OTHER
                    MOVE WS-CURRENT-CHAR TO
                         LS-OUTPUT-PATH(WS-OUTPUT-POS:1)
                    ADD 1 TO WS-OUTPUT-POS
              END-EVALUATE
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       SANITIZE-FILENAME-EXIT.
           EXIT.

      *================================================================
      * SAFE-PATH-JOIN: Join base path with filename safely
      * Input:  LS-INPUT-PATH (base), LS-INPUT-LENGTH
      *         LS-OUTPUT-PATH (filename to join)
      *         LS-OUTPUT-LENGTH (filename length)
      * Output: LS-OUTPUT-PATH (joined path), LS-RESULT, LS-ERROR-MSG
      *================================================================
       SAFE-PATH-JOIN SECTION.
           ENTRY "SAFE-PATH-JOIN" USING LS-INPUT-PATH LS-INPUT-LENGTH
                 LS-OUTPUT-PATH LS-OUTPUT-LENGTH LS-RESULT LS-ERROR-MSG.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-ERROR-MSG

      *    Check for traversal in filename
           PERFORM HAS-TRAVERSAL-CHECK
           IF WS-FOUND = 1
              MOVE 0 TO LS-RESULT
              MOVE "Path traversal detected" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Join paths (simplified - just concatenate with separator)
           STRING LS-INPUT-PATH(1:LS-INPUT-LENGTH)
                  "/"
                  LS-OUTPUT-PATH(1:LS-OUTPUT-LENGTH)
                  DELIMITED SIZE INTO LS-OUTPUT-PATH
           END-STRING

      *    Calculate new length
           COMPUTE LS-OUTPUT-LENGTH =
              LS-INPUT-LENGTH + 1 + LS-OUTPUT-LENGTH

           GOBACK.

       SAFE-PATH-JOIN-EXIT.
           EXIT.

      *================================================================
      * HAS-TRAVERSAL-CHECK: Internal check using output path
      *================================================================
       HAS-TRAVERSAL-CHECK SECTION.
           MOVE 0 TO WS-FOUND
           MOVE LS-OUTPUT-LENGTH TO WS-INPUT-LEN

      *    Check for ".."
           IF WS-INPUT-LEN >= 2
              COMPUTE WS-REMAINING = WS-INPUT-LEN - 1
              PERFORM VARYING WS-INDEX FROM 1 BY 1
                      UNTIL WS-INDEX > WS-REMAINING
                 IF LS-OUTPUT-PATH(WS-INDEX:2) = WS-DOTDOT
                    MOVE 1 TO WS-FOUND
                    EXIT PERFORM
                 END-IF
              END-PERFORM
           END-IF.

       HAS-TRAVERSAL-CHECK-EXIT.
           EXIT.

       END PROGRAM SAFE-PATH.
