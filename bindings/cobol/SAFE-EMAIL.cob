      * SPDX-License-Identifier: PMPL-1.0
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeEmail - Email validation for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-EMAIL.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX              PIC 9(4).
       01 WS-INPUT-LEN          PIC 9(4).
       01 WS-CURRENT-CHAR       PIC X(1).
       01 WS-AT-POS             PIC 9(4) VALUE 0.
       01 WS-DOT-POS            PIC 9(4) VALUE 0.
       01 WS-AT-COUNT           PIC 9(2) VALUE 0.
       01 WS-LOCAL-LEN          PIC 9(3) VALUE 0.
       01 WS-DOMAIN-LEN         PIC 9(3) VALUE 0.
       01 WS-IS-VALID-CHAR      PIC 9 VALUE 0.
       01 WS-HAS-DOT            PIC 9 VALUE 0.
       01 WS-TEMP-DOMAIN        PIC X(255).

      * Email length limits
       01 WS-EMAIL-LIMITS.
          05 WS-MAX-TOTAL       PIC 9(3) VALUE 254.
          05 WS-MAX-LOCAL       PIC 9(2) VALUE 64.
          05 WS-MAX-DOMAIN      PIC 9(3) VALUE 255.
          05 WS-MAX-LABEL       PIC 9(2) VALUE 63.

      * Disposable email domains (subset)
       01 WS-DISPOSABLE-DOMAINS.
          05 FILLER             PIC X(20) VALUE "mailinator.com".
          05 FILLER             PIC X(20) VALUE "guerrillamail.com".
          05 FILLER             PIC X(20) VALUE "10minutemail.com".
          05 FILLER             PIC X(20) VALUE "tempmail.com".
          05 FILLER             PIC X(20) VALUE "throwaway.email".
          05 FILLER             PIC X(20) VALUE "fakeinbox.com".
          05 FILLER             PIC X(20) VALUE "trashmail.com".
          05 FILLER             PIC X(20) VALUE "maildrop.cc".
       01 WS-DISPOSABLE-TABLE REDEFINES WS-DISPOSABLE-DOMAINS.
          05 WS-DISPOSABLE-ENTRY OCCURS 8 TIMES PIC X(20).

       01 WS-DISP-INDEX         PIC 9(2).
       01 WS-IS-DISPOSABLE      PIC 9 VALUE 0.

       LINKAGE SECTION.
       01 LS-EMAIL              PIC X(254).
       01 LS-EMAIL-LENGTH       PIC 9(4).
       01 LS-RESULT             PIC 9.
       01 LS-LOCAL-PART         PIC X(64).
       01 LS-DOMAIN             PIC X(255).
       01 LS-ERROR-MSG          PIC X(100).

       PROCEDURE DIVISION.

      *================================================================
      * IS-VALID-EMAIL: Basic email format validation
      * Input:  LS-EMAIL, LS-EMAIL-LENGTH
      * Output: LS-RESULT (1=valid, 0=invalid)
      *================================================================
       IS-VALID-EMAIL SECTION.
           ENTRY "IS-VALID-EMAIL" USING LS-EMAIL LS-EMAIL-LENGTH
                                        LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO WS-AT-COUNT
           MOVE 0 TO WS-AT-POS
           MOVE LS-EMAIL-LENGTH TO WS-INPUT-LEN

      *    Check length bounds
           IF WS-INPUT-LEN = 0 OR WS-INPUT-LEN > WS-MAX-TOTAL
              GOBACK
           END-IF

      *    Find @ symbol and count occurrences
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-EMAIL(WS-INDEX:1) TO WS-CURRENT-CHAR
              IF WS-CURRENT-CHAR = "@"
                 ADD 1 TO WS-AT-COUNT
                 MOVE WS-INDEX TO WS-AT-POS
              END-IF
           END-PERFORM

      *    Must have exactly one @
           IF WS-AT-COUNT NOT = 1
              GOBACK
           END-IF

      *    @ cannot be first or last character
           IF WS-AT-POS = 1 OR WS-AT-POS = WS-INPUT-LEN
              GOBACK
           END-IF

      *    Check local part length
           COMPUTE WS-LOCAL-LEN = WS-AT-POS - 1
           IF WS-LOCAL-LEN > WS-MAX-LOCAL
              GOBACK
           END-IF

      *    Check domain length
           COMPUTE WS-DOMAIN-LEN = WS-INPUT-LEN - WS-AT-POS
           IF WS-DOMAIN-LEN > WS-MAX-DOMAIN
              GOBACK
           END-IF

      *    Domain must have at least one dot
           MOVE 0 TO WS-HAS-DOT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-DOMAIN-LEN
                   OR WS-HAS-DOT = 1

              COMPUTE WS-DOT-POS = WS-AT-POS + WS-INDEX
              IF LS-EMAIL(WS-DOT-POS:1) = "."
                 MOVE 1 TO WS-HAS-DOT
              END-IF
           END-PERFORM

           IF WS-HAS-DOT = 0
              GOBACK
           END-IF

      *    All checks passed
           MOVE 1 TO LS-RESULT
           GOBACK.

       IS-VALID-EMAIL-EXIT.
           EXIT.

      *================================================================
      * PARSE-EMAIL: Parse email into local part and domain
      * Input:  LS-EMAIL, LS-EMAIL-LENGTH
      * Output: LS-RESULT, LS-LOCAL-PART, LS-DOMAIN, LS-ERROR-MSG
      *================================================================
       PARSE-EMAIL SECTION.
           ENTRY "PARSE-EMAIL" USING LS-EMAIL LS-EMAIL-LENGTH
                 LS-RESULT LS-LOCAL-PART LS-DOMAIN LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-LOCAL-PART
           INITIALIZE LS-DOMAIN
           INITIALIZE LS-ERROR-MSG
           MOVE 0 TO WS-AT-POS
           MOVE LS-EMAIL-LENGTH TO WS-INPUT-LEN

      *    Check length
           IF WS-INPUT-LEN = 0
              MOVE "Email is empty" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF WS-INPUT-LEN > WS-MAX-TOTAL
              MOVE "Email exceeds maximum length" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Find @ symbol
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN OR WS-AT-POS > 0

              IF LS-EMAIL(WS-INDEX:1) = "@"
                 MOVE WS-INDEX TO WS-AT-POS
              END-IF
           END-PERFORM

           IF WS-AT-POS = 0
              MOVE "Missing @ symbol" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Extract local part
           COMPUTE WS-LOCAL-LEN = WS-AT-POS - 1
           IF WS-LOCAL-LEN = 0
              MOVE "Local part is empty" TO LS-ERROR-MSG
              GOBACK
           END-IF
           IF WS-LOCAL-LEN > WS-MAX-LOCAL
              MOVE "Local part exceeds maximum length" TO LS-ERROR-MSG
              GOBACK
           END-IF
           MOVE LS-EMAIL(1:WS-LOCAL-LEN) TO LS-LOCAL-PART

      *    Extract domain
           COMPUTE WS-DOMAIN-LEN = WS-INPUT-LEN - WS-AT-POS
           IF WS-DOMAIN-LEN = 0
              MOVE "Domain is empty" TO LS-ERROR-MSG
              GOBACK
           END-IF
           COMPUTE WS-INDEX = WS-AT-POS + 1
           MOVE LS-EMAIL(WS-INDEX:WS-DOMAIN-LEN) TO LS-DOMAIN

      *    Validate domain has dot
           MOVE 0 TO WS-HAS-DOT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-DOMAIN-LEN
                   OR WS-HAS-DOT = 1

              IF LS-DOMAIN(WS-INDEX:1) = "."
                 MOVE 1 TO WS-HAS-DOT
              END-IF
           END-PERFORM

           IF WS-HAS-DOT = 0
              MOVE "Domain must contain a dot" TO LS-ERROR-MSG
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       PARSE-EMAIL-EXIT.
           EXIT.

      *================================================================
      * IS-DISPOSABLE-EMAIL: Check if email uses disposable domain
      * Input:  LS-DOMAIN (domain part of email)
      * Output: LS-RESULT (1=disposable, 0=not disposable)
      *================================================================
       IS-DISPOSABLE-EMAIL SECTION.
           ENTRY "IS-DISPOSABLE-EMAIL" USING LS-DOMAIN LS-RESULT.

           MOVE 0 TO LS-RESULT

      *    Convert domain to lowercase for comparison
           MOVE FUNCTION LOWER-CASE(LS-DOMAIN) TO WS-TEMP-DOMAIN

      *    Check against known disposable domains
           PERFORM VARYING WS-DISP-INDEX FROM 1 BY 1
                   UNTIL WS-DISP-INDEX > 8

              IF WS-TEMP-DOMAIN(1:20) =
                 WS-DISPOSABLE-ENTRY(WS-DISP-INDEX)
                 MOVE 1 TO LS-RESULT
                 EXIT PERFORM
              END-IF
           END-PERFORM

           GOBACK.

       IS-DISPOSABLE-EMAIL-EXIT.
           EXIT.

      *================================================================
      * NORMALIZE-EMAIL: Normalize email (lowercase domain)
      * Input:  LS-EMAIL, LS-EMAIL-LENGTH
      * Output: LS-EMAIL (normalized), LS-RESULT
      *================================================================
       NORMALIZE-EMAIL SECTION.
           ENTRY "NORMALIZE-EMAIL" USING LS-EMAIL LS-EMAIL-LENGTH
                                         LS-RESULT.

           MOVE 1 TO LS-RESULT
           MOVE 0 TO WS-AT-POS
           MOVE LS-EMAIL-LENGTH TO WS-INPUT-LEN

      *    Find @ symbol
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN OR WS-AT-POS > 0

              IF LS-EMAIL(WS-INDEX:1) = "@"
                 MOVE WS-INDEX TO WS-AT-POS
              END-IF
           END-PERFORM

           IF WS-AT-POS = 0
              MOVE 0 TO LS-RESULT
              GOBACK
           END-IF

      *    Lowercase the domain part
           COMPUTE WS-INDEX = WS-AT-POS + 1
           COMPUTE WS-DOMAIN-LEN = WS-INPUT-LEN - WS-AT-POS

           IF WS-DOMAIN-LEN > 0
              MOVE FUNCTION LOWER-CASE(
                   LS-EMAIL(WS-INDEX:WS-DOMAIN-LEN))
                   TO LS-EMAIL(WS-INDEX:WS-DOMAIN-LEN)
           END-IF

           GOBACK.

       NORMALIZE-EMAIL-EXIT.
           EXIT.

       END PROGRAM SAFE-EMAIL.
