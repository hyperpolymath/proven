      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeCrypto - FFI bindings to libproven crypto operations.
      *> All crypto operations are performed in verified Idris 2 code
      *> via libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-CRYPTO.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       01 WS-CRYPTO-STATUS      PIC S9(9) COMP.

       LINKAGE SECTION.
       01 LS-INPUT-A            PIC X(4096).
       01 LS-LENGTH-A           PIC 9(9) COMP.
       01 LS-INPUT-B            PIC X(4096).
       01 LS-LENGTH-B           PIC 9(9) COMP.
       01 LS-RESULT-FLAG        PIC 9 COMP.
       01 LS-OUTPUT-BUFFER      PIC X(4096).
       01 LS-OUTPUT-LENGTH      PIC 9(9) COMP.

       PROCEDURE DIVISION.

      *================================================================
      * CONSTANT-TIME-EQ: Timing-attack safe comparison via libproven
      *================================================================
       CONSTANT-TIME-EQ SECTION.
           ENTRY "CONSTANT-TIME-EQ" USING LS-INPUT-A LS-LENGTH-A
                                          LS-INPUT-B LS-LENGTH-B
                                          LS-RESULT-FLAG.

           CALL "proven_crypto_constant_time_eq"
               USING BY REFERENCE LS-INPUT-A
                     BY VALUE LS-LENGTH-A
                     BY REFERENCE LS-INPUT-B
                     BY VALUE LS-LENGTH-B
               RETURNING WS-BOOL-RESULT
           END-CALL

           IF WS-BOOL-STATUS = 0 AND WS-BOOL-VALUE = 1
              MOVE 1 TO LS-RESULT-FLAG
           ELSE
              MOVE 0 TO LS-RESULT-FLAG
           END-IF

           GOBACK.

       CONSTANT-TIME-EQ-EXIT.
           EXIT.

      *================================================================
      * RANDOM-BYTES: Generate secure random bytes via libproven
      *================================================================
       RANDOM-BYTES SECTION.
           ENTRY "RANDOM-BYTES" USING LS-OUTPUT-BUFFER
                                      LS-OUTPUT-LENGTH.

           CALL "proven_crypto_random_bytes"
               USING BY REFERENCE LS-OUTPUT-BUFFER
                     BY VALUE LS-OUTPUT-LENGTH
               RETURNING WS-CRYPTO-STATUS
           END-CALL

           GOBACK.

       RANDOM-BYTES-EXIT.
           EXIT.

       END PROGRAM SAFE-CRYPTO.
