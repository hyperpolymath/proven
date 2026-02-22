      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeEmail - FFI bindings to libproven email validation.
      *> All validation is performed in verified Idris 2 code via
      *> libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-EMAIL.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       LINKAGE SECTION.
       01 LS-EMAIL-STRING       PIC X(254).
       01 LS-EMAIL-LENGTH       PIC 9(9) COMP.
       01 LS-RESULT-FLAG        PIC 9 COMP.

       PROCEDURE DIVISION.

      *================================================================
      * IS-VALID-EMAIL: Validate email address via libproven
      *================================================================
       IS-VALID-EMAIL SECTION.
           ENTRY "IS-VALID-EMAIL" USING LS-EMAIL-STRING
                                        LS-EMAIL-LENGTH
                                        LS-RESULT-FLAG.

           CALL "proven_email_is_valid"
               USING BY REFERENCE LS-EMAIL-STRING
                     BY VALUE LS-EMAIL-LENGTH
               RETURNING WS-BOOL-RESULT
           END-CALL

           IF WS-BOOL-STATUS = 0 AND WS-BOOL-VALUE = 1
              MOVE 1 TO LS-RESULT-FLAG
           ELSE
              MOVE 0 TO LS-RESULT-FLAG
           END-IF

           GOBACK.

       IS-VALID-EMAIL-EXIT.
           EXIT.

       END PROGRAM SAFE-EMAIL.
