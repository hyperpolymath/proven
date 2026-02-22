      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeFloat - FFI bindings to libproven float operations.
      *> All float operations are performed in verified Idris 2 code
      *> via libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-FLOAT.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       01 WS-BOOL-FLAG          PIC 9 COMP.

       LINKAGE SECTION.
       01 LS-OPERAND-A          COMP-2.
       01 LS-OPERAND-B          COMP-2.
       01 LS-RESULT             COMP-2.
       01 LS-STATUS             PIC S9(9) COMP.
       01 LS-FLAG               PIC 9 COMP.

       PROCEDURE DIVISION.

      *================================================================
      * SAFE-FLOAT-DIV: Safe float division via libproven
      *================================================================
       SAFE-FLOAT-DIV SECTION.
           ENTRY "SAFE-FLOAT-DIV" USING LS-OPERAND-A LS-OPERAND-B
                                        LS-RESULT LS-STATUS.

           CALL "proven_float_div"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-FLOAT-RESULT
           END-CALL

           MOVE WS-FLOAT-STATUS TO LS-STATUS
           MOVE WS-FLOAT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-FLOAT-DIV-EXIT.
           EXIT.

      *================================================================
      * SAFE-FLOAT-SQRT: Safe square root via libproven
      *================================================================
       SAFE-FLOAT-SQRT SECTION.
           ENTRY "SAFE-FLOAT-SQRT" USING LS-OPERAND-A
                                         LS-RESULT LS-STATUS.

           CALL "proven_float_sqrt"
               USING BY VALUE LS-OPERAND-A
               RETURNING WS-FLOAT-RESULT
           END-CALL

           MOVE WS-FLOAT-STATUS TO LS-STATUS
           MOVE WS-FLOAT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-FLOAT-SQRT-EXIT.
           EXIT.

      *================================================================
      * SAFE-FLOAT-LN: Safe natural logarithm via libproven
      *================================================================
       SAFE-FLOAT-LN SECTION.
           ENTRY "SAFE-FLOAT-LN" USING LS-OPERAND-A
                                        LS-RESULT LS-STATUS.

           CALL "proven_float_ln"
               USING BY VALUE LS-OPERAND-A
               RETURNING WS-FLOAT-RESULT
           END-CALL

           MOVE WS-FLOAT-STATUS TO LS-STATUS
           MOVE WS-FLOAT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-FLOAT-LN-EXIT.
           EXIT.

      *================================================================
      * FLOAT-IS-FINITE: Check if finite via libproven
      *================================================================
       FLOAT-IS-FINITE SECTION.
           ENTRY "FLOAT-IS-FINITE" USING LS-OPERAND-A LS-FLAG.

           CALL "proven_float_is_finite"
               USING BY VALUE LS-OPERAND-A
               RETURNING WS-BOOL-FLAG
           END-CALL

           MOVE WS-BOOL-FLAG TO LS-FLAG
           GOBACK.

       FLOAT-IS-FINITE-EXIT.
           EXIT.

      *================================================================
      * FLOAT-IS-NAN: Check if NaN via libproven
      *================================================================
       FLOAT-IS-NAN SECTION.
           ENTRY "FLOAT-IS-NAN" USING LS-OPERAND-A LS-FLAG.

           CALL "proven_float_is_nan"
               USING BY VALUE LS-OPERAND-A
               RETURNING WS-BOOL-FLAG
           END-CALL

           MOVE WS-BOOL-FLAG TO LS-FLAG
           GOBACK.

       FLOAT-IS-NAN-EXIT.
           EXIT.

       END PROGRAM SAFE-FLOAT.
