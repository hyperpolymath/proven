      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeJSON - FFI bindings to libproven JSON validation.
      *> All JSON validation is performed in verified Idris 2 code
      *> via libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-JSON.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       01 WS-JSON-TYPE          PIC S9(9) COMP.

       LINKAGE SECTION.
       01 LS-JSON-STRING        PIC X(65535).
       01 LS-JSON-LENGTH        PIC 9(9) COMP.
       01 LS-RESULT-FLAG        PIC 9 COMP.
       01 LS-TYPE-CODE          PIC S9(9) COMP.

       PROCEDURE DIVISION.

      *================================================================
      * IS-VALID-JSON: Validate JSON string via libproven
      *================================================================
       IS-VALID-JSON SECTION.
           ENTRY "IS-VALID-JSON" USING LS-JSON-STRING
                                       LS-JSON-LENGTH
                                       LS-RESULT-FLAG.

           CALL "proven_json_is_valid"
               USING BY REFERENCE LS-JSON-STRING
                     BY VALUE LS-JSON-LENGTH
               RETURNING WS-BOOL-RESULT
           END-CALL

           IF WS-BOOL-STATUS = 0 AND WS-BOOL-VALUE = 1
              MOVE 1 TO LS-RESULT-FLAG
           ELSE
              MOVE 0 TO LS-RESULT-FLAG
           END-IF

           GOBACK.

       IS-VALID-JSON-EXIT.
           EXIT.

      *================================================================
      * GET-JSON-TYPE: Get root JSON value type via libproven
      *================================================================
       GET-JSON-TYPE SECTION.
           ENTRY "GET-JSON-TYPE" USING LS-JSON-STRING
                                       LS-JSON-LENGTH
                                       LS-TYPE-CODE.

           CALL "proven_json_get_type"
               USING BY REFERENCE LS-JSON-STRING
                     BY VALUE LS-JSON-LENGTH
               RETURNING WS-JSON-TYPE
           END-CALL

           MOVE WS-JSON-TYPE TO LS-TYPE-CODE
           GOBACK.

       GET-JSON-TYPE-EXIT.
           EXIT.

       END PROGRAM SAFE-JSON.
