      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeString - FFI bindings to libproven string operations.
      *> All escaping/sanitization is performed in verified Idris 2 code
      *> via libproven. This module uses CALL to invoke C ABI functions.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-STRING.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       LINKAGE SECTION.
       01 LS-INPUT-STRING       PIC X(4096).
       01 LS-INPUT-LENGTH       PIC 9(9) COMP.
       01 LS-OUTPUT-STRING      PIC X(8192).
       01 LS-OUTPUT-LENGTH      PIC 9(9) COMP.

       PROCEDURE DIVISION.

      *================================================================
      * ESCAPE-HTML: Escape HTML special characters via libproven
      *================================================================
       ESCAPE-HTML SECTION.
           ENTRY "ESCAPE-HTML" USING LS-INPUT-STRING LS-INPUT-LENGTH
                                     LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           CALL "proven_string_escape_html"
               USING BY REFERENCE LS-INPUT-STRING
                     BY VALUE LS-INPUT-LENGTH
               RETURNING WS-STRING-RESULT
           END-CALL

           IF WS-STR-STATUS = 0
              MOVE WS-STR-LENGTH TO LS-OUTPUT-LENGTH
              MOVE FUNCTION NATIONAL-OF(WS-STR-PTR)
                   TO LS-OUTPUT-STRING
              CALL "proven_free_string"
                   USING BY VALUE WS-STR-PTR
              END-CALL
           ELSE
              MOVE LS-INPUT-STRING TO LS-OUTPUT-STRING
              MOVE LS-INPUT-LENGTH TO LS-OUTPUT-LENGTH
           END-IF

           GOBACK.

       ESCAPE-HTML-EXIT.
           EXIT.

      *================================================================
      * ESCAPE-SQL: Escape SQL single quotes via libproven
      *================================================================
       ESCAPE-SQL SECTION.
           ENTRY "ESCAPE-SQL" USING LS-INPUT-STRING LS-INPUT-LENGTH
                                    LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           CALL "proven_string_escape_sql"
               USING BY REFERENCE LS-INPUT-STRING
                     BY VALUE LS-INPUT-LENGTH
               RETURNING WS-STRING-RESULT
           END-CALL

           IF WS-STR-STATUS = 0
              MOVE WS-STR-LENGTH TO LS-OUTPUT-LENGTH
              MOVE FUNCTION NATIONAL-OF(WS-STR-PTR)
                   TO LS-OUTPUT-STRING
              CALL "proven_free_string"
                   USING BY VALUE WS-STR-PTR
              END-CALL
           ELSE
              MOVE LS-INPUT-STRING TO LS-OUTPUT-STRING
              MOVE LS-INPUT-LENGTH TO LS-OUTPUT-LENGTH
           END-IF

           GOBACK.

       ESCAPE-SQL-EXIT.
           EXIT.

      *================================================================
      * ESCAPE-JS: Escape JavaScript special chars via libproven
      *================================================================
       ESCAPE-JS SECTION.
           ENTRY "ESCAPE-JS" USING LS-INPUT-STRING LS-INPUT-LENGTH
                                    LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           CALL "proven_string_escape_js"
               USING BY REFERENCE LS-INPUT-STRING
                     BY VALUE LS-INPUT-LENGTH
               RETURNING WS-STRING-RESULT
           END-CALL

           IF WS-STR-STATUS = 0
              MOVE WS-STR-LENGTH TO LS-OUTPUT-LENGTH
              MOVE FUNCTION NATIONAL-OF(WS-STR-PTR)
                   TO LS-OUTPUT-STRING
              CALL "proven_free_string"
                   USING BY VALUE WS-STR-PTR
              END-CALL
           ELSE
              MOVE LS-INPUT-STRING TO LS-OUTPUT-STRING
              MOVE LS-INPUT-LENGTH TO LS-OUTPUT-LENGTH
           END-IF

           GOBACK.

       ESCAPE-JS-EXIT.
           EXIT.

      *================================================================
      * IS-VALID-UTF8: Check if bytes are valid UTF-8 via libproven
      *================================================================
       IS-VALID-UTF8 SECTION.
           ENTRY "IS-VALID-UTF8" USING LS-INPUT-STRING
                                       LS-INPUT-LENGTH
                                       LS-OUTPUT-LENGTH.

           CALL "proven_string_is_valid_utf8"
               USING BY REFERENCE LS-INPUT-STRING
                     BY VALUE LS-INPUT-LENGTH
               RETURNING WS-BOOL-RESULT
           END-CALL

           IF WS-BOOL-STATUS = 0 AND WS-BOOL-VALUE = 1
              MOVE 1 TO LS-OUTPUT-LENGTH
           ELSE
              MOVE 0 TO LS-OUTPUT-LENGTH
           END-IF

           GOBACK.

       IS-VALID-UTF8-EXIT.
           EXIT.

       END PROGRAM SAFE-STRING.
