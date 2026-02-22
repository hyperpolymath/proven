      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafePath - FFI bindings to libproven path operations.
      *> All traversal detection and sanitization is performed in
      *> verified Idris 2 code via libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-PATH.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       LINKAGE SECTION.
       01 LS-PATH-STRING        PIC X(4096).
       01 LS-PATH-LENGTH        PIC 9(9) COMP.
       01 LS-OUTPUT-STRING      PIC X(4096).
       01 LS-OUTPUT-LENGTH      PIC 9(9) COMP.
       01 LS-RESULT-FLAG        PIC 9 COMP.

       PROCEDURE DIVISION.

      *================================================================
      * HAS-TRAVERSAL: Check for directory traversal via libproven
      *================================================================
       HAS-TRAVERSAL SECTION.
           ENTRY "HAS-TRAVERSAL" USING LS-PATH-STRING
                                       LS-PATH-LENGTH
                                       LS-RESULT-FLAG.

           CALL "proven_path_has_traversal"
               USING BY REFERENCE LS-PATH-STRING
                     BY VALUE LS-PATH-LENGTH
               RETURNING WS-BOOL-RESULT
           END-CALL

           IF WS-BOOL-STATUS = 0 AND WS-BOOL-VALUE = 1
              MOVE 1 TO LS-RESULT-FLAG
           ELSE
              MOVE 0 TO LS-RESULT-FLAG
           END-IF

           GOBACK.

       HAS-TRAVERSAL-EXIT.
           EXIT.

      *================================================================
      * SANITIZE-FILENAME: Sanitize filename via libproven
      *================================================================
       SANITIZE-FILENAME SECTION.
           ENTRY "SANITIZE-FILENAME" USING LS-PATH-STRING
                                           LS-PATH-LENGTH
                                           LS-OUTPUT-STRING
                                           LS-OUTPUT-LENGTH.

           CALL "proven_path_sanitize_filename"
               USING BY REFERENCE LS-PATH-STRING
                     BY VALUE LS-PATH-LENGTH
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
              MOVE LS-PATH-STRING TO LS-OUTPUT-STRING
              MOVE LS-PATH-LENGTH TO LS-OUTPUT-LENGTH
           END-IF

           GOBACK.

       SANITIZE-FILENAME-EXIT.
           EXIT.

       END PROGRAM SAFE-PATH.
