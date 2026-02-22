      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeUUID - FFI bindings to libproven UUID operations.
      *> All UUID operations are performed in verified Idris 2 code
      *> via libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-UUID.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

      * UUID result structure (matching ProvenUUIDResult)
       01 WS-UUID-RESULT.
          05 WS-UUID-STATUS     PIC S9(9) COMP.
          05 WS-UUID-BYTES      PIC X(16).

       01 WS-UUID-VERSION-VAL   PIC 9(3) COMP.
       01 WS-UUID-NIL-FLAG      PIC 9 COMP.

       LINKAGE SECTION.
       01 LS-UUID-STRING        PIC X(36).
       01 LS-UUID-LENGTH        PIC 9(9) COMP.
       01 LS-UUID-BYTES         PIC X(16).
       01 LS-OUTPUT-STRING      PIC X(36).
       01 LS-STATUS             PIC S9(9) COMP.
       01 LS-RESULT-FLAG        PIC 9 COMP.
       01 LS-VERSION            PIC 9(3) COMP.

       PROCEDURE DIVISION.

      *================================================================
      * UUID-V4: Generate UUID v4 (random) via libproven
      *================================================================
       UUID-V4 SECTION.
           ENTRY "UUID-V4" USING LS-UUID-BYTES LS-STATUS.

           CALL "proven_uuid_v4"
               RETURNING WS-UUID-RESULT
           END-CALL

           MOVE WS-UUID-STATUS TO LS-STATUS
           IF WS-UUID-STATUS = 0
              MOVE WS-UUID-BYTES TO LS-UUID-BYTES
           END-IF

           GOBACK.

       UUID-V4-EXIT.
           EXIT.

      *================================================================
      * UUID-TO-STRING: Format UUID as canonical string via libproven
      *================================================================
       UUID-TO-STRING SECTION.
           ENTRY "UUID-TO-STRING" USING LS-UUID-BYTES
                                        LS-OUTPUT-STRING
                                        LS-STATUS.

           CALL "proven_uuid_to_string"
               USING BY VALUE LS-UUID-BYTES
               RETURNING WS-STRING-RESULT
           END-CALL

           MOVE WS-STR-STATUS TO LS-STATUS
           IF WS-STR-STATUS = 0
              MOVE FUNCTION NATIONAL-OF(WS-STR-PTR)
                   TO LS-OUTPUT-STRING
              CALL "proven_free_string"
                   USING BY VALUE WS-STR-PTR
              END-CALL
           END-IF

           GOBACK.

       UUID-TO-STRING-EXIT.
           EXIT.

      *================================================================
      * UUID-PARSE: Parse UUID from string via libproven
      *================================================================
       UUID-PARSE SECTION.
           ENTRY "UUID-PARSE" USING LS-UUID-STRING LS-UUID-LENGTH
                                    LS-UUID-BYTES LS-STATUS.

           CALL "proven_uuid_parse"
               USING BY REFERENCE LS-UUID-STRING
                     BY VALUE LS-UUID-LENGTH
               RETURNING WS-UUID-RESULT
           END-CALL

           MOVE WS-UUID-STATUS TO LS-STATUS
           IF WS-UUID-STATUS = 0
              MOVE WS-UUID-BYTES TO LS-UUID-BYTES
           END-IF

           GOBACK.

       UUID-PARSE-EXIT.
           EXIT.

      *================================================================
      * UUID-IS-NIL: Check if UUID is nil via libproven
      *================================================================
       UUID-IS-NIL SECTION.
           ENTRY "UUID-IS-NIL" USING LS-UUID-BYTES LS-RESULT-FLAG.

           CALL "proven_uuid_is_nil"
               USING BY VALUE LS-UUID-BYTES
               RETURNING WS-UUID-NIL-FLAG
           END-CALL

           MOVE WS-UUID-NIL-FLAG TO LS-RESULT-FLAG
           GOBACK.

       UUID-IS-NIL-EXIT.
           EXIT.

      *================================================================
      * UUID-VERSION: Get UUID version via libproven
      *================================================================
       UUID-VERSION SECTION.
           ENTRY "UUID-VERSION" USING LS-UUID-BYTES LS-VERSION.

           CALL "proven_uuid_version"
               USING BY VALUE LS-UUID-BYTES
               RETURNING WS-UUID-VERSION-VAL
           END-CALL

           MOVE WS-UUID-VERSION-VAL TO LS-VERSION
           GOBACK.

       UUID-VERSION-EXIT.
           EXIT.

       END PROGRAM SAFE-UUID.
