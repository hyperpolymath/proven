      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeNetwork - FFI bindings to libproven network ops.
      *> All IP parsing and classification is performed in verified
      *> Idris 2 code via libproven.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-NETWORK.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

      * IPv4 address structure (matching ProvenIPv4Address)
       01 WS-IPV4-RESULT.
          05 WS-IPV4-STATUS     PIC S9(9) COMP.
          05 WS-IPV4-OCTETS.
             10 WS-IPV4-OCTET  OCCURS 4 TIMES PIC 9(3) COMP.

       01 WS-IS-PRIVATE-FLAG    PIC 9 COMP.
       01 WS-IS-LOOPBACK-FLAG   PIC 9 COMP.

       LINKAGE SECTION.
       01 LS-IP-STRING          PIC X(15).
       01 LS-IP-LENGTH          PIC 9(9) COMP.
       01 LS-OCTET-1            PIC 9(3) COMP.
       01 LS-OCTET-2            PIC 9(3) COMP.
       01 LS-OCTET-3            PIC 9(3) COMP.
       01 LS-OCTET-4            PIC 9(3) COMP.
       01 LS-STATUS             PIC S9(9) COMP.
       01 LS-RESULT-FLAG        PIC 9 COMP.

       PROCEDURE DIVISION.

      *================================================================
      * PARSE-IPV4: Parse IPv4 address string via libproven
      *================================================================
       PARSE-IPV4 SECTION.
           ENTRY "PARSE-IPV4" USING LS-IP-STRING LS-IP-LENGTH
                                    LS-OCTET-1 LS-OCTET-2
                                    LS-OCTET-3 LS-OCTET-4
                                    LS-STATUS.

           CALL "proven_network_parse_ipv4"
               USING BY REFERENCE LS-IP-STRING
                     BY VALUE LS-IP-LENGTH
               RETURNING WS-IPV4-RESULT
           END-CALL

           MOVE WS-IPV4-STATUS TO LS-STATUS
           IF WS-IPV4-STATUS = 0
              MOVE WS-IPV4-OCTET(1) TO LS-OCTET-1
              MOVE WS-IPV4-OCTET(2) TO LS-OCTET-2
              MOVE WS-IPV4-OCTET(3) TO LS-OCTET-3
              MOVE WS-IPV4-OCTET(4) TO LS-OCTET-4
           END-IF

           GOBACK.

       PARSE-IPV4-EXIT.
           EXIT.

      *================================================================
      * IS-PRIVATE-IP: Check if IPv4 is private via libproven
      *================================================================
       IS-PRIVATE-IP SECTION.
           ENTRY "IS-PRIVATE-IP" USING LS-OCTET-1 LS-OCTET-2
                                       LS-OCTET-3 LS-OCTET-4
                                       LS-RESULT-FLAG.

           CALL "proven_network_ipv4_is_private"
               USING BY VALUE LS-OCTET-1
                     BY VALUE LS-OCTET-2
                     BY VALUE LS-OCTET-3
                     BY VALUE LS-OCTET-4
               RETURNING WS-IS-PRIVATE-FLAG
           END-CALL

           MOVE WS-IS-PRIVATE-FLAG TO LS-RESULT-FLAG
           GOBACK.

       IS-PRIVATE-IP-EXIT.
           EXIT.

      *================================================================
      * IS-LOOPBACK: Check if IPv4 is loopback via libproven
      *================================================================
       IS-LOOPBACK SECTION.
           ENTRY "IS-LOOPBACK" USING LS-OCTET-1 LS-OCTET-2
                                     LS-OCTET-3 LS-OCTET-4
                                     LS-RESULT-FLAG.

           CALL "proven_network_ipv4_is_loopback"
               USING BY VALUE LS-OCTET-1
                     BY VALUE LS-OCTET-2
                     BY VALUE LS-OCTET-3
                     BY VALUE LS-OCTET-4
               RETURNING WS-IS-LOOPBACK-FLAG
           END-CALL

           MOVE WS-IS-LOOPBACK-FLAG TO LS-RESULT-FLAG
           GOBACK.

       IS-LOOPBACK-EXIT.
           EXIT.

       END PROGRAM SAFE-NETWORK.
