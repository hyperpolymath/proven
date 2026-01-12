      * SPDX-License-Identifier: PMPL-1.0
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeNetwork - IP validation for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-NETWORK.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX              PIC 9(4).
       01 WS-INPUT-LEN          PIC 9(4).
       01 WS-CURRENT-CHAR       PIC X(1).
       01 WS-OCTET-VALUE        PIC 9(4) VALUE 0.
       01 WS-OCTET-COUNT        PIC 9 VALUE 0.
       01 WS-DIGIT-COUNT        PIC 9 VALUE 0.
       01 WS-TEMP-STRING        PIC X(16).

      * Parsed IP address
       01 WS-IP-ADDRESS.
          05 WS-OCTET           OCCURS 4 TIMES PIC 9(3).

      * IP classification codes
       01 WS-IP-CLASS-INVALID   PIC 9 VALUE 0.
       01 WS-IP-CLASS-LOOPBACK  PIC 9 VALUE 1.
       01 WS-IP-CLASS-PRIVATE   PIC 9 VALUE 2.
       01 WS-IP-CLASS-RESERVED  PIC 9 VALUE 3.
       01 WS-IP-CLASS-PUBLIC    PIC 9 VALUE 4.

      * Port limits
       01 WS-PORT-LIMITS.
          05 WS-PORT-MIN        PIC 9(5) VALUE 1.
          05 WS-PORT-MAX        PIC 9(5) VALUE 65535.
          05 WS-PRIVILEGED-MAX  PIC 9(5) VALUE 1023.

       LINKAGE SECTION.
       01 LS-IP-STRING          PIC X(15).
       01 LS-IP-LENGTH          PIC 9(2).
       01 LS-RESULT             PIC 9.
       01 LS-OCTET-1            PIC 9(3).
       01 LS-OCTET-2            PIC 9(3).
       01 LS-OCTET-3            PIC 9(3).
       01 LS-OCTET-4            PIC 9(3).
       01 LS-CLASSIFICATION     PIC 9.
       01 LS-PORT               PIC 9(5).

       PROCEDURE DIVISION.

      *================================================================
      * PARSE-IPV4: Parse IPv4 address string
      * Input:  LS-IP-STRING, LS-IP-LENGTH
      * Output: LS-RESULT (1=valid, 0=invalid)
      *         LS-OCTET-1 through LS-OCTET-4
      *================================================================
       PARSE-IPV4 SECTION.
           ENTRY "PARSE-IPV4" USING LS-IP-STRING LS-IP-LENGTH
                 LS-RESULT LS-OCTET-1 LS-OCTET-2 LS-OCTET-3 LS-OCTET-4.

           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-OCTET-1
           MOVE 0 TO LS-OCTET-2
           MOVE 0 TO LS-OCTET-3
           MOVE 0 TO LS-OCTET-4
           MOVE 1 TO WS-OCTET-COUNT
           MOVE 0 TO WS-OCTET-VALUE
           MOVE 0 TO WS-DIGIT-COUNT
           MOVE LS-IP-LENGTH TO WS-INPUT-LEN

      *    Initialize octets
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 4
              MOVE 0 TO WS-OCTET(WS-INDEX)
           END-PERFORM

      *    Parse each character
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-IP-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              EVALUATE TRUE
                 WHEN WS-CURRENT-CHAR >= "0"
                      AND WS-CURRENT-CHAR <= "9"
      *             Accumulate digit
                    ADD 1 TO WS-DIGIT-COUNT
      *             Check for leading zeros (invalid)
                    IF WS-DIGIT-COUNT > 1 AND WS-OCTET-VALUE = 0
                       AND WS-CURRENT-CHAR NOT = "0"
      *                This is OK (e.g., "01" becomes 1)
                       CONTINUE
                    END-IF
                    IF WS-DIGIT-COUNT = 2 AND WS-OCTET-VALUE = 0
                       GOBACK
                    END-IF
                    COMPUTE WS-OCTET-VALUE =
                       WS-OCTET-VALUE * 10 +
                       FUNCTION ORD(WS-CURRENT-CHAR) - 49
      *             Check range
                    IF WS-OCTET-VALUE > 255
                       GOBACK
                    END-IF

                 WHEN WS-CURRENT-CHAR = "."
      *             Store current octet and move to next
                    IF WS-DIGIT-COUNT = 0
                       GOBACK
                    END-IF
                    IF WS-OCTET-COUNT > 3
                       GOBACK
                    END-IF
                    MOVE WS-OCTET-VALUE TO WS-OCTET(WS-OCTET-COUNT)
                    ADD 1 TO WS-OCTET-COUNT
                    MOVE 0 TO WS-OCTET-VALUE
                    MOVE 0 TO WS-DIGIT-COUNT

                 WHEN OTHER
      *             Invalid character
                    GOBACK
              END-EVALUATE
           END-PERFORM

      *    Store last octet
           IF WS-DIGIT-COUNT = 0
              GOBACK
           END-IF
           IF WS-OCTET-COUNT NOT = 4
              GOBACK
           END-IF
           MOVE WS-OCTET-VALUE TO WS-OCTET(4)

      *    Copy to output
           MOVE WS-OCTET(1) TO LS-OCTET-1
           MOVE WS-OCTET(2) TO LS-OCTET-2
           MOVE WS-OCTET(3) TO LS-OCTET-3
           MOVE WS-OCTET(4) TO LS-OCTET-4

           MOVE 1 TO LS-RESULT
           GOBACK.

       PARSE-IPV4-EXIT.
           EXIT.

      *================================================================
      * IS-LOOPBACK: Check if IP is loopback (127.x.x.x)
      * Input:  LS-OCTET-1
      * Output: LS-RESULT (1=loopback, 0=not loopback)
      *================================================================
       IS-LOOPBACK SECTION.
           ENTRY "IS-LOOPBACK" USING LS-OCTET-1 LS-RESULT.

           IF LS-OCTET-1 = 127
              MOVE 1 TO LS-RESULT
           ELSE
              MOVE 0 TO LS-RESULT
           END-IF

           GOBACK.

       IS-LOOPBACK-EXIT.
           EXIT.

      *================================================================
      * IS-PRIVATE-IP: Check if IP is private (RFC 1918)
      * Input:  LS-OCTET-1, LS-OCTET-2
      * Output: LS-RESULT (1=private, 0=not private)
      *================================================================
       IS-PRIVATE-IP SECTION.
           ENTRY "IS-PRIVATE-IP" USING LS-OCTET-1 LS-OCTET-2 LS-RESULT.

           MOVE 0 TO LS-RESULT

      *    10.0.0.0/8
           IF LS-OCTET-1 = 10
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    172.16.0.0/12 (172.16-31.x.x)
           IF LS-OCTET-1 = 172
              AND LS-OCTET-2 >= 16 AND LS-OCTET-2 <= 31
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    192.168.0.0/16
           IF LS-OCTET-1 = 192 AND LS-OCTET-2 = 168
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

           GOBACK.

       IS-PRIVATE-IP-EXIT.
           EXIT.

      *================================================================
      * IS-RESERVED-IP: Check if IP is reserved
      * Input:  LS-OCTET-1, LS-OCTET-2, LS-OCTET-3
      * Output: LS-RESULT (1=reserved, 0=not reserved)
      *================================================================
       IS-RESERVED-IP SECTION.
           ENTRY "IS-RESERVED-IP" USING LS-OCTET-1 LS-OCTET-2
                                        LS-OCTET-3 LS-RESULT.

           MOVE 0 TO LS-RESULT

      *    0.0.0.0/8
           IF LS-OCTET-1 = 0
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    100.64.0.0/10 (CGNAT)
           IF LS-OCTET-1 = 100
              AND LS-OCTET-2 >= 64 AND LS-OCTET-2 <= 127
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    169.254.0.0/16 (link-local)
           IF LS-OCTET-1 = 169 AND LS-OCTET-2 = 254
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    224-239.x.x.x (multicast)
           IF LS-OCTET-1 >= 224 AND LS-OCTET-1 <= 239
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

      *    240-255.x.x.x (reserved/broadcast)
           IF LS-OCTET-1 >= 240
              MOVE 1 TO LS-RESULT
              GOBACK
           END-IF

           GOBACK.

       IS-RESERVED-IP-EXIT.
           EXIT.

      *================================================================
      * CLASSIFY-IP: Classify IP address
      * Input:  LS-OCTET-1, LS-OCTET-2, LS-OCTET-3
      * Output: LS-CLASSIFICATION (0-4)
      *================================================================
       CLASSIFY-IP SECTION.
           ENTRY "CLASSIFY-IP" USING LS-OCTET-1 LS-OCTET-2
                                     LS-OCTET-3 LS-CLASSIFICATION.

           MOVE WS-IP-CLASS-PUBLIC TO LS-CLASSIFICATION

      *    Check loopback
           IF LS-OCTET-1 = 127
              MOVE WS-IP-CLASS-LOOPBACK TO LS-CLASSIFICATION
              GOBACK
           END-IF

      *    Check private
           IF LS-OCTET-1 = 10
              MOVE WS-IP-CLASS-PRIVATE TO LS-CLASSIFICATION
              GOBACK
           END-IF
           IF LS-OCTET-1 = 172
              AND LS-OCTET-2 >= 16 AND LS-OCTET-2 <= 31
              MOVE WS-IP-CLASS-PRIVATE TO LS-CLASSIFICATION
              GOBACK
           END-IF
           IF LS-OCTET-1 = 192 AND LS-OCTET-2 = 168
              MOVE WS-IP-CLASS-PRIVATE TO LS-CLASSIFICATION
              GOBACK
           END-IF

      *    Check reserved
           IF LS-OCTET-1 = 0
              OR (LS-OCTET-1 = 100
                  AND LS-OCTET-2 >= 64 AND LS-OCTET-2 <= 127)
              OR (LS-OCTET-1 = 169 AND LS-OCTET-2 = 254)
              OR (LS-OCTET-1 >= 224)
              MOVE WS-IP-CLASS-RESERVED TO LS-CLASSIFICATION
              GOBACK
           END-IF

           GOBACK.

       CLASSIFY-IP-EXIT.
           EXIT.

      *================================================================
      * IS-VALID-PORT: Check if port is in valid range
      * Input:  LS-PORT
      * Output: LS-RESULT (1=valid, 0=invalid)
      *================================================================
       IS-VALID-PORT SECTION.
           ENTRY "IS-VALID-PORT" USING LS-PORT LS-RESULT.

           IF LS-PORT >= WS-PORT-MIN AND LS-PORT <= WS-PORT-MAX
              MOVE 1 TO LS-RESULT
           ELSE
              MOVE 0 TO LS-RESULT
           END-IF

           GOBACK.

       IS-VALID-PORT-EXIT.
           EXIT.

      *================================================================
      * IS-PRIVILEGED-PORT: Check if port is privileged (<1024)
      * Input:  LS-PORT
      * Output: LS-RESULT (1=privileged, 0=not privileged)
      *================================================================
       IS-PRIVILEGED-PORT SECTION.
           ENTRY "IS-PRIVILEGED-PORT" USING LS-PORT LS-RESULT.

           IF LS-PORT >= WS-PORT-MIN AND LS-PORT <= WS-PRIVILEGED-MAX
              MOVE 1 TO LS-RESULT
           ELSE
              MOVE 0 TO LS-RESULT
           END-IF

           GOBACK.

       IS-PRIVILEGED-PORT-EXIT.
           EXIT.

       END PROGRAM SAFE-NETWORK.
