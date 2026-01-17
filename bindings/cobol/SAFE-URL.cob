      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *>
      *> Proven SafeURL - URL parsing and validation for COBOL
      *>

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-URL.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(6).
       01 WS-OUTPUT-POS               PIC 9(6).
       01 WS-INPUT-LEN                PIC 9(6).
       01 WS-CURRENT-CHAR             PIC X(1).
       01 WS-TEMP-VALUE               PIC 9(4).
       01 WS-FOUND                    PIC 9 VALUE 0.

      * URL component positions
       01 WS-URL-POSITIONS.
          05 WS-SCHEME-END            PIC 9(6) VALUE 0.
          05 WS-HOST-START            PIC 9(6) VALUE 0.
          05 WS-HOST-END              PIC 9(6) VALUE 0.
          05 WS-PORT-START            PIC 9(6) VALUE 0.
          05 WS-PORT-END              PIC 9(6) VALUE 0.
          05 WS-PATH-START            PIC 9(6) VALUE 0.
          05 WS-PATH-END              PIC 9(6) VALUE 0.
          05 WS-QUERY-START           PIC 9(6) VALUE 0.
          05 WS-QUERY-END             PIC 9(6) VALUE 0.
          05 WS-FRAGMENT-START        PIC 9(6) VALUE 0.

      * Valid scheme characters
       01 WS-SCHEME-CHARS             PIC X(38)
          VALUE "abcdefghijklmnopqrstuvwxyz0123456789+-".

      * Percent-encoding work areas
       01 WS-HEX-PAIR                 PIC X(2).
       01 WS-DECODED-BYTE             PIC 9(3).
       01 WS-HIGH-NIBBLE              PIC 9(2).
       01 WS-LOW-NIBBLE               PIC 9(2).
       01 WS-HEX-CHARS                PIC X(16)
          VALUE "0123456789ABCDEF".

      * Unreserved characters (RFC 3986)
       01 WS-UNRESERVED               PIC X(66) VALUE
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~".

       LINKAGE SECTION.
       01 LS-URL-STRING               PIC X(2048).
       01 LS-URL-LENGTH               PIC 9(6).
       01 LS-SCHEME                   PIC X(20).
       01 LS-HOST                     PIC X(255).
       01 LS-PORT                     PIC 9(5).
       01 LS-PATH                     PIC X(1024).
       01 LS-QUERY                    PIC X(1024).
       01 LS-FRAGMENT                 PIC X(256).
       01 LS-RESULT                   PIC 9.
       01 LS-ERROR-MSG                PIC X(100).
       01 LS-OUTPUT-STRING            PIC X(4096).
       01 LS-OUTPUT-LENGTH            PIC 9(6).
       01 LS-INPUT-STRING             PIC X(2048).
       01 LS-INPUT-LENGTH             PIC 9(6).

       PROCEDURE DIVISION.

      *================================================================
      * PARSE-URL: Parse URL into components
      * Input:  LS-URL-STRING, LS-URL-LENGTH
      * Output: LS-SCHEME, LS-HOST, LS-PORT, LS-PATH, LS-QUERY,
      *         LS-FRAGMENT, LS-RESULT, LS-ERROR-MSG
      *================================================================
       PARSE-URL SECTION.
           ENTRY "PARSE-URL" USING LS-URL-STRING LS-URL-LENGTH
                 LS-SCHEME LS-HOST LS-PORT LS-PATH LS-QUERY
                 LS-FRAGMENT LS-RESULT LS-ERROR-MSG.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-SCHEME
           INITIALIZE LS-HOST
           MOVE 0 TO LS-PORT
           INITIALIZE LS-PATH
           INITIALIZE LS-QUERY
           INITIALIZE LS-FRAGMENT
           INITIALIZE LS-ERROR-MSG
           INITIALIZE WS-URL-POSITIONS
           MOVE LS-URL-LENGTH TO WS-INPUT-LEN

      *    Check minimum length
           IF WS-INPUT-LEN < 1
              MOVE "URL is empty" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Find scheme (ends with ://)
           MOVE 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
              OR WS-SCHEME-END > 0
              IF LS-URL-STRING(WS-INDEX:3) = "://"
                 MOVE WS-INDEX TO WS-SCHEME-END
                 COMPUTE WS-HOST-START = WS-INDEX + 3
              END-IF
              ADD 1 TO WS-INDEX
           END-PERFORM

           IF WS-SCHEME-END = 0
              MOVE "Missing scheme (://)" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Extract scheme
           COMPUTE WS-TEMP-VALUE = WS-SCHEME-END - 1
           IF WS-TEMP-VALUE > 0 AND WS-TEMP-VALUE <= 20
              MOVE LS-URL-STRING(1:WS-TEMP-VALUE) TO LS-SCHEME
              INSPECT LS-SCHEME
                 CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 TO "abcdefghijklmnopqrstuvwxyz"
           END-IF

      *    Find host end (first /, ?, # or end)
           MOVE WS-HOST-START TO WS-INDEX
           MOVE WS-INPUT-LEN TO WS-HOST-END
           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
              MOVE LS-URL-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
              IF WS-CURRENT-CHAR = "/" OR WS-CURRENT-CHAR = "?"
                 OR WS-CURRENT-CHAR = "#"
                 COMPUTE WS-HOST-END = WS-INDEX - 1
                 EXIT PERFORM
              END-IF
              ADD 1 TO WS-INDEX
           END-PERFORM

      *    Check for port in host (host:port)
           MOVE WS-HOST-START TO WS-INDEX
           MOVE 0 TO WS-PORT-START
           PERFORM UNTIL WS-INDEX > WS-HOST-END
              IF LS-URL-STRING(WS-INDEX:1) = ":"
                 COMPUTE WS-PORT-START = WS-INDEX + 1
                 COMPUTE WS-HOST-END = WS-INDEX - 1
                 EXIT PERFORM
              END-IF
              ADD 1 TO WS-INDEX
           END-PERFORM

      *    Extract host
           COMPUTE WS-TEMP-VALUE = WS-HOST-END - WS-HOST-START + 1
           IF WS-TEMP-VALUE > 0 AND WS-TEMP-VALUE <= 255
              MOVE LS-URL-STRING(WS-HOST-START:WS-TEMP-VALUE)
                 TO LS-HOST
              INSPECT LS-HOST
                 CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 TO "abcdefghijklmnopqrstuvwxyz"
           END-IF

      *    Extract port if present
           IF WS-PORT-START > 0
              MOVE 0 TO LS-PORT
              MOVE WS-PORT-START TO WS-INDEX
              PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
                 MOVE LS-URL-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
                 IF WS-CURRENT-CHAR < "0" OR WS-CURRENT-CHAR > "9"
                    EXIT PERFORM
                 END-IF
                 COMPUTE LS-PORT = LS-PORT * 10 +
                    FUNCTION ORD(WS-CURRENT-CHAR) - 49
                 ADD 1 TO WS-INDEX
              END-PERFORM
              COMPUTE WS-PATH-START = WS-INDEX
           ELSE
              COMPUTE WS-PATH-START = WS-HOST-END + 1
           END-IF

      *    Find and extract path
           IF WS-PATH-START <= WS-INPUT-LEN
              IF LS-URL-STRING(WS-PATH-START:1) = "/"
                 MOVE WS-PATH-START TO WS-INDEX
                 MOVE WS-INPUT-LEN TO WS-PATH-END
                 PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
                    IF LS-URL-STRING(WS-INDEX:1) = "?"
                       OR LS-URL-STRING(WS-INDEX:1) = "#"
                       COMPUTE WS-PATH-END = WS-INDEX - 1
                       EXIT PERFORM
                    END-IF
                    ADD 1 TO WS-INDEX
                 END-PERFORM
                 COMPUTE WS-TEMP-VALUE =
                    WS-PATH-END - WS-PATH-START + 1
                 IF WS-TEMP-VALUE > 0 AND WS-TEMP-VALUE <= 1024
                    MOVE LS-URL-STRING(WS-PATH-START:WS-TEMP-VALUE)
                       TO LS-PATH
                 END-IF
              END-IF
           END-IF

      *    Find and extract query
           MOVE 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
              IF LS-URL-STRING(WS-INDEX:1) = "?"
                 COMPUTE WS-QUERY-START = WS-INDEX + 1
                 MOVE WS-INPUT-LEN TO WS-QUERY-END
                 PERFORM VARYING WS-INDEX FROM WS-QUERY-START BY 1
                         UNTIL WS-INDEX > WS-INPUT-LEN
                    IF LS-URL-STRING(WS-INDEX:1) = "#"
                       COMPUTE WS-QUERY-END = WS-INDEX - 1
                       EXIT PERFORM
                    END-IF
                 END-PERFORM
                 COMPUTE WS-TEMP-VALUE =
                    WS-QUERY-END - WS-QUERY-START + 1
                 IF WS-TEMP-VALUE > 0 AND WS-TEMP-VALUE <= 1024
                    MOVE LS-URL-STRING(WS-QUERY-START:WS-TEMP-VALUE)
                       TO LS-QUERY
                 END-IF
                 EXIT PERFORM
              END-IF
              ADD 1 TO WS-INDEX
           END-PERFORM

      *    Find and extract fragment
           MOVE 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
              IF LS-URL-STRING(WS-INDEX:1) = "#"
                 COMPUTE WS-FRAGMENT-START = WS-INDEX + 1
                 COMPUTE WS-TEMP-VALUE =
                    WS-INPUT-LEN - WS-FRAGMENT-START + 1
                 IF WS-TEMP-VALUE > 0 AND WS-TEMP-VALUE <= 256
                    MOVE LS-URL-STRING(WS-FRAGMENT-START:WS-TEMP-VALUE)
                       TO LS-FRAGMENT
                 END-IF
                 EXIT PERFORM
              END-IF
              ADD 1 TO WS-INDEX
           END-PERFORM

           MOVE 1 TO LS-RESULT
           GOBACK.

       PARSE-URL-EXIT.
           EXIT.

      *================================================================
      * IS-VALID-URL: Check if URL is syntactically valid
      * Input:  LS-URL-STRING, LS-URL-LENGTH
      * Output: LS-RESULT (1=valid, 0=invalid)
      *================================================================
       IS-VALID-URL SECTION.
           ENTRY "IS-VALID-URL" USING LS-URL-STRING LS-URL-LENGTH
                 LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE LS-URL-LENGTH TO WS-INPUT-LEN

      *    Check minimum length
           IF WS-INPUT-LEN < 8
              GOBACK
           END-IF

      *    Must have scheme
           MOVE 0 TO WS-FOUND
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN OR WS-INDEX > 20
              IF LS-URL-STRING(WS-INDEX:3) = "://"
                 MOVE 1 TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM

           IF WS-FOUND = 0
              GOBACK
           END-IF

      *    Must have host after scheme
           COMPUTE WS-HOST-START = WS-INDEX + 3
           IF WS-HOST-START >= WS-INPUT-LEN
              GOBACK
           END-IF

      *    First char after :// must be valid host char
           MOVE LS-URL-STRING(WS-HOST-START:1) TO WS-CURRENT-CHAR
           IF NOT ((WS-CURRENT-CHAR >= "a" AND WS-CURRENT-CHAR <= "z")
              OR (WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "Z")
              OR (WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9")
              OR WS-CURRENT-CHAR = "[")
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       IS-VALID-URL-EXIT.
           EXIT.

      *================================================================
      * URL-ENCODE: Percent-encode a string for URL use
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       URL-ENCODE SECTION.
           ENTRY "URL-ENCODE" USING LS-INPUT-STRING LS-INPUT-LENGTH
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
              COMPUTE WS-DECODED-BYTE =
                 FUNCTION ORD(WS-CURRENT-CHAR) - 1

      *       Check if unreserved character
              MOVE 0 TO WS-FOUND
              INSPECT WS-UNRESERVED
                 TALLYING WS-FOUND FOR ALL WS-CURRENT-CHAR

              IF WS-FOUND > 0
      *          Unreserved - copy as-is
                 MOVE WS-CURRENT-CHAR TO
                    LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              ELSE
      *          Reserved or special - percent-encode
                 MOVE "%" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS

      *          High nibble
                 COMPUTE WS-HIGH-NIBBLE = WS-DECODED-BYTE / 16 + 1
                 MOVE WS-HEX-CHARS(WS-HIGH-NIBBLE:1) TO
                    LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS

      *          Low nibble
                 COMPUTE WS-LOW-NIBBLE =
                    FUNCTION MOD(WS-DECODED-BYTE 16) + 1
                 MOVE WS-HEX-CHARS(WS-LOW-NIBBLE:1) TO
                    LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       URL-ENCODE-EXIT.
           EXIT.

      *================================================================
      * URL-DECODE: Decode percent-encoded string
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       URL-DECODE SECTION.
           ENTRY "URL-DECODE" USING LS-INPUT-STRING LS-INPUT-LENGTH
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE 1 TO WS-INDEX
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              IF WS-CURRENT-CHAR = "%"
      *          Check we have 2 more chars
                 IF WS-INDEX + 2 > WS-INPUT-LEN
                    MOVE 0 TO LS-RESULT
                    GOBACK
                 END-IF

      *          Get hex pair
                 MOVE LS-INPUT-STRING(WS-INDEX + 1:2) TO WS-HEX-PAIR

      *          Convert high nibble
                 PERFORM CONVERT-HEX-NIBBLE-HIGH
                 IF WS-FOUND = 0
                    MOVE 0 TO LS-RESULT
                    GOBACK
                 END-IF
                 MOVE WS-TEMP-VALUE TO WS-HIGH-NIBBLE

      *          Convert low nibble
                 PERFORM CONVERT-HEX-NIBBLE-LOW
                 IF WS-FOUND = 0
                    MOVE 0 TO LS-RESULT
                    GOBACK
                 END-IF
                 MOVE WS-TEMP-VALUE TO WS-LOW-NIBBLE

      *          Combine nibbles
                 COMPUTE WS-DECODED-BYTE =
                    WS-HIGH-NIBBLE * 16 + WS-LOW-NIBBLE + 1
                 MOVE FUNCTION CHAR(WS-DECODED-BYTE)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 ADD 3 TO WS-INDEX

              ELSE IF WS-CURRENT-CHAR = "+"
      *          Plus is sometimes used for space
                 MOVE " " TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 ADD 1 TO WS-INDEX

              ELSE
      *          Regular character
                 MOVE WS-CURRENT-CHAR TO
                    LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 ADD 1 TO WS-INDEX
              END-IF
              END-IF
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       URL-DECODE-EXIT.
           EXIT.

      *================================================================
      * CONVERT-HEX-NIBBLE-HIGH: Convert first hex char of pair
      *================================================================
       CONVERT-HEX-NIBBLE-HIGH SECTION.
           MOVE WS-HEX-PAIR(1:1) TO WS-CURRENT-CHAR
           MOVE 1 TO WS-FOUND

           EVALUATE TRUE
              WHEN WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                 COMPUTE WS-TEMP-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 49
              WHEN WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "F"
                 COMPUTE WS-TEMP-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 56
              WHEN WS-CURRENT-CHAR >= "a" AND WS-CURRENT-CHAR <= "f"
                 COMPUTE WS-TEMP-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 88
              WHEN OTHER
                 MOVE 0 TO WS-FOUND
           END-EVALUATE.

       CONVERT-HEX-NIBBLE-HIGH-EXIT.
           EXIT.

      *================================================================
      * CONVERT-HEX-NIBBLE-LOW: Convert second hex char of pair
      *================================================================
       CONVERT-HEX-NIBBLE-LOW SECTION.
           MOVE WS-HEX-PAIR(2:1) TO WS-CURRENT-CHAR
           MOVE 1 TO WS-FOUND

           EVALUATE TRUE
              WHEN WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                 COMPUTE WS-TEMP-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 49
              WHEN WS-CURRENT-CHAR >= "A" AND WS-CURRENT-CHAR <= "F"
                 COMPUTE WS-TEMP-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 56
              WHEN WS-CURRENT-CHAR >= "a" AND WS-CURRENT-CHAR <= "f"
                 COMPUTE WS-TEMP-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 88
              WHEN OTHER
                 MOVE 0 TO WS-FOUND
           END-EVALUATE.

       CONVERT-HEX-NIBBLE-LOW-EXIT.
           EXIT.

      *================================================================
      * NORMALIZE-URL: Normalize URL to canonical form
      * Input:  LS-URL-STRING, LS-URL-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       NORMALIZE-URL SECTION.
           ENTRY "NORMALIZE-URL" USING LS-URL-STRING LS-URL-LENGTH
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 1 TO LS-RESULT
           MOVE LS-URL-STRING TO LS-OUTPUT-STRING
           MOVE LS-URL-LENGTH TO LS-OUTPUT-LENGTH

      *    Lowercase the scheme and host
           INSPECT LS-OUTPUT-STRING(1:LS-OUTPUT-LENGTH)
              CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              TO "abcdefghijklmnopqrstuvwxyz"

           GOBACK.

       NORMALIZE-URL-EXIT.
           EXIT.

      *================================================================
      * GET-URL-ORIGIN: Extract origin (scheme://host:port)
      * Input:  LS-URL-STRING, LS-URL-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       GET-URL-ORIGIN SECTION.
           ENTRY "GET-URL-ORIGIN" USING LS-URL-STRING LS-URL-LENGTH
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-URL-LENGTH TO WS-INPUT-LEN

      *    Find scheme end
           MOVE 0 TO WS-SCHEME-END
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN OR WS-SCHEME-END > 0
              IF LS-URL-STRING(WS-INDEX:3) = "://"
                 MOVE WS-INDEX TO WS-SCHEME-END
              END-IF
           END-PERFORM

           IF WS-SCHEME-END = 0
              GOBACK
           END-IF

      *    Find host end (first / after ://)
           COMPUTE WS-HOST-START = WS-SCHEME-END + 3
           MOVE WS-INPUT-LEN TO WS-HOST-END

           PERFORM VARYING WS-INDEX FROM WS-HOST-START BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN
              IF LS-URL-STRING(WS-INDEX:1) = "/"
                 COMPUTE WS-HOST-END = WS-INDEX - 1
                 EXIT PERFORM
              END-IF
           END-PERFORM

      *    Copy scheme://host:port
           COMPUTE WS-TEMP-VALUE = WS-HOST-END
           IF WS-TEMP-VALUE > 0 AND WS-TEMP-VALUE <= 2048
              MOVE LS-URL-STRING(1:WS-TEMP-VALUE)
                 TO LS-OUTPUT-STRING
              MOVE WS-TEMP-VALUE TO LS-OUTPUT-LENGTH
              MOVE 1 TO LS-RESULT
           END-IF

           GOBACK.

       GET-URL-ORIGIN-EXIT.
           EXIT.

       END PROGRAM SAFE-URL.
