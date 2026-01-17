      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeHex - Hexadecimal encoding/decoding for COBOL
      *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-HEX.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(6).
       01 WS-OUTPUT-POS               PIC 9(6).
       01 WS-INPUT-LEN                PIC 9(6).
       01 WS-CURRENT-CHAR             PIC X(1).
       01 WS-BYTE-VALUE               PIC 9(3).
       01 WS-HIGH-NIBBLE              PIC 9(2).
       01 WS-LOW-NIBBLE               PIC 9(2).
       01 WS-HEX-PAIR                 PIC X(2).
       01 WS-TEMP-VALUE               PIC 9(3).

      * Hex character lookup tables
       01 WS-HEX-CHARS-UPPER          PIC X(16)
          VALUE "0123456789ABCDEF".
       01 WS-HEX-CHARS-LOWER          PIC X(16)
          VALUE "0123456789abcdef".

      * Hex digit values (0-15 for 0-9, A-F)
       01 WS-HEX-VALUE                PIC 9(2).
       01 WS-IS-VALID-HEX             PIC 9 VALUE 0.

      * Group separator tracking
       01 WS-GROUP-COUNT              PIC 9(4) VALUE 0.

       LINKAGE SECTION.
       01 LS-INPUT-BYTES              PIC X(1024).
       01 LS-INPUT-LENGTH             PIC 9(6).
       01 LS-OUTPUT-STRING            PIC X(2048).
       01 LS-OUTPUT-LENGTH            PIC 9(6).
       01 LS-RESULT                   PIC 9.
       01 LS-ERROR-MSG                PIC X(50).
       01 LS-USE-UPPERCASE            PIC 9.
       01 LS-ADD-PREFIX               PIC 9.
       01 LS-BYTE-SEPARATOR           PIC X.
       01 LS-HEX-STRING               PIC X(2048).
       01 LS-HEX-LENGTH               PIC 9(6).
       01 LS-OUTPUT-BYTES             PIC X(1024).

       PROCEDURE DIVISION.

      *================================================================
      * HEX-ENCODE: Encode bytes to hexadecimal string
      * Input:  LS-INPUT-BYTES, LS-INPUT-LENGTH
      *         LS-USE-UPPERCASE (1=upper, 0=lower)
      *         LS-ADD-PREFIX (1=add 0x prefix, 0=no prefix)
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       HEX-ENCODE SECTION.
           ENTRY "HEX-ENCODE" USING LS-INPUT-BYTES LS-INPUT-LENGTH
                 LS-USE-UPPERCASE LS-ADD-PREFIX
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

      *    Check input length
           IF WS-INPUT-LEN = 0
              MOVE 0 TO LS-OUTPUT-LENGTH
              GOBACK
           END-IF

      *    Check output buffer size (need 2 chars per byte + prefix)
           COMPUTE WS-TEMP-VALUE = WS-INPUT-LEN * 2
           IF LS-ADD-PREFIX = 1
              ADD 2 TO WS-TEMP-VALUE
           END-IF
           IF WS-TEMP-VALUE > 2048
              MOVE 0 TO LS-RESULT
              MOVE "Input too large for output buffer" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Add 0x prefix if requested
           IF LS-ADD-PREFIX = 1
              MOVE "0x" TO LS-OUTPUT-STRING(1:2)
              MOVE 3 TO WS-OUTPUT-POS
           END-IF

      *    Convert each byte to two hex characters
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

      *       Get byte value (0-255)
              COMPUTE WS-BYTE-VALUE =
                 FUNCTION ORD(LS-INPUT-BYTES(WS-INDEX:1)) - 1

      *       Split into high and low nibbles
              COMPUTE WS-HIGH-NIBBLE = WS-BYTE-VALUE / 16
              COMPUTE WS-LOW-NIBBLE =
                 FUNCTION MOD(WS-BYTE-VALUE 16)

      *       Add 1 for 1-indexed COBOL strings
              ADD 1 TO WS-HIGH-NIBBLE
              ADD 1 TO WS-LOW-NIBBLE

      *       Convert nibbles to hex characters
              IF LS-USE-UPPERCASE = 1
                 MOVE WS-HEX-CHARS-UPPER(WS-HIGH-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 MOVE WS-HEX-CHARS-UPPER(WS-LOW-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
              ELSE
                 MOVE WS-HEX-CHARS-LOWER(WS-HIGH-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 MOVE WS-HEX-CHARS-LOWER(WS-LOW-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
              END-IF
              ADD 1 TO WS-OUTPUT-POS
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       HEX-ENCODE-EXIT.
           EXIT.

      *================================================================
      * HEX-ENCODE-SEP: Encode with byte separator
      * Input:  LS-INPUT-BYTES, LS-INPUT-LENGTH
      *         LS-USE-UPPERCASE, LS-BYTE-SEPARATOR
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       HEX-ENCODE-SEP SECTION.
           ENTRY "HEX-ENCODE-SEP" USING LS-INPUT-BYTES LS-INPUT-LENGTH
                 LS-USE-UPPERCASE LS-BYTE-SEPARATOR
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

      *    Check input length
           IF WS-INPUT-LEN = 0
              MOVE 0 TO LS-OUTPUT-LENGTH
              GOBACK
           END-IF

      *    Convert each byte to two hex characters with separator
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

      *       Add separator before all but first byte
              IF WS-INDEX > 1 AND LS-BYTE-SEPARATOR NOT = SPACE
                 MOVE LS-BYTE-SEPARATOR TO
                    LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF

      *       Get byte value (0-255)
              COMPUTE WS-BYTE-VALUE =
                 FUNCTION ORD(LS-INPUT-BYTES(WS-INDEX:1)) - 1

      *       Split into high and low nibbles
              COMPUTE WS-HIGH-NIBBLE = WS-BYTE-VALUE / 16
              COMPUTE WS-LOW-NIBBLE =
                 FUNCTION MOD(WS-BYTE-VALUE 16)

      *       Add 1 for 1-indexed COBOL strings
              ADD 1 TO WS-HIGH-NIBBLE
              ADD 1 TO WS-LOW-NIBBLE

      *       Convert nibbles to hex characters
              IF LS-USE-UPPERCASE = 1
                 MOVE WS-HEX-CHARS-UPPER(WS-HIGH-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 MOVE WS-HEX-CHARS-UPPER(WS-LOW-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
              ELSE
                 MOVE WS-HEX-CHARS-LOWER(WS-HIGH-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
                 MOVE WS-HEX-CHARS-LOWER(WS-LOW-NIBBLE:1)
                    TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
              END-IF
              ADD 1 TO WS-OUTPUT-POS
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       HEX-ENCODE-SEP-EXIT.
           EXIT.

      *================================================================
      * HEX-DECODE: Decode hexadecimal string to bytes
      * Input:  LS-HEX-STRING, LS-HEX-LENGTH
      * Output: LS-OUTPUT-BYTES, LS-OUTPUT-LENGTH, LS-RESULT,
      *         LS-ERROR-MSG
      *================================================================
       HEX-DECODE SECTION.
           ENTRY "HEX-DECODE" USING LS-HEX-STRING LS-HEX-LENGTH
                 LS-OUTPUT-BYTES LS-OUTPUT-LENGTH
                 LS-RESULT LS-ERROR-MSG.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-BYTES
           INITIALIZE LS-ERROR-MSG
           MOVE 0 TO LS-OUTPUT-LENGTH
           MOVE 1 TO WS-OUTPUT-POS
           MOVE 1 TO WS-INDEX
           MOVE LS-HEX-LENGTH TO WS-INPUT-LEN

      *    Skip optional "0x" or "0X" prefix
           IF WS-INPUT-LEN >= 2
              IF LS-HEX-STRING(1:2) = "0x"
                 OR LS-HEX-STRING(1:2) = "0X"
                 MOVE 3 TO WS-INDEX
                 SUBTRACT 2 FROM WS-INPUT-LEN
              END-IF
           END-IF

      *    Skip optional "#" prefix (for colors)
           IF LS-HEX-STRING(WS-INDEX:1) = "#"
              ADD 1 TO WS-INDEX
              SUBTRACT 1 FROM WS-INPUT-LEN
           END-IF

      *    Process hex string in pairs
           PERFORM UNTIL WS-INDEX > LS-HEX-LENGTH

      *       Skip separators (space, colon, dash)
              MOVE LS-HEX-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
              IF WS-CURRENT-CHAR = " "
                 OR WS-CURRENT-CHAR = ":"
                 OR WS-CURRENT-CHAR = "-"
                 ADD 1 TO WS-INDEX
                 CONTINUE
              END-IF

      *       Need at least 2 characters for a byte
              IF WS-INDEX + 1 > LS-HEX-LENGTH
                 MOVE 0 TO LS-RESULT
                 MOVE "Incomplete hex pair at end" TO LS-ERROR-MSG
                 GOBACK
              END-IF

      *       Get hex pair
              MOVE LS-HEX-STRING(WS-INDEX:2) TO WS-HEX-PAIR

      *       Validate and convert high nibble
              MOVE WS-HEX-PAIR(1:1) TO WS-CURRENT-CHAR
              PERFORM GET-HEX-VALUE
              IF WS-IS-VALID-HEX = 0
                 MOVE 0 TO LS-RESULT
                 STRING "Invalid hex character: " WS-CURRENT-CHAR
                    DELIMITED BY SIZE INTO LS-ERROR-MSG
                 GOBACK
              END-IF
              MOVE WS-HEX-VALUE TO WS-HIGH-NIBBLE

      *       Validate and convert low nibble
              MOVE WS-HEX-PAIR(2:1) TO WS-CURRENT-CHAR
              PERFORM GET-HEX-VALUE
              IF WS-IS-VALID-HEX = 0
                 MOVE 0 TO LS-RESULT
                 STRING "Invalid hex character: " WS-CURRENT-CHAR
                    DELIMITED BY SIZE INTO LS-ERROR-MSG
                 GOBACK
              END-IF
              MOVE WS-HEX-VALUE TO WS-LOW-NIBBLE

      *       Combine nibbles into byte value
              COMPUTE WS-BYTE-VALUE =
                 WS-HIGH-NIBBLE * 16 + WS-LOW-NIBBLE

      *       Convert to character and store
              MOVE FUNCTION CHAR(WS-BYTE-VALUE + 1)
                 TO LS-OUTPUT-BYTES(WS-OUTPUT-POS:1)
              ADD 1 TO WS-OUTPUT-POS
              ADD 2 TO WS-INDEX
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       HEX-DECODE-EXIT.
           EXIT.

      *================================================================
      * GET-HEX-VALUE: Convert hex character to value 0-15
      * Input:  WS-CURRENT-CHAR
      * Output: WS-HEX-VALUE, WS-IS-VALID-HEX
      *================================================================
       GET-HEX-VALUE SECTION.
           MOVE 0 TO WS-IS-VALID-HEX
           MOVE 0 TO WS-HEX-VALUE

           EVALUATE TRUE
              WHEN WS-CURRENT-CHAR >= "0"
                   AND WS-CURRENT-CHAR <= "9"
                 COMPUTE WS-HEX-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 49
                 MOVE 1 TO WS-IS-VALID-HEX

              WHEN WS-CURRENT-CHAR >= "A"
                   AND WS-CURRENT-CHAR <= "F"
                 COMPUTE WS-HEX-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 56
                 MOVE 1 TO WS-IS-VALID-HEX

              WHEN WS-CURRENT-CHAR >= "a"
                   AND WS-CURRENT-CHAR <= "f"
                 COMPUTE WS-HEX-VALUE =
                    FUNCTION ORD(WS-CURRENT-CHAR) - 88
                 MOVE 1 TO WS-IS-VALID-HEX

              WHEN OTHER
                 MOVE 0 TO WS-IS-VALID-HEX
           END-EVALUATE.

       GET-HEX-VALUE-EXIT.
           EXIT.

      *================================================================
      * IS-VALID-HEX-STRING: Check if string contains only valid hex
      * Input:  LS-HEX-STRING, LS-HEX-LENGTH
      * Output: LS-RESULT (1=valid, 0=invalid)
      *================================================================
       IS-VALID-HEX-STRING SECTION.
           ENTRY "IS-VALID-HEX-STRING" USING LS-HEX-STRING LS-HEX-LENGTH
                 LS-RESULT.

           MOVE 1 TO LS-RESULT
           MOVE 1 TO WS-INDEX
           MOVE LS-HEX-LENGTH TO WS-INPUT-LEN

      *    Skip optional prefix
           IF WS-INPUT-LEN >= 2
              IF LS-HEX-STRING(1:2) = "0x"
                 OR LS-HEX-STRING(1:2) = "0X"
                 MOVE 3 TO WS-INDEX
              END-IF
           END-IF

           IF LS-HEX-STRING(WS-INDEX:1) = "#"
              ADD 1 TO WS-INDEX
           END-IF

      *    Check each character
           PERFORM UNTIL WS-INDEX > LS-HEX-LENGTH

              MOVE LS-HEX-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

      *       Allow separators
              IF WS-CURRENT-CHAR = " "
                 OR WS-CURRENT-CHAR = ":"
                 OR WS-CURRENT-CHAR = "-"
                 ADD 1 TO WS-INDEX
                 CONTINUE
              END-IF

              PERFORM GET-HEX-VALUE
              IF WS-IS-VALID-HEX = 0
                 MOVE 0 TO LS-RESULT
                 GOBACK
              END-IF

              ADD 1 TO WS-INDEX
           END-PERFORM

           GOBACK.

       IS-VALID-HEX-STRING-EXIT.
           EXIT.

      *================================================================
      * BYTE-TO-HEX: Convert single byte to hex string
      * Input:  LS-INPUT-BYTES(1:1), LS-USE-UPPERCASE
      * Output: LS-OUTPUT-STRING(1:2)
      *================================================================
       BYTE-TO-HEX SECTION.
           ENTRY "BYTE-TO-HEX" USING LS-INPUT-BYTES LS-USE-UPPERCASE
                 LS-OUTPUT-STRING.

      *    Get byte value
           COMPUTE WS-BYTE-VALUE =
              FUNCTION ORD(LS-INPUT-BYTES(1:1)) - 1

      *    Split into nibbles
           COMPUTE WS-HIGH-NIBBLE = WS-BYTE-VALUE / 16
           COMPUTE WS-LOW-NIBBLE = FUNCTION MOD(WS-BYTE-VALUE 16)

      *    Add 1 for 1-indexed strings
           ADD 1 TO WS-HIGH-NIBBLE
           ADD 1 TO WS-LOW-NIBBLE

      *    Convert to hex
           IF LS-USE-UPPERCASE = 1
              MOVE WS-HEX-CHARS-UPPER(WS-HIGH-NIBBLE:1)
                 TO LS-OUTPUT-STRING(1:1)
              MOVE WS-HEX-CHARS-UPPER(WS-LOW-NIBBLE:1)
                 TO LS-OUTPUT-STRING(2:1)
           ELSE
              MOVE WS-HEX-CHARS-LOWER(WS-HIGH-NIBBLE:1)
                 TO LS-OUTPUT-STRING(1:1)
              MOVE WS-HEX-CHARS-LOWER(WS-LOW-NIBBLE:1)
                 TO LS-OUTPUT-STRING(2:1)
           END-IF

           GOBACK.

       BYTE-TO-HEX-EXIT.
           EXIT.

      *================================================================
      * HEX-TO-BYTE: Convert hex pair to single byte
      * Input:  LS-HEX-STRING(1:2)
      * Output: LS-OUTPUT-BYTES(1:1), LS-RESULT
      *================================================================
       HEX-TO-BYTE SECTION.
           ENTRY "HEX-TO-BYTE" USING LS-HEX-STRING
                 LS-OUTPUT-BYTES LS-RESULT.

           MOVE 1 TO LS-RESULT

      *    Validate and convert high nibble
           MOVE LS-HEX-STRING(1:1) TO WS-CURRENT-CHAR
           PERFORM GET-HEX-VALUE
           IF WS-IS-VALID-HEX = 0
              MOVE 0 TO LS-RESULT
              GOBACK
           END-IF
           MOVE WS-HEX-VALUE TO WS-HIGH-NIBBLE

      *    Validate and convert low nibble
           MOVE LS-HEX-STRING(2:1) TO WS-CURRENT-CHAR
           PERFORM GET-HEX-VALUE
           IF WS-IS-VALID-HEX = 0
              MOVE 0 TO LS-RESULT
              GOBACK
           END-IF
           MOVE WS-HEX-VALUE TO WS-LOW-NIBBLE

      *    Combine nibbles
           COMPUTE WS-BYTE-VALUE = WS-HIGH-NIBBLE * 16 + WS-LOW-NIBBLE
           MOVE FUNCTION CHAR(WS-BYTE-VALUE + 1)
              TO LS-OUTPUT-BYTES(1:1)

           GOBACK.

       HEX-TO-BYTE-EXIT.
           EXIT.

      *================================================================
      * NORMALIZE-HEX: Convert hex string to lowercase, strip prefix
      * Input:  LS-HEX-STRING, LS-HEX-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH
      *================================================================
       NORMALIZE-HEX SECTION.
           ENTRY "NORMALIZE-HEX" USING LS-HEX-STRING LS-HEX-LENGTH
                 LS-OUTPUT-STRING LS-OUTPUT-LENGTH.

           MOVE 1 TO WS-INDEX
           MOVE 1 TO WS-OUTPUT-POS
           INITIALIZE LS-OUTPUT-STRING

      *    Skip prefix
           IF LS-HEX-LENGTH >= 2
              IF LS-HEX-STRING(1:2) = "0x"
                 OR LS-HEX-STRING(1:2) = "0X"
                 MOVE 3 TO WS-INDEX
              END-IF
           END-IF

           IF WS-INDEX <= LS-HEX-LENGTH
              IF LS-HEX-STRING(WS-INDEX:1) = "#"
                 ADD 1 TO WS-INDEX
              END-IF
           END-IF

      *    Copy and lowercase, skip separators
           PERFORM UNTIL WS-INDEX > LS-HEX-LENGTH

              MOVE LS-HEX-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

      *       Skip separators
              IF WS-CURRENT-CHAR = " "
                 OR WS-CURRENT-CHAR = ":"
                 OR WS-CURRENT-CHAR = "-"
                 ADD 1 TO WS-INDEX
                 CONTINUE
              END-IF

      *       Convert to lowercase
              INSPECT WS-CURRENT-CHAR
                 CONVERTING "ABCDEF" TO "abcdef"

              MOVE WS-CURRENT-CHAR TO
                 LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
              ADD 1 TO WS-OUTPUT-POS
              ADD 1 TO WS-INDEX
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       NORMALIZE-HEX-EXIT.
           EXIT.

       END PROGRAM SAFE-HEX.
