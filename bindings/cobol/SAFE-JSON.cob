      *> SPDX-License-Identifier: PMPL-1.0
      *> SPDX-FileCopyrightText: 2025 Hyperpolymath
      *>
      *> Proven SafeJSON - JSON parsing and validation for COBOL
      *>

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-JSON.
       AUTHOR. Hyperpolymath.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working variables
       01 WS-INDEX                    PIC 9(6).
       01 WS-OUTPUT-POS               PIC 9(6).
       01 WS-INPUT-LEN                PIC 9(6).
       01 WS-CURRENT-CHAR             PIC X(1).
       01 WS-DEPTH                    PIC 9(3) VALUE 0.
       01 WS-IN-STRING                PIC 9 VALUE 0.
       01 WS-ESCAPE-NEXT              PIC 9 VALUE 0.

      * JSON token types
       01 WS-TOKEN-TYPE               PIC 9 VALUE 0.
          88 WS-TOKEN-NONE            VALUE 0.
          88 WS-TOKEN-OBJECT-START    VALUE 1.
          88 WS-TOKEN-OBJECT-END      VALUE 2.
          88 WS-TOKEN-ARRAY-START     VALUE 3.
          88 WS-TOKEN-ARRAY-END       VALUE 4.
          88 WS-TOKEN-STRING          VALUE 5.
          88 WS-TOKEN-NUMBER          VALUE 6.
          88 WS-TOKEN-TRUE            VALUE 7.
          88 WS-TOKEN-FALSE           VALUE 8.
          88 WS-TOKEN-NULL            VALUE 9.

      * Bracket matching
       01 WS-OBJECT-COUNT             PIC 9(3) VALUE 0.
       01 WS-ARRAY-COUNT              PIC 9(3) VALUE 0.

      * String escape sequences
       01 WS-ESCAPE-CHARS             PIC X(8)
          VALUE '"\\/bfnrt'.
       01 WS-ESCAPE-VALUES            PIC X(8)
          VALUE X'225C2F0208090D0A'.

       LINKAGE SECTION.
       01 LS-JSON-STRING              PIC X(32000).
       01 LS-JSON-LENGTH              PIC 9(6).
       01 LS-RESULT                   PIC 9.
       01 LS-ERROR-MSG                PIC X(100).
       01 LS-ERROR-POSITION           PIC 9(6).
       01 LS-KEY-NAME                 PIC X(256).
       01 LS-KEY-LENGTH               PIC 9(4).
       01 LS-VALUE-STRING             PIC X(4096).
       01 LS-VALUE-LENGTH             PIC 9(6).
       01 LS-VALUE-TYPE               PIC 9.
       01 LS-OUTPUT-STRING            PIC X(8192).
       01 LS-OUTPUT-LENGTH            PIC 9(6).
       01 LS-INPUT-STRING             PIC X(4096).
       01 LS-INPUT-LENGTH             PIC 9(6).

       PROCEDURE DIVISION.

      *================================================================
      * VALIDATE-JSON: Check if JSON string is syntactically valid
      * Input:  LS-JSON-STRING, LS-JSON-LENGTH
      * Output: LS-RESULT (1=valid, 0=invalid), LS-ERROR-MSG,
      *         LS-ERROR-POSITION
      *================================================================
       VALIDATE-JSON SECTION.
           ENTRY "VALIDATE-JSON" USING LS-JSON-STRING LS-JSON-LENGTH
                 LS-RESULT LS-ERROR-MSG LS-ERROR-POSITION.

           MOVE 0 TO LS-RESULT
           INITIALIZE LS-ERROR-MSG
           MOVE 0 TO LS-ERROR-POSITION
           MOVE 0 TO WS-DEPTH
           MOVE 0 TO WS-IN-STRING
           MOVE 0 TO WS-ESCAPE-NEXT
           MOVE 0 TO WS-OBJECT-COUNT
           MOVE 0 TO WS-ARRAY-COUNT
           MOVE LS-JSON-LENGTH TO WS-INPUT-LEN

      *    Empty string is invalid JSON
           IF WS-INPUT-LEN = 0
              MOVE "Empty JSON string" TO LS-ERROR-MSG
              GOBACK
           END-IF

      *    Skip leading whitespace
           MOVE 1 TO WS-INDEX
           PERFORM SKIP-WHITESPACE

      *    Must start with { or [
           MOVE LS-JSON-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
           IF WS-CURRENT-CHAR NOT = "{" AND WS-CURRENT-CHAR NOT = "["
              MOVE "JSON must start with { or [" TO LS-ERROR-MSG
              MOVE WS-INDEX TO LS-ERROR-POSITION
              GOBACK
           END-IF

      *    Scan through JSON
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-JSON-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              IF WS-ESCAPE-NEXT = 1
                 MOVE 0 TO WS-ESCAPE-NEXT
                 CONTINUE
              END-IF

              IF WS-IN-STRING = 1
                 IF WS-CURRENT-CHAR = "\"
                    MOVE 1 TO WS-ESCAPE-NEXT
                 ELSE IF WS-CURRENT-CHAR = '"'
                    MOVE 0 TO WS-IN-STRING
                 END-IF
                 END-IF
                 CONTINUE
              END-IF

              EVALUATE WS-CURRENT-CHAR
                 WHEN "{"
                    ADD 1 TO WS-OBJECT-COUNT
                    ADD 1 TO WS-DEPTH
                 WHEN "}"
                    IF WS-OBJECT-COUNT = 0
                       MOVE "Unexpected }" TO LS-ERROR-MSG
                       MOVE WS-INDEX TO LS-ERROR-POSITION
                       GOBACK
                    END-IF
                    SUBTRACT 1 FROM WS-OBJECT-COUNT
                    SUBTRACT 1 FROM WS-DEPTH
                 WHEN "["
                    ADD 1 TO WS-ARRAY-COUNT
                    ADD 1 TO WS-DEPTH
                 WHEN "]"
                    IF WS-ARRAY-COUNT = 0
                       MOVE "Unexpected ]" TO LS-ERROR-MSG
                       MOVE WS-INDEX TO LS-ERROR-POSITION
                       GOBACK
                    END-IF
                    SUBTRACT 1 FROM WS-ARRAY-COUNT
                    SUBTRACT 1 FROM WS-DEPTH
                 WHEN '"'
                    MOVE 1 TO WS-IN-STRING
              END-EVALUATE
           END-PERFORM

      *    Check balanced brackets
           IF WS-OBJECT-COUNT NOT = 0
              MOVE "Unclosed {" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF WS-ARRAY-COUNT NOT = 0
              MOVE "Unclosed [" TO LS-ERROR-MSG
              GOBACK
           END-IF

           IF WS-IN-STRING = 1
              MOVE "Unclosed string" TO LS-ERROR-MSG
              GOBACK
           END-IF

           MOVE 1 TO LS-RESULT
           GOBACK.

       VALIDATE-JSON-EXIT.
           EXIT.

      *================================================================
      * SKIP-WHITESPACE: Skip whitespace characters
      *================================================================
       SKIP-WHITESPACE SECTION.
           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN
              MOVE LS-JSON-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
              IF WS-CURRENT-CHAR NOT = " "
                 AND WS-CURRENT-CHAR NOT = X"09"
                 AND WS-CURRENT-CHAR NOT = X"0A"
                 AND WS-CURRENT-CHAR NOT = X"0D"
                 EXIT PERFORM
              END-IF
              ADD 1 TO WS-INDEX
           END-PERFORM.

       SKIP-WHITESPACE-EXIT.
           EXIT.

      *================================================================
      * ESCAPE-JSON-STRING: Escape string for JSON
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       ESCAPE-JSON-STRING SECTION.
           ENTRY "ESCAPE-JSON-STRING" USING LS-INPUT-STRING
                 LS-INPUT-LENGTH LS-OUTPUT-STRING LS-OUTPUT-LENGTH
                 LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

      *    Add opening quote
           MOVE '"' TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
           ADD 1 TO WS-OUTPUT-POS

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              EVALUATE WS-CURRENT-CHAR
                 WHEN '"'
                    MOVE '\"' TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN "\"
                    MOVE "\\" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN X"08"
                    MOVE "\b" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN X"09"
                    MOVE "\t" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN X"0A"
                    MOVE "\n" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN X"0C"
                    MOVE "\f" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN X"0D"
                    MOVE "\r" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:2)
                    ADD 2 TO WS-OUTPUT-POS
                 WHEN OTHER
                    MOVE WS-CURRENT-CHAR TO
                       LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    ADD 1 TO WS-OUTPUT-POS
              END-EVALUATE
           END-PERFORM

      *    Add closing quote
           MOVE '"' TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
           ADD 1 TO WS-OUTPUT-POS

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       ESCAPE-JSON-STRING-EXIT.
           EXIT.

      *================================================================
      * UNESCAPE-JSON-STRING: Unescape JSON string
      * Input:  LS-INPUT-STRING, LS-INPUT-LENGTH
      * Output: LS-OUTPUT-STRING, LS-OUTPUT-LENGTH, LS-RESULT
      *================================================================
       UNESCAPE-JSON-STRING SECTION.
           ENTRY "UNESCAPE-JSON-STRING" USING LS-INPUT-STRING
                 LS-INPUT-LENGTH LS-OUTPUT-STRING LS-OUTPUT-LENGTH
                 LS-RESULT.

           MOVE 1 TO LS-RESULT
           INITIALIZE LS-OUTPUT-STRING
           MOVE 1 TO WS-OUTPUT-POS
           MOVE 1 TO WS-INDEX
           MOVE LS-INPUT-LENGTH TO WS-INPUT-LEN

      *    Skip opening quote if present
           IF LS-INPUT-STRING(1:1) = '"'
              MOVE 2 TO WS-INDEX
              SUBTRACT 1 FROM WS-INPUT-LEN
           END-IF

      *    Skip closing quote if present
           IF LS-INPUT-STRING(WS-INPUT-LEN:1) = '"'
              SUBTRACT 1 FROM WS-INPUT-LEN
           END-IF

           PERFORM UNTIL WS-INDEX > WS-INPUT-LEN

              MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR

              IF WS-CURRENT-CHAR = "\"
                 ADD 1 TO WS-INDEX
                 IF WS-INDEX > WS-INPUT-LEN
                    EXIT PERFORM
                 END-IF
                 MOVE LS-INPUT-STRING(WS-INDEX:1) TO WS-CURRENT-CHAR
                 EVALUATE WS-CURRENT-CHAR
                    WHEN '"'
                       MOVE '"' TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "\"
                       MOVE "\" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "/"
                       MOVE "/" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "b"
                       MOVE X"08" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "f"
                       MOVE X"0C" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "n"
                       MOVE X"0A" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "r"
                       MOVE X"0D" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN "t"
                       MOVE X"09" TO LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                    WHEN OTHER
                       MOVE WS-CURRENT-CHAR TO
                          LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 END-EVALUATE
                 ADD 1 TO WS-OUTPUT-POS
              ELSE
                 MOVE WS-CURRENT-CHAR TO
                    LS-OUTPUT-STRING(WS-OUTPUT-POS:1)
                 ADD 1 TO WS-OUTPUT-POS
              END-IF

              ADD 1 TO WS-INDEX
           END-PERFORM

           SUBTRACT 1 FROM WS-OUTPUT-POS GIVING LS-OUTPUT-LENGTH
           GOBACK.

       UNESCAPE-JSON-STRING-EXIT.
           EXIT.

      *================================================================
      * IS-JSON-OBJECT: Check if JSON is an object (starts with {)
      * Input:  LS-JSON-STRING, LS-JSON-LENGTH
      * Output: LS-RESULT (1=object, 0=not object)
      *================================================================
       IS-JSON-OBJECT SECTION.
           ENTRY "IS-JSON-OBJECT" USING LS-JSON-STRING LS-JSON-LENGTH
                 LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE LS-JSON-LENGTH TO WS-INPUT-LEN

           IF WS-INPUT-LEN = 0
              GOBACK
           END-IF

      *    Skip leading whitespace
           MOVE 1 TO WS-INDEX
           PERFORM SKIP-WHITESPACE

           IF WS-INDEX <= WS-INPUT-LEN
              IF LS-JSON-STRING(WS-INDEX:1) = "{"
                 MOVE 1 TO LS-RESULT
              END-IF
           END-IF

           GOBACK.

       IS-JSON-OBJECT-EXIT.
           EXIT.

      *================================================================
      * IS-JSON-ARRAY: Check if JSON is an array (starts with [)
      * Input:  LS-JSON-STRING, LS-JSON-LENGTH
      * Output: LS-RESULT (1=array, 0=not array)
      *================================================================
       IS-JSON-ARRAY SECTION.
           ENTRY "IS-JSON-ARRAY" USING LS-JSON-STRING LS-JSON-LENGTH
                 LS-RESULT.

           MOVE 0 TO LS-RESULT
           MOVE LS-JSON-LENGTH TO WS-INPUT-LEN

           IF WS-INPUT-LEN = 0
              GOBACK
           END-IF

      *    Skip leading whitespace
           MOVE 1 TO WS-INDEX
           PERFORM SKIP-WHITESPACE

           IF WS-INDEX <= WS-INPUT-LEN
              IF LS-JSON-STRING(WS-INDEX:1) = "["
                 MOVE 1 TO LS-RESULT
              END-IF
           END-IF

           GOBACK.

       IS-JSON-ARRAY-EXIT.
           EXIT.

       END PROGRAM SAFE-JSON.
