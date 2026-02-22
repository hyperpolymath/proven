      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven SafeMath - FFI bindings to libproven math operations.
      *> All arithmetic is performed in verified Idris 2 code via
      *> libproven. This module uses CALL to invoke C ABI functions.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAFE-MATH.
       AUTHOR. Jonathan D.A. Jewell.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "PROVEN-TYPES.cpy".

       LINKAGE SECTION.
       01 LS-OPERAND-A         PIC S9(18) COMP.
       01 LS-OPERAND-B         PIC S9(18) COMP.
       01 LS-RESULT            PIC S9(18) COMP.
       01 LS-STATUS            PIC S9(9) COMP.
       01 LS-MIN-VALUE         PIC S9(18) COMP.
       01 LS-MAX-VALUE         PIC S9(18) COMP.

       PROCEDURE DIVISION.

      *================================================================
      * SAFE-ADD: Checked addition via libproven
      *================================================================
       SAFE-ADD SECTION.
           ENTRY "SAFE-ADD" USING LS-OPERAND-A LS-OPERAND-B
                                  LS-RESULT LS-STATUS.

           CALL "proven_math_add_checked"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-ADD-EXIT.
           EXIT.

      *================================================================
      * SAFE-SUBTRACT: Checked subtraction via libproven
      *================================================================
       SAFE-SUBTRACT SECTION.
           ENTRY "SAFE-SUBTRACT" USING LS-OPERAND-A LS-OPERAND-B
                                       LS-RESULT LS-STATUS.

           CALL "proven_math_sub_checked"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-SUBTRACT-EXIT.
           EXIT.

      *================================================================
      * SAFE-MULTIPLY: Checked multiplication via libproven
      *================================================================
       SAFE-MULTIPLY SECTION.
           ENTRY "SAFE-MULTIPLY" USING LS-OPERAND-A LS-OPERAND-B
                                       LS-RESULT LS-STATUS.

           CALL "proven_math_mul_checked"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-MULTIPLY-EXIT.
           EXIT.

      *================================================================
      * SAFE-DIVIDE: Safe division via libproven
      *================================================================
       SAFE-DIVIDE SECTION.
           ENTRY "SAFE-DIVIDE" USING LS-OPERAND-A LS-OPERAND-B
                                     LS-RESULT LS-STATUS.

           CALL "proven_math_div"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-DIVIDE-EXIT.
           EXIT.

      *================================================================
      * SAFE-MODULO: Safe modulo via libproven
      *================================================================
       SAFE-MODULO SECTION.
           ENTRY "SAFE-MODULO" USING LS-OPERAND-A LS-OPERAND-B
                                     LS-RESULT LS-STATUS.

           CALL "proven_math_mod"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-MODULO-EXIT.
           EXIT.

      *================================================================
      * SAFE-ABS: Safe absolute value via libproven
      *================================================================
       SAFE-ABS SECTION.
           ENTRY "SAFE-ABS" USING LS-OPERAND-A
                                   LS-RESULT LS-STATUS.

           CALL "proven_math_abs_safe"
               USING BY VALUE LS-OPERAND-A
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-ABS-EXIT.
           EXIT.

      *================================================================
      * CLAMP-VALUE: Clamp value to range via libproven
      *================================================================
       CLAMP-VALUE SECTION.
           ENTRY "CLAMP-VALUE" USING LS-OPERAND-A LS-MIN-VALUE
                                     LS-MAX-VALUE LS-RESULT.

           CALL "proven_math_clamp"
               USING BY VALUE LS-MIN-VALUE
                     BY VALUE LS-MAX-VALUE
                     BY VALUE LS-OPERAND-A
               RETURNING LS-RESULT
           END-CALL

           GOBACK.

       CLAMP-VALUE-EXIT.
           EXIT.

      *================================================================
      * SAFE-POW: Checked exponentiation via libproven
      *================================================================
       SAFE-POW SECTION.
           ENTRY "SAFE-POW" USING LS-OPERAND-A LS-OPERAND-B
                                   LS-RESULT LS-STATUS.

           CALL "proven_math_pow_checked"
               USING BY VALUE LS-OPERAND-A
                     BY VALUE LS-OPERAND-B
               RETURNING WS-INT-RESULT
           END-CALL

           MOVE WS-INT-STATUS TO LS-STATUS
           MOVE WS-INT-VALUE TO LS-RESULT
           GOBACK.

       SAFE-POW-EXIT.
           EXIT.

       END PROGRAM SAFE-MATH.
