//* SPDX-License-Identifier: PMPL-1.0-or-later
//* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//* <jonathan.jewell@open.ac.uk>
//*
//* Proven SafeMath - JCL job for safe arithmetic operations.
//*
//* All computation delegates to proven-cli (Idris 2 + Zig) via
//* BPXBATCH (Unix System Services).  No logic is reimplemented
//* in JCL.
//*
//* Symbolic parameters:
//*   &OP   - Operation: add, sub, mul, div, mod, abs, clamp, pow
//*   &A    - First operand
//*   &B    - Second operand (not used for abs)
//*   &C    - Third operand (only used for clamp: max value)
//*   &CLI  - Path to proven-cli binary
//*
//* Usage:
//*   SUBMIT PROVENM with PARM='OP=add,A=100,B=200'
//*
//* Output is written to STDOUT DD (SYSOUT=*).
//*
//*-------------------------------------------------------------------
//PROVENM  JOB (ACCT),'PROVEN MATH',
//         CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//*
//* ------- SYMBOLIC PARAMETERS --------------------------------------
//*
//         SET OP=add
//         SET A=0
//         SET B=0
//         SET C=0
//         SET CLI='/usr/local/bin/proven-cli'
//*
//* ------- SAFE ADDITION --------------------------------------------
//*
//ADD      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math add &A &B',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE SUBTRACTION -----------------------------------------
//*
//SUB      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math sub &A &B',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE MULTIPLICATION --------------------------------------
//*
//MUL      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math mul &A &B',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE DIVISION --------------------------------------------
//*
//DIV      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math div &A &B',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE MODULO ----------------------------------------------
//*
//MOD      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math mod &A &B',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE ABSOLUTE VALUE --------------------------------------
//*
//ABS      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math abs &A',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE CLAMP -----------------------------------------------
//*
//CLAMP    EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math clamp &A &B &C',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SAFE EXPONENTIATION --------------------------------------
//*
//POW      EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math pow &A &B',
//         COND=(0,NE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- DISPATCHER STEP ------------------------------------------
//* This step runs only the requested operation via a shell script.
//* Override the OP parameter to select which operation to run.
//*
//DISPATCH EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math &OP &A &B &C'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- CAPTURE RESULT TO DATASET --------------------------------
//* Optionally write result to a dataset for downstream processing.
//*
//SAVE     EXEC PGM=BPXBATCH,
//         PARM='SH &CLI math &OP &A &B &C'
//STDOUT   DD DSN=&&RESULT,
//         DISP=(NEW,PASS),
//         SPACE=(TRK,(1,1)),
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//STDERR   DD SYSOUT=*
//*
