//* SPDX-License-Identifier: PMPL-1.0-or-later
//* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//* <jonathan.jewell@open.ac.uk>
//*
//* Proven String - JCL job for safe string operations.
//*
//* All computation delegates to proven-cli (Idris 2 + Zig) via
//* BPXBATCH (Unix System Services).  No logic is reimplemented
//* in JCL.
//*
//* Symbolic parameters:
//*   &INPUT  - Input string to process
//*   &CLI    - Path to proven-cli binary
//*
//* Usage:
//*   SUBMIT PROVENS with PARM='INPUT=hello world'
//*
//* Output is written to STDOUT DD (SYSOUT=*).
//*
//*-------------------------------------------------------------------
//PROVENS  JOB (ACCT),'PROVEN STRING',
//         CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//*
//* ------- SYMBOLIC PARAMETERS --------------------------------------
//*
//         SET INPUT='test string'
//         SET CLI='/usr/local/bin/proven-cli'
//*
//* ------- STRING SANITIZATION --------------------------------------
//* Remove unsafe characters, retaining alphanumeric, underscore,
//* and hyphen only.
//*
//SANITIZE EXEC PGM=BPXBATCH,
//         PARM='SH &CLI string sanitize "&INPUT"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- HTML ESCAPING --------------------------------------------
//* Escape HTML special characters to prevent XSS attacks.
//* Converts <, >, &, ", '' to HTML entities.
//*
//ESCHTML  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI string escape-html "&INPUT"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SQL ESCAPING ---------------------------------------------
//* Escape single quotes in strings destined for SQL queries.
//* Prevents SQL injection attacks.
//*
//ESCSQL   EXEC PGM=BPXBATCH,
//         PARM='SH &CLI string escape-sql "&INPUT"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- JAVASCRIPT ESCAPING --------------------------------------
//* Escape strings for safe embedding in JavaScript contexts.
//*
//ESCJS    EXEC PGM=BPXBATCH,
//         PARM='SH &CLI string escape-js "&INPUT"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- UTF-8 VALIDATION -----------------------------------------
//* Check whether the input string is valid UTF-8.
//* Returns 'true' or 'false'.
//*
//VALUTF8  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI string is-valid-utf8 "&INPUT"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SLUGIFY --------------------------------------------------
//* Convert a string to a URL-safe slug.
//*
//SLUGIFY  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI string slugify "&INPUT"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- BATCH SANITIZATION FROM DATASET --------------------------
//* Read strings from an input dataset, sanitize each line, and
//* write results to an output dataset.
//*
//BATCHSAN EXEC PGM=BPXBATCH,
//  PARM='SH while IFS= read -r line; do &CLI string sanitize "$line";
// done'
//STDIN    DD DSN=&&STRIN,DISP=(OLD,DELETE)
//STDOUT   DD DSN=&&STROUT,
//         DISP=(NEW,PASS),
//         SPACE=(TRK,(1,1)),
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//STDERR   DD SYSOUT=*
//*
