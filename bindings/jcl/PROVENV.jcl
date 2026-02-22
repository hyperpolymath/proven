//* SPDX-License-Identifier: PMPL-1.0-or-later
//* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//* <jonathan.jewell@open.ac.uk>
//*
//* Proven Validation - JCL job for validation operations.
//*
//* All computation delegates to proven-cli (Idris 2 + Zig) via
//* BPXBATCH (Unix System Services).  No logic is reimplemented
//* in JCL.
//*
//* Symbolic parameters:
//*   &EMAIL - Email address to validate
//*   &URL   - URL to validate
//*   &IPV4  - IPv4 address to validate
//*   &PATH  - Filesystem path to validate
//*   &CLI   - Path to proven-cli binary
//*
//* Usage:
//*   SUBMIT PROVENV with PARM='EMAIL=user@example.com'
//*
//* Output is written to STDOUT DD (SYSOUT=*).
//*
//*-------------------------------------------------------------------
//PROVENV  JOB (ACCT),'PROVEN VALIDATE',
//         CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//*
//* ------- SYMBOLIC PARAMETERS --------------------------------------
//*
//         SET EMAIL='user@example.com'
//         SET URL='https://example.com'
//         SET IPV4='127.0.0.1'
//         SET PATH='/tmp/test.txt'
//         SET CLI='/usr/local/bin/proven-cli'
//*
//* ------- EMAIL VALIDATION -----------------------------------------
//* Validate an email address per RFC 5321.
//*
//VALEMAIL EXEC PGM=BPXBATCH,
//         PARM='SH &CLI email is-valid &EMAIL'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- EMAIL PARSING --------------------------------------------
//* Parse an email into local-part and domain components.
//*
//PRSEMAIL EXEC PGM=BPXBATCH,
//         PARM='SH &CLI email parse &EMAIL'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- URL VALIDATION -------------------------------------------
//* Parse and validate a URL.
//*
//VALURL   EXEC PGM=BPXBATCH,
//         PARM='SH &CLI url parse &URL'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- IPV4 VALIDATION ------------------------------------------
//* Parse and validate an IPv4 address.
//*
//VALIPV4  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI network parse-ipv4 &IPV4'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- IPV4 PRIVATE CHECK ---------------------------------------
//* Check whether an IPv4 address is in a private range.
//*
//PRIVIPV4 EXEC PGM=BPXBATCH,
//         PARM='SH &CLI network ipv4-is-private &IPV4'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- IPV4 LOOPBACK CHECK --------------------------------------
//* Check whether an IPv4 address is loopback (127.x.x.x).
//*
//LOOPIPV4 EXEC PGM=BPXBATCH,
//         PARM='SH &CLI network ipv4-is-loopback &IPV4'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- PATH TRAVERSAL CHECK -------------------------------------
//* Check whether a path contains directory traversal patterns.
//*
//VALPATH  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI path has-traversal &PATH'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- PATH SANITIZATION ----------------------------------------
//* Sanitize a filename by removing dangerous characters.
//*
//SANPATH  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI path sanitize-filename &PATH'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- CAPTURE RESULTS TO DATASET -------------------------------
//* Run all validations and capture output for downstream processing.
//*
//SAVEALL  EXEC PGM=BPXBATCH,
//  PARM='SH echo "=== EMAIL ===" && &CLI email is-valid &EMAIL && ech
//o "=== URL ===" && &CLI url parse &URL && echo "=== IPV4 ===" && &CL
//I network parse-ipv4 &IPV4 && echo "=== PATH ===" && &CLI path has-t
//raversal &PATH'
//STDOUT   DD DSN=&&VALRESLT,
//         DISP=(NEW,PASS),
//         SPACE=(TRK,(1,1)),
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//STDERR   DD SYSOUT=*
//*
