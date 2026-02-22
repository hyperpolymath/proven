//* SPDX-License-Identifier: PMPL-1.0-or-later
//* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//* <jonathan.jewell@open.ac.uk>
//*
//* Proven Crypto - JCL job for cryptographic operations.
//*
//* All computation delegates to proven-cli (Idris 2 + Zig) via
//* BPXBATCH (Unix System Services).  No logic is reimplemented
//* in JCL.
//*
//* Symbolic parameters:
//*   &DATA   - Input data for hashing or encoding
//*   &DATAB  - Second input for comparison operations
//*   &NBYTES - Number of random bytes to generate
//*   &HEXSTR - Hex string for decoding
//*   &CLI    - Path to proven-cli binary
//*
//* Usage:
//*   SUBMIT PROVENC with PARM='DATA=hello'
//*
//* Output is written to STDOUT DD (SYSOUT=*).
//*
//*-------------------------------------------------------------------
//PROVENC  JOB (ACCT),'PROVEN CRYPTO',
//         CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//*
//* ------- SYMBOLIC PARAMETERS --------------------------------------
//*
//         SET DATA='hello'
//         SET DATAB='world'
//         SET NBYTES=32
//         SET HEXSTR='48656c6c6f'
//         SET CLI='/usr/local/bin/proven-cli'
//*
//* ------- SHA-256 HASH ---------------------------------------------
//* Compute the SHA-256 hash of the input data.
//* Returns a 64-character hex-encoded digest.
//*
//SHA256   EXEC PGM=BPXBATCH,
//         PARM='SH &CLI crypto sha256 &DATA'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- CONSTANT-TIME COMPARISON ---------------------------------
//* Compare two strings in constant time to prevent timing attacks.
//* Returns 'true' if equal, 'false' otherwise.
//*
//CTIMEQ   EXEC PGM=BPXBATCH,
//         PARM='SH &CLI crypto constant-time-eq &DATA &DATAB'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- RANDOM BYTES ---------------------------------------------
//* Generate cryptographically secure random bytes (hex-encoded).
//*
//RANDBYTE EXEC PGM=BPXBATCH,
//         PARM='SH &CLI crypto random-bytes &NBYTES'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- TOKEN GENERATION -----------------------------------------
//* Generate a random token suitable for session IDs or API keys.
//*
//GENTOKEN EXEC PGM=BPXBATCH,
//         PARM='SH &CLI crypto generate-token &NBYTES'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- HEX ENCODE -----------------------------------------------
//* Encode input data to hexadecimal representation.
//*
//HEXENC   EXEC PGM=BPXBATCH,
//         PARM='SH &CLI hex encode &DATA'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- HEX DECODE -----------------------------------------------
//* Decode a hexadecimal string back to its byte representation.
//*
//HEXDEC   EXEC PGM=BPXBATCH,
//         PARM='SH &CLI hex decode &HEXSTR'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- SIMPLE HASH ----------------------------------------------
//* Compute a simple (non-cryptographic) hash of input data.
//*
//SMPHASH  EXEC PGM=BPXBATCH,
//         PARM='SH &CLI crypto simple-hash &DATA'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- CAPTURE HASH TO DATASET ----------------------------------
//* Write the SHA-256 hash to a temporary dataset for downstream use.
//*
//SAVEHASH EXEC PGM=BPXBATCH,
//         PARM='SH &CLI crypto sha256 &DATA'
//STDOUT   DD DSN=&&HASHOUT,
//         DISP=(NEW,PASS),
//         SPACE=(TRK,(1,1)),
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//STDERR   DD SYSOUT=*
//*
