//* SPDX-License-Identifier: PMPL-1.0-or-later
//* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//* <jonathan.jewell@open.ac.uk>
//*
//* Proven JSON - JCL job for JSON validation operations.
//*
//* All computation delegates to proven-cli (Idris 2 + Zig) via
//* BPXBATCH (Unix System Services).  No logic is reimplemented
//* in JCL.
//*
//* Because JCL symbolic parameters cannot easily hold JSON with
//* braces and quotes, this job reads JSON input from a dataset
//* or DD statement and pipes it to proven-cli via stdin.
//*
//* Symbolic parameters:
//*   &JSONDD - DD name containing JSON input (default: JSONIN)
//*   &CLI    - Path to proven-cli binary
//*
//* Usage:
//*   SUBMIT PROVENJ with JSON data in the JSONIN DD.
//*
//* Output is written to STDOUT DD (SYSOUT=*).
//*
//*-------------------------------------------------------------------
//PROVENJ  JOB (ACCT),'PROVEN JSON',
//         CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//*
//* ------- SYMBOLIC PARAMETERS --------------------------------------
//*
//         SET CLI='/usr/local/bin/proven-cli'
//*
//* ------- JSON VALIDATION (INLINE) ---------------------------------
//* Validate JSON passed inline via a temporary file.
//* This step writes JSON to a temp file, then validates it.
//*
//WRITJSON EXEC PGM=BPXBATCH,
//  PARM='SH cat > /tmp/proven_json_$$.txt <<ENDJSON'
//STDIN    DD *
{"name": "test", "value": 42, "active": true}
/*
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//VALJSON  EXEC PGM=BPXBATCH,
//  PARM='SH &CLI json is-valid "$(cat /tmp/proven_json_$$.txt)"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- JSON TYPE DETECTION --------------------------------------
//* Determine the type of a JSON value (object, array, string, etc.)
//*
//JSONTYPE EXEC PGM=BPXBATCH,
//  PARM='SH &CLI json get-type "$(cat /tmp/proven_json_$$.txt)"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- JSON PRETTY PRINT ----------------------------------------
//* Format JSON with indentation for readability.
//*
//JSONPRTY EXEC PGM=BPXBATCH,
//  PARM='SH &CLI json pretty "$(cat /tmp/proven_json_$$.txt)"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- JSON MINIFY ----------------------------------------------
//* Remove whitespace from JSON for compact representation.
//*
//JSONMINI EXEC PGM=BPXBATCH,
//  PARM='SH &CLI json minify "$(cat /tmp/proven_json_$$.txt)"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- JSON DEPTH -----------------------------------------------
//* Count the nesting depth of a JSON structure.
//*
//JSONDPTH EXEC PGM=BPXBATCH,
//  PARM='SH &CLI json depth "$(cat /tmp/proven_json_$$.txt)"'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- VALIDATE FROM DATASET ------------------------------------
//* Validate JSON read from a sequential dataset.
//* Override JSONIN DD to point to your dataset.
//*
//VALDS    EXEC PGM=BPXBATCH,
//  PARM='SH &CLI json is-valid "$(cat)"'
//STDIN    DD DSN=&&JSONIN,DISP=(OLD,DELETE)
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
//* ------- CLEANUP --------------------------------------------------
//* Remove the temporary JSON file.
//*
//CLEANUP  EXEC PGM=BPXBATCH,
//         PARM='SH rm -f /tmp/proven_json_$$.txt'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//*
