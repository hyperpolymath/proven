# Proven for JCL

Safe, validated operations library for IBM mainframe JCL batch jobs.

## Overview

This binding provides access to the formally verified Proven safety library from
JCL (Job Control Language) on IBM z/OS mainframes. Since JCL cannot perform
direct computation, all operations invoke the `proven-cli` binary via
`BPXBATCH` (Unix System Services). Every function delegates to Idris 2 verified
code through the Zig FFI layer -- no logic is reimplemented in JCL.

## Prerequisites

- IBM z/OS with Unix System Services (USS) enabled
- `proven-cli` installed in USS (default: `/usr/local/bin/proven-cli`)
- BPXBATCH available (standard on z/OS 2.1+)
- JES2 or JES3 for job submission

## Job Cards

| File         | Purpose                    | Operations                        |
|--------------|----------------------------|-----------------------------------|
| `PROVENM.jcl`| Safe math operations       | add, sub, mul, div, mod, abs, pow |
| `PROVENV.jcl`| Validation operations      | email, URL, IPv4, path            |
| `PROVENC.jcl`| Cryptographic operations   | sha256, hex encode/decode, random |
| `PROVENJ.jcl`| JSON validation            | is-valid, get-type, pretty, depth |
| `PROVENS.jcl`| String operations          | sanitize, escape-html/sql/js      |

## COBOL Copybook

`COPYLIB/PROVENH.cpy` provides COBOL data structures for parsing `proven-cli`
output in COBOL programs invoked from JCL jobs:

- `PROVEN-INT-RESULT` -- Integer operation results (math)
- `PROVEN-BOOL-RESULT` -- Boolean operation results (validation)
- `PROVEN-STRING-RESULT` -- String operation results (escaping)
- `PROVEN-HASH-RESULT` -- Hashing operation results (crypto)
- `PROVEN-EMAIL-RESULT` -- Parsed email components
- `PROVEN-IPV4-RESULT` -- Parsed IPv4 components
- `PROVEN-URL-RESULT` -- Parsed URL components
- `PROVEN-PATH-RESULT` -- Path validation results
- `PROVEN-JSON-RESULT` -- JSON validation results
- `PROVEN-HEX-RESULT` -- Hex encode/decode results
- `PROVEN-DATETIME-RESULT` -- Parsed date/time components
- `PROVEN-CLI-FIELDS` -- Common fields for CLI invocation

## Usage

### Submitting a math job

Override symbolic parameters to perform operations:

```jcl
//MYJOB    JOB (ACCT),'MY MATH',CLASS=A,MSGCLASS=X
//         JCLLIB ORDER=(PROVEN.PROCLIB)
//STEP1    EXEC PROVENM,OP=add,A=100,B=200
```

Or submit directly with parameter overrides:

```
SUBMIT 'PROVEN.JCLLIB(PROVENM)' PARM='OP=add,A=100,B=200'
```

### Safe arithmetic (PROVENM)

Each step in PROVENM invokes a different math operation:

```jcl
//* Addition: result = A + B
//         SET OP=add
//         SET A=100
//         SET B=200

//* Division with zero protection: result = A / B
//         SET OP=div
//         SET A=100
//         SET B=0
```

The DISPATCH step runs whichever operation is specified by `&OP`.

### Validation operations (PROVENV)

```jcl
//* Validate an email address
//         SET EMAIL='user@example.com'

//* Validate a URL
//         SET URL='https://example.com/path'

//* Check an IPv4 address
//         SET IPV4='192.168.1.1'

//* Check a path for directory traversal
//         SET PATH='../../../etc/passwd'
```

### Cryptographic operations (PROVENC)

```jcl
//* Compute SHA-256 hash
//         SET DATA='hello world'

//* Generate random bytes
//         SET NBYTES=32

//* Hex encode/decode
//         SET HEXSTR='48656c6c6f'
```

### JSON validation (PROVENJ)

JSON data is passed via inline DD statements because JCL symbolic parameters
cannot safely hold braces and quotes:

```jcl
//WRITJSON EXEC PGM=BPXBATCH,
//  PARM='SH cat > /tmp/proven_json_$$.txt <<ENDJSON'
//STDIN    DD *
{"name": "test", "value": 42}
/*
```

### String operations (PROVENS)

```jcl
//* Sanitize a string
//         SET INPUT='<script>alert(1)</script>'

//* Escape for HTML
//* Escape for SQL
//* Check UTF-8 validity
```

## Capturing Output

Each job includes steps that write results to temporary datasets for downstream
processing:

```jcl
//SAVE     EXEC PGM=BPXBATCH,
//         PARM='SH /usr/local/bin/proven-cli math add 100 200'
//STDOUT   DD DSN=&&RESULT,
//         DISP=(NEW,PASS),
//         SPACE=(TRK,(1,1)),
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//STDERR   DD SYSOUT=*
```

Use `DISP=(OLD,DELETE)` in subsequent steps to consume the temporary dataset.

## Using the COBOL Copybook

When writing COBOL programs that parse proven-cli output, include the copybook:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-PROVEN-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY PROVENH.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      *    Parse proven-cli math output
           MOVE 0 TO PIR-STATUS
           MOVE 300 TO PIR-VALUE
           IF PIR-SUCCESS
              DISPLAY "Result: " PIR-VALUE
           END-IF
           STOP RUN.
```

Compile with:

```
//COMPILE  EXEC PGM=IGYCRCTL,
//         PARM='LIB'
//SYSLIB   DD DSN=PROVEN.COPYLIB,DISP=SHR
```

## Architecture

```
JCL Job Card
    |
    v
BPXBATCH (Unix System Services)
    |
    v
proven-cli (binary)
    |
    v
Zig FFI layer
    |
    v
Idris 2 verified implementation
```

All computation happens in formally verified Idris 2 code. JCL is purely
a job scheduling layer that invokes the CLI through USS.

## Customization

### Changing the CLI path

Override the `&CLI` symbolic parameter:

```jcl
//         SET CLI='/opt/proven/bin/proven-cli'
```

### Adding to a JCL procedure library

Copy the JCL files to a PDS:

```
//COPYJCL  EXEC PGM=IEBCOPY
//SYSUT1   DD DSN=PROVEN.JCLLIB.SOURCE,DISP=SHR
//SYSUT2   DD DSN=YOUR.PROCLIB,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=SYSUT2,INDD=SYSUT1
/*
```

## Limitations

- JCL PARM fields are limited to 100 characters; very long inputs should be
  passed via datasets or USS files
- JSON with special characters (braces, quotes) must use the file-based approach
  shown in PROVENJ
- BPXBATCH requires USS to be configured on the z/OS system
- Return codes from proven-cli are reflected in the JCL step condition codes

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
