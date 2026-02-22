      *> SPDX-License-Identifier: PMPL-1.0-or-later
      *> Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
      *> <jonathan.jewell@open.ac.uk>
      *>
      *> Proven - COBOL copybook for proven data structures.
      *>
      *> This copybook defines the data types and status codes used
      *> when processing proven-cli output in COBOL programs that
      *> are invoked from JCL jobs.
      *>
      *> Include with: COPY PROVENH.
      *>
      *> These structures match the C ABI types from proven.h and
      *> are suitable for parsing proven-cli text output.

      *> ---------------------------------------------------------------
      *> Status codes returned by proven operations.
      *> Match the ProvenStatus enum in proven.h.
      *> ---------------------------------------------------------------
       01 PROVEN-STATUS-CODES.
          05 PROVEN-OK              PIC S9(4) COMP VALUE +0.
          05 PROVEN-ERR-NULL        PIC S9(4) COMP VALUE -1.
          05 PROVEN-ERR-INVAL       PIC S9(4) COMP VALUE -2.
          05 PROVEN-ERR-OVERFLOW    PIC S9(4) COMP VALUE -3.
          05 PROVEN-ERR-UNDERFLOW   PIC S9(4) COMP VALUE -4.
          05 PROVEN-ERR-DIVZERO     PIC S9(4) COMP VALUE -5.
          05 PROVEN-ERR-PARSE       PIC S9(4) COMP VALUE -6.
          05 PROVEN-ERR-VALIDATE    PIC S9(4) COMP VALUE -7.
          05 PROVEN-ERR-BOUNDS      PIC S9(4) COMP VALUE -8.
          05 PROVEN-ERR-ENCODING    PIC S9(4) COMP VALUE -9.
          05 PROVEN-ERR-ALLOC       PIC S9(4) COMP VALUE -10.

      *> ---------------------------------------------------------------
      *> IntResult - Result of an integer operation.
      *> Used by safe_add, safe_sub, safe_mul, safe_div, etc.
      *> ---------------------------------------------------------------
       01 PROVEN-INT-RESULT.
          05 PIR-STATUS             PIC S9(4) COMP.
             88 PIR-SUCCESS         VALUE 0.
             88 PIR-OVERFLOW        VALUE -3.
             88 PIR-UNDERFLOW       VALUE -4.
             88 PIR-DIVZERO         VALUE -5.
          05 PIR-VALUE              PIC S9(18).
          05 PIR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> BoolResult - Result of a boolean operation.
      *> Used by validate_email, validate_url, has_traversal, etc.
      *> ---------------------------------------------------------------
       01 PROVEN-BOOL-RESULT.
          05 PBR-STATUS             PIC S9(4) COMP.
             88 PBR-SUCCESS         VALUE 0.
             88 PBR-PARSE-ERR       VALUE -6.
             88 PBR-VALID-ERR       VALUE -7.
          05 PBR-VALUE              PIC 9.
             88 PBR-TRUE            VALUE 1.
             88 PBR-FALSE           VALUE 0.
          05 PBR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> StringResult - Result of a string operation.
      *> Used by escape_html, sanitize_string, hex_encode, etc.
      *> ---------------------------------------------------------------
       01 PROVEN-STRING-RESULT.
          05 PSR-STATUS             PIC S9(4) COMP.
             88 PSR-SUCCESS         VALUE 0.
             88 PSR-ENCODING-ERR    VALUE -9.
          05 PSR-VALUE              PIC X(4096).
          05 PSR-LENGTH             PIC 9(4) COMP.
          05 PSR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> HashResult - Result of a hashing operation.
      *> Used by hash_sha256, simple_hash, etc.
      *> ---------------------------------------------------------------
       01 PROVEN-HASH-RESULT.
          05 PHR-STATUS             PIC S9(4) COMP.
             88 PHR-SUCCESS         VALUE 0.
          05 PHR-DIGEST             PIC X(128).
          05 PHR-DIGEST-LEN         PIC 9(4) COMP.
          05 PHR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> EmailResult - Parsed email address components.
      *> Used by parse_email.
      *> ---------------------------------------------------------------
       01 PROVEN-EMAIL-RESULT.
          05 PER-STATUS             PIC S9(4) COMP.
             88 PER-SUCCESS         VALUE 0.
             88 PER-INVALID         VALUE -7.
          05 PER-LOCAL-PART         PIC X(64).
          05 PER-DOMAIN             PIC X(255).
          05 PER-IS-VALID           PIC 9.
             88 PER-VALID           VALUE 1.
             88 PER-NOT-VALID       VALUE 0.
          05 PER-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> IPv4Result - Parsed IPv4 address components.
      *> Used by parse_ipv4.
      *> ---------------------------------------------------------------
       01 PROVEN-IPV4-RESULT.
          05 P4R-STATUS             PIC S9(4) COMP.
             88 P4R-SUCCESS         VALUE 0.
             88 P4R-INVALID         VALUE -6.
          05 P4R-OCTET-1            PIC 9(3).
          05 P4R-OCTET-2            PIC 9(3).
          05 P4R-OCTET-3            PIC 9(3).
          05 P4R-OCTET-4            PIC 9(3).
          05 P4R-CLASS              PIC 9.
             88 P4R-INVALID-IP      VALUE 0.
             88 P4R-LOOPBACK        VALUE 1.
             88 P4R-PRIVATE         VALUE 2.
             88 P4R-RESERVED        VALUE 3.
             88 P4R-PUBLIC          VALUE 4.
          05 P4R-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> URLResult - Parsed URL components.
      *> Used by parse_url.
      *> ---------------------------------------------------------------
       01 PROVEN-URL-RESULT.
          05 PUR-STATUS             PIC S9(4) COMP.
             88 PUR-SUCCESS         VALUE 0.
             88 PUR-INVALID         VALUE -6.
          05 PUR-SCHEME             PIC X(10).
          05 PUR-HOST               PIC X(255).
          05 PUR-PORT               PIC 9(5).
          05 PUR-PATH               PIC X(2048).
          05 PUR-QUERY              PIC X(2048).
          05 PUR-IS-HTTPS           PIC 9.
             88 PUR-HTTPS           VALUE 1.
             88 PUR-NOT-HTTPS       VALUE 0.
          05 PUR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> PathResult - Path validation result.
      *> Used by has_traversal, sanitize_filename.
      *> ---------------------------------------------------------------
       01 PROVEN-PATH-RESULT.
          05 PPR-STATUS             PIC S9(4) COMP.
             88 PPR-SUCCESS         VALUE 0.
          05 PPR-HAS-TRAVERSAL      PIC 9.
             88 PPR-TRAVERSAL       VALUE 1.
             88 PPR-SAFE            VALUE 0.
          05 PPR-SANITIZED          PIC X(255).
          05 PPR-SANITIZED-LEN      PIC 9(4) COMP.
          05 PPR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> JSONResult - JSON validation result.
      *> Used by validate_json, json_type.
      *> ---------------------------------------------------------------
       01 PROVEN-JSON-RESULT.
          05 PJR-STATUS             PIC S9(4) COMP.
             88 PJR-SUCCESS         VALUE 0.
             88 PJR-INVALID         VALUE -6.
          05 PJR-IS-VALID           PIC 9.
             88 PJR-VALID           VALUE 1.
             88 PJR-NOT-VALID       VALUE 0.
          05 PJR-TYPE               PIC X(10).
             88 PJR-OBJECT          VALUE 'object'.
             88 PJR-ARRAY           VALUE 'array'.
             88 PJR-STRING          VALUE 'string'.
             88 PJR-NUMBER          VALUE 'number'.
             88 PJR-BOOLEAN         VALUE 'boolean'.
             88 PJR-NULL            VALUE 'null'.
          05 PJR-DEPTH              PIC 9(4) COMP.
          05 PJR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> HexResult - Hex encoding/decoding result.
      *> Used by hex_encode, hex_decode.
      *> ---------------------------------------------------------------
       01 PROVEN-HEX-RESULT.
          05 PXR-STATUS             PIC S9(4) COMP.
             88 PXR-SUCCESS         VALUE 0.
             88 PXR-INVALID-HEX     VALUE -6.
          05 PXR-VALUE              PIC X(8192).
          05 PXR-LENGTH             PIC 9(4) COMP.
          05 PXR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> DateTimeResult - Parsed ISO 8601 date/time.
      *> Used by format_datetime.
      *> ---------------------------------------------------------------
       01 PROVEN-DATETIME-RESULT.
          05 PDR-STATUS             PIC S9(4) COMP.
             88 PDR-SUCCESS         VALUE 0.
             88 PDR-INVALID         VALUE -6.
          05 PDR-YEAR               PIC 9(4).
          05 PDR-MONTH              PIC 9(2).
          05 PDR-DAY                PIC 9(2).
          05 PDR-HOUR               PIC 9(2).
          05 PDR-MINUTE             PIC 9(2).
          05 PDR-SECOND             PIC 9(2).
          05 PDR-IS-LEAP-YEAR       PIC 9.
             88 PDR-LEAP            VALUE 1.
             88 PDR-NOT-LEAP        VALUE 0.
          05 PDR-ERROR-MSG          PIC X(80).

      *> ---------------------------------------------------------------
      *> Common working fields for proven-cli invocation.
      *> ---------------------------------------------------------------
       01 PROVEN-CLI-FIELDS.
          05 PCF-CLI-PATH           PIC X(256)
             VALUE '/usr/local/bin/proven-cli'.
          05 PCF-COMMAND            PIC X(1024).
          05 PCF-RETURN-CODE        PIC S9(4) COMP.
          05 PCF-OUTPUT-LINE        PIC X(4096).
