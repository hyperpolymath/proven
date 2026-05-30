// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeJson - JSON validation and type detection via libproven FFI.
//
// Status: GATED on proven#88.

module SafeJson {

  public use LibProven;

  /** Check if string is valid JSON. */
  proc isValid(s: string): Maybe(bool) {
    var (ptr, len) = toCBytes(s);
    var r = provenJsonIsValid(ptr, len);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

  /** Get JSON value type at root level (see JSON_* constants in LibProven). */
  proc getType(s: string): int(32) {
    var (ptr, len) = toCBytes(s);
    return provenJsonGetType(ptr, len);
  }

  /** Convert a JSON type code to a human-readable name (no FFI call). */
  proc typeName(typeCode: int(32)): string {
    select typeCode {
      when JSON_NULL    do return "null";
      when JSON_BOOL    do return "boolean";
      when JSON_NUMBER  do return "number";
      when JSON_STRING  do return "string";
      when JSON_ARRAY   do return "array";
      when JSON_OBJECT  do return "object";
      otherwise         do return "invalid";
    }
  }

}
