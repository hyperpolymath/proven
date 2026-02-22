# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeJson - JSON validation and type detection.
#
# Thin wrapper over libproven's SafeJson module.  All JSON parsing and
# validation is performed in formally verified Idris 2 code.  This module
# does NOT reimplement any JSON logic.

unit module Proven::SafeJson;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# JSON type constants (mirrors ProvenJsonType enum)
# ============================================================================

enum JsonType is export (
    JSON_NULL    =>  0,
    JSON_BOOL    =>  1,
    JSON_NUMBER  =>  2,
    JSON_STRING  =>  3,
    JSON_ARRAY   =>  4,
    JSON_OBJECT  =>  5,
    JSON_INVALID => -1,
);

# ============================================================================
# JSON validation
# ============================================================================

#| Check if a string is valid JSON.
#| Returns True/False, or Nil on internal error.
sub json-is-valid(Str:D $s --> Bool) is export {
    my ($buf, $len) = str-to-buf($s);
    my BoolResult $r = proven_json_is_valid(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return $r.value;
}

# ============================================================================
# JSON type detection
# ============================================================================

#| Get the JSON value type at the root level.
#| Returns a JsonType enum value, or JSON_INVALID if not valid JSON.
sub json-get-type(Str:D $s --> JsonType) is export {
    my ($buf, $len) = str-to-buf($s);
    my int32 $type = proven_json_get_type(nativecast(Pointer, $buf), $len);
    given $type {
        when  0 { return JSON_NULL;    }
        when  1 { return JSON_BOOL;    }
        when  2 { return JSON_NUMBER;  }
        when  3 { return JSON_STRING;  }
        when  4 { return JSON_ARRAY;   }
        when  5 { return JSON_OBJECT;  }
        default { return JSON_INVALID; }
    }
}

#| Get the JSON value type as a human-readable string.
#| Returns one of: "null", "boolean", "number", "string", "array", "object",
#| or "invalid".
sub json-type-name(Str:D $s --> Str) is export {
    my $type = json-get-type($s);
    given $type {
        when JSON_NULL    { return 'null';    }
        when JSON_BOOL    { return 'boolean'; }
        when JSON_NUMBER  { return 'number';  }
        when JSON_STRING  { return 'string';  }
        when JSON_ARRAY   { return 'array';   }
        when JSON_OBJECT  { return 'object';  }
        default           { return 'invalid'; }
    }
}
