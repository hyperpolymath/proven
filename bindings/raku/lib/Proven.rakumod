# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven - Raku FFI binding to libproven (formally verified safety library).
#
# This is the main entry-point module that re-exports all sub-modules.
# All computation is performed in Idris 2 (with dependent types and totality
# checking) via a Zig FFI layer.  These Raku modules are thin NativeCall
# wrappers; they do NOT reimplement any logic.
#
# Usage:
#   use Proven;  # imports everything
#
#   # Or import individual modules:
#   use Proven::SafeMath;
#   use Proven::SafeString;
#   use Proven::SafePath;
#   use Proven::SafeEmail;
#   use Proven::SafeUrl;
#   use Proven::SafeNetwork;
#   use Proven::SafeCrypto;
#   use Proven::SafeJson;
#   use Proven::SafeDateTime;

unit module Proven;

use Proven::LibProven;
use Proven::SafeMath;
use Proven::SafeString;
use Proven::SafePath;
use Proven::SafeEmail;
use Proven::SafeUrl;
use Proven::SafeNetwork;
use Proven::SafeCrypto;
use Proven::SafeJson;
use Proven::SafeDateTime;

# ============================================================================
# Library version
# ============================================================================

our constant VERSION = '0.5.0';

# ============================================================================
# Lifecycle management
# ============================================================================

#| Initialize the Proven runtime (includes Idris 2 runtime).
#| Must be called before any other Proven function.
#| Returns True on success, False on error.
sub proven-init(--> Bool) is export {
    my int32 $status = proven_init();
    return $status == 0;
}

#| Shut down the Proven runtime.
#| Call when done using Proven functions.  All allocated resources should be
#| freed before calling this.
sub proven-deinit() is export {
    proven_deinit();
}

#| Check if the Proven runtime is initialized.
sub proven-is-initialized(--> Bool) is export {
    return proven_is_initialized();
}

# ============================================================================
# Version information
# ============================================================================

#| Get the FFI ABI version for compatibility checking.
sub proven-abi-version(--> Int) is export {
    return proven_ffi_abi_version();
}

#| Get the library version as a "major.minor.patch" string.
sub proven-version(--> Str) is export {
    my $major = proven_version_major();
    my $minor = proven_version_minor();
    my $patch = proven_version_patch();
    return "$major.$minor.$patch";
}

#| Get the total number of modules in the library.
sub proven-module-count(--> Int) is export {
    return proven_module_count();
}

# ============================================================================
# Re-exports from sub-modules
# ============================================================================

# SafeMath
sub safe-add(|c)     is export { Proven::SafeMath::safe-add(|c)   }
sub safe-sub(|c)     is export { Proven::SafeMath::safe-sub(|c)   }
sub safe-mul(|c)     is export { Proven::SafeMath::safe-mul(|c)   }
sub safe-div(|c)     is export { Proven::SafeMath::safe-div(|c)   }
sub safe-mod(|c)     is export { Proven::SafeMath::safe-mod(|c)   }
sub safe-abs(|c)     is export { Proven::SafeMath::safe-abs(|c)   }
sub safe-clamp(|c)   is export { Proven::SafeMath::safe-clamp(|c) }
sub safe-pow(|c)     is export { Proven::SafeMath::safe-pow(|c)   }

# SafeString
sub is-valid-utf8(|c) is export { Proven::SafeString::is-valid-utf8(|c) }
sub escape-sql(|c)    is export { Proven::SafeString::escape-sql(|c)    }
sub escape-html(|c)   is export { Proven::SafeString::escape-html(|c)   }
sub escape-js(|c)     is export { Proven::SafeString::escape-js(|c)     }

# SafePath
sub has-traversal(|c)     is export { Proven::SafePath::has-traversal(|c)     }
sub sanitize-filename(|c) is export { Proven::SafePath::sanitize-filename(|c) }

# SafeEmail
sub email-is-valid(|c) is export { Proven::SafeEmail::email-is-valid(|c) }

# SafeUrl
sub url-encode(|c) is export { Proven::SafeUrl::url-encode(|c) }
sub url-decode(|c) is export { Proven::SafeUrl::url-decode(|c) }

# SafeNetwork
sub parse-ipv4(|c)      is export { Proven::SafeNetwork::parse-ipv4(|c)      }
sub ipv4-is-private(|c)  is export { Proven::SafeNetwork::ipv4-is-private(|c)  }
sub ipv4-is-loopback(|c) is export { Proven::SafeNetwork::ipv4-is-loopback(|c) }

# SafeCrypto
sub constant-time-eq(|c)       is export { Proven::SafeCrypto::constant-time-eq(|c)       }
sub random-bytes(|c)           is export { Proven::SafeCrypto::random-bytes(|c)           }
sub hex-encode(|c)             is export { Proven::SafeCrypto::hex-encode(|c)             }
sub hex-decode(|c)             is export { Proven::SafeCrypto::hex-decode(|c)             }
sub checksum-crc32(|c)         is export { Proven::SafeCrypto::checksum-crc32(|c)         }
sub checksum-verify-crc32(|c)  is export { Proven::SafeCrypto::checksum-verify-crc32(|c)  }

# SafeJson
sub json-is-valid(|c)  is export { Proven::SafeJson::json-is-valid(|c)  }
sub json-get-type(|c)  is export { Proven::SafeJson::json-get-type(|c)  }
sub json-type-name(|c) is export { Proven::SafeJson::json-type-name(|c) }

# SafeDateTime
sub datetime-parse(|c)          is export { Proven::SafeDateTime::datetime-parse(|c)          }
sub datetime-format-iso8601(|c) is export { Proven::SafeDateTime::datetime-format-iso8601(|c) }
sub is-leap-year(|c)            is export { Proven::SafeDateTime::is-leap-year(|c)            }
sub days-in-month(|c)           is export { Proven::SafeDateTime::days-in-month(|c)           }
