-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeDNS operations
|||
||| This module exports DNS validation and homograph detection to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - Hostname parsing -> (status: Int, value: String)
|||   - status = 0: Valid hostname
|||   - status = 1: Invalid hostname
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeDNS

import Proven.SafeDNS

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Label Validation
--------------------------------------------------------------------------------

export
proven_idris_dns_is_valid_label : String -> Int
proven_idris_dns_is_valid_label = encodeBool . isValidLabel

export
proven_idris_dns_is_valid_hostname : String -> Int
proven_idris_dns_is_valid_hostname = encodeBool . isValidHostname

export
proven_idris_dns_parse_hostname : String -> (Int, String)
proven_idris_dns_parse_hostname s = case parseHostname s of
  Nothing => (1, "Invalid hostname")
  Just h => (0, hostnameToString h)

--------------------------------------------------------------------------------
-- Homograph Detection
--------------------------------------------------------------------------------

export
proven_idris_dns_has_homoglyphs : String -> Int
proven_idris_dns_has_homoglyphs = encodeBool . hasHomoglyphs

export
proven_idris_dns_is_mixed_script : String -> Int
proven_idris_dns_is_mixed_script = encodeBool . isMixedScript

export
proven_idris_dns_deconfuse : String -> String
proven_idris_dns_deconfuse = deconfuse

export
proven_idris_dns_is_homograph_of : String -> String -> Int
proven_idris_dns_is_homograph_of suspect trusted =
  encodeBool (isHomographOf suspect trusted)

--------------------------------------------------------------------------------
-- DNS Safety Checks
--------------------------------------------------------------------------------

export
proven_idris_dns_is_private_ip : String -> Int
proven_idris_dns_is_private_ip = encodeBool . isPrivateIP

export
proven_idris_dns_is_valid_caa_tag : String -> Int
proven_idris_dns_is_valid_caa_tag = encodeBool . isValidCAATag

--------------------------------------------------------------------------------
-- Record Type Info
--------------------------------------------------------------------------------

export
proven_idris_dns_record_type_name : Int -> String
proven_idris_dns_record_type_name 0 = show A
proven_idris_dns_record_type_name 1 = show AAAA
proven_idris_dns_record_type_name 2 = show CNAME
proven_idris_dns_record_type_name 3 = show MX
proven_idris_dns_record_type_name 4 = show NS
proven_idris_dns_record_type_name 5 = show TXT
proven_idris_dns_record_type_name 6 = show SRV
proven_idris_dns_record_type_name 7 = show SOA
proven_idris_dns_record_type_name 8 = show PTR
proven_idris_dns_record_type_name 9 = show CAA
proven_idris_dns_record_type_name _ = "Unknown"
