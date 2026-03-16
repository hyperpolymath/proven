-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Entry point for RefC code generation
|||
||| Calls all FFI-exported functions in IO to prevent dead-code elimination.
module Proven.FFI.Main

import Proven.FFI.SafeUrl

main : IO ()
main = do
  -- URL parsing
  let (s1, r1) = proven_idris_url_parse "http://example.com"
  putStrLn (show s1 ++ r1)
  -- Validation
  let v = proven_idris_url_is_valid "http://test.com"
  putStrLn (show v)
  -- Components
  let (_, _) = proven_idris_url_get_scheme "http://x"
  let (_, _) = proven_idris_url_get_host "http://x"
  let (_, _) = proven_idris_url_get_port "http://x"
  let (_, _) = proven_idris_url_get_path "http://x"
  let (_, _) = proven_idris_url_get_query "http://x"
  let (_, _) = proven_idris_url_get_fragment "http://x"
  -- Scheme checks
  let _ = proven_idris_url_is_http "http://x"
  let _ = proven_idris_url_is_https "https://x"
  let _ = proven_idris_url_is_secure "https://x"
  let _ = proven_idris_url_is_mailto "mailto:x"
  let _ = proven_idris_url_is_file "file:///x"
  -- Normalize
  let (_, _) = proven_idris_url_normalize "http://x"
  -- Query
  let _ = proven_idris_url_query_get "http://x?a=1" "a"
  let _ = proven_idris_url_query_has "http://x?a=1" "a"
  let _ = proven_idris_url_query_keys "http://x?a=1"
  -- Error checks
  let _ = proven_idris_url_is_parse_error "test"
  let _ = proven_idris_url_is_scheme_error "test"
  let _ = proven_idris_url_is_host_error "test"
  let _ = proven_idris_url_is_port_error "test"
  -- Default ports
  putStrLn (show proven_idris_url_default_port_http)
  putStrLn (show proven_idris_url_default_port_https)
  putStrLn (show proven_idris_url_default_port_ftp)
  putStrLn (show proven_idris_url_default_port_ftps)
  pure ()
