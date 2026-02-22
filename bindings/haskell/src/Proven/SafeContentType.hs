{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe Content-Type operations via libproven FFI.
--
-- MIME type parsing and sniffing detection are performed by the
-- Idris 2 verified core.
module Proven.SafeContentType
  ( canSniffDangerous
  , isJsonContentType
  , isXmlContentType
  ) where

import Proven.FFI (c_proven_content_type_can_sniff_dangerous,
                   c_proven_content_type_is_json,
                   c_proven_content_type_is_xml)
import Proven.Core (withCStringLen', boolResultToBool)

-- | Check if a content type can be sniffed to something dangerous.
-- Delegates to @proven_content_type_can_sniff_dangerous@ in libproven.
canSniffDangerous :: String -> IO (Maybe Bool)
canSniffDangerous ct = withCStringLen' ct $ \ptr len ->
  boolResultToBool <$> c_proven_content_type_can_sniff_dangerous ptr len

-- | Check if content type is JSON.
-- Delegates to @proven_content_type_is_json@ in libproven.
isJsonContentType :: String -> String -> IO (Maybe Bool)
isJsonContentType subtype suffix =
  withCStringLen' subtype $ \stPtr stLen ->
    withCStringLen' suffix $ \suPtr suLen ->
      boolResultToBool <$> c_proven_content_type_is_json stPtr stLen suPtr suLen

-- | Check if content type is XML.
-- Delegates to @proven_content_type_is_xml@ in libproven.
isXmlContentType :: String -> String -> IO (Maybe Bool)
isXmlContentType subtype suffix =
  withCStringLen' subtype $ \stPtr stLen ->
    withCStringLen' suffix $ \suPtr suLen ->
      boolResultToBool <$> c_proven_content_type_is_xml stPtr stLen suPtr suLen
