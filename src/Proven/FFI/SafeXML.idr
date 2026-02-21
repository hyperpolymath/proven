-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeXML operations
|||
||| This module exports XML processing with XXE prevention to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against entity expansion bombs.
|||
||| Return conventions:
||| - XML parsing → (status: Int, result/error: String)
|||   - status = 0: Success, result is rendered XML
|||   - status = 1: Error, result contains error message
||| - Query operations → (status: Int, result: String)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use secure defaults for untrusted XML. External entities disabled by default.
module Proven.FFI.SafeXML

import Proven.SafeXML
import Proven.SafeXML.Types
import Proven.SafeXML.Parser
import Proven.SafeXML.Builder
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode XMLResult as (status, result/error)
encodeXMLResult : {a : Type} -> Show a => XMLResult a -> (Int, String)
encodeXMLResult (Err err) = (1, show err)
encodeXMLResult (Ok val) = (0, show val)

||| Encode XMLResult XMLDocument as (status, rendered/error)
encodeDocResult : XMLResult XMLDocument -> (Int, String)
encodeDocResult (Err err) = (1, show err)
encodeDocResult (Ok doc) = (0, renderDocument doc)

||| Encode XMLResult XMLNode as (status, rendered/error)
encodeNodeResult : XMLResult XMLNode -> (Int, String)
encodeNodeResult (Err err) = (1, show err)
encodeNodeResult (Ok node) = (0, renderNode node)

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

--------------------------------------------------------------------------------
-- XML Parsing
--------------------------------------------------------------------------------

export
proven_idris_xml_parse : String -> (Int, String)
proven_idris_xml_parse s = encodeDocResult (parse s)

export
proven_idris_xml_is_valid : String -> Int
proven_idris_xml_is_valid s = encodeBool (isSafe s)

export
proven_idris_xml_validate : String -> (Int, String)
proven_idris_xml_validate s =
  case validate s of
    Err err => (1, show err)
    Ok () => (0, "")

--------------------------------------------------------------------------------
-- Element Building
--------------------------------------------------------------------------------

export
proven_idris_xml_text_element : String -> String -> (Int, String)
proven_idris_xml_text_element name content =
  encodeNodeResult (textElem name content)

export
proven_idris_xml_empty_element : String -> (Int, String)
proven_idris_xml_empty_element name = encodeNodeResult (emptyElem name)

export
proven_idris_xml_comment : String -> (Int, String)
proven_idris_xml_comment content = encodeNodeResult (comment content)

--------------------------------------------------------------------------------
-- Text Escaping
--------------------------------------------------------------------------------

export
proven_idris_xml_escape_text : String -> String
proven_idris_xml_escape_text s =
  let xmlTxt = xmlText s
  in xmlTxt.escaped

export
proven_idris_xml_escape_attr : String -> String
proven_idris_xml_escape_attr s =
  let xmlAttr = xmlAttrValue s
  in xmlAttr.escaped

--------------------------------------------------------------------------------
-- Query Operations
--------------------------------------------------------------------------------

export
proven_idris_xml_find_element : String -> String -> (Int, String)
proven_idris_xml_find_element xmlStr elemName =
  case parse xmlStr of
    Err err => (1, show err)
    Ok doc => case findElement elemName doc.root of
      Nothing => (1, "Element not found")
      Just node => (0, renderNode node)

export
proven_idris_xml_get_attribute : String -> String -> String -> (Int, String)
proven_idris_xml_get_attribute xmlStr elemName attrName =
  case parse xmlStr of
    Err err => (1, show err)
    Ok doc => case findElement elemName doc.root of
      Nothing => (1, "Element not found")
      Just node => encodeMaybeString (getAttribute attrName node)

export
proven_idris_xml_get_text_content : String -> String -> (Int, String)
proven_idris_xml_get_text_content xmlStr elemName =
  case parse xmlStr of
    Err err => (1, show err)
    Ok doc => case findElement elemName doc.root of
      Nothing => (1, "Element not found")
      Just node => (0, getTextContent node)

--------------------------------------------------------------------------------
-- Security Options Constants
--------------------------------------------------------------------------------

export
proven_idris_xml_secure_max_entity_expansions : Int
proven_idris_xml_secure_max_entity_expansions = cast secureDefaults.maxEntityExpansions

export
proven_idris_xml_secure_max_nesting_depth : Int
proven_idris_xml_secure_max_nesting_depth = cast secureDefaults.maxNestingDepth

export
proven_idris_xml_secure_max_attributes : Int
proven_idris_xml_secure_max_attributes = cast secureDefaults.maxAttributesPerElement

export
proven_idris_xml_secure_max_text_size : Int
proven_idris_xml_secure_max_text_size = cast secureDefaults.maxTextNodeSize

export
proven_idris_xml_secure_allow_external_entities : Int
proven_idris_xml_secure_allow_external_entities = encodeBool secureDefaults.allowExternalEntities

export
proven_idris_xml_secure_allow_internal_entities : Int
proven_idris_xml_secure_allow_internal_entities = encodeBool secureDefaults.allowInternalEntities

export
proven_idris_xml_secure_allow_dtd : Int
proven_idris_xml_secure_allow_dtd = encodeBool secureDefaults.allowDTD

export
proven_idris_xml_secure_allow_pi : Int
proven_idris_xml_secure_allow_pi = encodeBool secureDefaults.allowProcessingInstructions

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

export
proven_idris_xml_is_xxe_error : String -> Int
proven_idris_xml_is_xxe_error errorMsg =
  if isInfixOf "external entity" (toLower errorMsg) || isInfixOf "xxe" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_xml_is_entity_expansion_error : String -> Int
proven_idris_xml_is_entity_expansion_error errorMsg =
  if isInfixOf "entity expansion" (toLower errorMsg) || isInfixOf "expansion limit" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_xml_is_nesting_error : String -> Int
proven_idris_xml_is_nesting_error errorMsg =
  if isInfixOf "nesting" (toLower errorMsg) || isInfixOf "depth" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_xml_is_invalid_name_error : String -> Int
proven_idris_xml_is_invalid_name_error errorMsg =
  if isInfixOf "invalid name" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_xml_is_malformed_error : String -> Int
proven_idris_xml_is_malformed_error errorMsg =
  if isInfixOf "malformed" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_xml_is_dtd_error : String -> Int
proven_idris_xml_is_dtd_error errorMsg =
  if isInfixOf "dtd" (toLower errorMsg) || isInfixOf "doctype" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Security Warnings
--------------------------------------------------------------------------------

export
proven_idris_xml_friendly_error : String -> String
proven_idris_xml_friendly_error errorMsg =
  if isInfixOf "external entity" (toLower errorMsg)
    then "XML contains external entities (XXE attack detected)"
  else if isInfixOf "entity expansion" (toLower errorMsg)
    then "XML entity expansion limit exceeded (billion laughs attack)"
  else if isInfixOf "nesting" (toLower errorMsg)
    then "XML nesting depth exceeded"
  else if isInfixOf "dtd" (toLower errorMsg)
    then "XML DTD declarations not allowed"
  else
    "XML processing error"
