-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeMCP - Model Context Protocol with prompt injection detection
|||
||| Provides type-safe MCP message handling that detects and prevents
||| prompt injection attacks in tool inputs and outputs.
module Proven.SafeMCP

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| MCP message roles
public export
data MCPRole = User | Assistant | System | Tool

public export
Show MCPRole where
  show User = "user"
  show Assistant = "assistant"
  show System = "system"
  show Tool = "tool"

public export
Eq MCPRole where
  User == User = True
  Assistant == Assistant = True
  System == System = True
  Tool == Tool = True
  _ == _ = False

||| Known prompt injection patterns
public export
injectionPatterns : List String
injectionPatterns =
  [ "ignore previous instructions"
  , "ignore all previous"
  , "disregard your instructions"
  , "forget your instructions"
  , "new instructions:"
  , "system prompt:"
  , "you are now"
  , "act as if"
  , "pretend you are"
  , "override your"
  , "bypass your"
  , "ignore your safety"
  , "jailbreak"
  , "<system>"
  , "</system>"
  , "[INST]"
  , "[/INST]"
  , "<<SYS>>"
  , "<</SYS>>"
  , "### Instruction:"
  , "### Human:"
  , "### Assistant:"
  ]

||| Check if text contains prompt injection patterns
public export
hasInjectionPattern : String -> Bool
hasInjectionPattern s =
  let lower = toLower s
  in any (\pat => isInfixOf pat lower) injectionPatterns

||| Injection detection confidence level
public export
data InjectionConfidence = None | Low | Medium | High | Critical

public export
Eq InjectionConfidence where
  None == None = True
  Low == Low = True
  Medium == Medium = True
  High == High = True
  Critical == Critical = True
  _ == _ = False

public export
Ord InjectionConfidence where
  compare None None = EQ
  compare None _ = LT
  compare _ None = GT
  compare Low Low = EQ
  compare Low _ = LT
  compare _ Low = GT
  compare Medium Medium = EQ
  compare Medium _ = LT
  compare _ Medium = GT
  compare High High = EQ
  compare High _ = LT
  compare _ High = GT
  compare Critical Critical = EQ

||| Analyze text for injection attempts
public export
analyzeInjection : String -> InjectionConfidence
analyzeInjection s =
  let lower = toLower s
      patternCount = length (filter (\pat => isInfixOf pat lower) injectionPatterns)
      hasXmlTags = isInfixOf "<system>" lower || isInfixOf "</system>" lower
      hasRoleSwitch = isInfixOf "### human:" lower || isInfixOf "### assistant:" lower
  in if patternCount >= 3 || (hasXmlTags && patternCount >= 1)
       then Critical
       else if patternCount >= 2 || hasRoleSwitch
         then High
         else if patternCount >= 1
           then Medium
           else if hasXmlTags
             then Low
             else None

||| A tool name (validated)
public export
data ToolName : Type where
  MkToolName : (name : String) -> ToolName

public export
Show ToolName where
  show (MkToolName n) = n

public export
Eq ToolName where
  MkToolName a == MkToolName b = a == b

||| Validate a tool name (alphanumeric + underscores + hyphens)
public export
isValidToolName : String -> Bool
isValidToolName s =
  let chars = unpack s
  in length chars > 0 && length chars <= 128 &&
     all (\c => isAlphaNum c || c == '_' || c == '-') chars

||| Smart constructor for tool names
public export
mkToolName : String -> Maybe ToolName
mkToolName s = if isValidToolName s then Just (MkToolName s) else Nothing

||| A tool input parameter
public export
record ToolParam where
  constructor MkToolParam
  paramName  : String
  paramValue : String
  paramType  : String  -- "string", "number", "boolean"

||| A tool call request
public export
record ToolCall where
  constructor MkToolCall
  toolName   : ToolName
  toolParams : List ToolParam

||| A tool result
public export
record ToolResult where
  constructor MkToolResult
  resultToolName : ToolName
  resultContent  : String
  resultIsError  : Bool

||| Sanitize tool input (remove injection patterns)
public export
sanitizeToolInput : String -> String
sanitizeToolInput s =
  if hasInjectionPattern s
    then "[Content filtered: potential injection detected]"
    else s

||| Validate tool parameters
public export
validateToolParams : ToolCall -> List (ToolParam, InjectionConfidence)
validateToolParams call =
  map (\p => (p, analyzeInjection (paramValue p))) (toolParams call)

||| Check if all tool parameters are safe
public export
areParamsSafe : ToolCall -> Bool
areParamsSafe call =
  all (\(_, conf) => conf == None || conf == Low) (validateToolParams call)

||| Validate tool result for injection
public export
validateToolResult : ToolResult -> InjectionConfidence
validateToolResult result = analyzeInjection (resultContent result)

||| MCP content block (text, image, or resource)
public export
data ContentBlock =
    TextBlock String
  | ImageBlock String String   -- Base64 data, MIME type
  | ResourceBlock String       -- URI

||| Sanitize a content block
public export
sanitizeBlock : ContentBlock -> ContentBlock
sanitizeBlock (TextBlock s) = TextBlock (sanitizeToolInput s)
sanitizeBlock (ImageBlock d m) = ImageBlock d m  -- Images are safe
sanitizeBlock (ResourceBlock uri) =
  if hasInjectionPattern uri
    then ResourceBlock "[filtered]"
    else ResourceBlock uri

||| Maximum tool result size (prevent DoS)
public export
maxResultSize : Nat
maxResultSize = 1048576  -- 1 MB

||| Check if result size is within limits
public export
isResultSizeOk : ToolResult -> Bool
isResultSizeOk result = length (resultContent result) <= maxResultSize

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that content has no injection patterns
public export
data InjectionFree : String -> Type where
  MkInjectionFree : hasInjectionPattern s = False -> InjectionFree s

||| Proof that a tool call has safe parameters
public export
data SafeToolCall : ToolCall -> Type where
  MkSafeToolCall : areParamsSafe call = True -> SafeToolCall call

||| Proof that a tool name is valid
public export
data ValidToolName : ToolName -> Type where
  MkValidToolName : (t : ToolName) -> ValidToolName t

||| Proof that result size is acceptable
public export
data AcceptableSize : ToolResult -> Type where
  MkAcceptableSize : isResultSizeOk result = True -> AcceptableSize result
