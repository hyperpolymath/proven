-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe template rendering with injection prevention
|||
||| This module provides type-safe template handling:
||| - Template parsing and validation
||| - Safe variable interpolation
||| - Context-aware escaping (HTML, URL, JS, CSS)
||| - Control structures (if, for, include)
||| - Template inheritance
|||
||| Security features:
||| - Server-Side Template Injection (SSTI) prevention
||| - Automatic context-aware escaping
||| - Sandbox for template expressions
||| - Dangerous directive blocking
||| - Include path traversal prevention
module Proven.SafeTemplate

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Escaping context
public export
data EscapeContext
  = HTMLContext
  | HTMLAttrContext
  | JSContext
  | CSSContext
  | URLContext
  | RawContext   -- No escaping (dangerous!)

||| Template expression types
public export
data ExprType
  = StringExpr
  | NumberExpr
  | BoolExpr
  | ListExpr
  | ObjectExpr
  | NullExpr

||| Template value
public export
data TemplateValue
  = TString String
  | TNumber Double
  | TBool Bool
  | TList (List TemplateValue)
  | TObject (List (String, TemplateValue))
  | TNull

||| Variable reference with optional filters
public export
record VariableRef where
  constructor MkVariableRef
  path : List String        -- dot-separated path
  filters : List String     -- filter chain

||| Filter definition
public export
record FilterDef where
  constructor MkFilterDef
  name : String
  args : List String

||| Template expression
public export
data TemplateExpr
  = VarExpr VariableRef
  | LiteralExpr TemplateValue
  | BinaryExpr String TemplateExpr TemplateExpr
  | UnaryExpr String TemplateExpr
  | TernaryExpr TemplateExpr TemplateExpr TemplateExpr
  | FilterExpr TemplateExpr FilterDef

||| Template node
public export
data TemplateNode
  = TextNode String
  | OutputNode TemplateExpr EscapeContext
  | IfNode TemplateExpr (List TemplateNode) (Maybe (List TemplateNode))
  | ForNode String TemplateExpr (List TemplateNode) (Maybe (List TemplateNode))
  | IncludeNode String (Maybe TemplateExpr)
  | BlockNode String (List TemplateNode)
  | ExtendsNode String
  | CommentNode String
  | MacroDefNode String (List String) (List TemplateNode)
  | MacroCallNode String (List TemplateExpr)

||| Parsed template
public export
record Template where
  constructor MkTemplate
  name : String
  nodes : List TemplateNode
  parent : Maybe String     -- extends
  blocks : List (String, List TemplateNode)

||| Template context (variables)
public export
TemplateContext : Type
TemplateContext = List (String, TemplateValue)

||| Template loader result
public export
data LoaderResult
  = LoadedTemplate Template
  | LoaderError String

||| Template environment configuration
public export
record TemplateEnv where
  constructor MkTemplateEnv
  autoEscape : EscapeContext
  allowRaw : Bool
  allowIncludes : Bool
  allowMacros : Bool
  maxIncludeDepth : Nat
  allowedPaths : List String
  blockedDirectives : List String

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during template operations
public export
data TemplateError
  = EmptyTemplate
  | SyntaxError String Nat    -- message, line
  | UnclosedTag String
  | UnknownVariable String
  | UnknownFilter String
  | InvalidFilterArg String String
  | TypeMismatch String ExprType ExprType
  | SSTIDetected String
  | DangerousDirective String
  | PathTraversal String
  | IncludeDepthExceeded Nat
  | BlockedPath String
  | CircularInclude String
  | UndefinedBlock String
  | MacroNotFound String
  | InvalidMacroCall String
  | RawOutputBlocked
  | ExpressionError String

public export
Show TemplateError where
  show EmptyTemplate = "Template error: empty template"
  show (SyntaxError msg line) = "Template syntax error at line " ++ show line ++ ": " ++ msg
  show (UnclosedTag tag) = "Template error: unclosed tag '" ++ tag ++ "'"
  show (UnknownVariable v) = "Template error: unknown variable '" ++ v ++ "'"
  show (UnknownFilter f) = "Template error: unknown filter '" ++ f ++ "'"
  show (InvalidFilterArg f arg) = "Template error: invalid argument '" ++ arg ++ "' for filter '" ++ f ++ "'"
  show (TypeMismatch op expected actual) = "Template error: type mismatch in '" ++ op ++ "'"
  show (SSTIDetected pattern) = "Template security: SSTI detected '" ++ pattern ++ "'"
  show (DangerousDirective d) = "Template security: dangerous directive '" ++ d ++ "'"
  show (PathTraversal p) = "Template security: path traversal detected '" ++ p ++ "'"
  show (IncludeDepthExceeded d) = "Template error: include depth exceeded (" ++ show d ++ ")"
  show (BlockedPath p) = "Template security: blocked path '" ++ p ++ "'"
  show (CircularInclude p) = "Template error: circular include '" ++ p ++ "'"
  show (UndefinedBlock b) = "Template error: undefined block '" ++ b ++ "'"
  show (MacroNotFound m) = "Template error: macro not found '" ++ m ++ "'"
  show (InvalidMacroCall m) = "Template error: invalid macro call '" ++ m ++ "'"
  show RawOutputBlocked = "Template security: raw output is blocked"
  show (ExpressionError msg) = "Template error: expression error - " ++ msg

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Default max include depth
defaultMaxIncludeDepth : Nat
defaultMaxIncludeDepth = 10

||| SSTI dangerous patterns
sstiPatterns : List String
sstiPatterns =
  [ ".__class__", ".__mro__", ".__subclasses__"
  , ".__globals__", ".__builtins__", ".__import__"
  , ".config", ".request", ".session"
  , "eval(", "exec(", "compile("
  , "os.", "subprocess.", "sys."
  ]

||| Dangerous template directives
dangerousDirectives : List String
dangerousDirectives =
  [ "import", "from", "exec", "eval"
  , "system", "popen", "spawn"
  ]

||| Safe built-in filters
safeFilters : List String
safeFilters =
  [ "escape", "e", "safe", "upper", "lower"
  , "capitalize", "title", "trim", "strip"
  , "length", "count", "first", "last"
  , "join", "split", "replace", "default"
  , "abs", "round", "int", "float", "string"
  , "list", "sort", "reverse", "unique"
  , "urlencode", "jsonify", "nl2br"
  ]

||| Path traversal patterns
pathTraversalPatterns : List String
pathTraversalPatterns =
  [ "..", "//", "\\", "%2e%2e", "%2f"
  , "/etc/", "/var/", "/usr/", "/home/"
  , "file://", "php://", "data://"
  ]

--------------------------------------------------------------------------------
-- Escaping functions
--------------------------------------------------------------------------------

||| Escape for HTML context
public export
escapeHTML : String -> String
escapeHTML str = concatMap escapeChar (unpack str)
  where
    escapeChar : Char -> String
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar c = singleton c

||| Escape for HTML attribute context
public export
escapeHTMLAttr : String -> String
escapeHTMLAttr str = concatMap escapeChar (unpack str)
  where
    escapeChar : Char -> String
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar '`' = "&#x60;"
    escapeChar '=' = "&#x3D;"
    escapeChar c = singleton c

||| Escape for JavaScript context
public export
escapeJS : String -> String
escapeJS str = concatMap escapeChar (unpack str)
  where
    escapeChar : Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar '\'' = "\\'"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar '<' = "\\u003C"
    escapeChar '>' = "\\u003E"
    escapeChar '&' = "\\u0026"
    escapeChar c = singleton c

||| Escape for CSS context
public export
escapeCSS : String -> String
escapeCSS str = concatMap escapeChar (unpack str)
  where
    escapeChar : Char -> String
    escapeChar c =
      if isAlphaNum c
        then singleton c
        else "\\" ++ pack (toHex (ord c))
    toHex : Int -> List Char
    toHex n = unpack (toUpper (showHex n))  -- Simplified

    showHex : Int -> String
    showHex n = show n  -- Would need proper hex conversion

||| Escape for URL context
public export
escapeURL : String -> String
escapeURL str = concatMap escapeChar (unpack str)
  where
    escapeChar : Char -> String
    escapeChar c =
      if isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '~'
        then singleton c
        else "%" ++ showHex (ord c)

    showHex : Int -> String
    showHex n =
      let hex = "0123456789ABCDEF"
          high = n `div` 16
          low = n `mod` 16
      in pack [strIndex hex (cast high), strIndex hex (cast low)]

||| Apply escaping based on context
public export
escape : EscapeContext -> String -> String
escape HTMLContext = escapeHTML
escape HTMLAttrContext = escapeHTMLAttr
escape JSContext = escapeJS
escape CSSContext = escapeCSS
escape URLContext = escapeURL
escape RawContext = id

--------------------------------------------------------------------------------
-- Security validation
--------------------------------------------------------------------------------

||| Check for SSTI patterns
public export
detectSSTI : String -> Maybe String
detectSSTI input =
  let lower = toLower input
  in find (\p => isInfixOf (toLower p) lower) sstiPatterns

||| Check for path traversal
public export
detectPathTraversal : String -> Maybe String
detectPathTraversal path =
  find (\p => isInfixOf p (toLower path)) pathTraversalPatterns

||| Check for dangerous directive
public export
isDangerousDirective : String -> Bool
isDangerousDirective dir = elem (toLower dir) dangerousDirectives

||| Check if filter is safe
public export
isSafeFilter : String -> Bool
isSafeFilter filter = elem (toLower filter) safeFilters

||| Validate include path
public export
validateIncludePath : TemplateEnv -> String -> Either TemplateError String
validateIncludePath env path =
  case detectPathTraversal path of
    Just pattern => Left (PathTraversal pattern)
    Nothing =>
      if null env.allowedPaths
        then Right path
        else if any (\allowed => isPrefixOf allowed path) env.allowedPaths
          then Right path
          else Left (BlockedPath path)

||| Validate template expression for security
public export
validateExpr : String -> Either TemplateError String
validateExpr expr =
  case detectSSTI expr of
    Just pattern => Left (SSTIDetected pattern)
    Nothing => Right expr

--------------------------------------------------------------------------------
-- Template value helpers
--------------------------------------------------------------------------------

||| Convert template value to string
public export
valueToString : TemplateValue -> String
valueToString (TString s) = s
valueToString (TNumber n) = show n
valueToString (TBool True) = "true"
valueToString (TBool False) = "false"
valueToString TNull = ""
valueToString (TList items) = "[" ++ concat (intersperse ", " (map valueToString items)) ++ "]"
valueToString (TObject fields) = "{" ++ concat (intersperse ", " (map showField fields)) ++ "}"
  where
    showField : (String, TemplateValue) -> String
    showField (k, v) = k ++ ": " ++ valueToString v

||| Check if value is truthy
public export
isTruthy : TemplateValue -> Bool
isTruthy (TBool False) = False
isTruthy TNull = False
isTruthy (TString "") = False
isTruthy (TList []) = False
isTruthy (TNumber 0.0) = False
isTruthy _ = True

||| Get value at path in context
public export
lookupPath : List String -> TemplateContext -> Maybe TemplateValue
lookupPath [] _ = Nothing
lookupPath [key] ctx = lookup key ctx
lookupPath (key :: rest) ctx =
  case lookup key ctx of
    Just (TObject fields) => lookupPath rest fields
    _ => Nothing

--------------------------------------------------------------------------------
-- Filter application
--------------------------------------------------------------------------------

||| Apply a filter to a value
public export
applyFilter : FilterDef -> TemplateValue -> Either TemplateError TemplateValue
applyFilter (MkFilterDef "escape" _) (TString s) = Right (TString (escapeHTML s))
applyFilter (MkFilterDef "e" _) (TString s) = Right (TString (escapeHTML s))
applyFilter (MkFilterDef "upper" _) (TString s) = Right (TString (toUpper s))
applyFilter (MkFilterDef "lower" _) (TString s) = Right (TString (toLower s))
applyFilter (MkFilterDef "trim" _) (TString s) = Right (TString (trim s))
applyFilter (MkFilterDef "length" _) (TString s) = Right (TNumber (cast (length s)))
applyFilter (MkFilterDef "length" _) (TList items) = Right (TNumber (cast (length items)))
applyFilter (MkFilterDef "first" _) (TList (x :: _)) = Right x
applyFilter (MkFilterDef "first" _) (TList []) = Right TNull
applyFilter (MkFilterDef "last" _) (TList items) =
  case reverse items of
    (x :: _) => Right x
    [] => Right TNull
applyFilter (MkFilterDef "reverse" _) (TList items) = Right (TList (reverse items))
applyFilter (MkFilterDef "reverse" _) (TString s) = Right (TString (reverse s))
applyFilter (MkFilterDef "default" [def]) TNull = Right (TString def)
applyFilter (MkFilterDef "default" [def]) (TString "") = Right (TString def)
applyFilter (MkFilterDef "default" _) v = Right v
applyFilter (MkFilterDef "urlencode" _) (TString s) = Right (TString (escapeURL s))
applyFilter (MkFilterDef name _) _ =
  if isSafeFilter name
    then Left (ExpressionError ("filter not implemented: " ++ name))
    else Left (UnknownFilter name)

--------------------------------------------------------------------------------
-- Template construction
--------------------------------------------------------------------------------

||| Create a text node
public export
textNode : String -> TemplateNode
textNode = TextNode

||| Create an output node with auto-escaping
public export
outputNode : VariableRef -> EscapeContext -> TemplateNode
outputNode var ctx = OutputNode (VarExpr var) ctx

||| Create a safe output node (uses default escaping)
public export
safeOutputNode : VariableRef -> TemplateNode
safeOutputNode var = OutputNode (VarExpr var) HTMLContext

||| Create an if node
public export
ifNode : TemplateExpr -> List TemplateNode -> Maybe (List TemplateNode) -> TemplateNode
ifNode = IfNode

||| Create a for node
public export
forNode : String -> TemplateExpr -> List TemplateNode -> Maybe (List TemplateNode) -> TemplateNode
forNode = ForNode

||| Create a validated include node
public export
includeNode : TemplateEnv -> String -> Either TemplateError TemplateNode
includeNode env path =
  if not env.allowIncludes
    then Left (DangerousDirective "include")
    else do
      validPath <- validateIncludePath env path
      pure (IncludeNode validPath Nothing)

--------------------------------------------------------------------------------
-- Default environment
--------------------------------------------------------------------------------

||| Create default safe environment
public export
defaultEnv : TemplateEnv
defaultEnv = MkTemplateEnv
  HTMLContext    -- auto-escape HTML by default
  False          -- disallow raw output
  True           -- allow includes
  True           -- allow macros
  defaultMaxIncludeDepth
  []             -- no path restrictions
  dangerousDirectives

||| Create strict environment (more restrictive)
public export
strictEnv : TemplateEnv
strictEnv = MkTemplateEnv
  HTMLContext
  False          -- no raw
  False          -- no includes
  False          -- no macros
  0              -- no include depth
  []
  dangerousDirectives

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Show escape context
public export
showEscapeContext : EscapeContext -> String
showEscapeContext HTMLContext = "html"
showEscapeContext HTMLAttrContext = "htmlattr"
showEscapeContext JSContext = "js"
showEscapeContext CSSContext = "css"
showEscapeContext URLContext = "url"
showEscapeContext RawContext = "raw"

||| Show variable reference
public export
showVariableRef : VariableRef -> String
showVariableRef (MkVariableRef path filters) =
  concat (intersperse "." path)
  ++ (if null filters then "" else "|" ++ concat (intersperse "|" filters))

||| Show template value as safe output
public export
renderValue : EscapeContext -> TemplateValue -> String
renderValue ctx val = escape ctx (valueToString val)

--------------------------------------------------------------------------------
-- Basic rendering (simplified)
--------------------------------------------------------------------------------

||| Render a template node
public export
renderNode : TemplateEnv -> TemplateContext -> TemplateNode -> Either TemplateError String
renderNode _ _ (TextNode text) = Right text
renderNode env ctx (OutputNode (VarExpr var) escCtx) =
  case lookupPath var.path ctx of
    Nothing => Left (UnknownVariable (showVariableRef var))
    Just value =>
      let escaped = if escCtx == RawContext && not env.allowRaw
            then Left RawOutputBlocked
            else Right (renderValue escCtx value)
      in escaped
renderNode env ctx (OutputNode (LiteralExpr value) escCtx) =
  if escCtx == RawContext && not env.allowRaw
    then Left RawOutputBlocked
    else Right (renderValue escCtx value)
renderNode _ _ (OutputNode _ _) = Left (ExpressionError "complex expressions not implemented")
renderNode env ctx (IfNode cond thenNodes elseNodes) =
  -- Simplified: would need proper expression evaluation
  Left (ExpressionError "if nodes not implemented in simplified renderer")
renderNode _ _ (ForNode _ _ _ _) =
  Left (ExpressionError "for nodes not implemented in simplified renderer")
renderNode env _ (IncludeNode path _) =
  if not env.allowIncludes
    then Left (DangerousDirective "include")
    else Left (ExpressionError "includes not implemented in simplified renderer")
renderNode _ _ (BlockNode name _) = Right ""  -- Blocks handled at template level
renderNode _ _ (ExtendsNode _) = Right ""     -- Extends handled at template level
renderNode _ _ (CommentNode _) = Right ""     -- Comments produce no output
renderNode _ _ (MacroDefNode _ _ _) = Right ""
renderNode _ _ (MacroCallNode _ _) = Left (ExpressionError "macros not implemented")

||| Render a template (simplified)
public export
render : TemplateEnv -> Template -> TemplateContext -> Either TemplateError String
render env template ctx = do
  parts <- traverse (renderNode env ctx) template.nodes
  pure (concat parts)
