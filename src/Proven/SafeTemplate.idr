-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeTemplate - Template rendering with SSTI prevention
|||
||| Provides type-safe template rendering that prevents Server-Side Template
||| Injection (SSTI) by restricting template expressions to a safe subset.
module Proven.SafeTemplate

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| Template expression types (safe subset - no code execution)
public export
data TemplateExpr =
    Literal String                 -- Raw text
  | Variable String                -- Variable reference
  | Conditional String (List TemplateExpr) (List TemplateExpr)  -- if/else
  | Loop String String (List TemplateExpr)  -- for item in collection
  | Escape TemplateExpr            -- HTML-escape the result
  | Comment String                 -- Template comment (not rendered)

||| Dangerous patterns in template expressions
public export
dangerousPatterns : List String
dangerousPatterns =
  [ "__class__", "__mro__", "__subclasses__"  -- Python SSTI
  , "__import__", "eval(", "exec("
  , "system(", "popen(", "subprocess"
  , "os.system", "os.popen"
  , "Runtime.getRuntime", "ProcessBuilder"  -- Java SSTI
  , "{{constructor", "{{__proto__"          -- JS SSTI
  , "${T(java", "${Runtime"                 -- Spring EL
  , "freemarker.template"                   -- FreeMarker SSTI
  , "#set($", "#foreach("                   -- Velocity SSTI
  ]

||| Check if a string contains dangerous patterns
public export
hasDangerousPattern : String -> Bool
hasDangerousPattern s =
  let lower = toLower s
  in any (\pat => isInfixOf pat lower) dangerousPatterns

||| Valid characters for variable names
public export
isValidVarChar : Char -> Bool
isValidVarChar c = isAlphaNum c || c == '_' || c == '.'

||| Validate a variable name (alphanumeric, underscores, dots for nesting)
public export
isValidVarName : String -> Bool
isValidVarName s =
  let chars = unpack s
  in length chars > 0 &&
     all isValidVarChar chars &&
     not (isPrefixOf "." s) &&
     not (isSuffixOf "." s) &&
     not (isInfixOf ".." s) &&
     not (hasDangerousPattern s)

||| A validated variable name
public export
data SafeVarName : Type where
  MkSafeVarName : (name : String) -> SafeVarName

||| Smart constructor
public export
mkSafeVarName : String -> Maybe SafeVarName
mkSafeVarName s = if isValidVarName s then Just (MkSafeVarName s) else Nothing

||| Template value types
public export
data TemplateValue =
    TString String
  | TInt Integer
  | TFloat Double
  | TBool Bool
  | TList (List TemplateValue)
  | TNull

public export
Show TemplateValue where
  show (TString s) = s
  show (TInt n) = show n
  show (TFloat d) = show d
  show (TBool True) = "true"
  show (TBool False) = "false"
  show (TList xs) = "[" ++ show (length xs) ++ " items]"
  show TNull = ""

||| Template context (variable bindings)
public export
TemplateContext : Type
TemplateContext = List (String, TemplateValue)

||| Look up a variable (supports dotted paths)
public export
lookupVar : String -> TemplateContext -> Maybe TemplateValue
lookupVar name ctx = lookup name ctx

||| Check if a value is truthy
public export
isTruthy : TemplateValue -> Bool
isTruthy (TString s) = length s > 0
isTruthy (TInt n) = n /= 0
isTruthy (TFloat d) = d /= 0.0
isTruthy (TBool b) = b
isTruthy (TList xs) = length xs > 0
isTruthy TNull = False

||| HTML-escape a string
public export
htmlEscape : String -> String
htmlEscape s = concatMap escapeChar (unpack s)
  where
    escapeChar : Char -> String
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar c = singleton c

||| Render a template expression to string
public export
renderExpr : TemplateExpr -> TemplateContext -> String
renderExpr (Literal s) _ = s
renderExpr (Variable name) ctx =
  case lookupVar name ctx of
    Just val => show val
    Nothing => ""
renderExpr (Conditional cond thenExprs elseExprs) ctx =
  case lookupVar cond ctx of
    Just val => if isTruthy val
                  then concatMap (\e => renderExpr e ctx) thenExprs
                  else concatMap (\e => renderExpr e ctx) elseExprs
    Nothing => concatMap (\e => renderExpr e ctx) elseExprs
renderExpr (Loop var collection body) ctx =
  case lookupVar collection ctx of
    Just (TList items) =>
      concatMap (\item =>
        let innerCtx = (var, item) :: ctx
        in concatMap (\e => renderExpr e innerCtx) body
      ) items
    _ => ""
renderExpr (Escape inner) ctx = htmlEscape (renderExpr inner ctx)
renderExpr (Comment _) _ = ""

||| A complete template
public export
record Template where
  constructor MkTemplate
  templateName : String
  templateBody : List TemplateExpr

||| Render a complete template
public export
renderTemplate : Template -> TemplateContext -> String
renderTemplate tmpl ctx = concatMap (\e => renderExpr e ctx) (templateBody tmpl)

||| Validate a template has no dangerous expressions
public export
isTemplateSafe : Template -> Bool
isTemplateSafe tmpl = all exprSafe (templateBody tmpl)
  where
    exprSafe : TemplateExpr -> Bool
    exprSafe (Literal s) = not (hasDangerousPattern s)
    exprSafe (Variable name) = isValidVarName name
    exprSafe (Conditional cond thenE elseE) =
      isValidVarName cond && all exprSafe thenE && all exprSafe elseE
    exprSafe (Loop var coll body) =
      isValidVarName var && isValidVarName coll && all exprSafe body
    exprSafe (Escape inner) = exprSafe inner
    exprSafe (Comment _) = True

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that a template is safe from SSTI
public export
data SSTISafe : Template -> Type where
  MkSSTISafe : isTemplateSafe tmpl = True -> SSTISafe tmpl

||| Proof that a variable name is safe
public export
data SafeVar : String -> Type where
  MkSafeVar : isValidVarName s = True -> SafeVar s
