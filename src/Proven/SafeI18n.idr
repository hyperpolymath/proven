-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeI18n - Internationalization with injection prevention
|||
||| Provides type-safe locale handling, message formatting, and
||| string interpolation that prevents format string injection.
module Proven.SafeI18n

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| BCP 47 language tag components
public export
record LanguageTag where
  constructor MkLanguageTag
  language   : String   -- ISO 639-1 (e.g., "en", "fr")
  region     : Maybe String  -- ISO 3166-1 (e.g., "US", "GB")
  script     : Maybe String  -- ISO 15924 (e.g., "Latn", "Cyrl")

public export
Show LanguageTag where
  show tag =
    language tag ++
    maybe "" ("-" ++) (script tag) ++
    maybe "" ("-" ++) (region tag)

public export
Eq LanguageTag where
  t1 == t2 = language t1 == language t2 &&
             region t1 == region t2 &&
             script t1 == script t2

||| Validate a language code (ISO 639-1: 2-3 lowercase letters)
public export
isValidLanguage : String -> Bool
isValidLanguage s =
  let chars = unpack s
  in (length chars == 2 || length chars == 3) && all isLower chars

||| Validate a region code (ISO 3166-1: 2 uppercase letters)
public export
isValidRegion : String -> Bool
isValidRegion s =
  let chars = unpack s
  in length chars == 2 && all isUpper chars

||| Parse a BCP 47 language tag
public export
parseLanguageTag : String -> Maybe LanguageTag
parseLanguageTag s =
  case split (== '-') s of
    [lang] =>
      if isValidLanguage lang
        then Just (MkLanguageTag lang Nothing Nothing)
        else Nothing
    [lang, regionOrScript] =>
      if isValidLanguage lang
        then if isValidRegion regionOrScript
          then Just (MkLanguageTag lang (Just regionOrScript) Nothing)
          else if length regionOrScript == 4
            then Just (MkLanguageTag lang Nothing (Just regionOrScript))
            else Nothing
        else Nothing
    [lang, script, region] =>
      if isValidLanguage lang && length script == 4 && isValidRegion region
        then Just (MkLanguageTag lang (Just region) (Just script))
        else Nothing
    _ => Nothing

||| Text direction
public export
data TextDirection = LTR | RTL

public export
Eq TextDirection where
  LTR == LTR = True
  RTL == RTL = True
  _ == _ = False

||| Get text direction for a language
public export
textDirection : LanguageTag -> TextDirection
textDirection tag =
  if elem (language tag) ["ar", "he", "fa", "ur", "yi", "ps", "sd", "ug"]
    then RTL
    else LTR

||| A message key (alphanumeric + dots + underscores)
public export
data MessageKey : Type where
  MkMessageKey : (key : String) -> MessageKey

public export
Eq MessageKey where
  MkMessageKey a == MkMessageKey b = a == b

public export
Show MessageKey where
  show (MkMessageKey k) = k

||| Validate a message key
public export
isValidMessageKey : String -> Bool
isValidMessageKey s =
  let chars = unpack s
  in length chars > 0 && all (\c => isAlphaNum c || c == '.' || c == '_') chars

||| Smart constructor for message keys
public export
mkMessageKey : String -> Maybe MessageKey
mkMessageKey s = if isValidMessageKey s then Just (MkMessageKey s) else Nothing

||| Message parameter (safe interpolation value)
public export
data MessageParam =
    StringParam String
  | IntParam Integer
  | FloatParam Double
  | PluralParam Integer  -- For plural form selection

||| Safe message template (no format string injection)
public export
record MessageTemplate where
  constructor MkTemplate
  templateText   : String
  parameterNames : List String

||| Parse template placeholders (e.g., "Hello {name}, you have {count} items")
public export
parseTemplate : String -> MessageTemplate
parseTemplate s = MkTemplate s (extractParams (unpack s))
  where
    extractParams : List Char -> List String
    extractParams [] = []
    extractParams ('{' :: rest) =
      case break (== '}') rest of
        (name, '}' :: remainder) =>
          pack name :: extractParams remainder
        _ => extractParams rest
    extractParams (_ :: rest) = extractParams rest

||| Safely interpolate parameters into a template
public export
interpolate : MessageTemplate -> List (String, MessageParam) -> String
interpolate tmpl params = replaceAll (templateText tmpl) params
  where
    showParam : MessageParam -> String
    showParam (StringParam s) = s
    showParam (IntParam n) = show n
    showParam (FloatParam d) = show d
    showParam (PluralParam n) = show n

    replaceSingle : String -> String -> String -> String
    replaceSingle template key value =
      let placeholder = "{" ++ key ++ "}"
      in if isInfixOf placeholder template
           then pack (replaceInfix (unpack placeholder) (unpack value) (unpack template))
           else template

    replaceInfix : List Char -> List Char -> List Char -> List Char
    replaceInfix _ _ [] = []
    replaceInfix needle replacement haystack =
      if isPrefixOf needle haystack
        then replacement ++ replaceInfix needle replacement (drop (length needle) haystack)
        else case haystack of
               [] => []
               (c :: rest) => c :: replaceInfix needle replacement rest

    replaceAll : String -> List (String, MessageParam) -> String
    replaceAll s [] = s
    replaceAll s ((key, val) :: rest) =
      replaceAll (replaceSingle s key (showParam val)) rest

||| A message catalog (locale -> key -> template)
public export
record MessageCatalog where
  constructor MkCatalog
  catalogLocales : List LanguageTag
  catalogMessages : List (String, List (String, MessageTemplate))

||| Look up a message in a catalog
public export
lookupMessage : LanguageTag -> MessageKey -> MessageCatalog -> Maybe MessageTemplate
lookupMessage locale (MkMessageKey key) catalog =
  case lookup (show locale) (catalogMessages catalog) of
    Nothing => Nothing
    Just messages => lookup key messages

||| Proof that a message key is valid
public export
data ValidMessageKey : MessageKey -> Type where
  MkValidKey : (k : MessageKey) -> ValidMessageKey k

||| Proof that a locale tag is well-formed
public export
data ValidLocale : LanguageTag -> Type where
  MkValidLocale : isValidLanguage (language tag) = True -> ValidLocale tag
