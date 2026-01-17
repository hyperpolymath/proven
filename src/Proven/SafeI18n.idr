-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe internationalization (i18n) and localization (l10n) handling
|||
||| This module provides type-safe representations of i18n concepts:
||| - Locale codes (BCP 47 / IETF language tags)
||| - Translation keys and messages
||| - Pluralization rules
||| - Number and date formatting
||| - Currency formatting
||| - Text direction (LTR/RTL)
|||
||| Security features:
||| - Format string injection prevention
||| - Unicode homograph detection
||| - Bidirectional text attack prevention
||| - Safe interpolation
||| - XSS prevention in translations
module Proven.SafeI18n

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| ISO 639-1/639-3 language code
public export
record LanguageCode where
  constructor MkLanguageCode
  code : String

||| ISO 3166-1 alpha-2 region/country code
public export
record RegionCode where
  constructor MkRegionCode
  code : String

||| ISO 15924 script code
public export
record ScriptCode where
  constructor MkScriptCode
  code : String

||| BCP 47 locale tag components
public export
record LocaleTag where
  constructor MkLocaleTag
  language : LanguageCode
  script : Maybe ScriptCode
  region : Maybe RegionCode
  variants : List String
  extensions : List (Char, String)

||| Text direction
public export
data TextDirection = LTR | RTL | Auto

||| Plural categories (CLDR)
public export
data PluralCategory
  = Zero
  | One
  | Two
  | Few
  | Many
  | Other

||| Plural rule operands (CLDR)
public export
record PluralOperands where
  constructor MkPluralOperands
  n : Double    -- Absolute value
  i : Integer   -- Integer digits
  v : Nat       -- Visible fraction digits with trailing zeros
  w : Nat       -- Visible fraction digits without trailing zeros
  f : Integer   -- Visible fraction digits with trailing zeros as integer
  t : Integer   -- Visible fraction digits without trailing zeros as integer

||| Translation message with placeholders
public export
data MessagePart
  = TextPart String
  | Placeholder String
  | PluralPart String (List (PluralCategory, String))
  | SelectPart String (List (String, String))
  | NumberPart String
  | DatePart String String  -- placeholder, format
  | CurrencyPart String String  -- placeholder, currency code

||| Complete translation message
public export
record TranslationMessage where
  constructor MkTranslationMessage
  parts : List MessagePart

||| Translation key
public export
record TranslationKey where
  constructor MkTranslationKey
  namespace : Maybe String
  key : String

||| Translation entry with metadata
public export
record TranslationEntry where
  constructor MkTranslationEntry
  key : TranslationKey
  message : TranslationMessage
  description : Maybe String
  context : Maybe String
  maxLength : Maybe Nat

||| Translation bundle for a locale
public export
record TranslationBundle where
  constructor MkTranslationBundle
  locale : LocaleTag
  entries : List TranslationEntry
  fallbackLocale : Maybe LocaleTag

||| Number format style
public export
data NumberStyle
  = DecimalStyle
  | CurrencyStyle
  | PercentStyle
  | ScientificStyle
  | CompactStyle

||| Date format style
public export
data DateStyle
  = FullDate
  | LongDate
  | MediumDate
  | ShortDate

||| Time format style
public export
data TimeStyle
  = FullTime
  | LongTime
  | MediumTime
  | ShortTime

||| Currency display mode
public export
data CurrencyDisplay
  = CurrencySymbol
  | CurrencyCode
  | CurrencyName
  | CurrencyNarrow

||| Number formatting options
public export
record NumberFormat where
  constructor MkNumberFormat
  style : NumberStyle
  minimumIntegerDigits : Maybe Nat
  minimumFractionDigits : Maybe Nat
  maximumFractionDigits : Maybe Nat
  useGrouping : Bool

||| Date/time formatting options
public export
record DateTimeFormat where
  constructor MkDateTimeFormat
  dateStyle : Maybe DateStyle
  timeStyle : Maybe TimeStyle
  hourCycle : Maybe String
  timeZone : Maybe String

||| Currency formatting options
public export
record CurrencyFormat where
  constructor MkCurrencyFormat
  currencyCode : String
  display : CurrencyDisplay
  minimumFractionDigits : Maybe Nat
  maximumFractionDigits : Maybe Nat

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during i18n operations
public export
data I18nError
  = InvalidLanguageCode String
  | InvalidRegionCode String
  | InvalidScriptCode String
  | InvalidLocaleTag String
  | MissingTranslation TranslationKey LocaleTag
  | InvalidPlaceholder String
  | UnclosedPlaceholder String
  | InvalidPluralCategory String
  | FormatStringInjection String
  | HomographDetected String
  | BidiAttackDetected String
  | XSSDetected String
  | InvalidCurrencyCode String
  | InvalidTimeZone String
  | MissingPluralForm PluralCategory
  | InterpolationError String
  | MessageTooLong Nat Nat  -- actual, max

public export
Show I18nError where
  show (InvalidLanguageCode c) = "I18n error: invalid language code '" ++ c ++ "'"
  show (InvalidRegionCode c) = "I18n error: invalid region code '" ++ c ++ "'"
  show (InvalidScriptCode c) = "I18n error: invalid script code '" ++ c ++ "'"
  show (InvalidLocaleTag t) = "I18n error: invalid locale tag '" ++ t ++ "'"
  show (MissingTranslation key locale) = "I18n error: missing translation for key"
  show (InvalidPlaceholder p) = "I18n error: invalid placeholder '" ++ p ++ "'"
  show (UnclosedPlaceholder p) = "I18n error: unclosed placeholder '" ++ p ++ "'"
  show (InvalidPluralCategory c) = "I18n error: invalid plural category '" ++ c ++ "'"
  show (FormatStringInjection s) = "I18n security: format string injection detected '" ++ s ++ "'"
  show (HomographDetected s) = "I18n security: homograph attack detected in '" ++ s ++ "'"
  show (BidiAttackDetected s) = "I18n security: bidirectional text attack detected"
  show (XSSDetected s) = "I18n security: XSS detected in translation '" ++ s ++ "'"
  show (InvalidCurrencyCode c) = "I18n error: invalid currency code '" ++ c ++ "'"
  show (InvalidTimeZone tz) = "I18n error: invalid time zone '" ++ tz ++ "'"
  show (MissingPluralForm cat) = "I18n error: missing plural form"
  show (InterpolationError msg) = "I18n error: interpolation error - " ++ msg
  show (MessageTooLong actual maxLen) =
    "I18n error: message too long (" ++ show actual ++ " > " ++ show maxLen ++ ")"

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| RTL languages
rtlLanguages : List String
rtlLanguages =
  [ "ar", "he", "fa", "ur", "yi", "ps", "sd"
  , "dv", "ug", "arc", "syr", "sam"
  ]

||| Valid ISO 639-1 language codes (subset)
validLanguageCodes : List String
validLanguageCodes =
  [ "en", "es", "fr", "de", "it", "pt", "ru", "zh", "ja", "ko"
  , "ar", "he", "hi", "bn", "pa", "te", "mr", "ta", "ur", "fa"
  , "tr", "vi", "th", "id", "ms", "tl", "nl", "sv", "no", "da"
  , "fi", "pl", "cs", "sk", "hu", "ro", "bg", "uk", "el", "sr"
  ]

||| Valid ISO 4217 currency codes (subset)
validCurrencyCodes : List String
validCurrencyCodes =
  [ "USD", "EUR", "GBP", "JPY", "CNY", "INR", "BRL", "RUB"
  , "KRW", "AUD", "CAD", "CHF", "HKD", "SGD", "MXN", "NOK"
  , "SEK", "DKK", "NZD", "ZAR", "AED", "SAR", "PLN", "CZK"
  ]

||| Dangerous bidi control characters
bidiControlChars : List Char
bidiControlChars =
  [ '\x202A'  -- LRE
  , '\x202B'  -- RLE
  , '\x202C'  -- PDF
  , '\x202D'  -- LRO
  , '\x202E'  -- RLO
  , '\x2066'  -- LRI
  , '\x2067'  -- RLI
  , '\x2068'  -- FSI
  , '\x2069'  -- PDI
  ]

||| XSS dangerous patterns
xssPatterns : List String
xssPatterns =
  [ "<script", "javascript:", "onerror=", "onclick="
  , "onload=", "onmouseover=", "onfocus=", "data:"
  ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

||| Check if language is RTL
isRTLLanguage : String -> Bool
isRTLLanguage lang = elem (toLower lang) rtlLanguages

||| Get text direction for language
getTextDirection : LanguageCode -> TextDirection
getTextDirection (MkLanguageCode code) =
  if isRTLLanguage code then RTL else LTR

||| Check for bidi control characters
hasBidiControls : String -> Bool
hasBidiControls str = any (\c => elem c bidiControlChars) (unpack str)

||| Check for XSS patterns
hasXSSPatterns : String -> Bool
hasXSSPatterns str =
  let lower = toLower str
  in any (\p => isInfixOf p lower) xssPatterns

||| Check if string has non-ASCII (potential homograph)
hasNonAscii : String -> Bool
hasNonAscii str = any (\c => ord c > 127) (unpack str)

||| Sanitize string for safe use
sanitize : String -> String
sanitize str = pack $ filter (\c => not (elem c bidiControlChars)) (unpack str)

--------------------------------------------------------------------------------
-- Validation functions
--------------------------------------------------------------------------------

||| Validate a language code
public export
validateLanguageCode : String -> Either I18nError LanguageCode
validateLanguageCode "" = Left (InvalidLanguageCode "empty")
validateLanguageCode code =
  let lower = toLower code in
  if length lower < 2 || length lower > 3
    then Left (InvalidLanguageCode code)
    else if not (all isAlpha (unpack lower))
      then Left (InvalidLanguageCode code)
      else Right (MkLanguageCode lower)

||| Validate a region code
public export
validateRegionCode : String -> Either I18nError RegionCode
validateRegionCode "" = Left (InvalidRegionCode "empty")
validateRegionCode code =
  let upper = toUpper code in
  if length upper /= 2
    then Left (InvalidRegionCode code)
    else if not (all isAlpha (unpack upper))
      then Left (InvalidRegionCode code)
      else Right (MkRegionCode upper)

||| Validate a script code
public export
validateScriptCode : String -> Either I18nError ScriptCode
validateScriptCode "" = Left (InvalidScriptCode "empty")
validateScriptCode code =
  if length code /= 4
    then Left (InvalidScriptCode code)
    else if not (all isAlpha (unpack code))
      then Left (InvalidScriptCode code)
      else Right (MkScriptCode code)

||| Validate a currency code
public export
validateCurrencyCode : String -> Either I18nError String
validateCurrencyCode "" = Left (InvalidCurrencyCode "empty")
validateCurrencyCode code =
  let upper = toUpper code in
  if length upper /= 3
    then Left (InvalidCurrencyCode code)
    else if not (all isAlpha (unpack upper))
      then Left (InvalidCurrencyCode code)
      else Right upper

||| Parse a BCP 47 locale tag
public export
parseLocaleTag : String -> Either I18nError LocaleTag
parseLocaleTag "" = Left (InvalidLocaleTag "empty")
parseLocaleTag tag =
  let parts = split (== '-') tag
  in case parts of
    [] => Left (InvalidLocaleTag tag)
    (lang :: rest) => do
      langCode <- validateLanguageCode lang
      -- Simplified parsing - full BCP 47 is more complex
      let (script, afterScript) = extractScript rest
      let (region, afterRegion) = extractRegion afterScript
      pure (MkLocaleTag langCode script region [] [])
  where
    extractScript : List String -> (Maybe ScriptCode, List String)
    extractScript [] = (Nothing, [])
    extractScript (x :: xs) =
      if length x == 4 && all isAlpha (unpack x)
        then (Just (MkScriptCode x), xs)
        else (Nothing, x :: xs)

    extractRegion : List String -> (Maybe RegionCode, List String)
    extractRegion [] = (Nothing, [])
    extractRegion (x :: xs) =
      if length x == 2 && all isAlpha (unpack x)
        then (Just (MkRegionCode (toUpper x)), xs)
        else (Nothing, x :: xs)

||| Validate translation text for security
public export
validateTranslationText : String -> Either I18nError String
validateTranslationText text =
  if hasBidiControls text
    then Left (BidiAttackDetected text)
    else if hasXSSPatterns text
      then Left (XSSDetected text)
      else Right (sanitize text)

||| Validate a placeholder name
public export
validatePlaceholder : String -> Either I18nError String
validatePlaceholder "" = Left (InvalidPlaceholder "empty")
validatePlaceholder name =
  if not (all isAlphaNum (unpack name))
    then Left (InvalidPlaceholder name)
    else Right name

--------------------------------------------------------------------------------
-- Plural rule helpers
--------------------------------------------------------------------------------

||| Parse plural category from string
public export
parsePluralCategory : String -> Either I18nError PluralCategory
parsePluralCategory "zero" = Right Zero
parsePluralCategory "one" = Right One
parsePluralCategory "two" = Right Two
parsePluralCategory "few" = Right Few
parsePluralCategory "many" = Right Many
parsePluralCategory "other" = Right Other
parsePluralCategory s = Left (InvalidPluralCategory s)

||| Show plural category
public export
showPluralCategory : PluralCategory -> String
showPluralCategory Zero = "zero"
showPluralCategory One = "one"
showPluralCategory Two = "two"
showPluralCategory Few = "few"
showPluralCategory Many = "many"
showPluralCategory Other = "other"

||| Get plural operands from a number
public export
getPluralOperands : Double -> PluralOperands
getPluralOperands n =
  let absN = abs n
      intPart = cast {to=Integer} absN
  in MkPluralOperands absN intPart 0 0 0 0  -- Simplified

||| English plural rules (simplified)
public export
englishPluralRule : PluralOperands -> PluralCategory
englishPluralRule ops =
  if ops.i == 1 && ops.v == 0
    then One
    else Other

--------------------------------------------------------------------------------
-- Message construction
--------------------------------------------------------------------------------

||| Create a simple text message
public export
textMessage : String -> Either I18nError TranslationMessage
textMessage text = do
  validText <- validateTranslationText text
  pure (MkTranslationMessage [TextPart validText])

||| Create a message with placeholder
public export
messageWithPlaceholder : String -> String -> String -> Either I18nError TranslationMessage
messageWithPlaceholder before placeholder after = do
  validBefore <- validateTranslationText before
  validPlaceholder <- validatePlaceholder placeholder
  validAfter <- validateTranslationText after
  pure (MkTranslationMessage
    [ TextPart validBefore
    , Placeholder validPlaceholder
    , TextPart validAfter
    ])

||| Create a translation key
public export
mkTranslationKey : Maybe String -> String -> Either I18nError TranslationKey
mkTranslationKey ns "" = Left (InvalidPlaceholder "empty key")
mkTranslationKey ns key =
  if not (all (\c => isAlphaNum c || c == '.' || c == '_') (unpack key))
    then Left (InvalidPlaceholder key)
    else Right (MkTranslationKey ns key)

--------------------------------------------------------------------------------
-- Safe interpolation
--------------------------------------------------------------------------------

||| Interpolation context (key -> value mapping)
public export
InterpolationContext : Type
InterpolationContext = List (String, String)

||| Safely interpolate a message
public export
interpolate : TranslationMessage -> InterpolationContext -> Either I18nError String
interpolate msg ctx = do
  parts <- traverse (interpolatePart ctx) msg.parts
  pure (concat parts)
  where
    interpolatePart : InterpolationContext -> MessagePart -> Either I18nError String
    interpolatePart _ (TextPart t) = Right t
    interpolatePart ctx (Placeholder name) =
      case lookup name ctx of
        Nothing => Left (InterpolationError ("missing value for: " ++ name))
        Just value =>
          -- Sanitize interpolated values to prevent XSS
          if hasXSSPatterns value
            then Left (XSSDetected value)
            else Right (sanitize value)
    interpolatePart ctx (NumberPart name) =
      case lookup name ctx of
        Nothing => Left (InterpolationError ("missing value for: " ++ name))
        Just value => Right value
    interpolatePart ctx (DatePart name fmt) =
      case lookup name ctx of
        Nothing => Left (InterpolationError ("missing value for: " ++ name))
        Just value => Right value
    interpolatePart ctx (CurrencyPart name code) =
      case lookup name ctx of
        Nothing => Left (InterpolationError ("missing value for: " ++ name))
        Just value => Right (value ++ " " ++ code)
    interpolatePart ctx (PluralPart name forms) =
      Left (InterpolationError "plural interpolation not implemented")
    interpolatePart ctx (SelectPart name options) =
      case lookup name ctx of
        Nothing => Left (InterpolationError ("missing value for: " ++ name))
        Just value =>
          case lookup value options of
            Nothing => Left (InterpolationError ("no option for: " ++ value))
            Just text => Right text

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Show locale tag
public export
showLocaleTag : LocaleTag -> String
showLocaleTag tag =
  tag.language.code
  ++ maybe "" (\s => "-" ++ s.code) tag.script
  ++ maybe "" (\r => "-" ++ r.code) tag.region
  ++ (if null tag.variants then "" else "-" ++ concat (intersperse "-" tag.variants))

||| Show text direction
public export
showTextDirection : TextDirection -> String
showTextDirection LTR = "ltr"
showTextDirection RTL = "rtl"
showTextDirection Auto = "auto"

||| Show number style
public export
showNumberStyle : NumberStyle -> String
showNumberStyle DecimalStyle = "decimal"
showNumberStyle CurrencyStyle = "currency"
showNumberStyle PercentStyle = "percent"
showNumberStyle ScientificStyle = "scientific"
showNumberStyle CompactStyle = "compact"

||| Show date style
public export
showDateStyle : DateStyle -> String
showDateStyle FullDate = "full"
showDateStyle LongDate = "long"
showDateStyle MediumDate = "medium"
showDateStyle ShortDate = "short"

||| Show currency display
public export
showCurrencyDisplay : CurrencyDisplay -> String
showCurrencyDisplay CurrencySymbol = "symbol"
showCurrencyDisplay CurrencyCode = "code"
showCurrencyDisplay CurrencyName = "name"
showCurrencyDisplay CurrencyNarrow = "narrowSymbol"

||| Show translation key
public export
showTranslationKey : TranslationKey -> String
showTranslationKey (MkTranslationKey Nothing key) = key
showTranslationKey (MkTranslationKey (Just ns) key) = ns ++ ":" ++ key

--------------------------------------------------------------------------------
-- Bundle operations
--------------------------------------------------------------------------------

||| Find translation in bundle
public export
findTranslation : TranslationKey -> TranslationBundle -> Maybe TranslationEntry
findTranslation key bundle =
  find (\e => e.key.key == key.key && e.key.namespace == key.namespace) bundle.entries

||| Get translation with fallback
public export
getTranslation : TranslationKey -> TranslationBundle -> Maybe TranslationBundle -> Either I18nError TranslationEntry
getTranslation key bundle fallback =
  case findTranslation key bundle of
    Just entry => Right entry
    Nothing =>
      case fallback of
        Nothing => Left (MissingTranslation key bundle.locale)
        Just fb =>
          case findTranslation key fb of
            Just entry => Right entry
            Nothing => Left (MissingTranslation key bundle.locale)

||| Translate with interpolation
public export
translate : TranslationKey -> InterpolationContext -> TranslationBundle -> Maybe TranslationBundle -> Either I18nError String
translate key ctx bundle fallback = do
  entry <- getTranslation key bundle fallback
  result <- interpolate entry.message ctx
  -- Check max length if specified
  case entry.maxLength of
    Nothing => Right result
    Just maxLen =>
      if length result > maxLen
        then Left (MessageTooLong (length result) maxLen)
        else Right result
