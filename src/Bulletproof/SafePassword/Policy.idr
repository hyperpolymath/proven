-- SPDX-License-Identifier: Palimpsest-MPL
||| Password Policy Types and Validation
|||
||| Defines configurable password policies and validation logic.
module Bulletproof.SafePassword.Policy

import Bulletproof.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Policy Configuration
--------------------------------------------------------------------------------

||| Password policy requirements
public export
record PasswordPolicy where
  constructor MkPolicy
  minLength : Nat
  maxLength : Nat
  requireUppercase : Bool
  requireLowercase : Bool
  requireDigit : Bool
  requireSymbol : Bool
  minUniqueChars : Nat
  maxRepeatedChars : Nat      -- Max consecutive repeated chars
  forbiddenPatterns : List String
  forbiddenWords : List String

||| Default secure policy (NIST SP 800-63B inspired)
public export
defaultPolicy : PasswordPolicy
defaultPolicy = MkPolicy
  { minLength = 8
  , maxLength = 128
  , requireUppercase = False  -- NIST doesn't require complexity
  , requireLowercase = False
  , requireDigit = False
  , requireSymbol = False
  , minUniqueChars = 4
  , maxRepeatedChars = 3
  , forbiddenPatterns = ["password", "123456", "qwerty"]
  , forbiddenWords = []
  }

||| Strict policy (traditional enterprise)
public export
strictPolicy : PasswordPolicy
strictPolicy = MkPolicy
  { minLength = 12
  , maxLength = 128
  , requireUppercase = True
  , requireLowercase = True
  , requireDigit = True
  , requireSymbol = True
  , minUniqueChars = 8
  , maxRepeatedChars = 2
  , forbiddenPatterns = ["password", "123456", "qwerty", "letmein", "admin"]
  , forbiddenWords = []
  }

||| Passphrase policy (longer, simpler)
public export
passphrasePolicy : PasswordPolicy
passphrasePolicy = MkPolicy
  { minLength = 20
  , maxLength = 256
  , requireUppercase = False
  , requireLowercase = False
  , requireDigit = False
  , requireSymbol = False
  , minUniqueChars = 10
  , maxRepeatedChars = 4
  , forbiddenPatterns = []
  , forbiddenWords = []
  }

--------------------------------------------------------------------------------
-- Policy Violations
--------------------------------------------------------------------------------

||| Specific policy violation types
public export
data PolicyViolation
  = TooShort Nat Nat              -- actual, required
  | TooLong Nat Nat               -- actual, max
  | MissingUppercase
  | MissingLowercase
  | MissingDigit
  | MissingSymbol
  | InsufficientUniqueChars Nat Nat  -- actual, required
  | TooManyRepeatedChars Nat Nat     -- found, max
  | ContainsForbiddenPattern String
  | ContainsForbiddenWord String
  | ContainsWhitespace

public export
Show PolicyViolation where
  show (TooShort actual req) =
    "Password too short: " ++ show actual ++ " chars (minimum " ++ show req ++ ")"
  show (TooLong actual max) =
    "Password too long: " ++ show actual ++ " chars (maximum " ++ show max ++ ")"
  show MissingUppercase = "Password must contain uppercase letter"
  show MissingLowercase = "Password must contain lowercase letter"
  show MissingDigit = "Password must contain digit"
  show MissingSymbol = "Password must contain symbol"
  show (InsufficientUniqueChars actual req) =
    "Password needs " ++ show req ++ " unique chars (has " ++ show actual ++ ")"
  show (TooManyRepeatedChars found max) =
    "Too many repeated chars: " ++ show found ++ " (max " ++ show max ++ ")"
  show (ContainsForbiddenPattern p) =
    "Password contains forbidden pattern: " ++ p
  show (ContainsForbiddenWord w) =
    "Password contains forbidden word: " ++ w
  show ContainsWhitespace =
    "Password contains whitespace"

--------------------------------------------------------------------------------
-- Policy Checking
--------------------------------------------------------------------------------

||| Proof type that a password meets a policy
public export
data MeetsPolicy : (policy : PasswordPolicy) -> (password : String) -> Type where
  PolicyMet : MeetsPolicy policy password

||| Check password against policy, returning all violations
public export
checkPolicy : PasswordPolicy -> String -> List PolicyViolation
checkPolicy policy pwd =
  let chars = unpack pwd
      len = length chars
  in catMaybes
    [ checkLength len policy.minLength policy.maxLength
    , checkUppercase chars policy.requireUppercase
    , checkLowercase chars policy.requireLowercase
    , checkDigit chars policy.requireDigit
    , checkSymbol chars policy.requireSymbol
    , checkUniqueChars chars policy.minUniqueChars
    , checkRepeatedChars chars policy.maxRepeatedChars
    , checkWhitespace chars
    ] ++ checkForbiddenPatterns (toLower pwd) policy.forbiddenPatterns
      ++ checkForbiddenWords (toLower pwd) policy.forbiddenWords
  where
    checkLength : Nat -> Nat -> Nat -> Maybe PolicyViolation
    checkLength len minLen maxLen =
      if len < minLen then Just (TooShort len minLen)
      else if len > maxLen then Just (TooLong len maxLen)
      else Nothing

    checkUppercase : List Char -> Bool -> Maybe PolicyViolation
    checkUppercase chars True =
      if any isUpper chars then Nothing else Just MissingUppercase
    checkUppercase _ False = Nothing

    checkLowercase : List Char -> Bool -> Maybe PolicyViolation
    checkLowercase chars True =
      if any isLower chars then Nothing else Just MissingLowercase
    checkLowercase _ False = Nothing

    checkDigit : List Char -> Bool -> Maybe PolicyViolation
    checkDigit chars True =
      if any isDigit chars then Nothing else Just MissingDigit
    checkDigit _ False = Nothing

    checkSymbol : List Char -> Bool -> Maybe PolicyViolation
    checkSymbol chars True =
      if any (\c => not (isAlphaNum c) && not (isSpace c)) chars
        then Nothing
        else Just MissingSymbol
    checkSymbol _ False = Nothing

    checkUniqueChars : List Char -> Nat -> Maybe PolicyViolation
    checkUniqueChars chars minUnique =
      let unique = length (nub chars)
      in if unique < minUnique
           then Just (InsufficientUniqueChars unique minUnique)
           else Nothing

    checkRepeatedChars : List Char -> Nat -> Maybe PolicyViolation
    checkRepeatedChars chars maxRepeat =
      let maxFound = maxConsecutiveRepeats chars
      in if maxFound > maxRepeat
           then Just (TooManyRepeatedChars maxFound maxRepeat)
           else Nothing

    maxConsecutiveRepeats : List Char -> Nat
    maxConsecutiveRepeats [] = 0
    maxConsecutiveRepeats (c :: cs) = go c 1 1 cs
      where
        go : Char -> Nat -> Nat -> List Char -> Nat
        go _ _ maxSoFar [] = maxSoFar
        go prev count maxSoFar (x :: xs) =
          if x == prev
            then go prev (S count) (max maxSoFar (S count)) xs
            else go x 1 maxSoFar xs

    checkWhitespace : List Char -> Maybe PolicyViolation
    checkWhitespace chars =
      if any isSpace chars then Just ContainsWhitespace else Nothing

    checkForbiddenPatterns : String -> List String -> List PolicyViolation
    checkForbiddenPatterns pwd patterns =
      map ContainsForbiddenPattern (filter (`isInfixOf` pwd) patterns)

    checkForbiddenWords : String -> List String -> List PolicyViolation
    checkForbiddenWords pwd words =
      map ContainsForbiddenWord (filter (`isInfixOf` pwd) words)

--------------------------------------------------------------------------------
-- Policy Builder
--------------------------------------------------------------------------------

||| Fluent API for building policies
public export
record PolicyBuilder where
  constructor MkPolicyBuilder
  policy : PasswordPolicy

||| Start with default policy
public export
policyBuilder : PolicyBuilder
policyBuilder = MkPolicyBuilder defaultPolicy

||| Set minimum length
public export
withMinLength : Nat -> PolicyBuilder -> PolicyBuilder
withMinLength n (MkPolicyBuilder p) =
  MkPolicyBuilder ({ minLength := n } p)

||| Set maximum length
public export
withMaxLength : Nat -> PolicyBuilder -> PolicyBuilder
withMaxLength n (MkPolicyBuilder p) =
  MkPolicyBuilder ({ maxLength := n } p)

||| Require uppercase
public export
withUppercase : PolicyBuilder -> PolicyBuilder
withUppercase (MkPolicyBuilder p) =
  MkPolicyBuilder ({ requireUppercase := True } p)

||| Require lowercase
public export
withLowercase : PolicyBuilder -> PolicyBuilder
withLowercase (MkPolicyBuilder p) =
  MkPolicyBuilder ({ requireLowercase := True } p)

||| Require digit
public export
withDigit : PolicyBuilder -> PolicyBuilder
withDigit (MkPolicyBuilder p) =
  MkPolicyBuilder ({ requireDigit := True } p)

||| Require symbol
public export
withSymbol : PolicyBuilder -> PolicyBuilder
withSymbol (MkPolicyBuilder p) =
  MkPolicyBuilder ({ requireSymbol := True } p)

||| Set minimum unique characters
public export
withMinUniqueChars : Nat -> PolicyBuilder -> PolicyBuilder
withMinUniqueChars n (MkPolicyBuilder p) =
  MkPolicyBuilder ({ minUniqueChars := n } p)

||| Add forbidden pattern
public export
withForbiddenPattern : String -> PolicyBuilder -> PolicyBuilder
withForbiddenPattern pattern (MkPolicyBuilder p) =
  MkPolicyBuilder ({ forbiddenPatterns $= (pattern ::) } p)

||| Add forbidden word (e.g., username, company name)
public export
withForbiddenWord : String -> PolicyBuilder -> PolicyBuilder
withForbiddenWord word (MkPolicyBuilder p) =
  MkPolicyBuilder ({ forbiddenWords $= (toLower word ::) } p)

||| Build the final policy
public export
build : PolicyBuilder -> PasswordPolicy
build (MkPolicyBuilder p) = p

--------------------------------------------------------------------------------
-- Policy Presets
--------------------------------------------------------------------------------

||| NIST SP 800-63B compliant policy
public export
nistPolicy : PasswordPolicy
nistPolicy = build $
  policyBuilder
    |> withMinLength 8
    |> withMaxLength 64
    |> withMinUniqueChars 1

||| PCI-DSS compliant policy
public export
pciDssPolicy : PasswordPolicy
pciDssPolicy = build $
  policyBuilder
    |> withMinLength 7
    |> withUppercase
    |> withLowercase
    |> withDigit

||| HIPAA compliant policy
public export
hipaaPolicy : PasswordPolicy
hipaaPolicy = build $
  policyBuilder
    |> withMinLength 8
    |> withUppercase
    |> withLowercase
    |> withDigit
    |> withSymbol
    |> withMinUniqueChars 6

||| Apply function (flip ($))
infixl 1 |>
public export
(|>) : a -> (a -> b) -> b
x |> f = f x
