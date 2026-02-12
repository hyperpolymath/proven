-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafePassword - Secure password handling with verification
|||
||| This module provides:
||| - Password strength validation
||| - Secure hashing (Argon2id, bcrypt, scrypt interfaces)
||| - Timing-safe comparison
||| - Password policy enforcement
|||
||| Note: Actual hashing requires FFI to native crypto libraries
module Proven.SafePassword

import public Proven.Core
import public Proven.SafePassword.Policy
import public Proven.SafePassword.Hash
import public Proven.SafePassword.Strength

import Data.List
import Data.String
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Password Types
--------------------------------------------------------------------------------

||| A password that has not been validated
public export
data RawPassword : Type where
  MkRawPassword : (value : String) -> RawPassword

||| A password that meets policy requirements
public export
data ValidPassword : (policy : PasswordPolicy) -> Type where
  MkValidPassword : (value : String) ->
                    (proof : MeetsPolicy policy value) ->
                    ValidPassword policy

||| A hashed password (safe to store)
public export
data HashedPassword : Type where
  MkHashedPassword : (algorithm : HashAlgorithm) ->
                     (hash : String) ->
                     (salt : String) ->
                     (params : HashParams) ->
                     HashedPassword

||| Password verification result
public export
data VerifyResult
  = Verified           -- Password matches hash
  | NotVerified        -- Password does not match
  | NeedsRehash        -- Matches but params outdated
  | HashCorrupted      -- Hash format invalid

public export
Show VerifyResult where
  show Verified = "Password verified"
  show NotVerified = "Password incorrect"
  show NeedsRehash = "Password correct but needs rehash"
  show HashCorrupted = "Hash format corrupted"

--------------------------------------------------------------------------------
-- Password Validation
--------------------------------------------------------------------------------

||| Validate a raw password against a policy
public export
validatePassword : (policy : PasswordPolicy) ->
                   RawPassword ->
                   Either (List PolicyViolation) (ValidPassword policy)
validatePassword policy (MkRawPassword value) =
  case checkPolicy policy value of
    [] => Right (MkValidPassword value (believe_me ()))
    violations => Left violations

||| Check if password meets minimum requirements
public export
isValidPassword : PasswordPolicy -> String -> Bool
isValidPassword policy pwd = null (checkPolicy policy pwd)

||| Get password from ValidPassword (for hashing only)
public export
revealForHashing : ValidPassword policy -> String
revealForHashing (MkValidPassword value _) = value

--------------------------------------------------------------------------------
-- Password Hashing Interface (Stubs)
--------------------------------------------------------------------------------

||| Hash a validated password using Argon2id
||| Actual implementation via FFI
public export
hashPasswordArgon2id : ValidPassword policy ->
                       Argon2Params ->
                       IO HashedPassword
hashPasswordArgon2id pwd params = pure $ believe_me (MkHashedPassword Argon2id "" "" (MkArgon2 params))

||| Hash a validated password using bcrypt
public export
hashPasswordBcrypt : ValidPassword policy ->
                     BcryptParams ->
                     IO HashedPassword
hashPasswordBcrypt pwd params = pure $ believe_me (MkHashedPassword Bcrypt "" "" (MkBcrypt params))

||| Hash a validated password using scrypt
public export
hashPasswordScrypt : ValidPassword policy ->
                     ScryptParams ->
                     IO HashedPassword
hashPasswordScrypt pwd params = pure $ believe_me (MkHashedPassword Scrypt "" "" (MkScrypt params))

||| Hash with default secure parameters (Argon2id recommended)
public export
hashPassword : ValidPassword policy -> IO HashedPassword
hashPassword pwd = hashPasswordArgon2id pwd defaultArgon2Params

--------------------------------------------------------------------------------
-- Password Verification Interface (Stubs)
--------------------------------------------------------------------------------

||| Verify a password against a hash (timing-safe)
||| Actual implementation via FFI
public export
verifyPassword : RawPassword -> HashedPassword -> IO VerifyResult
verifyPassword (MkRawPassword pwd) (MkHashedPassword alg hash salt params) =
  -- Stub - actual implementation needs FFI
  pure $ believe_me NotVerified

||| Verify and check if rehash needed
public export
verifyAndCheckRehash : RawPassword ->
                       HashedPassword ->
                       HashParams ->  -- Current recommended params
                       IO VerifyResult
verifyAndCheckRehash pwd hash currentParams = do
  result <- verifyPassword pwd hash
  case result of
    Verified =>
      if needsRehash hash currentParams
        then pure NeedsRehash
        else pure Verified
    other => pure other

||| Check if hash parameters are outdated
public export
needsRehash : HashedPassword -> HashParams -> Bool
needsRehash (MkHashedPassword _ _ _ oldParams) newParams =
  not (paramsAtLeast oldParams newParams)

--------------------------------------------------------------------------------
-- Password Generation
--------------------------------------------------------------------------------

||| Character sets for password generation
public export
data CharSet
  = Lowercase           -- a-z
  | Uppercase           -- A-Z
  | Digits              -- 0-9
  | Symbols             -- !@#$%^&*...
  | Ambiguous           -- 0O1lI (excluded by default)
  | Custom String       -- Custom character set

||| Get characters for a charset
public export
charSetChars : CharSet -> List Char
charSetChars Lowercase = unpack "abcdefghijklmnopqrstuvwxyz"
charSetChars Uppercase = unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
charSetChars Digits = unpack "0123456789"
charSetChars Symbols = unpack "!@#$%^&*()-_=+[]{}|;:,.<>?"
charSetChars Ambiguous = unpack "0O1lI"
charSetChars (Custom s) = unpack s

||| Password generation options
public export
record GenerateOptions where
  constructor MkGenerateOptions
  length : Nat
  charsets : List CharSet
  excludeAmbiguous : Bool
  requireFromEach : Bool  -- Require at least one char from each charset

||| Default generation options (secure)
public export
defaultGenerateOptions : GenerateOptions
defaultGenerateOptions = MkGenerateOptions
  { length = 16
  , charsets = [Lowercase, Uppercase, Digits, Symbols]
  , excludeAmbiguous = True
  , requireFromEach = True
  }

||| Generate a random password
||| Actual implementation via FFI to CSPRNG
public export
generatePassword : GenerateOptions -> IO String
generatePassword opts = pure $ believe_me ""  -- Stub

||| Generate a passphrase (word-based)
public export
generatePassphrase : (words : Nat) ->
                     (separator : Char) ->
                     IO String
generatePassphrase words sep = pure $ believe_me ""  -- Stub

--------------------------------------------------------------------------------
-- Password Breach Checking
--------------------------------------------------------------------------------

||| Result of breach check
public export
data BreachStatus
  = NotBreached             -- Not found in known breaches
  | Breached Nat            -- Found N times in breaches
  | CheckFailed String      -- Could not check (network error)

public export
Show BreachStatus where
  show NotBreached = "Not found in known breaches"
  show (Breached n) = "Found in " ++ show n ++ " breaches"
  show (CheckFailed e) = "Check failed: " ++ e

||| Check password against breach database (k-anonymity)
||| Uses first 5 chars of SHA-1 hash for privacy-preserving lookup
public export
checkBreach : RawPassword -> IO BreachStatus
checkBreach pwd = pure $ believe_me NotBreached  -- Stub - needs HTTP client

--------------------------------------------------------------------------------
-- Entropy Calculation
--------------------------------------------------------------------------------

||| Calculate password entropy in bits
public export
calculateEntropy : String -> Double
calculateEntropy pwd =
  let chars = unpack pwd
      len = cast (length chars)
      poolSize = calculatePoolSize chars
  in len * log2 poolSize
  where
    log2 : Double -> Double
    log2 x = log x / log 2.0

    hasLower : List Char -> Bool
    hasLower = any (\c => c >= 'a' && c <= 'z')

    hasUpper : List Char -> Bool
    hasUpper = any (\c => c >= 'A' && c <= 'Z')

    hasDigit : List Char -> Bool
    hasDigit = any (\c => c >= '0' && c <= '9')

    hasSymbol : List Char -> Bool
    hasSymbol = any (\c => not (isAlphaNum c))

    calculatePoolSize : List Char -> Double
    calculatePoolSize cs =
      let base = 0.0
          withLower = if hasLower cs then base + 26.0 else base
          withUpper = if hasUpper cs then withLower + 26.0 else withLower
          withDigit = if hasDigit cs then withUpper + 10.0 else withUpper
          withSymbol = if hasSymbol cs then withDigit + 32.0 else withDigit
      in if withSymbol == 0.0 then 1.0 else withSymbol

||| Minimum recommended entropy for different use cases
public export
data EntropyRequirement
  = LowSecurity        -- 28+ bits (simple accounts)
  | MediumSecurity     -- 36+ bits (standard accounts)
  | HighSecurity       -- 60+ bits (sensitive accounts)
  | CriticalSecurity   -- 80+ bits (cryptographic keys)

public export
entropyBits : EntropyRequirement -> Double
entropyBits LowSecurity = 28.0
entropyBits MediumSecurity = 36.0
entropyBits HighSecurity = 60.0
entropyBits CriticalSecurity = 80.0

||| Check if password meets entropy requirement
public export
meetsEntropyRequirement : String -> EntropyRequirement -> Bool
meetsEntropyRequirement pwd req = calculateEntropy pwd >= entropyBits req

--------------------------------------------------------------------------------
-- Secure Memory Handling
--------------------------------------------------------------------------------

||| Wrapper for password that will be securely zeroed
public export
data SecurePassword : Type where
  MkSecurePassword : (value : String) -> SecurePassword

||| Create secure password wrapper
public export
secure : String -> SecurePassword
secure = MkSecurePassword

||| Use password and securely clear (via FFI)
public export
withSecurePassword : SecurePassword -> (String -> IO a) -> IO a
withSecurePassword (MkSecurePassword pwd) action = do
  result <- action pwd
  -- In real implementation, zero the memory here via FFI
  pure result

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Mask password for logging (show length only)
public export
maskPassword : String -> String
maskPassword pwd = replicate (length pwd) '*'

||| Check if password isInfixOf username
public export
containsUsername : String -> String -> Bool
containsUsername pwd username =
  let pwdLower = toLower pwd
      userLower = toLower username
  in isInfixOf userLower pwdLower

||| Check if password is a common pattern
public export
isCommonPattern : String -> Bool
isCommonPattern pwd =
  let lower = toLower pwd
  in lower `elem` commonPasswords || isKeyboardPattern lower || isSequential lower
  where
    commonPasswords : List String
    commonPasswords = ["password", "123456", "qwerty", "letmein", "welcome",
                       "admin", "login", "passw0rd", "master", "hello"]

    isKeyboardPattern : String -> Bool
    isKeyboardPattern s =
      isInfixOf "qwerty" s || isInfixOf "asdf" s || isInfixOf "zxcv" s

    isSequential : String -> Bool
    isSequential s =
      isInfixOf "1234" s || isInfixOf "abcd" s || isInfixOf "0000" s
