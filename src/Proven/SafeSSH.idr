-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe SSH key and configuration handling
|||
||| This module provides type-safe representations of SSH concepts:
||| - Public and private key formats
||| - Key algorithms (RSA, Ed25519, ECDSA)
||| - SSH fingerprints
||| - Known hosts entries
||| - SSH config file options
||| - Authorized keys entries
|||
||| Security features:
||| - Weak algorithm detection (DSA, small RSA)
||| - Private key exposure prevention
||| - Fingerprint validation
||| - Dangerous option warnings
||| - Key age tracking
module Proven.SafeSSH

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| SSH key algorithms
public export
data KeyAlgorithm
  = RSA
  | DSA           -- Deprecated
  | ECDSA_256
  | ECDSA_384
  | ECDSA_521
  | Ed25519
  | Ed448
  | SK_ECDSA     -- Security key
  | SK_Ed25519   -- Security key

||| RSA key sizes
public export
data RSAKeySize
  = RSA1024   -- Weak
  | RSA2048   -- Minimum recommended
  | RSA3072
  | RSA4096
  | RSAOther Nat

||| Key strength assessment
public export
data KeyStrength = Weak | Acceptable | Strong | Excellent

||| SSH fingerprint hash algorithm
public export
data FingerprintHash = MD5Hash | SHA256Hash

||| SSH fingerprint
public export
record Fingerprint where
  constructor MkFingerprint
  hashAlgo : FingerprintHash
  value : String

||| SSH public key
public export
record PublicKey where
  constructor MkPublicKey
  algorithm : KeyAlgorithm
  keyData : String        -- Base64 encoded
  comment : Maybe String

||| SSH private key (opaque - never expose contents)
public export
record PrivateKey where
  constructor MkPrivateKey
  algorithm : KeyAlgorithm
  isEncrypted : Bool
  keyPath : Maybe String  -- Path only, never contents

||| SSH key pair
public export
record KeyPair where
  constructor MkKeyPair
  publicKey : PublicKey
  privateKeyPath : String
  privateKeyEncrypted : Bool

||| Known hosts entry pattern
public export
data HostPattern
  = ExactHost String
  | HashedHost String
  | WildcardHost String
  | NegatedHost String

||| Known hosts entry
public export
record KnownHostsEntry where
  constructor MkKnownHostsEntry
  patterns : List HostPattern
  algorithm : KeyAlgorithm
  publicKey : String
  marker : Maybe String   -- @cert-authority, @revoked

||| SSH config match criteria
public export
data MatchCriteria
  = MatchHost String
  | MatchUser String
  | MatchLocalUser String
  | MatchAddress String
  | MatchExec String

||| SSH config options
public export
data SSHOption
  = HostName String
  | Port Nat
  | User String
  | IdentityFile String
  | IdentitiesOnly Bool
  | ForwardAgent Bool
  | ForwardX11 Bool
  | ProxyJump String
  | ProxyCommand String
  | StrictHostKeyChecking StrictHostKeyMode
  | UserKnownHostsFile String
  | BatchMode Bool
  | Compression Bool
  | ServerAliveInterval Nat
  | ServerAliveCountMax Nat
  | AddKeysToAgent AddKeyMode
  | PasswordAuthentication Bool
  | PubkeyAuthentication Bool
  | PreferredAuthentications (List String)

||| Strict host key checking modes
public export
data StrictHostKeyMode = Yes | No | Ask | AcceptNew

||| Add keys to agent mode
public export
data AddKeyMode = AddYes | AddNo | AddConfirm | AddAsk

||| SSH config host block
public export
record HostBlock where
  constructor MkHostBlock
  patterns : List String
  options : List SSHOption

||| SSH config file
public export
record SSHConfig where
  constructor MkSSHConfig
  globalOptions : List SSHOption
  hostBlocks : List HostBlock

||| Authorized keys options
public export
data AuthorizedKeyOption
  = Command String
  | Environment String String
  | From String
  | NoAgentForwarding
  | NoPortForwarding
  | NoX11Forwarding
  | NoPty
  | Principals String
  | Expiry String         -- Valid until
  | RestrictOption        -- restrict

||| Authorized keys entry
public export
record AuthorizedKeysEntry where
  constructor MkAuthorizedKeysEntry
  options : List AuthorizedKeyOption
  key : PublicKey

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during SSH operations
public export
data SSHError
  = EmptyKey
  | InvalidKeyFormat String
  | WeakAlgorithm KeyAlgorithm
  | WeakKeySize KeyAlgorithm Nat
  | InvalidFingerprint String
  | MD5FingerprintWarning
  | PrivateKeyExposed
  | UnencryptedPrivateKey
  | InvalidHostPattern String
  | DangerousOption String
  | InvalidPort Nat
  | MissingIdentityFile String
  | AgentForwardingWarning
  | X11ForwardingWarning
  | StrictHostKeyDisabled
  | InvalidBase64 String
  | ExpiredKey String
  | UnsupportedAlgorithm String

public export
Show SSHError where
  show EmptyKey = "SSH error: empty key"
  show (InvalidKeyFormat f) = "SSH error: invalid key format '" ++ f ++ "'"
  show (WeakAlgorithm algo) = "SSH security: weak algorithm " ++ showAlgorithm algo
  show (WeakKeySize algo size) = "SSH security: weak key size " ++ show size ++ " for " ++ showAlgorithm algo
  show (InvalidFingerprint f) = "SSH error: invalid fingerprint '" ++ f ++ "'"
  show MD5FingerprintWarning = "SSH warning: MD5 fingerprints are deprecated, use SHA256"
  show PrivateKeyExposed = "SSH security: private key contents should never be exposed"
  show UnencryptedPrivateKey = "SSH security: private key should be encrypted"
  show (InvalidHostPattern p) = "SSH error: invalid host pattern '" ++ p ++ "'"
  show (DangerousOption opt) = "SSH security: dangerous option '" ++ opt ++ "'"
  show (InvalidPort p) = "SSH error: invalid port " ++ show p
  show (MissingIdentityFile f) = "SSH error: identity file not found '" ++ f ++ "'"
  show AgentForwardingWarning = "SSH security: agent forwarding can be dangerous"
  show X11ForwardingWarning = "SSH security: X11 forwarding can be dangerous"
  show StrictHostKeyDisabled = "SSH security: strict host key checking is disabled"
  show (InvalidBase64 s) = "SSH error: invalid base64 encoding"
  show (ExpiredKey date) = "SSH error: key expired on " ++ date
  show (UnsupportedAlgorithm algo) = "SSH error: unsupported algorithm '" ++ algo ++ "'"

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Minimum recommended RSA key size
minRSAKeySize : Nat
minRSAKeySize = 2048

||| Algorithm name strings
algorithmNames : List (String, KeyAlgorithm)
algorithmNames =
  [ ("ssh-rsa", RSA)
  , ("ssh-dss", DSA)
  , ("ecdsa-sha2-nistp256", ECDSA_256)
  , ("ecdsa-sha2-nistp384", ECDSA_384)
  , ("ecdsa-sha2-nistp521", ECDSA_521)
  , ("ssh-ed25519", Ed25519)
  , ("ssh-ed448", Ed448)
  , ("sk-ecdsa-sha2-nistp256@openssh.com", SK_ECDSA)
  , ("sk-ssh-ed25519@openssh.com", SK_Ed25519)
  ]

||| Deprecated/weak algorithms
weakAlgorithms : List KeyAlgorithm
weakAlgorithms = [DSA]

||| Dangerous SSH options
dangerousOptions : List String
dangerousOptions =
  [ "PermitRootLogin"
  , "PermitEmptyPasswords"
  , "PasswordAuthentication"  -- when enabled, prefer keys
  ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

||| Show algorithm name
showAlgorithm : KeyAlgorithm -> String
showAlgorithm RSA = "ssh-rsa"
showAlgorithm DSA = "ssh-dss"
showAlgorithm ECDSA_256 = "ecdsa-sha2-nistp256"
showAlgorithm ECDSA_384 = "ecdsa-sha2-nistp384"
showAlgorithm ECDSA_521 = "ecdsa-sha2-nistp521"
showAlgorithm Ed25519 = "ssh-ed25519"
showAlgorithm Ed448 = "ssh-ed448"
showAlgorithm SK_ECDSA = "sk-ecdsa-sha2-nistp256@openssh.com"
showAlgorithm SK_Ed25519 = "sk-ssh-ed25519@openssh.com"

||| Parse algorithm from string
parseAlgorithm : String -> Maybe KeyAlgorithm
parseAlgorithm name = lookup name algorithmNames

||| Check if algorithm is weak
isWeakAlgorithm : KeyAlgorithm -> Bool
isWeakAlgorithm algo = elem algo weakAlgorithms

||| Check if character is valid base64
isBase64Char : Char -> Bool
isBase64Char c = isAlphaNum c || c == '+' || c == '/' || c == '='

||| Check all chars are base64
allBase64 : String -> Bool
allBase64 str = all isBase64Char (unpack str)

--------------------------------------------------------------------------------
-- Key strength assessment
--------------------------------------------------------------------------------

||| Assess RSA key strength
public export
assessRSAStrength : Nat -> KeyStrength
assessRSAStrength size =
  if size < 1024 then Weak
  else if size < 2048 then Weak
  else if size < 3072 then Acceptable
  else if size < 4096 then Strong
  else Excellent

||| Assess key algorithm strength
public export
assessAlgorithmStrength : KeyAlgorithm -> KeyStrength
assessAlgorithmStrength DSA = Weak
assessAlgorithmStrength RSA = Acceptable      -- depends on key size
assessAlgorithmStrength ECDSA_256 = Strong
assessAlgorithmStrength ECDSA_384 = Strong
assessAlgorithmStrength ECDSA_521 = Excellent
assessAlgorithmStrength Ed25519 = Excellent
assessAlgorithmStrength Ed448 = Excellent
assessAlgorithmStrength SK_ECDSA = Excellent
assessAlgorithmStrength SK_Ed25519 = Excellent

--------------------------------------------------------------------------------
-- Validation functions
--------------------------------------------------------------------------------

||| Validate a key algorithm
public export
validateAlgorithm : String -> Either SSHError KeyAlgorithm
validateAlgorithm name =
  case parseAlgorithm name of
    Nothing => Left (UnsupportedAlgorithm name)
    Just algo =>
      if isWeakAlgorithm algo
        then Left (WeakAlgorithm algo)
        else Right algo

||| Validate a public key
public export
validatePublicKey : String -> String -> Maybe String -> Either SSHError PublicKey
validatePublicKey algoName keyData comment = do
  algo <- validateAlgorithm algoName
  if not (allBase64 keyData)
    then Left (InvalidBase64 keyData)
    else Right (MkPublicKey algo keyData comment)

||| Parse a public key from authorized_keys format
public export
parsePublicKey : String -> Either SSHError PublicKey
parsePublicKey "" = Left EmptyKey
parsePublicKey line =
  case words line of
    [] => Left EmptyKey
    [_] => Left (InvalidKeyFormat "missing key data")
    [algoName, keyData] => validatePublicKey algoName keyData Nothing
    (algoName :: keyData :: rest) =>
      validatePublicKey algoName keyData (Just (unwords rest))

||| Validate a fingerprint
public export
validateFingerprint : String -> Either SSHError Fingerprint
validateFingerprint "" = Left (InvalidFingerprint "empty")
validateFingerprint fp =
  if isPrefixOf "SHA256:" fp
    then Right (MkFingerprint SHA256Hash (strSubstr 7 (minus (length fp) 7) fp))
    else if isPrefixOf "MD5:" fp
      then Right (MkFingerprint MD5Hash (strSubstr 4 (minus (length fp) 4) fp))
      else -- Assume it's a bare fingerprint
        if isInfixOf ":" fp
          then Right (MkFingerprint MD5Hash fp)  -- Old MD5 format with colons
          else Right (MkFingerprint SHA256Hash fp)

||| Validate a port number
public export
validatePort : Nat -> Either SSHError Nat
validatePort 0 = Left (InvalidPort 0)
validatePort p =
  if p > 65535
    then Left (InvalidPort p)
    else Right p

||| Validate a host pattern
public export
validateHostPattern : String -> Either SSHError HostPattern
validateHostPattern "" = Left (InvalidHostPattern "empty")
validateHostPattern pattern =
  if isPrefixOf "|" pattern
    then Right (HashedHost pattern)
    else if isPrefixOf "!" pattern
      then Right (NegatedHost (strTail pattern))
      else if isInfixOf "*" pattern || isInfixOf "?" pattern
        then Right (WildcardHost pattern)
        else Right (ExactHost pattern)

--------------------------------------------------------------------------------
-- Security validation
--------------------------------------------------------------------------------

||| Check for agent forwarding (security warning)
public export
checkAgentForwarding : SSHOption -> Either SSHError SSHOption
checkAgentForwarding opt@(ForwardAgent True) = Left AgentForwardingWarning
checkAgentForwarding opt = Right opt

||| Check for X11 forwarding (security warning)
public export
checkX11Forwarding : SSHOption -> Either SSHError SSHOption
checkX11Forwarding opt@(ForwardX11 True) = Left X11ForwardingWarning
checkX11Forwarding opt = Right opt

||| Check for disabled strict host key checking
public export
checkStrictHostKey : SSHOption -> Either SSHError SSHOption
checkStrictHostKey opt@(StrictHostKeyChecking No) = Left StrictHostKeyDisabled
checkStrictHostKey opt = Right opt

||| Validate SSH option for security
public export
validateOption : SSHOption -> Either SSHError SSHOption
validateOption opt@(ForwardAgent True) = Left AgentForwardingWarning
validateOption opt@(ForwardX11 True) = Left X11ForwardingWarning
validateOption opt@(StrictHostKeyChecking No) = Left StrictHostKeyDisabled
validateOption opt@(Port p) = validatePort p >>= \_ => Right opt
validateOption opt = Right opt

||| Check if private key should be encrypted
public export
validatePrivateKeyEncryption : PrivateKey -> Either SSHError PrivateKey
validatePrivateKeyEncryption key =
  if not key.isEncrypted
    then Left UnencryptedPrivateKey
    else Right key

--------------------------------------------------------------------------------
-- Construction functions
--------------------------------------------------------------------------------

||| Create a public key (validating)
public export
mkPublicKey : String -> String -> Maybe String -> Either SSHError PublicKey
mkPublicKey = validatePublicKey

||| Create a private key reference (never the actual key content!)
public export
mkPrivateKeyRef : KeyAlgorithm -> Bool -> String -> PrivateKey
mkPrivateKeyRef algo encrypted path = MkPrivateKey algo encrypted (Just path)

||| Create a known hosts entry
public export
mkKnownHostsEntry : List String -> String -> String -> Maybe String -> Either SSHError KnownHostsEntry
mkKnownHostsEntry patterns algoName keyData marker = do
  hostPatterns <- traverse validateHostPattern patterns
  algo <- validateAlgorithm algoName
  pure (MkKnownHostsEntry hostPatterns algo keyData marker)

||| Create an SSH config option
public export
mkSSHOption : String -> String -> Either SSHError SSHOption
mkSSHOption "HostName" value = Right (HostName value)
mkSSHOption "Port" value =
  case parsePositive value of
    Nothing => Left (InvalidPort 0)
    Just p => validatePort p >>= \port => Right (Port port)
mkSSHOption "User" value = Right (User value)
mkSSHOption "IdentityFile" value = Right (IdentityFile value)
mkSSHOption "ForwardAgent" "yes" = Left AgentForwardingWarning
mkSSHOption "ForwardAgent" _ = Right (ForwardAgent False)
mkSSHOption "ForwardX11" "yes" = Left X11ForwardingWarning
mkSSHOption "ForwardX11" _ = Right (ForwardX11 False)
mkSSHOption "StrictHostKeyChecking" "no" = Left StrictHostKeyDisabled
mkSSHOption "StrictHostKeyChecking" "yes" = Right (StrictHostKeyChecking Yes)
mkSSHOption "StrictHostKeyChecking" "ask" = Right (StrictHostKeyChecking Ask)
mkSSHOption "StrictHostKeyChecking" "accept-new" = Right (StrictHostKeyChecking AcceptNew)
mkSSHOption "StrictHostKeyChecking" _ = Right (StrictHostKeyChecking Ask)
mkSSHOption name _ = Left (DangerousOption name)

||| Create an authorized keys entry
public export
mkAuthorizedKeysEntry : List AuthorizedKeyOption -> String -> Either SSHError AuthorizedKeysEntry
mkAuthorizedKeysEntry opts keyLine = do
  key <- parsePublicKey keyLine
  pure (MkAuthorizedKeysEntry opts key)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Serialize a fingerprint hash type
public export
showFingerprintHash : FingerprintHash -> String
showFingerprintHash MD5Hash = "MD5"
showFingerprintHash SHA256Hash = "SHA256"

||| Serialize a fingerprint
public export
showFingerprint : Fingerprint -> String
showFingerprint (MkFingerprint hash value) =
  showFingerprintHash hash ++ ":" ++ value

||| Serialize a public key
public export
showPublicKey : PublicKey -> String
showPublicKey (MkPublicKey algo keyData comment) =
  showAlgorithm algo ++ " " ++ keyData ++
  maybe "" (\c => " " ++ c) comment

||| Serialize a host pattern
public export
showHostPattern : HostPattern -> String
showHostPattern (ExactHost h) = h
showHostPattern (HashedHost h) = h
showHostPattern (WildcardHost h) = h
showHostPattern (NegatedHost h) = "!" ++ h

||| Serialize a known hosts entry
public export
showKnownHostsEntry : KnownHostsEntry -> String
showKnownHostsEntry entry =
  let patternsStr = concat (intersperse "," (map showHostPattern entry.patterns))
      markerStr = maybe "" (\m => m ++ " ") entry.marker
  in markerStr ++ patternsStr ++ " " ++ showAlgorithm entry.algorithm ++ " " ++ entry.publicKey

||| Serialize strict host key mode
public export
showStrictHostKeyMode : StrictHostKeyMode -> String
showStrictHostKeyMode Yes = "yes"
showStrictHostKeyMode No = "no"
showStrictHostKeyMode Ask = "ask"
showStrictHostKeyMode AcceptNew = "accept-new"

||| Serialize add key mode
public export
showAddKeyMode : AddKeyMode -> String
showAddKeyMode AddYes = "yes"
showAddKeyMode AddNo = "no"
showAddKeyMode AddConfirm = "confirm"
showAddKeyMode AddAsk = "ask"

||| Serialize an SSH option
public export
showSSHOption : SSHOption -> String
showSSHOption (HostName h) = "HostName " ++ h
showSSHOption (Port p) = "Port " ++ show p
showSSHOption (User u) = "User " ++ u
showSSHOption (IdentityFile f) = "IdentityFile " ++ f
showSSHOption (IdentitiesOnly b) = "IdentitiesOnly " ++ if b then "yes" else "no"
showSSHOption (ForwardAgent b) = "ForwardAgent " ++ if b then "yes" else "no"
showSSHOption (ForwardX11 b) = "ForwardX11 " ++ if b then "yes" else "no"
showSSHOption (ProxyJump j) = "ProxyJump " ++ j
showSSHOption (ProxyCommand c) = "ProxyCommand " ++ c
showSSHOption (StrictHostKeyChecking m) = "StrictHostKeyChecking " ++ showStrictHostKeyMode m
showSSHOption (UserKnownHostsFile f) = "UserKnownHostsFile " ++ f
showSSHOption (BatchMode b) = "BatchMode " ++ if b then "yes" else "no"
showSSHOption (Compression b) = "Compression " ++ if b then "yes" else "no"
showSSHOption (ServerAliveInterval n) = "ServerAliveInterval " ++ show n
showSSHOption (ServerAliveCountMax n) = "ServerAliveCountMax " ++ show n
showSSHOption (AddKeysToAgent m) = "AddKeysToAgent " ++ showAddKeyMode m
showSSHOption (PasswordAuthentication b) = "PasswordAuthentication " ++ if b then "yes" else "no"
showSSHOption (PubkeyAuthentication b) = "PubkeyAuthentication " ++ if b then "yes" else "no"
showSSHOption (PreferredAuthentications auths) = "PreferredAuthentications " ++ concat (intersperse "," auths)

||| Serialize a host block
public export
showHostBlock : HostBlock -> String
showHostBlock block =
  "Host " ++ unwords block.patterns ++ "\n" ++
  concat (map (\opt => "    " ++ showSSHOption opt ++ "\n") block.options)

||| Serialize authorized key option
public export
showAuthKeyOption : AuthorizedKeyOption -> String
showAuthKeyOption (Command cmd) = "command=\"" ++ cmd ++ "\""
showAuthKeyOption (Environment name val) = "environment=\"" ++ name ++ "=" ++ val ++ "\""
showAuthKeyOption (From hosts) = "from=\"" ++ hosts ++ "\""
showAuthKeyOption NoAgentForwarding = "no-agent-forwarding"
showAuthKeyOption NoPortForwarding = "no-port-forwarding"
showAuthKeyOption NoX11Forwarding = "no-X11-forwarding"
showAuthKeyOption NoPty = "no-pty"
showAuthKeyOption (Principals p) = "principals=\"" ++ p ++ "\""
showAuthKeyOption (Expiry e) = "expiry-time=\"" ++ e ++ "\""
showAuthKeyOption RestrictOption = "restrict"

||| Serialize an authorized keys entry
public export
showAuthorizedKeysEntry : AuthorizedKeysEntry -> String
showAuthorizedKeysEntry entry =
  let optsStr = if null entry.options
        then ""
        else concat (intersperse "," (map showAuthKeyOption entry.options)) ++ " "
  in optsStr ++ showPublicKey entry.key

--------------------------------------------------------------------------------
-- Security report
--------------------------------------------------------------------------------

||| Security findings from SSH configuration
public export
record SSHSecurityReport where
  constructor MkSSHSecurityReport
  weakAlgorithms : List KeyAlgorithm
  unencryptedKeys : List String
  dangerousOptions : List String
  disabledChecks : List String

||| Empty security report
public export
emptySSHSecurityReport : SSHSecurityReport
emptySSHSecurityReport = MkSSHSecurityReport [] [] [] []

||| Analyze SSH config for security issues
public export
analyzeSSHConfig : SSHConfig -> SSHSecurityReport
analyzeSSHConfig config =
  let allOptions = config.globalOptions ++ concatMap (.options) config.hostBlocks
      report = emptySSHSecurityReport
      report' = foldl checkOption report allOptions
  in report'
  where
    checkOption : SSHSecurityReport -> SSHOption -> SSHSecurityReport
    checkOption rep (ForwardAgent True) =
      { dangerousOptions $= ("ForwardAgent yes" ::) } rep
    checkOption rep (ForwardX11 True) =
      { dangerousOptions $= ("ForwardX11 yes" ::) } rep
    checkOption rep (StrictHostKeyChecking No) =
      { disabledChecks $= ("StrictHostKeyChecking" ::) } rep
    checkOption rep (PasswordAuthentication True) =
      { dangerousOptions $= ("PasswordAuthentication yes" ::) } rep
    checkOption rep _ = rep
