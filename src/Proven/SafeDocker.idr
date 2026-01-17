-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe Docker container and image reference handling
|||
||| This module provides type-safe representations of Docker concepts:
||| - Image references (registry/repository:tag@digest)
||| - Container names and IDs
||| - Volume mounts and bind mounts
||| - Port mappings
||| - Environment variables (with secret detection)
||| - Dockerfile instructions
|||
||| Security features:
||| - Registry whitelist validation
||| - Dangerous mount path detection
||| - Privileged mode warnings
||| - Secret leak prevention in environment variables
||| - Tag mutability warnings (prefer digests)
module Proven.SafeDocker

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Docker image tag (e.g., "latest", "v1.0.0", "sha256:abc...")
public export
record ImageTag where
  constructor MkImageTag
  tag : String

||| Docker image digest (sha256:...)
public export
record ImageDigest where
  constructor MkImageDigest
  algorithm : String
  hash : String

||| Docker registry hostname
public export
record Registry where
  constructor MkRegistry
  hostname : String
  port : Maybe Nat

||| Docker repository name (namespace/name)
public export
record Repository where
  constructor MkRepository
  namespace : Maybe String
  name : String

||| Complete Docker image reference
public export
record ImageRef where
  constructor MkImageRef
  registry : Maybe Registry
  repository : Repository
  tag : Maybe ImageTag
  digest : Maybe ImageDigest

||| Container name (must match [a-zA-Z0-9][a-zA-Z0-9_.-]+)
public export
record ContainerName where
  constructor MkContainerName
  name : String

||| Container ID (64 hex characters, or short form 12+)
public export
record ContainerId where
  constructor MkContainerId
  id : String

||| Volume mount type
public export
data MountType
  = BindMount      -- Host path to container path
  | VolumeMount    -- Named volume
  | TmpfsMount     -- In-memory filesystem

||| Mount propagation mode
public export
data MountPropagation
  = Private
  | Shared
  | Slave
  | RPPrivate
  | RShared
  | RSlave

||| Volume mount specification
public export
record VolumeMount where
  constructor MkVolumeMount
  mountType : MountType
  source : String
  target : String
  readOnly : Bool
  propagation : Maybe MountPropagation

||| Port protocol
public export
data PortProtocol = TCP | UDP | SCTP

||| Port mapping
public export
record PortMapping where
  constructor MkPortMapping
  hostIp : Maybe String
  hostPort : Maybe Nat
  containerPort : Nat
  protocol : PortProtocol

||| Environment variable (with sensitivity tracking)
public export
record EnvVar where
  constructor MkEnvVar
  name : String
  value : String
  isSensitive : Bool

||| Dockerfile instruction types
public export
data DockerInstruction
  = FROM ImageRef (Maybe String)           -- base image, optional alias
  | RUN String                             -- shell command
  | CMD (List String)                      -- default command
  | ENTRYPOINT (List String)               -- entrypoint
  | COPY String String (Maybe String)      -- src, dest, optional --from
  | ADD String String                      -- src, dest
  | ENV String String                      -- name, value
  | EXPOSE Nat PortProtocol                -- port, protocol
  | VOLUME String                          -- volume path
  | WORKDIR String                         -- working directory
  | USER String                            -- user
  | ARG String (Maybe String)              -- arg name, optional default
  | LABEL String String                    -- key, value
  | STOPSIGNAL String                      -- signal
  | HEALTHCHECK (Maybe (List String))      -- command or NONE
  | SHELL (List String)                    -- shell command

||| Parsed Dockerfile
public export
record Dockerfile where
  constructor MkDockerfile
  instructions : List DockerInstruction

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during Docker operations
public export
data DockerError
  = EmptyImageName
  | InvalidRegistryHostname String
  | InvalidRepositoryName String
  | InvalidTag String
  | InvalidDigest String
  | InvalidContainerName String
  | InvalidContainerId String
  | DangerousMountPath String
  | PrivilegedModeWarning
  | MutableTagWarning String
  | SensitiveEnvVarDetected String
  | UntrustedRegistry String
  | InvalidPortNumber Nat
  | InvalidDockerfile String
  | EmptyContainerName
  | EmptyContainerId
  | HostNetworkWarning
  | CapabilityWarning String

public export
Show DockerError where
  show EmptyImageName = "Docker error: empty image name"
  show (InvalidRegistryHostname h) = "Docker error: invalid registry hostname '" ++ h ++ "'"
  show (InvalidRepositoryName n) = "Docker error: invalid repository name '" ++ n ++ "'"
  show (InvalidTag t) = "Docker error: invalid tag '" ++ t ++ "'"
  show (InvalidDigest d) = "Docker error: invalid digest '" ++ d ++ "'"
  show (InvalidContainerName n) = "Docker error: invalid container name '" ++ n ++ "'"
  show (InvalidContainerId i) = "Docker error: invalid container ID '" ++ i ++ "'"
  show (DangerousMountPath p) = "Docker security: dangerous mount path '" ++ p ++ "'"
  show PrivilegedModeWarning = "Docker security: privileged mode is dangerous"
  show (MutableTagWarning t) = "Docker warning: tag '" ++ t ++ "' is mutable, prefer digest"
  show (SensitiveEnvVarDetected v) = "Docker security: sensitive data detected in env var '" ++ v ++ "'"
  show (UntrustedRegistry r) = "Docker security: untrusted registry '" ++ r ++ "'"
  show (InvalidPortNumber p) = "Docker error: invalid port number " ++ show p
  show (InvalidDockerfile msg) = "Docker error: invalid Dockerfile - " ++ msg
  show EmptyContainerName = "Docker error: empty container name"
  show EmptyContainerId = "Docker error: empty container ID"
  show HostNetworkWarning = "Docker security: host network mode bypasses isolation"
  show (CapabilityWarning cap) = "Docker security: dangerous capability '" ++ cap ++ "'"

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Maximum tag length
maxTagLength : Nat
maxTagLength = 128

||| Maximum repository name length
maxRepoLength : Nat
maxRepoLength = 256

||| Maximum container name length
maxContainerNameLength : Nat
maxContainerNameLength = 128

||| Default Docker registry
defaultRegistry : String
defaultRegistry = "docker.io"

||| Sensitive environment variable name patterns
sensitiveEnvPatterns : List String
sensitiveEnvPatterns =
  [ "PASSWORD", "SECRET", "TOKEN", "KEY", "CREDENTIAL"
  , "API_KEY", "APIKEY", "AUTH", "PRIVATE"
  , "AWS_SECRET", "GITHUB_TOKEN", "NPM_TOKEN"
  ]

||| Dangerous mount paths
dangerousMountPaths : List String
dangerousMountPaths =
  [ "/", "/etc", "/var", "/usr", "/bin", "/sbin"
  , "/lib", "/lib64", "/boot", "/proc", "/sys"
  , "/dev", "/root", "/home"
  , "/var/run/docker.sock"
  ]

||| Dangerous Linux capabilities
dangerousCapabilities : List String
dangerousCapabilities =
  [ "SYS_ADMIN", "NET_ADMIN", "SYS_PTRACE"
  , "DAC_READ_SEARCH", "SYS_MODULE", "SYS_RAWIO"
  ]

||| Trusted registries (example whitelist)
trustedRegistries : List String
trustedRegistries =
  [ "docker.io", "ghcr.io", "gcr.io", "quay.io"
  , "registry.access.redhat.com", "mcr.microsoft.com"
  ]

--------------------------------------------------------------------------------
-- Character validation helpers
--------------------------------------------------------------------------------

||| Check if character is valid in a tag
isValidTagChar : Char -> Bool
isValidTagChar c = isAlphaNum c || c == '.' || c == '_' || c == '-'

||| Check if character is valid in a repository name
isValidRepoChar : Char -> Bool
isValidRepoChar c = isAlphaNum c || c == '.' || c == '_' || c == '-' || c == '/'

||| Check if character is valid in a container name
isValidContainerNameChar : Char -> Bool
isValidContainerNameChar c = isAlphaNum c || c == '_' || c == '.' || c == '-'

||| Check if character is a hex digit
isHexDigit : Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

||| Check if all characters satisfy predicate
allChars : (Char -> Bool) -> String -> Bool
allChars pred str = all pred (unpack str)

--------------------------------------------------------------------------------
-- Validation functions
--------------------------------------------------------------------------------

||| Validate a Docker image tag
public export
validateTag : String -> Either DockerError ImageTag
validateTag "" = Left (InvalidTag "empty tag")
validateTag t =
  if length t > maxTagLength
    then Left (InvalidTag ("tag too long: " ++ show (length t)))
    else if not (allChars isValidTagChar t)
      then Left (InvalidTag t)
      else Right (MkImageTag t)

||| Validate a Docker image digest
public export
validateDigest : String -> Either DockerError ImageDigest
validateDigest "" = Left (InvalidDigest "empty digest")
validateDigest d =
  case break (== ':') d of
    (algo, rest) =>
      if length rest < 2
        then Left (InvalidDigest "missing hash after algorithm")
        else let hash = strTail rest in
          if algo /= "sha256" && algo /= "sha512"
            then Left (InvalidDigest ("unsupported algorithm: " ++ algo))
            else if not (allChars isHexDigit hash)
              then Left (InvalidDigest "invalid hash characters")
              else Right (MkImageDigest algo hash)

||| Validate a repository name
public export
validateRepository : String -> Either DockerError Repository
validateRepository "" = Left (InvalidRepositoryName "empty repository name")
validateRepository name =
  if length name > maxRepoLength
    then Left (InvalidRepositoryName ("name too long: " ++ show (length name)))
    else if not (allChars isValidRepoChar name)
      then Left (InvalidRepositoryName name)
      else case break (== '/') name of
        (ns, rest) =>
          if length rest > 0
            then Right (MkRepository (Just ns) (strTail rest))
            else Right (MkRepository Nothing name)

||| Validate a container name
public export
validateContainerName : String -> Either DockerError ContainerName
validateContainerName "" = Left EmptyContainerName
validateContainerName name =
  if length name > maxContainerNameLength
    then Left (InvalidContainerName ("name too long: " ++ show (length name)))
    else case unpack name of
      [] => Left EmptyContainerName
      (c :: _) =>
        if not (isAlphaNum c)
          then Left (InvalidContainerName "must start with alphanumeric")
          else if not (allChars isValidContainerNameChar name)
            then Left (InvalidContainerName name)
            else Right (MkContainerName name)

||| Validate a container ID
public export
validateContainerId : String -> Either DockerError ContainerId
validateContainerId "" = Left EmptyContainerId
validateContainerId id =
  let len = length id in
  if len < 12
    then Left (InvalidContainerId "too short (minimum 12 characters)")
    else if len > 64
      then Left (InvalidContainerId "too long (maximum 64 characters)")
      else if not (allChars isHexDigit id)
        then Left (InvalidContainerId "must be hexadecimal")
        else Right (MkContainerId id)

||| Validate a port number
public export
validatePort : Nat -> Either DockerError Nat
validatePort 0 = Left (InvalidPortNumber 0)
validatePort p =
  if p > 65535
    then Left (InvalidPortNumber p)
    else Right p

--------------------------------------------------------------------------------
-- Security validation
--------------------------------------------------------------------------------

||| Check if a mount path is dangerous
public export
isDangerousMountPath : String -> Bool
isDangerousMountPath path =
  any (\dangerous => path == dangerous || isPrefixOf (dangerous ++ "/") path)
      dangerousMountPaths

||| Validate a volume mount for security
public export
validateMount : VolumeMount -> Either DockerError VolumeMount
validateMount mount =
  case mount.mountType of
    BindMount =>
      if isDangerousMountPath mount.source
        then Left (DangerousMountPath mount.source)
        else if isDangerousMountPath mount.target
          then Left (DangerousMountPath mount.target)
          else Right mount
    _ => Right mount

||| Check if environment variable name suggests sensitive data
public export
isSensitiveEnvName : String -> Bool
isSensitiveEnvName name =
  let upper = toUpper name in
  any (\pattern => isInfixOf pattern upper) sensitiveEnvPatterns

||| Validate an environment variable
public export
validateEnvVar : String -> String -> Either DockerError EnvVar
validateEnvVar name value =
  if isSensitiveEnvName name
    then Left (SensitiveEnvVarDetected name)
    else Right (MkEnvVar name value False)

||| Create an environment variable, marking sensitivity
public export
mkEnvVar : String -> String -> EnvVar
mkEnvVar name value = MkEnvVar name value (isSensitiveEnvName name)

||| Check if a registry is trusted
public export
isRegistryTrusted : String -> Bool
isRegistryTrusted reg = elem reg trustedRegistries

||| Validate registry against whitelist
public export
validateRegistry : String -> Either DockerError Registry
validateRegistry "" = Right (MkRegistry defaultRegistry Nothing)
validateRegistry hostname =
  if not (isRegistryTrusted hostname)
    then Left (UntrustedRegistry hostname)
    else Right (MkRegistry hostname Nothing)

||| Check if a capability is dangerous
public export
isDangerousCapability : String -> Bool
isDangerousCapability cap = elem (toUpper cap) dangerousCapabilities

||| Validate a Linux capability
public export
validateCapability : String -> Either DockerError String
validateCapability cap =
  if isDangerousCapability cap
    then Left (CapabilityWarning cap)
    else Right cap

--------------------------------------------------------------------------------
-- Tag mutability warnings
--------------------------------------------------------------------------------

||| Common mutable tags that should be avoided
mutableTags : List String
mutableTags = ["latest", "stable", "edge", "master", "main", "develop"]

||| Check if a tag is considered mutable
public export
isMutableTag : String -> Bool
isMutableTag tag = elem (toLower tag) mutableTags

||| Warn about mutable tags
public export
warnMutableTag : ImageTag -> Either DockerError ImageTag
warnMutableTag it@(MkImageTag tag) =
  if isMutableTag tag
    then Left (MutableTagWarning tag)
    else Right it

--------------------------------------------------------------------------------
-- Image reference parsing
--------------------------------------------------------------------------------

||| Parse a simple image reference string
||| Format: [registry/]repository[:tag][@digest]
public export
parseImageRef : String -> Either DockerError ImageRef
parseImageRef "" = Left EmptyImageName
parseImageRef ref =
  -- Simplified parsing - in production would need full grammar
  let (beforeAt, afterAt) = break (== '@') ref
      hasDigest = length afterAt > 0
      (beforeTag, afterTag) = break (== ':') beforeAt
      hasTag = length afterTag > 0 && (not hasDigest || length afterTag < length afterAt)
  in do
    repo <- validateRepository beforeAt
    pure (MkImageRef Nothing repo Nothing Nothing)

--------------------------------------------------------------------------------
-- Dockerfile helpers
--------------------------------------------------------------------------------

||| Create a FROM instruction
public export
mkFrom : String -> Maybe String -> Either DockerError DockerInstruction
mkFrom image alias = do
  ref <- parseImageRef image
  pure (FROM ref alias)

||| Create a safe RUN instruction (basic shell injection check)
public export
mkRun : String -> Either DockerError DockerInstruction
mkRun "" = Left (InvalidDockerfile "empty RUN command")
mkRun cmd =
  -- Basic check for dangerous patterns
  if isInfixOf "rm -rf /" cmd || isInfixOf "chmod 777" cmd
    then Left (InvalidDockerfile "potentially dangerous command")
    else Right (RUN cmd)

||| Create an EXPOSE instruction
public export
mkExpose : Nat -> PortProtocol -> Either DockerError DockerInstruction
mkExpose port proto = do
  _ <- validatePort port
  pure (EXPOSE port proto)

||| Create a COPY instruction
public export
mkCopy : String -> String -> Maybe String -> DockerInstruction
mkCopy src dest from = COPY src dest from

||| Create a WORKDIR instruction
public export
mkWorkdir : String -> Either DockerError DockerInstruction
mkWorkdir "" = Left (InvalidDockerfile "empty WORKDIR")
mkWorkdir path =
  if not (isPrefixOf "/" path) && not (isPrefixOf "$" path)
    then Left (InvalidDockerfile "WORKDIR should be absolute path")
    else Right (WORKDIR path)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Serialize a port protocol
public export
showProtocol : PortProtocol -> String
showProtocol TCP = "tcp"
showProtocol UDP = "udp"
showProtocol SCTP = "sctp"

||| Serialize an image tag
public export
showImageTag : ImageTag -> String
showImageTag (MkImageTag t) = t

||| Serialize an image digest
public export
showImageDigest : ImageDigest -> String
showImageDigest (MkImageDigest algo hash) = algo ++ ":" ++ hash

||| Serialize a registry
public export
showRegistry : Registry -> String
showRegistry (MkRegistry hostname Nothing) = hostname
showRegistry (MkRegistry hostname (Just p)) = hostname ++ ":" ++ show p

||| Serialize a repository
public export
showRepository : Repository -> String
showRepository (MkRepository Nothing name) = name
showRepository (MkRepository (Just ns) name) = ns ++ "/" ++ name

||| Serialize an image reference
public export
showImageRef : ImageRef -> String
showImageRef ref =
  let base = maybe "" (\r => showRegistry r ++ "/") ref.registry
              ++ showRepository ref.repository
      withTag = maybe base (\t => base ++ ":" ++ showImageTag t) ref.tag
      withDigest = maybe withTag (\d => withTag ++ "@" ++ showImageDigest d) ref.digest
  in withDigest

||| Serialize a port mapping
public export
showPortMapping : PortMapping -> String
showPortMapping pm =
  let hostPart = maybe "" (\ip => ip ++ ":") pm.hostIp
                  ++ maybe "" (\p => show p ++ ":") pm.hostPort
  in hostPart ++ show pm.containerPort ++ "/" ++ showProtocol pm.protocol

||| Serialize an environment variable (masks sensitive values)
public export
showEnvVar : EnvVar -> String
showEnvVar (MkEnvVar name value True) = name ++ "=***REDACTED***"
showEnvVar (MkEnvVar name value False) = name ++ "=" ++ value

||| Serialize a Docker instruction
public export
showInstruction : DockerInstruction -> String
showInstruction (FROM ref alias) =
  "FROM " ++ showImageRef ref ++ maybe "" (\a => " AS " ++ a) alias
showInstruction (RUN cmd) = "RUN " ++ cmd
showInstruction (CMD args) = "CMD " ++ show args
showInstruction (ENTRYPOINT args) = "ENTRYPOINT " ++ show args
showInstruction (COPY src dest from) =
  "COPY " ++ maybe "" (\f => "--from=" ++ f ++ " ") from ++ src ++ " " ++ dest
showInstruction (ADD src dest) = "ADD " ++ src ++ " " ++ dest
showInstruction (ENV name value) = "ENV " ++ name ++ "=" ++ value
showInstruction (EXPOSE port proto) =
  "EXPOSE " ++ show port ++ "/" ++ showProtocol proto
showInstruction (VOLUME path) = "VOLUME " ++ path
showInstruction (WORKDIR path) = "WORKDIR " ++ path
showInstruction (USER user) = "USER " ++ user
showInstruction (ARG name def) =
  "ARG " ++ name ++ maybe "" (\d => "=" ++ d) def
showInstruction (LABEL key value) = "LABEL " ++ key ++ "=" ++ show value
showInstruction (STOPSIGNAL sig) = "STOPSIGNAL " ++ sig
showInstruction (HEALTHCHECK Nothing) = "HEALTHCHECK NONE"
showInstruction (HEALTHCHECK (Just cmd)) = "HEALTHCHECK CMD " ++ show cmd
showInstruction (SHELL args) = "SHELL " ++ show args

--------------------------------------------------------------------------------
-- Security analysis
--------------------------------------------------------------------------------

||| Security findings from analyzing Docker configuration
public export
record SecurityReport where
  constructor MkSecurityReport
  warnings : List DockerError
  sensitiveEnvVars : List String
  dangerousMounts : List String
  untrustedRegistries : List String
  mutableTags : List String

||| Empty security report
public export
emptySecurityReport : SecurityReport
emptySecurityReport = MkSecurityReport [] [] [] [] []

||| Analyze an image reference for security issues
public export
analyzeImageRef : ImageRef -> SecurityReport -> SecurityReport
analyzeImageRef ref report =
  let report' = case ref.registry of
        Nothing => report
        Just reg =>
          if not (isRegistryTrusted reg.hostname)
            then { untrustedRegistries $= (reg.hostname ::) } report
            else report
      report'' = case ref.tag of
        Nothing => report'
        Just (MkImageTag t) =>
          if isMutableTag t
            then { mutableTags $= (t ::), warnings $= (MutableTagWarning t ::) } report'
            else report'
  in report''

||| Analyze environment variables for security issues
public export
analyzeEnvVars : List EnvVar -> SecurityReport -> SecurityReport
analyzeEnvVars [] report = report
analyzeEnvVars (env :: rest) report =
  let report' = if env.isSensitive
        then { sensitiveEnvVars $= (env.name ::)
             , warnings $= (SensitiveEnvVarDetected env.name ::) } report
        else report
  in analyzeEnvVars rest report'

||| Analyze volume mounts for security issues
public export
analyzeMounts : List VolumeMount -> SecurityReport -> SecurityReport
analyzeMounts [] report = report
analyzeMounts (mount :: rest) report =
  let report' = case mount.mountType of
        BindMount =>
          if isDangerousMountPath mount.source
            then { dangerousMounts $= (mount.source ::)
                 , warnings $= (DangerousMountPath mount.source ::) } report
            else report
        _ => report
  in analyzeMounts rest report'
