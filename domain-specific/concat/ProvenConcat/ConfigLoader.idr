-- SPDX-License-Identifier: PMPL-1.0
module ProvenConcat.ConfigLoader

import Proven.SafeArgs
import Proven.SafeEnv
import Proven.SafePath
import Proven.SafeFile
import Proven.SafeSchema
import Proven.SafeJson
import Proven.SafeTOML
import Proven.SafeYAML

%default total

public export
data ConfigFormat = JSON | TOML | YAML

public export
data CoreError
  = ArgError String
  | EnvError String
  | PathError String
  | FileError String
  | ParseError String
  | SchemaError String

public export
record ConfigRequest where
  constructor MkConfigRequest
  format : ConfigFormat
  path : String

public export
record ConfigResult where
  constructor MkConfigResult
  -- Placeholder for validated config. Replace with concrete type.
  payload : String

public export
envLookupFromList : List (String, String) -> EnvLookup
envLookupFromList pairs key =
  case pairs of
    [] => Nothing
    (k, v) :: rest => if k == key then Just v else envLookupFromList rest key

public export
argvSpecs : List ArgSpec
argvSpecs =
  [ configOption
  , withAllowed ["json", "toml", "yaml"]
      (withDefault "json" (mkLongOption "format" "Config format"))
  ]

public export
formatFromString : String -> ConfigFormat
formatFromString "json" = JSON
formatFromString "toml" = TOML
formatFromString "yaml" = YAML
formatFromString _ = JSON

public export
formatFromArgs : ParsedArgs -> ConfigFormat
formatFromArgs args = formatFromString (getValueOr args "format" "json")

public export
resolveConfigPath : ParsedArgs -> EnvLookup -> Either CoreError String
resolveConfigPath args lookup =
  case requireOption args "config" of
    Ok path => Right path
    Err _ =>
      case getEnv lookup "CONFIG_PATH" of
        Ok path => Right path
        Err e => Left (EnvError (friendlyError e))

-- Core entry point (pure). Runtime should supply raw argv/env/file bytes.
public export
validateConfigRequest : List String -> List (String, String) -> Either CoreError ConfigRequest
validateConfigRequest argv envPairs =
  case parse argvSpecs argv of
    Ok parsed =>
      case resolveConfigPath parsed (envLookupFromList envPairs) of
        Right path => Right (MkConfigRequest (formatFromArgs parsed) path)
        Left e => Left e
    Err e => Left (ArgError (friendlyError e))

public export
parseAndValidateConfig : ConfigRequest -> String -> Either CoreError ConfigResult
parseAndValidateConfig req raw =
  case req.format of
    JSON => Right (MkConfigResult raw)
    TOML => Right (MkConfigResult raw)
    YAML => Right (MkConfigResult raw)
