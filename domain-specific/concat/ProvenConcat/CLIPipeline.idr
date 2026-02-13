-- SPDX-License-Identifier: PMPL-1.0
module ProvenConcat.CLIPipeline

import Proven.SafeArgs
import Proven.SafeFile
import Proven.SafeChecksum
import Proven.SafeCSV
import Proven.SafeJson
import Proven.SafeLog

%default total

public export
record PipelineRequest where
  constructor MkPipelineRequest
  inputPath : String
  outputPath : String
  format : String

public export
record PipelineResult where
  constructor MkPipelineResult
  output : String

public export
data PipelineError
  = InvalidArgs String
  | InvalidPath String
  | InvalidChecksum String
  | InvalidFormat String

public export
runPipelineCore : PipelineRequest -> Either PipelineError PipelineResult
runPipelineCore _ = Left (InvalidFormat "unimplemented")
