-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-logcollector structured log ingestion server.
||| Defines closed sum types for log levels, input formats, output targets,
||| filter operations, and pipeline stages.
module Logcollector.Types

%default total

---------------------------------------------------------------------------
-- LogLevel: Severity classification for log entries.
---------------------------------------------------------------------------

||| Standard log severity levels, ordered from most verbose (Trace) to
||| most severe (Fatal). Used for filtering and routing decisions.
public export
data LogLevel
  = Trace -- ^ Very fine-grained diagnostic output
  | Debug -- ^ Diagnostic information useful during development
  | Info  -- ^ Normal operational messages
  | Warn  -- ^ Potential issue that does not prevent normal operation
  | Error -- ^ Error condition that impairs functionality
  | Fatal -- ^ Unrecoverable error requiring immediate shutdown

||| Display a human-readable label for each log level.
public export
Show LogLevel where
  show Trace = "Trace"
  show Debug = "Debug"
  show Info  = "Info"
  show Warn  = "Warn"
  show Error = "Error"
  show Fatal = "Fatal"

---------------------------------------------------------------------------
-- InputFormat: The wire format of incoming log data.
---------------------------------------------------------------------------

||| Specifies the serialisation format of log data received by the
||| collector on its input endpoints.
public export
data InputFormat
  = JSON   -- ^ Structured JSON log lines
  | Logfmt -- ^ Key=value logfmt format (Heroku/Go convention)
  | Syslog -- ^ RFC 5424 / RFC 3164 syslog messages
  | CEF    -- ^ ArcSight Common Event Format
  | GELF   -- ^ Graylog Extended Log Format
  | Raw    -- ^ Unstructured plain text (line-delimited)

||| Display a human-readable label for each input format.
public export
Show InputFormat where
  show JSON   = "JSON"
  show Logfmt = "Logfmt"
  show Syslog = "Syslog"
  show CEF    = "CEF"
  show GELF   = "GELF"
  show Raw    = "Raw"

---------------------------------------------------------------------------
-- OutputTarget: Destination for processed log data.
---------------------------------------------------------------------------

||| Specifies where processed log entries are shipped after passing
||| through the processing pipeline.
public export
data OutputTarget
  = File          -- ^ Write to local or remote filesystem
  | Elasticsearch -- ^ Ship to an Elasticsearch/OpenSearch cluster
  | S3            -- ^ Write to S3-compatible object storage
  | Kafka         -- ^ Publish to an Apache Kafka topic
  | Stdout        -- ^ Write to standard output (useful for debugging)

||| Display a human-readable label for each output target.
public export
Show OutputTarget where
  show File          = "File"
  show Elasticsearch = "Elasticsearch"
  show S3            = "S3"
  show Kafka         = "Kafka"
  show Stdout        = "Stdout"

---------------------------------------------------------------------------
-- FilterOp: Operations applied to log entries during pipeline processing.
---------------------------------------------------------------------------

||| Describes a processing operation applied to log entries as they flow
||| through the collection pipeline.
public export
data FilterOp
  = Include   -- ^ Keep entries matching a condition (whitelist)
  | Exclude   -- ^ Drop entries matching a condition (blacklist)
  | Transform -- ^ Modify entry fields (rename, parse, enrich)
  | Redact    -- ^ Mask or remove sensitive data from entry fields
  | Sample    -- ^ Probabilistically sample entries (1-in-N or percentage)

||| Display a human-readable label for each filter operation.
public export
Show FilterOp where
  show Include   = "Include"
  show Exclude   = "Exclude"
  show Transform = "Transform"
  show Redact    = "Redact"
  show Sample    = "Sample"

---------------------------------------------------------------------------
-- PipelineStage: Stages in the log processing pipeline.
---------------------------------------------------------------------------

||| Represents a named stage in the log processing pipeline. Entries
||| flow from Input through to Output in order.
public export
data PipelineStage
  = Input     -- ^ Receive raw log data from sources
  | Parse     -- ^ Deserialise and structure raw log data
  | Filter    -- ^ Apply include/exclude rules
  | PTransform -- ^ Apply field transformations and enrichments
  | Output    -- ^ Ship processed entries to output targets

||| Display a human-readable label for each pipeline stage.
public export
Show PipelineStage where
  show Input      = "Input"
  show Parse      = "Parse"
  show Filter     = "Filter"
  show PTransform = "Transform"
  show Output     = "Output"
