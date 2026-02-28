-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.Data - Convenience re-export module for data format and serialisation safety
|||
||| Groups all data format Safe* modules into a single import for applications
||| that parse, validate, or generate structured data. Covers JSON, XML, YAML,
||| TOML, CSV, Markdown, SQL queries, schema validation, BibTeX bibliography
||| entries, and template rendering with injection prevention.
|||
||| Usage:
|||   import Proven.Data
|||
||| This single import provides access to all data format safety types,
||| constructors, parsers, and validation functions without needing 10
||| separate imports.
module Proven.Data

import public Proven.SafeJson
import public Proven.SafeXML
import public Proven.SafeYAML
import public Proven.SafeTOML
import public Proven.SafeCSV
import public Proven.SafeMarkdown
import public Proven.SafeSQL
import public Proven.SafeSchema
import public Proven.SafeBibTeX
import public Proven.SafeTemplate
