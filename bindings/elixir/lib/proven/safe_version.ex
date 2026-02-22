# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeVersion do
  @moduledoc """
  Safe semantic version operations via libproven FFI.

  Version parsing and comparison are performed by the Idris 2 verified
  core via `proven_version_parse` and `proven_version_compare`.

  Note: Full version parsing via libproven requires marshaling the
  `VersionResult`/`SemanticVersion` structs through the NIF. These
  will be added as the NIF layer expands.
  """

  # Note: proven_version_parse and proven_version_compare involve
  # complex SemanticVersion struct marshaling. These will be added
  # when the NIF layer supports the SemanticVersion C struct.
end
