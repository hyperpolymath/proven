# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeDateTime do
  @moduledoc """
  Safe date/time operations via libproven FFI.

  Leap year detection and days-in-month calculation are performed by
  the Idris 2 verified core. DateTime parsing and formatting use the
  libproven `proven_datetime_*` functions.

  Note: Full datetime parsing via libproven requires marshaling the
  `DateTimeResult` struct through the NIF. Functions that map directly
  to libproven FFI calls are included; others will be added as the
  NIF layer expands.
  """

  # Note: proven_datetime_parse and proven_datetime_format_iso8601 involve
  # complex DateTime struct marshaling. These will be added when the
  # NIF layer supports the DateTime C struct.
end
