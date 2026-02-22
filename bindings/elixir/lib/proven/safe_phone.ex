# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafePhone do
  @moduledoc """
  Safe phone number operations via libproven FFI.

  Phone number parsing and E.164 formatting are performed by the Idris 2
  verified core via `proven_phone_parse` and `proven_phone_format_e164`.

  Note: Full phone operations via libproven require marshaling the
  `PhoneResult` struct through the NIF. These will be added as the
  NIF layer expands.
  """

  # Note: proven_phone_parse and proven_phone_format_e164 involve
  # complex PhoneResult struct marshaling. These will be added
  # when the NIF layer supports the phone C structs.
end
