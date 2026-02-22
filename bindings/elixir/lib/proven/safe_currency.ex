# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeCurrency do
  @moduledoc """
  Safe currency operations via libproven FFI.

  Currency parsing and formatting are performed by the Idris 2 verified
  core via `proven_currency_parse` and `proven_currency_format`.

  Note: Full currency operations via libproven require marshaling the
  `CurrencyResult` struct through the NIF. These will be added as the
  NIF layer expands.
  """

  # Note: proven_currency_parse and proven_currency_format involve
  # complex CurrencyResult struct marshaling. These will be added
  # when the NIF layer supports the currency C structs.
end
