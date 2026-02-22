# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeJson do
  @moduledoc """
  Safe JSON validation via libproven FFI.

  JSON validation is performed by the Idris 2 verified core.
  """

  @doc """
  Validate that a string is valid JSON.

  Uses the verified JSON parser in libproven.
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(json_string) when is_binary(json_string) do
    Proven.NIF.nif_json_is_valid(json_string)
  end
end
