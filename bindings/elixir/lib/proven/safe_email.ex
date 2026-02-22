# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeEmail do
  @moduledoc """
  Safe email validation via libproven FFI.

  Email validation is performed by the Idris 2 verified core
  implementing RFC 5321.
  """

  @doc """
  Check if an email address is valid (RFC 5321 simplified).
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(email) when is_binary(email) do
    Proven.NIF.nif_email_is_valid(email)
  end
end
