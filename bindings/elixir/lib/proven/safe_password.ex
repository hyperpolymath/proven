# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafePassword do
  @moduledoc """
  Safe password validation via libproven FFI.

  Password strength analysis and common pattern detection are
  performed by the Idris 2 verified core.
  """

  @doc """
  Validate password strength.

  Returns `{:ok, strength}` where strength is one of:
  `"very_weak"`, `"weak"`, `"fair"`, `"strong"`, `"very_strong"`.
  """
  @spec validate(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def validate(password) when is_binary(password) do
    Proven.NIF.nif_password_validate(password)
  end

  @doc """
  Check if a password matches common/weak patterns.
  """
  @spec common?(String.t()) :: boolean()
  def common?(password) when is_binary(password) do
    Proven.NIF.nif_password_is_common(password)
  end
end
