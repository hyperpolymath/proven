# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeCookie do
  @moduledoc """
  Safe HTTP cookie operations via libproven FFI.

  Cookie injection detection and prefix classification are performed
  by the Idris 2 verified core.
  """

  @doc """
  Check if a cookie value contains injection characters (; \\r \\n).
  """
  @spec has_injection?(String.t()) :: boolean()
  def has_injection?(s) when is_binary(s) do
    Proven.NIF.nif_cookie_has_injection(s)
  end

  @doc """
  Get the cookie prefix type.

  Returns `:no_prefix`, `:secure_prefix`, or `:host_prefix`.
  """
  @spec get_prefix(String.t()) :: :no_prefix | :secure_prefix | :host_prefix
  def get_prefix(name) when is_binary(name) do
    case Proven.NIF.nif_cookie_get_prefix(name) do
      0 -> :no_prefix
      1 -> :secure_prefix
      2 -> :host_prefix
      _ -> :no_prefix
    end
  end
end
