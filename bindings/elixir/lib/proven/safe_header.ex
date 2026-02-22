# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeHeader do
  @moduledoc """
  Safe HTTP header operations via libproven FFI.

  CRLF injection detection, header name validation, and dangerous
  header detection are performed by the Idris 2 verified core.
  """

  @doc """
  Check if a string contains CRLF injection characters.
  """
  @spec has_crlf?(String.t()) :: boolean()
  def has_crlf?(s) when is_binary(s) do
    Proven.NIF.nif_header_has_crlf(s)
  end

  @doc """
  Check if a header name is valid per HTTP specification.
  """
  @spec valid_name?(String.t()) :: boolean()
  def valid_name?(name) when is_binary(name) do
    Proven.NIF.nif_header_is_valid_name(name)
  end

  @doc """
  Check if a header name is a dangerous hop-by-hop header.
  """
  @spec dangerous?(String.t()) :: boolean()
  def dangerous?(name) when is_binary(name) do
    Proven.NIF.nif_header_is_dangerous(name)
  end
end
