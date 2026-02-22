# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeString do
  @moduledoc """
  Safe string escaping operations via libproven FFI.

  All escaping is performed by the Idris 2 verified core, ensuring
  correctness of HTML, SQL, and JavaScript escaping.
  """

  @doc """
  Escape a string for safe HTML insertion.

  Returns `{:ok, escaped}` or `{:error, reason}`.
  """
  @spec escape_html(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def escape_html(value) when is_binary(value) do
    Proven.NIF.nif_string_escape_html(value)
  end

  @doc """
  Escape a string for safe SQL interpolation.

  Note: Prefer parameterized queries over string interpolation.
  Returns `{:ok, escaped}` or `{:error, reason}`.
  """
  @spec escape_sql(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def escape_sql(value) when is_binary(value) do
    Proven.NIF.nif_string_escape_sql(value)
  end

  @doc """
  Escape a string for safe JavaScript string literal insertion.

  Returns `{:ok, escaped}` or `{:error, reason}`.
  """
  @spec escape_js(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def escape_js(value) when is_binary(value) do
    Proven.NIF.nif_string_escape_js(value)
  end

  @doc """
  Check if a binary is valid UTF-8.
  """
  @spec valid_utf8?(binary()) :: boolean()
  def valid_utf8?(data) when is_binary(data) do
    Proven.NIF.nif_string_is_valid_utf8(data)
  end
end
