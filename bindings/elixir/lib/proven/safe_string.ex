# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeString do
  @moduledoc """
  Safe string operations for escaping and sanitization.
  """

  @doc """
  Escape a string for safe HTML insertion.
  """
  @spec escape_html(String.t()) :: String.t()
  def escape_html(value) do
    value
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
    |> String.replace("'", "&#x27;")
  end

  @doc """
  Escape a string for safe SQL interpolation.
  Note: Prefer parameterized queries over string interpolation.
  """
  @spec escape_sql(String.t()) :: String.t()
  def escape_sql(value) do
    String.replace(value, "'", "''")
  end

  @doc """
  Escape a string for safe JavaScript string literal insertion.
  """
  @spec escape_js(String.t()) :: String.t()
  def escape_js(value) do
    value
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("'", "\\'")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\r")
    |> String.replace("\t", "\\t")
  end

  @doc """
  Percent-encode a string for safe URL inclusion.
  """
  @spec escape_url(String.t()) :: String.t()
  def escape_url(value) do
    URI.encode(value, &URI.char_unreserved?/1)
  end

  @doc """
  Safely truncate a string to a maximum length, respecting UTF-8 boundaries.
  """
  @spec truncate_safe(String.t(), non_neg_integer(), String.t()) :: String.t()
  def truncate_safe(value, max_length, suffix \\ "...") do
    cond do
      max_length < 0 -> ""
      String.length(value) <= max_length -> value
      max_length <= String.length(suffix) -> String.slice(value, 0, max_length)
      true -> String.slice(value, 0, max_length - String.length(suffix)) <> suffix
    end
  end
end
