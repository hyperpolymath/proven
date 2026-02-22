# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafePath do
  @moduledoc """
  Safe filesystem path operations via libproven FFI.

  Traversal detection and filename sanitization are performed by the
  Idris 2 verified core.
  """

  @doc """
  Check if a path contains directory traversal sequences.
  """
  @spec has_traversal?(String.t()) :: boolean()
  def has_traversal?(path) when is_binary(path) do
    Proven.NIF.nif_path_has_traversal(path)
  end

  @doc """
  Check if a path is safe (no traversal attacks).
  """
  @spec safe?(String.t()) :: boolean()
  def safe?(path) when is_binary(path) do
    not has_traversal?(path)
  end

  @doc """
  Sanitize a filename by removing dangerous characters.

  Returns `{:ok, sanitized}` or `{:error, reason}`.
  """
  @spec sanitize_filename(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def sanitize_filename(filename) when is_binary(filename) do
    Proven.NIF.nif_path_sanitize_filename(filename)
  end

  @doc """
  Safely join path components, rejecting traversal attempts.

  Returns `{:ok, path}` or `{:error, :traversal_detected}`.
  """
  @spec safe_join(String.t(), [String.t()]) :: {:ok, String.t()} | {:error, :traversal_detected}
  def safe_join(base, parts) when is_binary(base) and is_list(parts) do
    if Enum.any?(parts, &has_traversal?/1) do
      {:error, :traversal_detected}
    else
      sanitized =
        Enum.map(parts, fn part ->
          case sanitize_filename(part) do
            {:ok, s} -> s
            {:error, _} -> "_"
          end
        end)

      {:ok, Path.join([base | sanitized])}
    end
  end
end
