# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafePath do
  @moduledoc """
  Safe filesystem path operations with traversal attack prevention.
  """

  @doc """
  Check if a path contains directory traversal sequences.
  """
  @spec has_traversal?(String.t()) :: boolean()
  def has_traversal?(path) do
    String.contains?(path, "..") or String.contains?(path, "~")
  end

  @doc """
  Check if a path is safe (no traversal attacks).
  """
  @spec safe?(String.t()) :: boolean()
  def safe?(path) do
    not has_traversal?(path)
  end

  @doc """
  Sanitize a filename by removing dangerous characters.
  """
  @spec sanitize_filename(String.t()) :: String.t()
  def sanitize_filename(filename) do
    filename
    |> String.replace("..", "_")
    |> String.replace("/", "_")
    |> String.replace("\\", "_")
    |> String.replace("<", "_")
    |> String.replace(">", "_")
    |> String.replace(":", "_")
    |> String.replace("\"", "_")
    |> String.replace("|", "_")
    |> String.replace("?", "_")
    |> String.replace("*", "_")
    |> String.replace(<<0>>, "_")
  end

  @doc """
  Safely join path components, rejecting traversal attempts.
  Returns {:ok, path} or {:error, :traversal_detected}.
  """
  @spec safe_join(String.t(), [String.t()]) :: {:ok, String.t()} | {:error, :traversal_detected}
  def safe_join(base, parts) do
    if Enum.any?(parts, &has_traversal?/1) do
      {:error, :traversal_detected}
    else
      sanitized = Enum.map(parts, &sanitize_filename/1)
      path = Path.join([base | sanitized])
      {:ok, path}
    end
  end

  @doc """
  Bang version that raises on traversal detection.
  """
  @spec safe_join!(String.t(), [String.t()]) :: String.t()
  def safe_join!(base, parts) do
    case safe_join(base, parts) do
      {:ok, path} -> path
      {:error, :traversal_detected} -> raise ArgumentError, "path traversal detected"
    end
  end
end
