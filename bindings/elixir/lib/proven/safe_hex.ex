# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeHex do
  @moduledoc """
  Safe hexadecimal encoding and decoding via libproven FFI.

  Hex operations are performed by the Idris 2 verified core.
  """

  @doc """
  Encode binary data to a hexadecimal string.

  ## Options

  - `:case` - `:lower` (default) or `:upper`

  Returns `{:ok, hex_string}`.
  """
  @spec encode(binary(), keyword()) :: {:ok, String.t()} | {:error, atom()}
  def encode(data, opts \\ []) when is_binary(data) do
    uppercase = Keyword.get(opts, :case, :lower) == :upper
    Proven.NIF.nif_hex_encode(data, uppercase)
  end

  @doc """
  Decode a hexadecimal string to binary.

  Returns `{:ok, binary}` or `{:error, reason}`.
  """
  @spec decode(String.t()) :: {:ok, binary()} | {:error, atom()}
  def decode(hex_string) when is_binary(hex_string) do
    Proven.NIF.nif_hex_decode(hex_string)
  end

  @doc """
  Check if a string is valid hexadecimal (even length, valid chars).
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(hex_string) when is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Compare two hex strings in constant time via libproven.

  Prevents timing attacks when comparing secrets.
  """
  @spec constant_time_equal(String.t(), String.t()) :: boolean()
  def constant_time_equal(hex_a, hex_b) when is_binary(hex_a) and is_binary(hex_b) do
    with {:ok, a} <- decode(hex_a),
         {:ok, b} <- decode(hex_b) do
      Proven.SafeCrypto.constant_time_compare(a, b)
    else
      _ -> false
    end
  end
end
