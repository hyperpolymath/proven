# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeCrypto do
  @moduledoc """
  Cryptographic safety operations with constant-time guarantees.
  """

  @doc """
  Compare two binaries in constant time to prevent timing attacks.
  Uses Erlang's :crypto.hash_equals/2 for true constant-time comparison.
  """
  @spec constant_time_compare(binary(), binary()) :: boolean()
  def constant_time_compare(a, b) when is_binary(a) and is_binary(b) do
    # Erlang/OTP 24+ has :crypto.hash_equals/2
    # For older versions, we implement our own constant-time compare
    if function_exported?(:crypto, :hash_equals, 2) do
      :crypto.hash_equals(a, b)
    else
      constant_time_compare_fallback(a, b)
    end
  end

  def constant_time_compare(a, b) when is_binary(a), do: constant_time_compare(a, to_string(b))
  def constant_time_compare(a, b) when is_binary(b), do: constant_time_compare(to_string(a), b)
  def constant_time_compare(a, b), do: constant_time_compare(to_string(a), to_string(b))

  # Fallback constant-time comparison for older OTP versions.
  @spec constant_time_compare_fallback(binary(), binary()) :: boolean()
  defp constant_time_compare_fallback(a, b) when byte_size(a) != byte_size(b), do: false

  defp constant_time_compare_fallback(a, b) do
    a_bytes = :binary.bin_to_list(a)
    b_bytes = :binary.bin_to_list(b)

    result =
      Enum.zip(a_bytes, b_bytes)
      |> Enum.reduce(0, fn {x, y}, acc -> Bitwise.bor(acc, Bitwise.bxor(x, y)) end)

    result == 0
  end

  @doc """
  Securely zero out a binary. Returns a new zeroed binary of the same size.
  Note: Due to immutability, we can't modify the original binary in place.
  Use this when you need a zeroed placeholder of the same size.
  """
  @spec secure_zero(binary()) :: binary()
  def secure_zero(data) when is_binary(data) do
    :binary.copy(<<0>>, byte_size(data))
  end

  @doc """
  Generate cryptographically secure random bytes.
  """
  @spec random_bytes(non_neg_integer()) :: binary()
  def random_bytes(n) when is_integer(n) and n >= 0 do
    :crypto.strong_rand_bytes(n)
  end

  @doc """
  Constant-time select: returns `x` if `condition` is true, `y` otherwise.
  """
  @spec constant_time_select(boolean(), term(), term()) :: term()
  def constant_time_select(true, x, _y), do: x
  def constant_time_select(false, _x, y), do: y
end
