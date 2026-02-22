# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeCrypto do
  @moduledoc """
  Cryptographic safety operations via libproven FFI.

  Constant-time comparison and secure random byte generation are
  performed by the Idris 2 verified core.
  """

  @doc """
  Compare two binaries in constant time to prevent timing attacks.
  """
  @spec constant_time_compare(binary(), binary()) :: boolean()
  def constant_time_compare(a, b) when is_binary(a) and is_binary(b) do
    Proven.NIF.nif_crypto_constant_time_eq(a, b)
  end

  @doc """
  Generate cryptographically secure random bytes.
  """
  @spec random_bytes(non_neg_integer()) :: binary()
  def random_bytes(n) when is_integer(n) and n >= 0 do
    Proven.NIF.nif_crypto_random_bytes(n)
  end

  @doc """
  Securely zero out a binary. Returns a new zeroed binary of the same size.

  Note: Due to immutability, we cannot modify the original binary in place.
  """
  @spec secure_zero(binary()) :: binary()
  def secure_zero(data) when is_binary(data) do
    :binary.copy(<<0>>, byte_size(data))
  end
end
