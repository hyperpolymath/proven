# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeChecksum do
  @moduledoc """
  Safe checksum operations via libproven FFI.

  CRC-32 computation is performed by the Idris 2 verified core.
  """

  @doc """
  Compute CRC-32 checksum of binary data.

  Returns `{:ok, checksum}` or `{:error, reason}`.
  """
  @spec crc32(binary()) :: {:ok, integer()} | {:error, atom()}
  def crc32(data) when is_binary(data) do
    case Proven.NIF.nif_checksum_crc32(data) do
      {:ok, value} -> {:ok, value}
      {:error, status} -> {:error, status}
    end
  end
end
