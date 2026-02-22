# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeUuid do
  @moduledoc """
  Safe UUID operations via libproven FFI.

  UUID generation, parsing, and validation are performed by the
  Idris 2 verified core.
  """

  @doc """
  Parse a UUID string and return its canonical form.

  Returns `{:ok, canonical_string}` or `{:error, :invalid_uuid}`.
  """
  @spec parse(String.t()) :: {:ok, String.t()} | {:error, :invalid_uuid}
  def parse(uuid_string) when is_binary(uuid_string) do
    case Proven.NIF.nif_uuid_parse(uuid_string) do
      {:ok, canonical} -> {:ok, canonical}
      {:error, _} -> {:error, :invalid_uuid}
    end
  end

  @doc """
  Check if a string is a valid UUID format.
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Generate a random UUID v4.

  Returns `{:ok, uuid_string}`.
  """
  @spec generate_v4() :: {:ok, String.t()} | {:error, atom()}
  def generate_v4 do
    Proven.NIF.nif_uuid_v4()
  end

  @doc """
  Check if a UUID is the nil UUID (all zeros).
  """
  @spec nil?(String.t()) :: boolean()
  def nil?(uuid_string) when is_binary(uuid_string) do
    Proven.NIF.nif_uuid_is_nil(uuid_string)
  end

  @doc """
  Get the version of a UUID.

  Returns `{:ok, version}` or `{:error, :invalid_uuid}`.
  """
  @spec get_version(String.t()) :: {:ok, non_neg_integer()} | {:error, :invalid_uuid}
  def get_version(uuid_string) when is_binary(uuid_string) do
    case Proven.NIF.nif_uuid_version(uuid_string) do
      {:ok, version} -> {:ok, version}
      {:error, _} -> {:error, :invalid_uuid}
    end
  end
end
