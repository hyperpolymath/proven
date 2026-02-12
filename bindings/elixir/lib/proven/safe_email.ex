# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeEmail do
  @moduledoc """
  Safe email validation and parsing operations.
  """

  defmodule Parts do
    @moduledoc """
    Represents the parts of an email address.
    """
    defstruct [:local_part, :domain]

    @type t :: %__MODULE__{
            local_part: String.t(),
            domain: String.t()
          }
  end

  @doc """
  Check if an email address is valid (basic check).
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(email) do
    case String.split(email, "@") do
      [local_part, domain] ->
        String.length(local_part) > 0 and
          String.length(domain) >= 3 and
          String.contains?(domain, ".") and
          not String.starts_with?(domain, ".") and
          not String.ends_with?(domain, ".")

      _ ->
        false
    end
  end

  @doc """
  Split an email into local part and domain.
  Returns {:ok, parts} or {:error, :invalid_email}.
  """
  @spec split(String.t()) :: {:ok, Parts.t()} | {:error, :invalid_email}
  def split(email) do
    if valid?(email) do
      [local_part, domain] = String.split(email, "@")
      {:ok, %Parts{local_part: local_part, domain: domain}}
    else
      {:error, :invalid_email}
    end
  end

  @doc """
  Extract the domain from an email address.
  """
  @spec get_domain(String.t()) :: {:ok, String.t()} | {:error, :invalid_email}
  def get_domain(email) do
    case split(email) do
      {:ok, parts} -> {:ok, parts.domain}
      error -> error
    end
  end

  @doc """
  Extract the local part from an email address.
  """
  @spec get_local_part(String.t()) :: {:ok, String.t()} | {:error, :invalid_email}
  def get_local_part(email) do
    case split(email) do
      {:ok, parts} -> {:ok, parts.local_part}
      error -> error
    end
  end

  @doc """
  Normalize an email address (lowercase domain).
  """
  @spec normalize(String.t()) :: {:ok, String.t()} | {:error, :invalid_email}
  def normalize(email) do
    case split(email) do
      {:ok, parts} -> {:ok, parts.local_part <> "@" <> String.downcase(parts.domain)}
      error -> error
    end
  end
end
