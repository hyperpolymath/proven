# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeNetwork do
  @moduledoc """
  Safe network operations for IP address validation and classification.
  """

  defmodule IPv4 do
    @moduledoc """
    Represents an IPv4 address.
    """
    defstruct [:a, :b, :c, :d]

    @type t :: %__MODULE__{
            a: 0..255,
            b: 0..255,
            c: 0..255,
            d: 0..255
          }

    defimpl String.Chars do
      def to_string(%IPv4{a: a, b: b, c: c, d: d}) do
        "#{a}.#{b}.#{c}.#{d}"
      end
    end
  end

  @doc """
  Parse an IPv4 address string.
  Returns {:ok, ipv4} or {:error, :invalid_ipv4}.
  """
  @spec parse_ipv4(String.t()) :: {:ok, IPv4.t()} | {:error, :invalid_ipv4}
  def parse_ipv4(address) do
    parts = String.split(address, ".")

    if length(parts) == 4 do
      octets =
        Enum.map(parts, fn part ->
          case Integer.parse(part) do
            {n, ""} when n >= 0 and n <= 255 -> n
            _ -> nil
          end
        end)

      if Enum.all?(octets, &(&1 != nil)) do
        [a, b, c, d] = octets
        {:ok, %IPv4{a: a, b: b, c: c, d: d}}
      else
        {:error, :invalid_ipv4}
      end
    else
      {:error, :invalid_ipv4}
    end
  end

  @doc """
  Check if a string is a valid IPv4 address.
  """
  @spec valid_ipv4?(String.t()) :: boolean()
  def valid_ipv4?(address) do
    case parse_ipv4(address) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Check if an IPv4 address is in a private range.
  """
  @spec private?(String.t()) :: boolean()
  def private?(address) do
    case parse_ipv4(address) do
      {:ok, %IPv4{a: 10}} ->
        true

      {:ok, %IPv4{a: 172, b: b}} when b >= 16 and b <= 31 ->
        true

      {:ok, %IPv4{a: 192, b: 168}} ->
        true

      _ ->
        false
    end
  end

  @doc """
  Check if an IPv4 address is a loopback address (127.0.0.0/8).
  """
  @spec loopback?(String.t()) :: boolean()
  def loopback?(address) do
    case parse_ipv4(address) do
      {:ok, %IPv4{a: 127}} -> true
      _ -> false
    end
  end

  @doc """
  Check if an IPv4 address is public (not private or loopback).
  """
  @spec public?(String.t()) :: boolean()
  def public?(address) do
    valid_ipv4?(address) and not private?(address) and not loopback?(address)
  end

  @doc """
  Format an IPv4 address as a string.
  """
  @spec format_ipv4(IPv4.t()) :: String.t()
  def format_ipv4(ip), do: to_string(ip)
end
