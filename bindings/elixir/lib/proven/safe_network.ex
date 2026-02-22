# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeNetwork do
  @moduledoc """
  Safe network operations via libproven FFI.

  IP address validation and classification are performed by the
  Idris 2 verified core.
  """

  @doc """
  Parse an IPv4 address string.

  Returns `{:ok, {a, b, c, d}}` or `{:error, :invalid_ipv4}`.
  """
  @spec parse_ipv4(String.t()) :: {:ok, {0..255, 0..255, 0..255, 0..255}} | {:error, :invalid_ipv4}
  def parse_ipv4(address) when is_binary(address) do
    case Proven.NIF.nif_network_parse_ipv4(address) do
      {:ok, {a, b, c, d}} -> {:ok, {a, b, c, d}}
      {:error, _} -> {:error, :invalid_ipv4}
    end
  end

  @doc """
  Check if a string is a valid IPv4 address.
  """
  @spec valid_ipv4?(String.t()) :: boolean()
  def valid_ipv4?(address) when is_binary(address) do
    case parse_ipv4(address) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Check if an IPv4 address is in a private range (RFC 1918).
  """
  @spec private?(String.t()) :: boolean()
  def private?(address) when is_binary(address) do
    case parse_ipv4(address) do
      {:ok, {a, b, c, d}} -> Proven.NIF.nif_network_ipv4_is_private(a, b, c, d)
      {:error, _} -> false
    end
  end

  @doc """
  Check if an IPv4 address is a loopback address (127.0.0.0/8).
  """
  @spec loopback?(String.t()) :: boolean()
  def loopback?(address) when is_binary(address) do
    case parse_ipv4(address) do
      {:ok, {a, b, c, d}} -> Proven.NIF.nif_network_ipv4_is_loopback(a, b, c, d)
      {:error, _} -> false
    end
  end

  @doc """
  Check if an IPv4 address is public (not private or loopback).
  """
  @spec public?(String.t()) :: boolean()
  def public?(address) when is_binary(address) do
    valid_ipv4?(address) and not private?(address) and not loopback?(address)
  end
end
