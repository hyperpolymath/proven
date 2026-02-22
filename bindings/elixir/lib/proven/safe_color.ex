# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeColor do
  @moduledoc """
  Safe color manipulation via libproven FFI.

  Color parsing, RGB/HSL conversion, and hex formatting are performed
  by the Idris 2 verified core.
  """

  @type rgb :: {0..255, 0..255, 0..255}
  @type hsl :: {float(), float(), float()}

  @doc """
  Parse a hex color string to RGB tuple.

  Supports formats: #RGB, #RRGGBB, RGB, RRGGBB.

  Returns `{:ok, {r, g, b}}` or `{:error, :invalid_format}`.
  """
  @spec from_hex(String.t()) :: {:ok, rgb()} | {:error, atom()}
  def from_hex(hex) when is_binary(hex) do
    case Proven.NIF.nif_color_parse_hex(hex) do
      {:ok, {r, g, b}} -> {:ok, {r, g, b}}
      {:error, _} -> {:error, :invalid_format}
    end
  end

  @doc """
  Convert RGB to hex string.

  Returns `{:ok, hex_string}` or `{:error, reason}`.
  """
  @spec to_hex(rgb()) :: {:ok, String.t()} | {:error, atom()}
  def to_hex({r, g, b})
      when is_integer(r) and r >= 0 and r <= 255 and
             is_integer(g) and g >= 0 and g <= 255 and
             is_integer(b) and b >= 0 and b <= 255 do
    Proven.NIF.nif_color_to_hex(r, g, b)
  end

  def to_hex(_), do: {:error, :invalid_rgb}

  @doc """
  Convert RGB to HSL.

  Returns `{:ok, {h, s, l}}` where h is 0-360, s and l are 0.0-1.0.
  """
  @spec to_hsl(rgb()) :: {:ok, hsl()}
  def to_hsl({r, g, b})
      when is_integer(r) and r >= 0 and r <= 255 and
             is_integer(g) and g >= 0 and g <= 255 and
             is_integer(b) and b >= 0 and b <= 255 do
    {h, s, l} = Proven.NIF.nif_color_rgb_to_hsl(r, g, b)
    {:ok, {h, s, l}}
  end

  def to_hsl(_), do: {:error, :invalid_rgb}
end
