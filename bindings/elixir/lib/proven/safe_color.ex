# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeColor do
  @moduledoc """
  Safe color manipulation with validated RGB, HSL, and hex formats.

  Provides color parsing, conversion, and manipulation with proper
  bounds checking and format validation.
  """

  @type rgb :: {0..255, 0..255, 0..255}
  @type rgba :: {0..255, 0..255, 0..255, float()}
  @type hsl :: {float(), float(), float()}
  @type color_result :: {:ok, rgb()} | {:error, atom()}

  @doc """
  Parse a hex color string to RGB.

  Supports formats: #RGB, #RRGGBB, RGB, RRGGBB.

  ## Examples

      iex> Proven.SafeColor.from_hex("#ff0000")
      {:ok, {255, 0, 0}}

      iex> Proven.SafeColor.from_hex("#f00")
      {:ok, {255, 0, 0}}
  """
  @spec from_hex(String.t()) :: color_result()
  def from_hex(hex) when is_binary(hex) do
    normalized = hex |> String.trim() |> String.trim_leading("#") |> String.upcase()

    case normalized do
      <<r::binary-size(1), g::binary-size(1), b::binary-size(1)>> ->
        parse_rgb(r <> r, g <> g, b <> b)

      <<r::binary-size(2), g::binary-size(2), b::binary-size(2)>> ->
        parse_rgb(r, g, b)

      _ ->
        {:error, :invalid_format}
    end
  end

  @doc """
  Convert RGB to hex string.

  ## Examples

      iex> Proven.SafeColor.to_hex({255, 0, 0})
      {:ok, "#FF0000"}
  """
  @spec to_hex(rgb()) :: {:ok, String.t()} | {:error, atom()}
  def to_hex({r, g, b}) when is_valid_rgb(r, g, b) do
    hex = "#" <> Base.encode16(<<r, g, b>>)
    {:ok, hex}
  end

  def to_hex(_), do: {:error, :invalid_rgb}

  @doc """
  Create an RGB color with validation.

  Returns `{:ok, rgb}` or `{:error, reason}`.
  """
  @spec rgb(integer(), integer(), integer()) :: color_result()
  def rgb(r, g, b) when is_valid_rgb(r, g, b), do: {:ok, {r, g, b}}
  def rgb(_, _, _), do: {:error, :out_of_range}

  @doc """
  Convert RGB to HSL.

  Returns `{:ok, {h, s, l}}` where h is 0-360, s and l are 0.0-1.0.
  """
  @spec to_hsl(rgb()) :: {:ok, hsl()} | {:error, atom()}
  def to_hsl({r, g, b}) when is_valid_rgb(r, g, b) do
    r_norm = r / 255.0
    g_norm = g / 255.0
    b_norm = b / 255.0

    max_c = max(max(r_norm, g_norm), b_norm)
    min_c = min(min(r_norm, g_norm), b_norm)
    delta = max_c - min_c

    l = (max_c + min_c) / 2.0

    {h, s} =
      if delta < 0.0001 do
        {0.0, 0.0}
      else
        s =
          if l > 0.5 do
            delta / (2.0 - max_c - min_c)
          else
            delta / (max_c + min_c)
          end

        h =
          cond do
            max_c == r_norm ->
              60.0 * rem_float((g_norm - b_norm) / delta, 6.0)

            max_c == g_norm ->
              60.0 * ((b_norm - r_norm) / delta + 2.0)

            true ->
              60.0 * ((r_norm - g_norm) / delta + 4.0)
          end

        h = if h < 0.0, do: h + 360.0, else: h
        {h, s}
      end

    {:ok, {h, s, l}}
  end

  def to_hsl(_), do: {:error, :invalid_rgb}

  @doc """
  Convert HSL to RGB.

  Takes h (0-360), s (0.0-1.0), l (0.0-1.0).
  """
  @spec from_hsl(float(), float(), float()) :: color_result()
  def from_hsl(h, s, l) when is_float(h) and is_float(s) and is_float(l) do
    h_clamped = rem_float(h, 360.0)
    h_clamped = if h_clamped < 0.0, do: h_clamped + 360.0, else: h_clamped
    s_clamped = clamp(s, 0.0, 1.0)
    l_clamped = clamp(l, 0.0, 1.0)

    c = (1.0 - abs(2.0 * l_clamped - 1.0)) * s_clamped
    x = c * (1.0 - abs(rem_float(h_clamped / 60.0, 2.0) - 1.0))
    m = l_clamped - c / 2.0

    {r1, g1, b1} =
      cond do
        h_clamped < 60.0 -> {c, x, 0.0}
        h_clamped < 120.0 -> {x, c, 0.0}
        h_clamped < 180.0 -> {0.0, c, x}
        h_clamped < 240.0 -> {0.0, x, c}
        h_clamped < 300.0 -> {x, 0.0, c}
        true -> {c, 0.0, x}
      end

    r = round((r1 + m) * 255.0)
    g = round((g1 + m) * 255.0)
    b = round((b1 + m) * 255.0)

    {:ok, {clamp_byte(r), clamp_byte(g), clamp_byte(b)}}
  end

  @doc """
  Lighten a color by a percentage (0.0-1.0).
  """
  @spec lighten(rgb(), float()) :: color_result()
  def lighten({r, g, b}, amount) when is_valid_rgb(r, g, b) and is_float(amount) do
    with {:ok, {h, s, l}} <- to_hsl({r, g, b}) do
      new_l = clamp(l + amount, 0.0, 1.0)
      from_hsl(h, s, new_l)
    end
  end

  @doc """
  Darken a color by a percentage (0.0-1.0).
  """
  @spec darken(rgb(), float()) :: color_result()
  def darken({r, g, b}, amount) when is_valid_rgb(r, g, b) and is_float(amount) do
    with {:ok, {h, s, l}} <- to_hsl({r, g, b}) do
      new_l = clamp(l - amount, 0.0, 1.0)
      from_hsl(h, s, new_l)
    end
  end

  @doc """
  Mix two colors with a ratio (0.0-1.0 weight for first color).
  """
  @spec mix(rgb(), rgb(), float()) :: color_result()
  def mix({r1, g1, b1}, {r2, g2, b2}, ratio)
      when is_valid_rgb(r1, g1, b1) and is_valid_rgb(r2, g2, b2) and is_float(ratio) do
    weight = clamp(ratio, 0.0, 1.0)
    inv_weight = 1.0 - weight

    r = round(r1 * weight + r2 * inv_weight)
    g = round(g1 * weight + g2 * inv_weight)
    b = round(b1 * weight + b2 * inv_weight)

    {:ok, {clamp_byte(r), clamp_byte(g), clamp_byte(b)}}
  end

  @doc """
  Get the luminance of a color (0.0-1.0).

  Uses the sRGB luminance formula.
  """
  @spec luminance(rgb()) :: float()
  def luminance({r, g, b}) when is_valid_rgb(r, g, b) do
    r_linear = srgb_to_linear(r / 255.0)
    g_linear = srgb_to_linear(g / 255.0)
    b_linear = srgb_to_linear(b / 255.0)

    0.2126 * r_linear + 0.7152 * g_linear + 0.0722 * b_linear
  end

  @doc """
  Calculate contrast ratio between two colors.

  Returns a value from 1.0 to 21.0.
  """
  @spec contrast_ratio(rgb(), rgb()) :: float()
  def contrast_ratio(color1, color2) do
    l1 = luminance(color1)
    l2 = luminance(color2)

    lighter = max(l1, l2)
    darker = min(l1, l2)

    (lighter + 0.05) / (darker + 0.05)
  end

  @doc """
  Check if colors have sufficient contrast for WCAG AA (4.5:1 for normal text).
  """
  @spec wcag_aa?(rgb(), rgb()) :: boolean()
  def wcag_aa?(color1, color2) do
    contrast_ratio(color1, color2) >= 4.5
  end

  @doc """
  Check if colors have sufficient contrast for WCAG AAA (7:1 for normal text).
  """
  @spec wcag_aaa?(rgb(), rgb()) :: boolean()
  def wcag_aaa?(color1, color2) do
    contrast_ratio(color1, color2) >= 7.0
  end

  # Private helpers
  defguardp is_valid_rgb(r, g, b)
            when is_integer(r) and r >= 0 and r <= 255 and
                 is_integer(g) and g >= 0 and g <= 255 and
                 is_integer(b) and b >= 0 and b <= 255

  defp parse_rgb(r_hex, g_hex, b_hex) do
    with {:ok, r} <- parse_hex_byte(r_hex),
         {:ok, g} <- parse_hex_byte(g_hex),
         {:ok, b} <- parse_hex_byte(b_hex) do
      {:ok, {r, g, b}}
    end
  end

  defp parse_hex_byte(hex) do
    case Integer.parse(hex, 16) do
      {value, ""} when value >= 0 and value <= 255 -> {:ok, value}
      _ -> {:error, :invalid_hex}
    end
  end

  defp clamp(value, min_val, max_val) do
    value |> max(min_val) |> min(max_val)
  end

  defp clamp_byte(value) do
    value |> max(0) |> min(255)
  end

  defp rem_float(a, b), do: a - Float.floor(a / b) * b

  defp srgb_to_linear(value) do
    if value <= 0.04045 do
      value / 12.92
    else
      :math.pow((value + 0.055) / 1.055, 2.4)
    end
  end
end
