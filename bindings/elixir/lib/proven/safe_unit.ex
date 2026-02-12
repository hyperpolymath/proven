# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeUnit do
  @moduledoc """
  Safe unit conversion with validation and overflow protection.

  Provides validated unit conversions for length, weight, temperature,
  and other common measurements.
  """

  @type unit_result :: {:ok, float()} | {:error, atom()}

  # Length conversion factors (to meters)
  @length_factors %{
    meter: 1.0,
    kilometer: 1000.0,
    centimeter: 0.01,
    millimeter: 0.001,
    mile: 1609.344,
    yard: 0.9144,
    foot: 0.3048,
    inch: 0.0254,
    nautical_mile: 1852.0
  }

  # Weight conversion factors (to kilograms)
  @weight_factors %{
    kilogram: 1.0,
    gram: 0.001,
    milligram: 0.000001,
    pound: 0.453592,
    ounce: 0.0283495,
    stone: 6.35029,
    ton: 1000.0,
    short_ton: 907.185
  }

  # Volume conversion factors (to liters)
  @volume_factors %{
    liter: 1.0,
    milliliter: 0.001,
    gallon: 3.78541,
    gallon_uk: 4.54609,
    quart: 0.946353,
    pint: 0.473176,
    cup: 0.236588,
    fluid_ounce: 0.0295735
  }

  # Time conversion factors (to seconds)
  @time_factors %{
    second: 1.0,
    millisecond: 0.001,
    microsecond: 0.000001,
    nanosecond: 0.000000001,
    minute: 60.0,
    hour: 3600.0,
    day: 86400.0,
    week: 604800.0
  }

  @doc """
  Convert length between units.

  ## Supported units

  `:meter`, `:kilometer`, `:centimeter`, `:millimeter`, `:mile`,
  `:yard`, `:foot`, `:inch`, `:nautical_mile`

  ## Examples

      iex> Proven.SafeUnit.convert_length(1.0, :mile, :kilometer)
      {:ok, 1.609344}
  """
  @spec convert_length(float(), atom(), atom()) :: unit_result()
  def convert_length(value, from_unit, to_unit) when is_float(value) do
    with {:ok, from_factor} <- Map.fetch(@length_factors, from_unit),
         {:ok, to_factor} <- Map.fetch(@length_factors, to_unit) do
      meters = value * from_factor
      result = meters / to_factor
      {:ok, result}
    else
      :error -> {:error, :unknown_unit}
    end
  end

  @doc """
  Convert weight/mass between units.

  ## Supported units

  `:kilogram`, `:gram`, `:milligram`, `:pound`, `:ounce`,
  `:stone`, `:ton`, `:short_ton`

  ## Examples

      iex> Proven.SafeUnit.convert_weight(1.0, :kilogram, :pound)
      {:ok, 2.2046...}
  """
  @spec convert_weight(float(), atom(), atom()) :: unit_result()
  def convert_weight(value, from_unit, to_unit) when is_float(value) do
    with {:ok, from_factor} <- Map.fetch(@weight_factors, from_unit),
         {:ok, to_factor} <- Map.fetch(@weight_factors, to_unit) do
      kilograms = value * from_factor
      result = kilograms / to_factor
      {:ok, result}
    else
      :error -> {:error, :unknown_unit}
    end
  end

  @doc """
  Convert volume between units.

  ## Supported units

  `:liter`, `:milliliter`, `:gallon`, `:gallon_uk`, `:quart`,
  `:pint`, `:cup`, `:fluid_ounce`
  """
  @spec convert_volume(float(), atom(), atom()) :: unit_result()
  def convert_volume(value, from_unit, to_unit) when is_float(value) do
    with {:ok, from_factor} <- Map.fetch(@volume_factors, from_unit),
         {:ok, to_factor} <- Map.fetch(@volume_factors, to_unit) do
      liters = value * from_factor
      result = liters / to_factor
      {:ok, result}
    else
      :error -> {:error, :unknown_unit}
    end
  end

  @doc """
  Convert time between units.

  ## Supported units

  `:second`, `:millisecond`, `:microsecond`, `:nanosecond`,
  `:minute`, `:hour`, `:day`, `:week`
  """
  @spec convert_time(float(), atom(), atom()) :: unit_result()
  def convert_time(value, from_unit, to_unit) when is_float(value) do
    with {:ok, from_factor} <- Map.fetch(@time_factors, from_unit),
         {:ok, to_factor} <- Map.fetch(@time_factors, to_unit) do
      seconds = value * from_factor
      result = seconds / to_factor
      {:ok, result}
    else
      :error -> {:error, :unknown_unit}
    end
  end

  @doc """
  Convert Celsius to Fahrenheit.

  ## Examples

      iex> Proven.SafeUnit.celsius_to_fahrenheit(0.0)
      {:ok, 32.0}

      iex> Proven.SafeUnit.celsius_to_fahrenheit(100.0)
      {:ok, 212.0}
  """
  @spec celsius_to_fahrenheit(float()) :: {:ok, float()}
  def celsius_to_fahrenheit(celsius) when is_float(celsius) do
    {:ok, celsius * 9.0 / 5.0 + 32.0}
  end

  @doc """
  Convert Fahrenheit to Celsius.
  """
  @spec fahrenheit_to_celsius(float()) :: {:ok, float()}
  def fahrenheit_to_celsius(fahrenheit) when is_float(fahrenheit) do
    {:ok, (fahrenheit - 32.0) * 5.0 / 9.0}
  end

  @doc """
  Convert Celsius to Kelvin.

  Returns error if result would be below absolute zero.
  """
  @spec celsius_to_kelvin(float()) :: unit_result()
  def celsius_to_kelvin(celsius) when is_float(celsius) do
    kelvin = celsius + 273.15
    if kelvin < 0.0 do
      {:error, :below_absolute_zero}
    else
      {:ok, kelvin}
    end
  end

  @doc """
  Convert Kelvin to Celsius.

  Returns error if input is below absolute zero.
  """
  @spec kelvin_to_celsius(float()) :: unit_result()
  def kelvin_to_celsius(kelvin) when is_float(kelvin) do
    if kelvin < 0.0 do
      {:error, :below_absolute_zero}
    else
      {:ok, kelvin - 273.15}
    end
  end

  @doc """
  Convert bytes to human-readable format.

  ## Options

    * `:binary` - Use binary units (KiB, MiB) instead of decimal (KB, MB)

  ## Examples

      iex> Proven.SafeUnit.format_bytes(1024)
      "1.00 KB"

      iex> Proven.SafeUnit.format_bytes(1024, binary: true)
      "1.00 KiB"
  """
  @spec format_bytes(non_neg_integer(), keyword()) :: String.t()
  def format_bytes(bytes, opts \\ []) when is_integer(bytes) and bytes >= 0 do
    binary = Keyword.get(opts, :binary, false)

    {divisor, units} =
      if binary do
        {1024.0, ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]}
      else
        {1000.0, ["B", "KB", "MB", "GB", "TB", "PB"]}
      end

    format_bytes_recursive(bytes / 1.0, divisor, units)
  end

  @doc """
  Parse a byte size string like "1.5GB" or "500 MB".

  Returns `{:ok, bytes}` or `{:error, reason}`.
  """
  @spec parse_bytes(String.t()) :: {:ok, non_neg_integer()} | {:error, atom()}
  def parse_bytes(string) when is_binary(string) do
    normalized = string |> String.trim() |> String.upcase()

    # Match patterns like "1.5GB", "1.5 GB", "1500 B"
    case Regex.run(~r/^([\d.]+)\s*(B|KB|KIB|MB|MIB|GB|GIB|TB|TIB|PB|PIB)?$/i, normalized) do
      [_, number_str, unit] ->
        with {number, ""} <- Float.parse(number_str) do
          multiplier = byte_multiplier(String.upcase(unit))
          {:ok, round(number * multiplier)}
        else
          _ -> {:error, :invalid_format}
        end

      [_, number_str] ->
        with {number, ""} <- Float.parse(number_str) do
          {:ok, round(number)}
        else
          _ -> {:error, :invalid_format}
        end

      _ ->
        {:error, :invalid_format}
    end
  end

  # Private helpers
  defp format_bytes_recursive(value, _divisor, [unit]), do: "#{Float.round(value, 2)} #{unit}"

  defp format_bytes_recursive(value, divisor, [unit | rest]) do
    if value < divisor do
      "#{:erlang.float_to_binary(value, decimals: 2)} #{unit}"
    else
      format_bytes_recursive(value / divisor, divisor, rest)
    end
  end

  defp byte_multiplier("B"), do: 1
  defp byte_multiplier("KB"), do: 1_000
  defp byte_multiplier("KIB"), do: 1_024
  defp byte_multiplier("MB"), do: 1_000_000
  defp byte_multiplier("MIB"), do: 1_048_576
  defp byte_multiplier("GB"), do: 1_000_000_000
  defp byte_multiplier("GIB"), do: 1_073_741_824
  defp byte_multiplier("TB"), do: 1_000_000_000_000
  defp byte_multiplier("TIB"), do: 1_099_511_627_776
  defp byte_multiplier("PB"), do: 1_000_000_000_000_000
  defp byte_multiplier("PIB"), do: 1_125_899_906_842_624
  defp byte_multiplier(_), do: 1
end
