# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeUnit do
  @moduledoc """
  Safe unit conversion via libproven FFI.

  Length and temperature conversions are performed by the Idris 2
  verified core.
  """

  # Length unit enum values (must match Zig LengthUnit)
  @length_units %{
    meter: 0,
    kilometer: 1,
    centimeter: 2,
    millimeter: 3,
    mile: 4,
    yard: 5,
    foot: 6,
    inch: 7,
    nautical_mile: 8
  }

  # Temperature unit enum values (must match Zig TempUnit)
  @temp_units %{
    celsius: 0,
    fahrenheit: 1,
    kelvin: 2
  }

  @doc """
  Convert length between units.

  ## Supported units

  `:meter`, `:kilometer`, `:centimeter`, `:millimeter`, `:mile`,
  `:yard`, `:foot`, `:inch`, `:nautical_mile`

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec convert_length(float(), atom(), atom()) :: {:ok, float()} | {:error, atom()}
  def convert_length(value, from_unit, to_unit) when is_float(value) do
    with {:ok, from_id} <- lookup_length_unit(from_unit),
         {:ok, to_id} <- lookup_length_unit(to_unit) do
      Proven.NIF.nif_unit_convert_length(value, from_id, to_id)
    end
  end

  @doc """
  Convert temperature between units.

  ## Supported units

  `:celsius`, `:fahrenheit`, `:kelvin`

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec convert_temp(float(), atom(), atom()) :: {:ok, float()} | {:error, atom()}
  def convert_temp(value, from_unit, to_unit) when is_float(value) do
    with {:ok, from_id} <- lookup_temp_unit(from_unit),
         {:ok, to_id} <- lookup_temp_unit(to_unit) do
      Proven.NIF.nif_unit_convert_temp(value, from_id, to_id)
    end
  end

  @doc """
  Convert Celsius to Fahrenheit.
  """
  @spec celsius_to_fahrenheit(float()) :: {:ok, float()} | {:error, atom()}
  def celsius_to_fahrenheit(celsius) when is_float(celsius) do
    convert_temp(celsius, :celsius, :fahrenheit)
  end

  @doc """
  Convert Fahrenheit to Celsius.
  """
  @spec fahrenheit_to_celsius(float()) :: {:ok, float()} | {:error, atom()}
  def fahrenheit_to_celsius(fahrenheit) when is_float(fahrenheit) do
    convert_temp(fahrenheit, :fahrenheit, :celsius)
  end

  @doc """
  Convert Celsius to Kelvin.
  """
  @spec celsius_to_kelvin(float()) :: {:ok, float()} | {:error, atom()}
  def celsius_to_kelvin(celsius) when is_float(celsius) do
    convert_temp(celsius, :celsius, :kelvin)
  end

  @doc """
  Convert Kelvin to Celsius.
  """
  @spec kelvin_to_celsius(float()) :: {:ok, float()} | {:error, atom()}
  def kelvin_to_celsius(kelvin) when is_float(kelvin) do
    convert_temp(kelvin, :kelvin, :celsius)
  end

  # Private helpers

  defp lookup_length_unit(unit) do
    case Map.get(@length_units, unit) do
      nil -> {:error, :unknown_unit}
      id -> {:ok, id}
    end
  end

  defp lookup_temp_unit(unit) do
    case Map.get(@temp_units, unit) do
      nil -> {:error, :unknown_unit}
      id -> {:ok, id}
    end
  end
end
