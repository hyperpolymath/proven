# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeFloat do
  @moduledoc """
  Safe floating-point operations with NaN/Infinity handling.

  Provides validated float operations that handle edge cases like NaN,
  Infinity, and precision issues that commonly cause bugs.
  """

  @type float_result :: {:ok, float()} | {:error, atom()}

  # Default epsilon for approximate equality
  @default_epsilon 1.0e-10

  @doc """
  Check if a float is finite (not NaN or Infinity).

  ## Examples

      iex> Proven.SafeFloat.finite?(1.5)
      true

      iex> Proven.SafeFloat.finite?(:math.pow(10, 309))
      false
  """
  @spec finite?(float()) :: boolean()
  def finite?(value) when is_float(value) do
    not (is_nan?(value) or is_infinity?(value))
  end

  @doc """
  Check if a value is NaN.

  Note: In Elixir, NaN comparisons are tricky. This uses the property that NaN != NaN.
  """
  @spec is_nan?(float()) :: boolean()
  def is_nan?(value) when is_float(value) do
    value != value
  end

  @doc """
  Check if a value is positive or negative infinity.
  """
  @spec is_infinity?(float()) :: boolean()
  def is_infinity?(value) when is_float(value) do
    abs(value) > 1.7976931348623157e308
  end

  @doc """
  Validate that a float is finite.

  Returns `{:ok, value}` if finite, `{:error, reason}` otherwise.
  """
  @spec validate(float()) :: float_result()
  def validate(value) when is_float(value) do
    cond do
      is_nan?(value) -> {:error, :nan}
      is_infinity?(value) -> {:error, :infinity}
      true -> {:ok, value}
    end
  end

  @doc """
  Clamp a float to a range.

  ## Examples

      iex> Proven.SafeFloat.clamp(1.5, 0.0, 1.0)
      1.0

      iex> Proven.SafeFloat.clamp(-0.5, 0.0, 1.0)
      0.0
  """
  @spec clamp(float(), float(), float()) :: float()
  def clamp(value, min_val, max_val) when is_float(value) and is_float(min_val) and is_float(max_val) do
    value
    |> max(min_val)
    |> min(max_val)
  end

  @doc """
  Check if two floats are approximately equal within epsilon.

  ## Examples

      iex> Proven.SafeFloat.approximately_equal?(0.1 + 0.2, 0.3)
      true
  """
  @spec approximately_equal?(float(), float(), float()) :: boolean()
  def approximately_equal?(a, b, epsilon \\ @default_epsilon)
      when is_float(a) and is_float(b) and is_float(epsilon) do
    abs(a - b) < epsilon
  end

  @doc """
  Safe division that handles division by zero.

  Returns `{:ok, result}` or `{:error, :division_by_zero}`.
  """
  @spec safe_div(float(), float()) :: float_result()
  def safe_div(_numerator, +0.0), do: {:error, :division_by_zero}
  def safe_div(numerator, denominator) when is_float(numerator) and is_float(denominator) do
    result = numerator / denominator
    validate(result)
  end

  @doc """
  Safe square root that handles negative numbers.

  Returns `{:ok, result}` or `{:error, :negative_input}`.
  """
  @spec safe_sqrt(float()) :: float_result()
  def safe_sqrt(value) when value < 0.0, do: {:error, :negative_input}
  def safe_sqrt(value) when is_float(value), do: {:ok, :math.sqrt(value)}

  @doc """
  Safe logarithm that handles non-positive numbers.

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec safe_log(float()) :: float_result()
  def safe_log(value) when value <= 0.0, do: {:error, :non_positive_input}
  def safe_log(value) when is_float(value), do: {:ok, :math.log(value)}

  @doc """
  Round to a specific number of decimal places.
  """
  @spec round_to(float(), non_neg_integer()) :: float()
  def round_to(value, decimals) when is_float(value) and is_integer(decimals) and decimals >= 0 do
    multiplier = :math.pow(10, decimals)
    round(value * multiplier) / multiplier
  end

  @doc """
  Convert to integer, returning error if value would overflow.

  Returns `{:ok, integer}` or `{:error, reason}`.
  """
  @spec to_integer(float()) :: {:ok, integer()} | {:error, atom()}
  def to_integer(value) when is_float(value) do
    if finite?(value) do
      {:ok, trunc(value)}
    else
      {:error, :not_finite}
    end
  end

  @doc """
  Get the sign of a float (-1, 0, or 1).
  """
  @spec sign(float()) :: -1 | 0 | 1
  def sign(value) when is_float(value) do
    cond do
      value > 0.0 -> 1
      value < 0.0 -> -1
      true -> 0
    end
  end

  @doc """
  Lerp (linear interpolation) between two values.

  The `t` parameter should be between 0.0 and 1.0.
  """
  @spec lerp(float(), float(), float()) :: float()
  def lerp(a, b, t) when is_float(a) and is_float(b) and is_float(t) do
    t_clamped = clamp(t, 0.0, 1.0)
    a + (b - a) * t_clamped
  end

  @doc """
  Inverse lerp - find the t value that would produce the given value.

  Returns `{:ok, t}` or `{:error, reason}`.
  """
  @spec inverse_lerp(float(), float(), float()) :: float_result()
  def inverse_lerp(a, b, value) when is_float(a) and is_float(b) and is_float(value) do
    if approximately_equal?(a, b) do
      {:error, :same_bounds}
    else
      {:ok, (value - a) / (b - a)}
    end
  end

  @doc """
  Map a value from one range to another.
  """
  @spec map_range(float(), float(), float(), float(), float()) :: float_result()
  def map_range(value, in_min, in_max, out_min, out_max) do
    with {:ok, t} <- inverse_lerp(in_min, in_max, value) do
      {:ok, lerp(out_min, out_max, t)}
    end
  end
end
