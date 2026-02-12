# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeMath do
  @moduledoc """
  Safe arithmetic operations that cannot crash or overflow unexpectedly.
  """

  # Maximum and minimum values for 64-bit signed integers
  @max_int64 9_223_372_036_854_775_807
  @min_int64 -9_223_372_036_854_775_808

  @doc """
  Safely divide two integers, returning {:ok, result} or {:error, :division_by_zero}.
  """
  @spec safe_div(integer(), integer()) :: {:ok, integer()} | {:error, :division_by_zero}
  def safe_div(_numerator, 0), do: {:error, :division_by_zero}
  def safe_div(numerator, denominator), do: {:ok, div(numerator, denominator)}

  @doc """
  Safely compute modulo, returning {:ok, result} or {:error, :division_by_zero}.
  """
  @spec safe_mod(integer(), integer()) :: {:ok, integer()} | {:error, :division_by_zero}
  def safe_mod(_numerator, 0), do: {:error, :division_by_zero}
  def safe_mod(numerator, denominator), do: {:ok, rem(numerator, denominator)}

  @doc """
  Safely add two integers, returning {:ok, result} or {:error, :overflow}.
  Note: Elixir integers are arbitrary precision, so overflow is simulated for bounded contexts.
  """
  @spec safe_add(integer(), integer()) :: {:ok, integer()} | {:error, :overflow}
  def safe_add(a, b) do
    result = a + b
    if result > @max_int64 or result < @min_int64 do
      {:error, :overflow}
    else
      {:ok, result}
    end
  end

  @doc """
  Safely subtract two integers, returning {:ok, result} or {:error, :overflow}.
  """
  @spec safe_sub(integer(), integer()) :: {:ok, integer()} | {:error, :overflow}
  def safe_sub(a, b) do
    result = a - b
    if result > @max_int64 or result < @min_int64 do
      {:error, :overflow}
    else
      {:ok, result}
    end
  end

  @doc """
  Safely multiply two integers, returning {:ok, result} or {:error, :overflow}.
  """
  @spec safe_mul(integer(), integer()) :: {:ok, integer()} | {:error, :overflow}
  def safe_mul(a, b) do
    result = a * b
    if result > @max_int64 or result < @min_int64 do
      {:error, :overflow}
    else
      {:ok, result}
    end
  end

  @doc """
  Bang versions that raise on error.
  """
  def safe_div!(numerator, denominator) do
    case safe_div(numerator, denominator) do
      {:ok, result} -> result
      {:error, :division_by_zero} -> raise ArithmeticError, "division by zero"
    end
  end

  def safe_add!(a, b) do
    case safe_add(a, b) do
      {:ok, result} -> result
      {:error, :overflow} -> raise ArithmeticError, "integer overflow"
    end
  end

  def safe_sub!(a, b) do
    case safe_sub(a, b) do
      {:ok, result} -> result
      {:error, :overflow} -> raise ArithmeticError, "integer overflow"
    end
  end

  def safe_mul!(a, b) do
    case safe_mul(a, b) do
      {:ok, result} -> result
      {:error, :overflow} -> raise ArithmeticError, "integer overflow"
    end
  end
end
