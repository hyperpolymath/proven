# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeMath do
  @moduledoc """
  Safe arithmetic operations via libproven FFI.

  All computation is performed in Idris 2 verified code.
  These functions cannot crash or overflow unexpectedly.
  """

  @doc """
  Safely divide two integers.

  Returns `{:ok, result}` or `{:error, :division_by_zero}`.
  """
  @spec safe_div(integer(), integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_div(numerator, denominator) when is_integer(numerator) and is_integer(denominator) do
    case Proven.NIF.nif_math_div(numerator, denominator) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :division_by_zero}
    end
  end

  @doc """
  Safely compute modulo.

  Returns `{:ok, result}` or `{:error, :division_by_zero}`.
  """
  @spec safe_mod(integer(), integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_mod(numerator, denominator) when is_integer(numerator) and is_integer(denominator) do
    case Proven.NIF.nif_math_mod(numerator, denominator) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :division_by_zero}
    end
  end

  @doc """
  Safely add two integers with overflow detection.

  Returns `{:ok, result}` or `{:error, :overflow}`.
  """
  @spec safe_add(integer(), integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_add(a, b) when is_integer(a) and is_integer(b) do
    case Proven.NIF.nif_math_add(a, b) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :overflow}
    end
  end

  @doc """
  Safely subtract two integers with overflow detection.

  Returns `{:ok, result}` or `{:error, :overflow}`.
  """
  @spec safe_sub(integer(), integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_sub(a, b) when is_integer(a) and is_integer(b) do
    case Proven.NIF.nif_math_sub(a, b) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :overflow}
    end
  end

  @doc """
  Safely multiply two integers with overflow detection.

  Returns `{:ok, result}` or `{:error, :overflow}`.
  """
  @spec safe_mul(integer(), integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_mul(a, b) when is_integer(a) and is_integer(b) do
    case Proven.NIF.nif_math_mul(a, b) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :overflow}
    end
  end

  @doc """
  Safe absolute value that handles MIN_INT correctly.

  Returns `{:ok, result}` or `{:error, :overflow}`.
  """
  @spec safe_abs(integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_abs(n) when is_integer(n) do
    case Proven.NIF.nif_math_abs(n) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :overflow}
    end
  end

  @doc """
  Clamp a value to [lo, hi].
  """
  @spec clamp(integer(), integer(), integer()) :: integer()
  def clamp(lo, hi, value) when is_integer(lo) and is_integer(hi) and is_integer(value) do
    Proven.NIF.nif_math_clamp(lo, hi, value)
  end

  @doc """
  Integer power with overflow checking.

  Returns `{:ok, result}` or `{:error, :overflow}`.
  """
  @spec safe_pow(integer(), non_neg_integer()) :: {:ok, integer()} | {:error, atom()}
  def safe_pow(base, exp) when is_integer(base) and is_integer(exp) and exp >= 0 do
    case Proven.NIF.nif_math_pow(base, exp) do
      {:ok, value} -> {:ok, value}
      {:error, _status} -> {:error, :overflow}
    end
  end
end
