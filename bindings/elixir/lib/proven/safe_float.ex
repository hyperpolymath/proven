# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeFloat do
  @moduledoc """
  Safe floating-point operations via libproven FFI.

  NaN/Infinity detection, safe division, square root, and logarithm
  are performed by the Idris 2 verified core.
  """

  @doc """
  Check if a float is finite (not NaN or Infinity).
  """
  @spec finite?(float()) :: boolean()
  def finite?(value) when is_float(value) do
    Proven.NIF.nif_float_is_finite(value)
  end

  @doc """
  Check if a value is NaN.
  """
  @spec is_nan?(float()) :: boolean()
  def is_nan?(value) when is_float(value) do
    Proven.NIF.nif_float_is_nan(value)
  end

  @doc """
  Safe division that handles division by zero and non-finite results.

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec safe_div(float(), float()) :: {:ok, float()} | {:error, atom()}
  def safe_div(numerator, denominator)
      when is_float(numerator) and is_float(denominator) do
    Proven.NIF.nif_float_div(numerator, denominator)
  end

  @doc """
  Safe square root that handles negative numbers.

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec safe_sqrt(float()) :: {:ok, float()} | {:error, atom()}
  def safe_sqrt(value) when is_float(value) do
    Proven.NIF.nif_float_sqrt(value)
  end

  @doc """
  Safe natural logarithm that handles non-positive numbers.

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec safe_log(float()) :: {:ok, float()} | {:error, atom()}
  def safe_log(value) when is_float(value) do
    Proven.NIF.nif_float_ln(value)
  end
end
