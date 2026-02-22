# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeProbability do
  @moduledoc """
  Safe probability operations via libproven FFI.

  Probability clamping and composition are performed by the
  Idris 2 verified core.
  """

  @doc """
  Create a probability value clamped to [0.0, 1.0].
  """
  @spec create(float()) :: float()
  def create(value) when is_float(value) do
    Proven.NIF.nif_probability_create(value)
  end

  @doc """
  Compute P(A and B) for independent events.
  """
  @spec prob_and(float(), float()) :: float()
  def prob_and(a, b) when is_float(a) and is_float(b) do
    Proven.NIF.nif_probability_and(a, b)
  end

  @doc """
  Compute P(A or B) for mutually exclusive events.
  """
  @spec prob_or(float(), float()) :: float()
  def prob_or(a, b) when is_float(a) and is_float(b) do
    Proven.NIF.nif_probability_or(a, b)
  end

  @doc """
  Compute P(not A) = 1 - P(A).
  """
  @spec complement(float()) :: float()
  def complement(p) when is_float(p) do
    Proven.NIF.nif_probability_not(p)
  end
end
