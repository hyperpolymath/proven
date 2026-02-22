# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeMl do
  @moduledoc """
  Safe machine learning activation functions via libproven FFI.

  Sigmoid and ReLU are performed by the Idris 2 verified core.
  """

  @doc """
  Sigmoid activation function: 1 / (1 + exp(-x)).
  """
  @spec sigmoid(float()) :: float()
  def sigmoid(x) when is_float(x) do
    Proven.NIF.nif_ml_sigmoid(x)
  end

  @doc """
  ReLU activation function: max(0, x).
  """
  @spec relu(float()) :: float()
  def relu(x) when is_float(x) do
    Proven.NIF.nif_ml_relu(x)
  end
end
