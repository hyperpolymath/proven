# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeAngle do
  @moduledoc """
  Safe angle operations via libproven FFI.

  Degree/radian conversion and normalization are performed by the
  Idris 2 verified core.
  """

  @doc """
  Convert degrees to radians.
  """
  @spec to_radians(float()) :: {:ok, float()}
  def to_radians(degrees) when is_float(degrees) do
    {:ok, Proven.NIF.nif_angle_deg_to_rad(degrees)}
  end

  @doc """
  Convert radians to degrees.
  """
  @spec to_degrees(float()) :: {:ok, float()}
  def to_degrees(radians) when is_float(radians) do
    {:ok, Proven.NIF.nif_angle_rad_to_deg(radians)}
  end

  @doc """
  Normalize an angle to [0, 360) degrees.
  """
  @spec normalize_degrees(float()) :: {:ok, float()}
  def normalize_degrees(degrees) when is_float(degrees) do
    {:ok, Proven.NIF.nif_angle_normalize_degrees(degrees)}
  end

  @doc """
  Normalize an angle to [0, 2*pi) radians.
  """
  @spec normalize_radians(float()) :: {:ok, float()}
  def normalize_radians(radians) when is_float(radians) do
    {:ok, Proven.NIF.nif_angle_normalize_radians(radians)}
  end
end
