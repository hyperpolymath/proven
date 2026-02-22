# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeGeo do
  @moduledoc """
  Safe geographic coordinate operations via libproven FFI.

  Coordinate validation and Haversine distance calculation are
  performed by the Idris 2 verified core.
  """

  @doc """
  Validate a geographic coordinate.

  Returns `{:ok, {lat, lon}}` or `{:error, reason}`.
  """
  @spec validate(float(), float()) :: {:ok, {float(), float()}} | {:error, atom()}
  def validate(lat, lon) when is_float(lat) and is_float(lon) do
    Proven.NIF.nif_geo_validate(lat, lon)
  end

  @doc """
  Calculate Haversine distance between two coordinates.

  Returns `{:ok, distance_meters}` or `{:error, reason}`.
  """
  @spec distance(float(), float(), float(), float()) :: {:ok, float()} | {:error, atom()}
  def distance(lat1, lon1, lat2, lon2)
      when is_float(lat1) and is_float(lon1) and is_float(lat2) and is_float(lon2) do
    Proven.NIF.nif_geo_distance(lat1, lon1, lat2, lon2)
  end
end
