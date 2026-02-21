# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeAngle do
  @moduledoc """
  Safe angle operations with conversion and normalization.

  Provides validated angle operations with proper handling of
  different units (degrees, radians, gradians, turns).
  """

  @type angle_unit :: :degrees | :radians | :gradians | :turns
  @type angle_result :: {:ok, float()} | {:error, atom()}

  # Constants for conversion
  @pi :math.pi()
  @two_pi 2.0 * :math.pi()

  @doc """
  Convert degrees to radians.

  ## Examples

      iex> Proven.SafeAngle.to_radians(180.0)
      {:ok, 3.141592653589793}
  """
  @spec to_radians(float()) :: {:ok, float()}
  def to_radians(degrees) when is_float(degrees) do
    {:ok, degrees * @pi / 180.0}
  end

  @doc """
  Convert radians to degrees.

  ## Examples

      iex> Proven.SafeAngle.to_degrees(:math.pi())
      {:ok, 180.0}
  """
  @spec to_degrees(float()) :: {:ok, float()}
  def to_degrees(radians) when is_float(radians) do
    {:ok, radians * 180.0 / @pi}
  end

  @doc """
  Convert gradians to degrees.
  """
  @spec gradians_to_degrees(float()) :: {:ok, float()}
  def gradians_to_degrees(gradians) when is_float(gradians) do
    {:ok, gradians * 0.9}
  end

  @doc """
  Convert degrees to gradians.
  """
  @spec degrees_to_gradians(float()) :: {:ok, float()}
  def degrees_to_gradians(degrees) when is_float(degrees) do
    {:ok, degrees / 0.9}
  end

  @doc """
  Convert turns to degrees.
  """
  @spec turns_to_degrees(float()) :: {:ok, float()}
  def turns_to_degrees(turns) when is_float(turns) do
    {:ok, turns * 360.0}
  end

  @doc """
  Convert degrees to turns.
  """
  @spec degrees_to_turns(float()) :: {:ok, float()}
  def degrees_to_turns(degrees) when is_float(degrees) do
    {:ok, degrees / 360.0}
  end

  @doc """
  Normalize an angle to [0, 360) degrees.

  ## Examples

      iex> Proven.SafeAngle.normalize_degrees(450.0)
      {:ok, 90.0}

      iex> Proven.SafeAngle.normalize_degrees(-90.0)
      {:ok, 270.0}
  """
  @spec normalize_degrees(float()) :: {:ok, float()}
  def normalize_degrees(degrees) when is_float(degrees) do
    normalized = rem_float(degrees, 360.0)
    result = if normalized < 0.0, do: normalized + 360.0, else: normalized
    {:ok, result}
  end

  @doc """
  Normalize an angle to [0, 2*pi) radians.
  """
  @spec normalize_radians(float()) :: {:ok, float()}
  def normalize_radians(radians) when is_float(radians) do
    normalized = rem_float(radians, @two_pi)
    result = if normalized < 0.0, do: normalized + @two_pi, else: normalized
    {:ok, result}
  end

  @doc """
  Normalize an angle to [-180, 180) degrees.
  """
  @spec normalize_signed_degrees(float()) :: {:ok, float()}
  def normalize_signed_degrees(degrees) when is_float(degrees) do
    {:ok, normalized} = normalize_degrees(degrees)
    result = if normalized >= 180.0, do: normalized - 360.0, else: normalized
    {:ok, result}
  end

  @doc """
  Normalize an angle to [-pi, pi) radians.
  """
  @spec normalize_signed_radians(float()) :: {:ok, float()}
  def normalize_signed_radians(radians) when is_float(radians) do
    {:ok, normalized} = normalize_radians(radians)
    result = if normalized >= @pi, do: normalized - @two_pi, else: normalized
    {:ok, result}
  end

  @doc """
  Calculate the shortest angular difference between two angles in degrees.

  Returns a value in [-180, 180].
  """
  @spec difference_degrees(float(), float()) :: {:ok, float()}
  def difference_degrees(from_deg, to_deg) when is_float(from_deg) and is_float(to_deg) do
    diff = to_deg - from_deg
    normalize_signed_degrees(diff)
  end

  @doc """
  Calculate the shortest angular difference between two angles in radians.

  Returns a value in [-pi, pi].
  """
  @spec difference_radians(float(), float()) :: {:ok, float()}
  def difference_radians(from_rad, to_rad) when is_float(from_rad) and is_float(to_rad) do
    diff = to_rad - from_rad
    normalize_signed_radians(diff)
  end

  @doc """
  Interpolate between two angles in degrees.

  The `t` parameter should be between 0.0 and 1.0.
  Uses the shortest path.
  """
  @spec lerp_degrees(float(), float(), float()) :: {:ok, float()}
  def lerp_degrees(from_deg, to_deg, t) when is_float(from_deg) and is_float(to_deg) and is_float(t) do
    t_clamped = max(0.0, min(1.0, t))
    {:ok, diff} = difference_degrees(from_deg, to_deg)
    normalize_degrees(from_deg + diff * t_clamped)
  end

  @doc """
  Check if an angle (degrees) is within a range.

  Handles wrap-around correctly.
  """
  @spec in_range_degrees?(float(), float(), float()) :: boolean()
  def in_range_degrees?(angle, start_angle, end_angle) do
    {:ok, norm_angle} = normalize_degrees(angle)
    {:ok, norm_start} = normalize_degrees(start_angle)
    {:ok, norm_end} = normalize_degrees(end_angle)

    if norm_start <= norm_end do
      norm_angle >= norm_start and norm_angle <= norm_end
    else
      # Range wraps around 0/360
      norm_angle >= norm_start or norm_angle <= norm_end
    end
  end

  @doc """
  Convert between any angle units.
  """
  @spec convert(float(), angle_unit(), angle_unit()) :: {:ok, float()}
  def convert(value, from_unit, to_unit) when is_float(value) do
    # First convert to degrees
    {:ok, in_degrees} =
      case from_unit do
        :degrees -> {:ok, value}
        :radians -> to_degrees(value)
        :gradians -> gradians_to_degrees(value)
        :turns -> turns_to_degrees(value)
      end

    # Then convert from degrees to target
    case to_unit do
      :degrees -> {:ok, in_degrees}
      :radians -> to_radians(in_degrees)
      :gradians -> degrees_to_gradians(in_degrees)
      :turns -> degrees_to_turns(in_degrees)
    end
  end

  @doc """
  Get compass bearing name from degrees.

  Returns one of: "N", "NE", "E", "SE", "S", "SW", "W", "NW".
  """
  @spec compass_direction(float()) :: String.t()
  def compass_direction(degrees) when is_float(degrees) do
    {:ok, normalized} = normalize_degrees(degrees)

    cond do
      normalized < 22.5 or normalized >= 337.5 -> "N"
      normalized < 67.5 -> "NE"
      normalized < 112.5 -> "E"
      normalized < 157.5 -> "SE"
      normalized < 202.5 -> "S"
      normalized < 247.5 -> "SW"
      normalized < 292.5 -> "W"
      true -> "NW"
    end
  end

  @doc """
  Get compass bearing name with 16 points.
  """
  @spec compass_direction_16(float()) :: String.t()
  def compass_direction_16(degrees) when is_float(degrees) do
    {:ok, normalized} = normalize_degrees(degrees)
    directions = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]
    index = round(normalized / 22.5) |> rem(16)
    Enum.at(directions, index)
  end

  # Private helpers
  defp rem_float(a, b), do: a - Float.floor(a / b) * b
end
