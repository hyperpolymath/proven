# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeDateTime do
  @moduledoc """
  Safe date and time operations with validation and overflow protection.

  Provides validated datetime parsing, formatting, and arithmetic with
  protection against invalid dates and timezone issues.
  """

  @type datetime_result :: {:ok, DateTime.t()} | {:error, atom()}
  @type date_result :: {:ok, Date.t()} | {:error, atom()}
  @type time_result :: {:ok, Time.t()} | {:error, atom()}

  @doc """
  Parse an ISO 8601 datetime string safely.

  Returns `{:ok, datetime}` or `{:error, reason}`.

  ## Examples

      iex> Proven.SafeDateTime.parse_iso8601("2025-01-15T10:30:00Z")
      {:ok, ~U[2025-01-15 10:30:00Z]}

      iex> Proven.SafeDateTime.parse_iso8601("invalid")
      {:error, :invalid_format}
  """
  @spec parse_iso8601(String.t()) :: datetime_result()
  def parse_iso8601(string) when is_binary(string) do
    case DateTime.from_iso8601(string) do
      {:ok, datetime, _offset} -> {:ok, datetime}
      {:error, _} -> {:error, :invalid_format}
    end
  end

  @doc """
  Parse a date string in YYYY-MM-DD format.

  Returns `{:ok, date}` or `{:error, reason}`.
  """
  @spec parse_date(String.t()) :: date_result()
  def parse_date(string) when is_binary(string) do
    case Date.from_iso8601(string) do
      {:ok, date} -> {:ok, date}
      {:error, _} -> {:error, :invalid_format}
    end
  end

  @doc """
  Parse a time string in HH:MM:SS format.

  Returns `{:ok, time}` or `{:error, reason}`.
  """
  @spec parse_time(String.t()) :: time_result()
  def parse_time(string) when is_binary(string) do
    case Time.from_iso8601(string) do
      {:ok, time} -> {:ok, time}
      {:error, _} -> {:error, :invalid_format}
    end
  end

  @doc """
  Safely add days to a date with overflow checking.

  Returns `{:ok, new_date}` or `{:error, reason}`.
  """
  @spec add_days(Date.t(), integer()) :: date_result()
  def add_days(%Date{} = date, days) when is_integer(days) do
    try do
      {:ok, Date.add(date, days)}
    rescue
      _ -> {:error, :overflow}
    end
  end

  @doc """
  Safely add seconds to a datetime with overflow checking.

  Returns `{:ok, new_datetime}` or `{:error, reason}`.
  """
  @spec add_seconds(DateTime.t(), integer()) :: datetime_result()
  def add_seconds(%DateTime{} = datetime, seconds) when is_integer(seconds) do
    try do
      {:ok, DateTime.add(datetime, seconds, :second)}
    rescue
      _ -> {:error, :overflow}
    end
  end

  @doc """
  Calculate the difference between two datetimes in seconds.

  Returns `{:ok, seconds}` or `{:error, reason}`.
  """
  @spec diff_seconds(DateTime.t(), DateTime.t()) :: {:ok, integer()} | {:error, atom()}
  def diff_seconds(%DateTime{} = dt1, %DateTime{} = dt2) do
    try do
      {:ok, DateTime.diff(dt1, dt2, :second)}
    rescue
      _ -> {:error, :calculation_error}
    end
  end

  @doc """
  Check if a datetime is in the past.
  """
  @spec in_past?(DateTime.t()) :: boolean()
  def in_past?(%DateTime{} = datetime) do
    DateTime.compare(datetime, DateTime.utc_now()) == :lt
  end

  @doc """
  Check if a datetime is in the future.
  """
  @spec in_future?(DateTime.t()) :: boolean()
  def in_future?(%DateTime{} = datetime) do
    DateTime.compare(datetime, DateTime.utc_now()) == :gt
  end

  @doc """
  Validate that a date has valid month/day combinations.

  Returns `{:ok, date}` if valid, `{:error, reason}` otherwise.
  """
  @spec validate_date(integer(), integer(), integer()) :: date_result()
  def validate_date(year, month, day) do
    case Date.new(year, month, day) do
      {:ok, date} -> {:ok, date}
      {:error, _} -> {:error, :invalid_date}
    end
  end

  @doc """
  Get the start of day for a datetime (midnight).
  """
  @spec start_of_day(DateTime.t()) :: DateTime.t()
  def start_of_day(%DateTime{} = datetime) do
    %{datetime | hour: 0, minute: 0, second: 0, microsecond: {0, 0}}
  end

  @doc """
  Get the end of day for a datetime (23:59:59).
  """
  @spec end_of_day(DateTime.t()) :: DateTime.t()
  def end_of_day(%DateTime{} = datetime) do
    %{datetime | hour: 23, minute: 59, second: 59, microsecond: {999_999, 6}}
  end

  @doc """
  Check if a year is a leap year.
  """
  @spec leap_year?(integer()) :: boolean()
  def leap_year?(year) when is_integer(year) do
    Calendar.ISO.leap_year?(year)
  end

  @doc """
  Get days in a month for a given year.

  Returns `{:ok, days}` or `{:error, reason}`.
  """
  @spec days_in_month(integer(), integer()) :: {:ok, integer()} | {:error, atom()}
  def days_in_month(year, month) when is_integer(year) and month in 1..12 do
    {:ok, Calendar.ISO.days_in_month(year, month)}
  end

  def days_in_month(_year, _month), do: {:error, :invalid_month}

  @doc """
  Format datetime to ISO 8601 string.
  """
  @spec to_iso8601(DateTime.t()) :: String.t()
  def to_iso8601(%DateTime{} = datetime) do
    DateTime.to_iso8601(datetime)
  end

  @doc """
  Get Unix timestamp from datetime.
  """
  @spec to_unix(DateTime.t()) :: integer()
  def to_unix(%DateTime{} = datetime) do
    DateTime.to_unix(datetime)
  end

  @doc """
  Create datetime from Unix timestamp.

  Returns `{:ok, datetime}` or `{:error, reason}`.
  """
  @spec from_unix(integer()) :: datetime_result()
  def from_unix(timestamp) when is_integer(timestamp) do
    case DateTime.from_unix(timestamp) do
      {:ok, datetime} -> {:ok, datetime}
      {:error, _} -> {:error, :invalid_timestamp}
    end
  end
end
