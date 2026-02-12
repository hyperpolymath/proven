# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeJson do
  @moduledoc """
  Safe JSON operations with validation and size limits.

  Provides validated JSON parsing and serialization with protection against
  common attacks like deeply nested structures and oversized payloads.
  """

  @max_depth 64
  @max_size 10_485_760  # 10 MB

  @type json_value :: nil | boolean() | number() | String.t() | [json_value()] | %{String.t() => json_value()}

  @doc """
  Safely parse JSON with depth and size limits.

  Returns `{:ok, value}` on success or `{:error, reason}` on failure.

  ## Options

    * `:max_depth` - Maximum nesting depth (default: 64)
    * `:max_size` - Maximum input size in bytes (default: 10MB)

  ## Examples

      iex> Proven.SafeJson.parse(~s({"key": "value"}))
      {:ok, %{"key" => "value"}}

      iex> Proven.SafeJson.parse("invalid")
      {:error, :invalid_json}
  """
  @spec parse(String.t(), keyword()) :: {:ok, json_value()} | {:error, atom() | String.t()}
  def parse(json_string, opts \\ []) when is_binary(json_string) do
    max_size = Keyword.get(opts, :max_size, @max_size)
    max_depth = Keyword.get(opts, :max_depth, @max_depth)

    cond do
      byte_size(json_string) > max_size ->
        {:error, :payload_too_large}

      true ->
        case Jason.decode(json_string) do
          {:ok, value} ->
            if check_depth(value, max_depth, 0) do
              {:ok, value}
            else
              {:error, :max_depth_exceeded}
            end

          {:error, %Jason.DecodeError{}} ->
            {:error, :invalid_json}
        end
    end
  rescue
    _ -> {:error, :parse_error}
  end

  @doc """
  Parse JSON, raising on error.
  """
  @spec parse!(String.t(), keyword()) :: json_value()
  def parse!(json_string, opts \\ []) do
    case parse(json_string, opts) do
      {:ok, value} -> value
      {:error, reason} -> raise ArgumentError, "JSON parse error: #{inspect(reason)}"
    end
  end

  @doc """
  Safely encode a value to JSON.

  Returns `{:ok, json_string}` on success or `{:error, reason}` on failure.
  """
  @spec encode(json_value(), keyword()) :: {:ok, String.t()} | {:error, atom()}
  def encode(value, opts \\ []) do
    max_size = Keyword.get(opts, :max_size, @max_size)

    case Jason.encode(value) do
      {:ok, json} when byte_size(json) <= max_size ->
        {:ok, json}

      {:ok, _json} ->
        {:error, :payload_too_large}

      {:error, _} ->
        {:error, :encode_error}
    end
  rescue
    _ -> {:error, :encode_error}
  end

  @doc """
  Encode to JSON, raising on error.
  """
  @spec encode!(json_value(), keyword()) :: String.t()
  def encode!(value, opts \\ []) do
    case encode(value, opts) do
      {:ok, json} -> json
      {:error, reason} -> raise ArgumentError, "JSON encode error: #{inspect(reason)}"
    end
  end

  @doc """
  Validate that a string is valid JSON without fully parsing.
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(json_string) when is_binary(json_string) do
    case parse(json_string) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Get a value from nested JSON safely.

  Returns `{:ok, value}` if path exists, `{:error, :not_found}` otherwise.

  ## Examples

      iex> Proven.SafeJson.get_in(%{"a" => %{"b" => 1}}, ["a", "b"])
      {:ok, 1}

      iex> Proven.SafeJson.get_in(%{"a" => 1}, ["b"])
      {:error, :not_found}
  """
  @spec get_in(json_value(), [String.t()]) :: {:ok, json_value()} | {:error, :not_found}
  def get_in(value, []), do: {:ok, value}

  def get_in(value, [key | rest]) when is_map(value) do
    case Map.fetch(value, key) do
      {:ok, nested} -> get_in(nested, rest)
      :error -> {:error, :not_found}
    end
  end

  def get_in(value, [key | rest]) when is_list(value) do
    case Integer.parse(key) do
      {index, ""} when index >= 0 and index < length(value) ->
        get_in(Enum.at(value, index), rest)

      _ ->
        {:error, :not_found}
    end
  end

  def get_in(_value, _path), do: {:error, :not_found}

  # Private helper to check depth
  defp check_depth(_value, max_depth, current) when current > max_depth, do: false
  defp check_depth(value, max_depth, current) when is_map(value) do
    Enum.all?(Map.values(value), &check_depth(&1, max_depth, current + 1))
  end
  defp check_depth(value, max_depth, current) when is_list(value) do
    Enum.all?(value, &check_depth(&1, max_depth, current + 1))
  end
  defp check_depth(_value, _max_depth, _current), do: true
end
