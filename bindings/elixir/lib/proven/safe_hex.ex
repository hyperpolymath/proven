# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeHex do
  @moduledoc """
  Safe hexadecimal encoding and decoding operations.

  Provides hex encoding/decoding with validation, case control,
  and constant-time comparison for security-sensitive contexts.
  """

  @doc """
  Encode binary data to a hexadecimal string.

  ## Options

  - `:case` - `:lower` (default) or `:upper`

  ## Examples

      iex> Proven.SafeHex.encode(<<255, 0, 128>>)
      {:ok, "ff0080"}

      iex> Proven.SafeHex.encode(<<255, 0, 128>>, case: :upper)
      {:ok, "FF0080"}
  """
  @spec encode(binary(), keyword()) :: {:ok, String.t()}
  def encode(data, opts \\ []) when is_binary(data) do
    case_option = Keyword.get(opts, :case, :lower)
    {:ok, Base.encode16(data, case: case_option)}
  end

  @doc """
  Encode binary data to hex, raising on error.

  ## Examples

      iex> Proven.SafeHex.encode!(<<255, 0, 128>>)
      "ff0080"
  """
  @spec encode!(binary(), keyword()) :: String.t()
  def encode!(data, opts \\ []) when is_binary(data) do
    {:ok, result} = encode(data, opts)
    result
  end

  @doc """
  Decode a hexadecimal string to binary.

  Accepts both uppercase and lowercase hex characters.
  Validates that the input contains only valid hex characters
  and has an even length.

  ## Examples

      iex> Proven.SafeHex.decode("ff0080")
      {:ok, <<255, 0, 128>>}

      iex> Proven.SafeHex.decode("FF0080")
      {:ok, <<255, 0, 128>>}

      iex> Proven.SafeHex.decode("invalid")
      {:error, :invalid_hex}

      iex> Proven.SafeHex.decode("abc")
      {:error, :odd_length}
  """
  @spec decode(String.t()) :: {:ok, binary()} | {:error, :invalid_hex | :odd_length}
  def decode(hex_string) when is_binary(hex_string) do
    cleaned = String.trim(hex_string)

    cond do
      rem(byte_size(cleaned), 2) != 0 ->
        {:error, :odd_length}

      not valid_hex_chars?(cleaned) ->
        {:error, :invalid_hex}

      true ->
        {:ok, Base.decode16!(cleaned, case: :mixed)}
    end
  end

  @doc """
  Decode a hexadecimal string to binary, raising on error.

  ## Examples

      iex> Proven.SafeHex.decode!("ff0080")
      <<255, 0, 128>>
  """
  @spec decode!(String.t()) :: binary()
  def decode!(hex_string) when is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, result} -> result
      {:error, :invalid_hex} -> raise ArgumentError, "invalid hex characters in string"
      {:error, :odd_length} -> raise ArgumentError, "hex string has odd length"
    end
  end

  @doc """
  Check if a string is valid hexadecimal.

  ## Examples

      iex> Proven.SafeHex.valid?("ff0080")
      true

      iex> Proven.SafeHex.valid?("ABCDEF")
      true

      iex> Proven.SafeHex.valid?("xyz")
      false

      iex> Proven.SafeHex.valid?("abc")
      false  # Odd length
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(hex_string) when is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Compare two hex strings in constant time.

  Prevents timing attacks when comparing secrets like API keys or tokens.
  Both inputs are normalized to the same case before comparison.

  Returns `true` if both decode to the same binary, `false` otherwise
  (including if either is invalid hex).

  ## Examples

      iex> Proven.SafeHex.constant_time_equal("ff0080", "FF0080")
      true

      iex> Proven.SafeHex.constant_time_equal("ff0080", "ff0081")
      false
  """
  @spec constant_time_equal(String.t(), String.t()) :: boolean()
  def constant_time_equal(hex_a, hex_b) when is_binary(hex_a) and is_binary(hex_b) do
    with {:ok, a} <- decode(hex_a),
         {:ok, b} <- decode(hex_b) do
      Proven.SafeCrypto.constant_time_compare(a, b)
    else
      _ -> false
    end
  end

  @doc """
  Constant-time comparison of a binary and a hex string.

  ## Examples

      iex> Proven.SafeHex.constant_time_equal_binary(<<255, 0, 128>>, "ff0080")
      true
  """
  @spec constant_time_equal_binary(binary(), String.t()) :: boolean()
  def constant_time_equal_binary(binary, hex_string)
      when is_binary(binary) and is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, decoded} -> Proven.SafeCrypto.constant_time_compare(binary, decoded)
      {:error, _} -> false
    end
  end

  @doc """
  Convert a hex string to lowercase.

  ## Examples

      iex> Proven.SafeHex.to_lower("FF0080")
      {:ok, "ff0080"}
  """
  @spec to_lower(String.t()) :: {:ok, String.t()} | {:error, :invalid_hex | :odd_length}
  def to_lower(hex_string) when is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, binary} -> encode(binary, case: :lower)
      error -> error
    end
  end

  @doc """
  Convert a hex string to uppercase.

  ## Examples

      iex> Proven.SafeHex.to_upper("ff0080")
      {:ok, "FF0080"}
  """
  @spec to_upper(String.t()) :: {:ok, String.t()} | {:error, :invalid_hex | :odd_length}
  def to_upper(hex_string) when is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, binary} -> encode(binary, case: :upper)
      error -> error
    end
  end

  @doc """
  Encode an integer as a hex string.

  ## Options

  - `:case` - `:lower` (default) or `:upper`
  - `:pad` - Minimum number of hex digits (pads with leading zeros)

  ## Examples

      iex> Proven.SafeHex.encode_integer(255)
      {:ok, "ff"}

      iex> Proven.SafeHex.encode_integer(255, case: :upper)
      {:ok, "FF"}

      iex> Proven.SafeHex.encode_integer(255, pad: 4)
      {:ok, "00ff"}

      iex> Proven.SafeHex.encode_integer(-1)
      {:error, :negative_integer}
  """
  @spec encode_integer(integer(), keyword()) ::
          {:ok, String.t()} | {:error, :negative_integer}
  def encode_integer(n, opts \\ []) when is_integer(n) do
    if n < 0 do
      {:error, :negative_integer}
    else
      case_option = Keyword.get(opts, :case, :lower)
      pad = Keyword.get(opts, :pad, 0)

      hex = Integer.to_string(n, 16)

      hex =
        if case_option == :lower do
          String.downcase(hex)
        else
          hex
        end

      # Ensure even length for proper hex representation
      hex =
        if rem(String.length(hex), 2) == 1 do
          "0" <> hex
        else
          hex
        end

      # Apply padding
      hex = String.pad_leading(hex, pad, "0")

      {:ok, hex}
    end
  end

  @doc """
  Decode a hex string to an integer.

  ## Examples

      iex> Proven.SafeHex.decode_integer("ff")
      {:ok, 255}

      iex> Proven.SafeHex.decode_integer("00ff")
      {:ok, 255}
  """
  @spec decode_integer(String.t()) ::
          {:ok, non_neg_integer()} | {:error, :invalid_hex | :odd_length}
  def decode_integer(hex_string) when is_binary(hex_string) do
    cleaned = String.trim(hex_string)

    # Handle odd-length by prepending a zero (common for integers)
    normalized =
      if rem(byte_size(cleaned), 2) == 1 do
        "0" <> cleaned
      else
        cleaned
      end

    if valid_hex_chars?(normalized) do
      {:ok, String.to_integer(normalized, 16)}
    else
      {:error, :invalid_hex}
    end
  end

  @doc """
  Get the byte length that a hex string represents.

  ## Examples

      iex> Proven.SafeHex.byte_length("ff0080")
      {:ok, 3}
  """
  @spec byte_length(String.t()) :: {:ok, non_neg_integer()} | {:error, :invalid_hex | :odd_length}
  def byte_length(hex_string) when is_binary(hex_string) do
    case decode(hex_string) do
      {:ok, binary} -> {:ok, byte_size(binary)}
      error -> error
    end
  end

  @doc """
  XOR two hex strings of equal length.

  ## Examples

      iex> Proven.SafeHex.xor("ff00", "00ff")
      {:ok, "ffff"}
  """
  @spec xor(String.t(), String.t()) ::
          {:ok, String.t()} | {:error, :invalid_hex | :odd_length | :length_mismatch}
  def xor(hex_a, hex_b) when is_binary(hex_a) and is_binary(hex_b) do
    with {:ok, a} <- decode(hex_a),
         {:ok, b} <- decode(hex_b) do
      if byte_size(a) != byte_size(b) do
        {:error, :length_mismatch}
      else
        result = :crypto.exor(a, b)
        encode(result)
      end
    end
  end

  @doc """
  Slice a portion of a hex string by byte positions.

  ## Examples

      iex> Proven.SafeHex.slice("ff00ff00", 1, 2)
      {:ok, "00ff"}
  """
  @spec slice(String.t(), non_neg_integer(), non_neg_integer()) ::
          {:ok, String.t()} | {:error, :invalid_hex | :odd_length | :out_of_bounds}
  def slice(hex_string, start_byte, byte_count)
      when is_binary(hex_string) and is_integer(start_byte) and is_integer(byte_count) do
    case decode(hex_string) do
      {:ok, binary} ->
        if start_byte < 0 or byte_count < 0 or start_byte + byte_count > byte_size(binary) do
          {:error, :out_of_bounds}
        else
          sliced = :binary.part(binary, start_byte, byte_count)
          encode(sliced)
        end

      error ->
        error
    end
  end

  @doc """
  Concatenate multiple hex strings.

  ## Examples

      iex> Proven.SafeHex.concat(["ff", "00", "ff"])
      {:ok, "ff00ff"}
  """
  @spec concat([String.t()]) ::
          {:ok, String.t()} | {:error, :invalid_hex | :odd_length}
  def concat(hex_strings) when is_list(hex_strings) do
    hex_strings
    |> Enum.reduce_while({:ok, <<>>}, fn hex, {:ok, acc} ->
      case decode(hex) do
        {:ok, binary} -> {:cont, {:ok, acc <> binary}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, combined} -> encode(combined)
      error -> error
    end
  end

  # Private helpers

  defp valid_hex_chars?(string) do
    String.match?(string, ~r/^[0-9a-fA-F]*$/)
  end
end
