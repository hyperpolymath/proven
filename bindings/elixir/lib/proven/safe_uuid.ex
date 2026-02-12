# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeUuid do
  @moduledoc """
  Safe UUID parsing, validation, and formatting operations.

  Supports UUID versions 1-5 (RFC 4122) with strict validation
  and constant-time comparison for security contexts.
  """

  defmodule Uuid do
    @moduledoc """
    Represents a parsed UUID with version and variant information.
    """
    defstruct [:bytes, :version, :variant, :string]

    @type version :: 1 | 2 | 3 | 4 | 5 | :unknown
    @type variant :: :rfc4122 | :reserved_ncs | :reserved_microsoft | :reserved_future | :unknown

    @type t :: %__MODULE__{
            bytes: <<_::128>>,
            version: version(),
            variant: variant(),
            string: String.t()
          }
  end

  # UUID format regex: 8-4-4-4-12 hex digits with optional hyphens
  @uuid_regex ~r/^([0-9a-fA-F]{8})-?([0-9a-fA-F]{4})-?([0-9a-fA-F]{4})-?([0-9a-fA-F]{4})-?([0-9a-fA-F]{12})$/

  @doc """
  Parse a UUID string into a structured Uuid.

  Accepts UUIDs with or without hyphens, case-insensitive.

  ## Examples

      iex> Proven.SafeUuid.parse("550e8400-e29b-41d4-a716-446655440000")
      {:ok, %Proven.SafeUuid.Uuid{version: 4, variant: :rfc4122, ...}}

      iex> Proven.SafeUuid.parse("not-a-uuid")
      {:error, :invalid_uuid}
  """
  @spec parse(String.t()) :: {:ok, Uuid.t()} | {:error, :invalid_uuid}
  def parse(uuid_string) when is_binary(uuid_string) do
    case Regex.run(@uuid_regex, String.trim(uuid_string)) do
      [_full | groups] ->
        hex_string = Enum.join(groups)
        bytes = Base.decode16!(hex_string, case: :mixed)
        version = extract_version(bytes)
        variant = extract_variant(bytes)
        canonical = format_canonical(hex_string)

        {:ok,
         %Uuid{
           bytes: bytes,
           version: version,
           variant: variant,
           string: canonical
         }}

      nil ->
        {:error, :invalid_uuid}
    end
  end

  def parse(_), do: {:error, :invalid_uuid}

  @doc """
  Check if a string is a valid UUID format.

  ## Examples

      iex> Proven.SafeUuid.valid?("550e8400-e29b-41d4-a716-446655440000")
      true

      iex> Proven.SafeUuid.valid?("not-valid")
      false
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(uuid_string) do
    case parse(uuid_string) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Format a UUID as a canonical lowercase hyphenated string.

  ## Examples

      iex> Proven.SafeUuid.format("550E8400E29B41D4A716446655440000")
      {:ok, "550e8400-e29b-41d4-a716-446655440000"}
  """
  @spec format(String.t() | Uuid.t()) :: {:ok, String.t()} | {:error, :invalid_uuid}
  def format(%Uuid{string: canonical}), do: {:ok, canonical}

  def format(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> {:ok, uuid.string}
      error -> error
    end
  end

  @doc """
  Format a UUID without hyphens (compact form).

  ## Examples

      iex> Proven.SafeUuid.format_compact("550e8400-e29b-41d4-a716-446655440000")
      {:ok, "550e8400e29b41d4a716446655440000"}
  """
  @spec format_compact(String.t() | Uuid.t()) :: {:ok, String.t()} | {:error, :invalid_uuid}
  def format_compact(%Uuid{bytes: bytes}) do
    {:ok, Base.encode16(bytes, case: :lower)}
  end

  def format_compact(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> format_compact(uuid)
      error -> error
    end
  end

  @doc """
  Format a UUID in uppercase canonical form.

  ## Examples

      iex> Proven.SafeUuid.format_upper("550e8400-e29b-41d4-a716-446655440000")
      {:ok, "550E8400-E29B-41D4-A716-446655440000"}
  """
  @spec format_upper(String.t() | Uuid.t()) :: {:ok, String.t()} | {:error, :invalid_uuid}
  def format_upper(%Uuid{bytes: bytes}) do
    hex = Base.encode16(bytes, case: :upper)
    {:ok, format_canonical(hex)}
  end

  def format_upper(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> format_upper(uuid)
      error -> error
    end
  end

  @doc """
  Compare two UUIDs for equality in constant time.

  Prevents timing attacks when UUIDs are used as tokens or secrets.

  ## Examples

      iex> Proven.SafeUuid.constant_time_equal("550e8400-e29b-41d4-a716-446655440000", "550e8400-e29b-41d4-a716-446655440000")
      true
  """
  @spec constant_time_equal(String.t() | Uuid.t(), String.t() | Uuid.t()) :: boolean()
  def constant_time_equal(uuid_a, uuid_b) do
    with {:ok, a} <- normalize_to_bytes(uuid_a),
         {:ok, b} <- normalize_to_bytes(uuid_b) do
      Proven.SafeCrypto.constant_time_compare(a, b)
    else
      _ -> false
    end
  end

  @doc """
  Get the version of a UUID.

  ## Examples

      iex> Proven.SafeUuid.get_version("550e8400-e29b-41d4-a716-446655440000")
      {:ok, 4}
  """
  @spec get_version(String.t() | Uuid.t()) :: {:ok, Uuid.version()} | {:error, :invalid_uuid}
  def get_version(%Uuid{version: version}), do: {:ok, version}

  def get_version(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> {:ok, uuid.version}
      error -> error
    end
  end

  @doc """
  Get the variant of a UUID.

  ## Examples

      iex> Proven.SafeUuid.get_variant("550e8400-e29b-41d4-a716-446655440000")
      {:ok, :rfc4122}
  """
  @spec get_variant(String.t() | Uuid.t()) :: {:ok, Uuid.variant()} | {:error, :invalid_uuid}
  def get_variant(%Uuid{variant: variant}), do: {:ok, variant}

  def get_variant(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> {:ok, uuid.variant}
      error -> error
    end
  end

  @doc """
  Generate a random UUID v4.

  Uses cryptographically secure random bytes.

  ## Examples

      iex> {:ok, uuid} = Proven.SafeUuid.generate_v4()
      iex> Proven.SafeUuid.valid?(uuid.string)
      true
  """
  @spec generate_v4() :: {:ok, Uuid.t()}
  def generate_v4 do
    # Generate 16 random bytes
    bytes = :crypto.strong_rand_bytes(16)

    # Set version (4) and variant (RFC 4122)
    <<a::48, _version::4, b::12, _variant::2, c::62>> = bytes
    uuid_bytes = <<a::48, 4::4, b::12, 2::2, c::62>>

    hex = Base.encode16(uuid_bytes, case: :lower)
    canonical = format_canonical(hex)

    {:ok,
     %Uuid{
       bytes: uuid_bytes,
       version: 4,
       variant: :rfc4122,
       string: canonical
     }}
  end

  @doc """
  Return the nil UUID (all zeros).

  ## Examples

      iex> Proven.SafeUuid.nil_uuid()
      {:ok, %Proven.SafeUuid.Uuid{string: "00000000-0000-0000-0000-000000000000", ...}}
  """
  @spec nil_uuid() :: {:ok, Uuid.t()}
  def nil_uuid do
    bytes = <<0::128>>

    {:ok,
     %Uuid{
       bytes: bytes,
       version: :unknown,
       variant: :reserved_ncs,
       string: "00000000-0000-0000-0000-000000000000"
     }}
  end

  @doc """
  Check if a UUID is the nil UUID.

  ## Examples

      iex> Proven.SafeUuid.nil?("00000000-0000-0000-0000-000000000000")
      true
  """
  @spec nil?(String.t() | Uuid.t()) :: boolean()
  def nil?(%Uuid{bytes: bytes}), do: bytes == <<0::128>>

  def nil?(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> nil?(uuid)
      {:error, _} -> false
    end
  end

  # Private helpers

  defp extract_version(<<_::48, version::4, _::76>>), do: normalize_version(version)

  defp normalize_version(1), do: 1
  defp normalize_version(2), do: 2
  defp normalize_version(3), do: 3
  defp normalize_version(4), do: 4
  defp normalize_version(5), do: 5
  defp normalize_version(_), do: :unknown

  defp extract_variant(<<_::64, variant_bits::2, _::62>>) do
    case variant_bits do
      0 -> :reserved_ncs
      1 -> :reserved_ncs
      2 -> :rfc4122
      3 -> :reserved_microsoft
    end
  end

  defp format_canonical(hex) when byte_size(hex) == 32 do
    hex = String.downcase(hex)

    String.slice(hex, 0, 8) <>
      "-" <>
      String.slice(hex, 8, 4) <>
      "-" <>
      String.slice(hex, 12, 4) <>
      "-" <>
      String.slice(hex, 16, 4) <>
      "-" <> String.slice(hex, 20, 12)
  end

  defp normalize_to_bytes(%Uuid{bytes: bytes}), do: {:ok, bytes}

  defp normalize_to_bytes(uuid_string) when is_binary(uuid_string) do
    case parse(uuid_string) do
      {:ok, uuid} -> {:ok, uuid.bytes}
      error -> error
    end
  end
end
