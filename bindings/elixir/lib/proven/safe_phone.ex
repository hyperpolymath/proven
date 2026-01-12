# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafePhone do
  @moduledoc """
  Safe phone number parsing, validation, and formatting operations.

  Follows E.164 international standard for phone number representation.
  Provides parsing from various formats and consistent formatting output.
  """

  defmodule PhoneNumber do
    @moduledoc """
    Represents a parsed phone number with country and national components.
    """
    defstruct [:country_code, :national_number, :extension, :raw, :e164]

    @type t :: %__MODULE__{
            country_code: String.t(),
            national_number: String.t(),
            extension: String.t() | nil,
            raw: String.t(),
            e164: String.t()
          }
  end

  # Common country calling codes: {code, name, national_length_range}
  @country_codes %{
    # North America
    "1" => {"US/CA", 10..10},
    # Russia/Kazakhstan
    "7" => {"RU/KZ", 10..10},
    # Egypt
    "20" => {"EG", 9..10},
    # South Africa
    "27" => {"ZA", 9..9},
    # Greece
    "30" => {"GR", 10..10},
    # Netherlands
    "31" => {"NL", 9..9},
    # Belgium
    "32" => {"BE", 8..9},
    # France
    "33" => {"FR", 9..9},
    # Spain
    "34" => {"ES", 9..9},
    # Hungary
    "36" => {"HU", 8..9},
    # Italy
    "39" => {"IT", 9..11},
    # Romania
    "40" => {"RO", 9..9},
    # Switzerland
    "41" => {"CH", 9..9},
    # Austria
    "43" => {"AT", 4..13},
    # UK
    "44" => {"GB", 10..10},
    # Denmark
    "45" => {"DK", 8..8},
    # Sweden
    "46" => {"SE", 7..13},
    # Norway
    "47" => {"NO", 8..8},
    # Poland
    "48" => {"PL", 9..9},
    # Germany
    "49" => {"DE", 3..14},
    # Peru
    "51" => {"PE", 9..9},
    # Mexico
    "52" => {"MX", 10..10},
    # Argentina
    "54" => {"AR", 10..10},
    # Brazil
    "55" => {"BR", 10..11},
    # Chile
    "56" => {"CL", 9..9},
    # Australia
    "61" => {"AU", 9..9},
    # Indonesia
    "62" => {"ID", 9..12},
    # Philippines
    "63" => {"PH", 10..10},
    # New Zealand
    "64" => {"NZ", 8..10},
    # Singapore
    "65" => {"SG", 8..8},
    # Thailand
    "66" => {"TH", 9..9},
    # Japan
    "81" => {"JP", 9..10},
    # South Korea
    "82" => {"KR", 8..11},
    # Vietnam
    "84" => {"VN", 9..10},
    # China
    "86" => {"CN", 11..11},
    # Turkey
    "90" => {"TR", 10..10},
    # India
    "91" => {"IN", 10..10},
    # Pakistan
    "92" => {"PK", 10..10},
    # UAE
    "971" => {"AE", 9..9},
    # Israel
    "972" => {"IL", 9..9},
    # Hong Kong
    "852" => {"HK", 8..8},
    # Taiwan
    "886" => {"TW", 9..9}
  }

  @doc """
  Parse a phone number string into a PhoneNumber struct.

  Accepts various formats:
  - E.164: +14155551234
  - With country code: 1-415-555-1234
  - National: (415) 555-1234 (requires default_country_code)
  - With extension: +14155551234 ext 123

  ## Examples

      iex> Proven.SafePhone.parse("+14155551234")
      {:ok, %Proven.SafePhone.PhoneNumber{country_code: "1", national_number: "4155551234", e164: "+14155551234"}}

      iex> Proven.SafePhone.parse("(415) 555-1234", default_country_code: "1")
      {:ok, %Proven.SafePhone.PhoneNumber{country_code: "1", national_number: "4155551234", e164: "+14155551234"}}
  """
  @spec parse(String.t(), keyword()) ::
          {:ok, PhoneNumber.t()} | {:error, :invalid_phone | :missing_country_code}
  def parse(phone_string, opts \\ []) when is_binary(phone_string) do
    default_country_code = Keyword.get(opts, :default_country_code)
    raw = String.trim(phone_string)

    # Extract extension if present
    {main_part, extension} = extract_extension(raw)

    # Extract digits and leading plus
    has_plus = String.starts_with?(main_part, "+")
    digits = extract_digits(main_part)

    cond do
      String.length(digits) < 4 ->
        {:error, :invalid_phone}

      has_plus ->
        # E.164 format or international format with +
        parse_international(digits, extension, raw)

      default_country_code != nil ->
        # National format with provided country code
        parse_with_country_code(digits, default_country_code, extension, raw)

      true ->
        # Try to detect country code from digits
        case detect_country_code(digits) do
          {:ok, country_code, national_number} ->
            build_phone_number(country_code, national_number, extension, raw)

          :error ->
            {:error, :missing_country_code}
        end
    end
  end

  @doc """
  Check if a phone number string appears valid.

  ## Examples

      iex> Proven.SafePhone.valid?("+14155551234")
      true

      iex> Proven.SafePhone.valid?("123")
      false
  """
  @spec valid?(String.t(), keyword()) :: boolean()
  def valid?(phone_string, opts \\ []) do
    case parse(phone_string, opts) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Format a phone number in E.164 format.

  ## Examples

      iex> {:ok, phone} = Proven.SafePhone.parse("+14155551234")
      iex> Proven.SafePhone.format_e164(phone)
      {:ok, "+14155551234"}
  """
  @spec format_e164(PhoneNumber.t() | String.t()) ::
          {:ok, String.t()} | {:error, :invalid_phone | :missing_country_code}
  def format_e164(%PhoneNumber{e164: e164}), do: {:ok, e164}

  def format_e164(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> {:ok, phone.e164}
      error -> error
    end
  end

  @doc """
  Format a phone number in national format.

  ## Examples

      iex> {:ok, phone} = Proven.SafePhone.parse("+14155551234")
      iex> Proven.SafePhone.format_national(phone)
      {:ok, "(415) 555-1234"}
  """
  @spec format_national(PhoneNumber.t() | String.t(), keyword()) ::
          {:ok, String.t()} | {:error, :invalid_phone | :missing_country_code}
  def format_national(%PhoneNumber{country_code: "1", national_number: national}, opts \\ []) do
    # NANP formatting (US, Canada)
    separator = Keyword.get(opts, :separator, "-")

    if String.length(national) == 10 do
      area = String.slice(national, 0, 3)
      exchange = String.slice(national, 3, 3)
      subscriber = String.slice(national, 6, 4)
      {:ok, "(#{area}) #{exchange}#{separator}#{subscriber}"}
    else
      {:ok, national}
    end
  end

  def format_national(%PhoneNumber{national_number: national}, _opts) do
    # Generic formatting - just return national number with spaces
    {:ok, national}
  end

  def format_national(phone_string, opts) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> format_national(phone, opts)
      error -> error
    end
  end

  @doc """
  Format a phone number in international format.

  ## Examples

      iex> {:ok, phone} = Proven.SafePhone.parse("+14155551234")
      iex> Proven.SafePhone.format_international(phone)
      {:ok, "+1 415 555 1234"}
  """
  @spec format_international(PhoneNumber.t() | String.t()) ::
          {:ok, String.t()} | {:error, :invalid_phone | :missing_country_code}
  def format_international(%PhoneNumber{country_code: "1", national_number: national}) do
    # NANP formatting
    if String.length(national) == 10 do
      area = String.slice(national, 0, 3)
      exchange = String.slice(national, 3, 3)
      subscriber = String.slice(national, 6, 4)
      {:ok, "+1 #{area} #{exchange} #{subscriber}"}
    else
      {:ok, "+1 #{national}"}
    end
  end

  def format_international(%PhoneNumber{country_code: code, national_number: national}) do
    {:ok, "+#{code} #{national}"}
  end

  def format_international(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> format_international(phone)
      error -> error
    end
  end

  @doc """
  Format a phone number in RFC 3966 tel: URI format.

  ## Examples

      iex> {:ok, phone} = Proven.SafePhone.parse("+14155551234 ext 123")
      iex> Proven.SafePhone.format_rfc3966(phone)
      {:ok, "tel:+14155551234;ext=123"}
  """
  @spec format_rfc3966(PhoneNumber.t() | String.t()) ::
          {:ok, String.t()} | {:error, :invalid_phone | :missing_country_code}
  def format_rfc3966(%PhoneNumber{e164: e164, extension: nil}), do: {:ok, "tel:#{e164}"}

  def format_rfc3966(%PhoneNumber{e164: e164, extension: ext}) do
    {:ok, "tel:#{e164};ext=#{ext}"}
  end

  def format_rfc3966(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> format_rfc3966(phone)
      error -> error
    end
  end

  @doc """
  Get the country code from a phone number.

  ## Examples

      iex> {:ok, phone} = Proven.SafePhone.parse("+14155551234")
      iex> Proven.SafePhone.get_country_code(phone)
      {:ok, "1"}
  """
  @spec get_country_code(PhoneNumber.t() | String.t()) ::
          {:ok, String.t()} | {:error, :invalid_phone | :missing_country_code}
  def get_country_code(%PhoneNumber{country_code: code}), do: {:ok, code}

  def get_country_code(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> {:ok, phone.country_code}
      error -> error
    end
  end

  @doc """
  Get the national number (without country code).

  ## Examples

      iex> {:ok, phone} = Proven.SafePhone.parse("+14155551234")
      iex> Proven.SafePhone.get_national_number(phone)
      {:ok, "4155551234"}
  """
  @spec get_national_number(PhoneNumber.t() | String.t()) ::
          {:ok, String.t()} | {:error, :invalid_phone | :missing_country_code}
  def get_national_number(%PhoneNumber{national_number: national}), do: {:ok, national}

  def get_national_number(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> {:ok, phone.national_number}
      error -> error
    end
  end

  @doc """
  Get country information for a country code.

  ## Examples

      iex> Proven.SafePhone.get_country_info("1")
      {:ok, {"US/CA", 10..10}}
  """
  @spec get_country_info(String.t()) ::
          {:ok, {String.t(), Range.t()}} | {:error, :unknown_country_code}
  def get_country_info(country_code) when is_binary(country_code) do
    case Map.get(@country_codes, country_code) do
      nil -> {:error, :unknown_country_code}
      info -> {:ok, info}
    end
  end

  @doc """
  List all known country codes.
  """
  @spec list_country_codes() :: [String.t()]
  def list_country_codes do
    Map.keys(@country_codes) |> Enum.sort()
  end

  @doc """
  Compare two phone numbers for equality.

  Normalizes both to E.164 before comparing.

  ## Examples

      iex> Proven.SafePhone.equal?("+14155551234", "1-415-555-1234")
      true
  """
  @spec equal?(String.t() | PhoneNumber.t(), String.t() | PhoneNumber.t()) :: boolean()
  def equal?(phone_a, phone_b) do
    with {:ok, a} <- normalize_to_e164(phone_a),
         {:ok, b} <- normalize_to_e164(phone_b) do
      a == b
    else
      _ -> false
    end
  end

  @doc """
  Check if a phone number is a mobile number (best effort).

  Note: This is heuristic-based and may not be accurate for all countries.
  """
  @spec mobile?(PhoneNumber.t() | String.t()) :: boolean()
  def mobile?(%PhoneNumber{country_code: "1", national_number: national}) do
    # NANP doesn't distinguish mobile/landline by number
    # All 10-digit numbers could be either
    String.length(national) == 10
  end

  def mobile?(%PhoneNumber{country_code: "44", national_number: national}) do
    # UK mobile numbers start with 7
    String.starts_with?(national, "7")
  end

  def mobile?(%PhoneNumber{country_code: "49", national_number: national}) do
    # German mobile prefixes
    String.starts_with?(national, "15") or
      String.starts_with?(national, "16") or
      String.starts_with?(national, "17")
  end

  def mobile?(%PhoneNumber{}) do
    # Unknown - return false to be safe
    false
  end

  def mobile?(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> mobile?(phone)
      {:error, _} -> false
    end
  end

  # Private helpers

  defp extract_extension(phone_string) do
    # Match common extension patterns: ext, x, ext., extension
    pattern = ~r/(?:\s*(?:ext\.?|x|extension)\s*)(\d+)$/i

    case Regex.run(pattern, phone_string) do
      [full_match, ext] ->
        main = String.replace(phone_string, full_match, "") |> String.trim()
        {main, ext}

      nil ->
        {phone_string, nil}
    end
  end

  defp extract_digits(string) do
    String.replace(string, ~r/[^\d]/, "")
  end

  defp parse_international(digits, extension, raw) do
    case detect_country_code(digits) do
      {:ok, country_code, national_number} ->
        build_phone_number(country_code, national_number, extension, raw)

      :error ->
        {:error, :invalid_phone}
    end
  end

  defp parse_with_country_code(digits, country_code, extension, raw) do
    # Remove country code if present at start
    national =
      if String.starts_with?(digits, country_code) do
        String.slice(digits, String.length(country_code)..-1//1)
      else
        digits
      end

    build_phone_number(country_code, national, extension, raw)
  end

  defp detect_country_code(digits) do
    # Try matching 3-digit, 2-digit, then 1-digit country codes
    [3, 2, 1]
    |> Enum.find_value(fn len ->
      if String.length(digits) > len do
        potential_code = String.slice(digits, 0, len)
        national = String.slice(digits, len..-1//1)

        case Map.get(@country_codes, potential_code) do
          {_country, range} when national != "" ->
            if String.length(national) in range do
              {:ok, potential_code, national}
            else
              nil
            end

          _ ->
            nil
        end
      end
    end) || :error
  end

  defp build_phone_number(country_code, national_number, extension, raw) do
    e164 = "+#{country_code}#{national_number}"

    {:ok,
     %PhoneNumber{
       country_code: country_code,
       national_number: national_number,
       extension: extension,
       raw: raw,
       e164: e164
     }}
  end

  defp normalize_to_e164(%PhoneNumber{e164: e164}), do: {:ok, e164}

  defp normalize_to_e164(phone_string) when is_binary(phone_string) do
    case parse(phone_string) do
      {:ok, phone} -> {:ok, phone.e164}
      error -> error
    end
  end
end
