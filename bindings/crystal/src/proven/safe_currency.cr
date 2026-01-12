# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # ISO 4217 currency codes.
  enum CurrencyCode
    USD  # US Dollar
    EUR  # Euro
    GBP  # British Pound
    JPY  # Japanese Yen
    CHF  # Swiss Franc
    CAD  # Canadian Dollar
    AUD  # Australian Dollar
    NZD  # New Zealand Dollar
    CNY  # Chinese Yuan
    INR  # Indian Rupee
    BRL  # Brazilian Real
    MXN  # Mexican Peso
    KRW  # South Korean Won
    SGD  # Singapore Dollar
    HKD  # Hong Kong Dollar
    SEK  # Swedish Krona
    NOK  # Norwegian Krone
    DKK  # Danish Krone
    PLN  # Polish Zloty
    RUB  # Russian Ruble
    ZAR  # South African Rand
    TRY  # Turkish Lira
    THB  # Thai Baht
    MYR  # Malaysian Ringgit
    IDR  # Indonesian Rupiah
    PHP  # Philippine Peso
    VND  # Vietnamese Dong
    AED  # UAE Dirham
    SAR  # Saudi Riyal
    ILS  # Israeli Shekel
    CZK  # Czech Koruna
    HUF  # Hungarian Forint
    RON  # Romanian Leu
    BGN  # Bulgarian Lev
    HRK  # Croatian Kuna
    ISK  # Icelandic Krona
    CLP  # Chilean Peso
    COP  # Colombian Peso
    PEN  # Peruvian Sol
    ARS  # Argentine Peso
    BTC  # Bitcoin
    ETH  # Ethereum

    # Get number of decimal places for this currency.
    def decimals : UInt8
      case self
      when .jpy?, .krw?, .vnd?
        0_u8
      when .btc?, .eth?
        8_u8
      else
        2_u8
      end
    end

    # Get currency symbol.
    def symbol : String
      case self
      when .usd?, .cad?, .aud?, .nzd?, .hkd?, .sgd?, .mxn?
        "$"
      when .eur?
        "\u20AC" # Euro sign
      when .gbp?
        "\u00A3" # Pound sign
      when .jpy?, .cny?
        "\u00A5" # Yen sign
      when .chf?
        "Fr"
      when .inr?
        "\u20B9" # Rupee sign
      when .krw?
        "\u20A9" # Won sign
      when .rub?
        "\u20BD" # Ruble sign
      when .btc?
        "\u20BF" # Bitcoin sign
      when .eth?
        "\u039E" # Xi (commonly used for ETH)
      when .brl?
        "R$"
      when .zar?
        "R"
      when .try?
        "\u20BA" # Lira sign
      when .pln?
        "z\u0142"
      when .ils?
        "\u20AA" # Shekel sign
      when .thb?
        "\u0E3F" # Baht sign
      else
        ""
      end
    end

    # Get currency name.
    def name : String
      case self
      when .usd? then "US Dollar"
      when .eur? then "Euro"
      when .gbp? then "British Pound"
      when .jpy? then "Japanese Yen"
      when .chf? then "Swiss Franc"
      when .cad? then "Canadian Dollar"
      when .aud? then "Australian Dollar"
      when .nzd? then "New Zealand Dollar"
      when .cny? then "Chinese Yuan"
      when .inr? then "Indian Rupee"
      when .brl? then "Brazilian Real"
      when .mxn? then "Mexican Peso"
      when .krw? then "South Korean Won"
      when .sgd? then "Singapore Dollar"
      when .hkd? then "Hong Kong Dollar"
      when .btc? then "Bitcoin"
      when .eth? then "Ethereum"
      else            self.to_s
      end
    end

    # Get ISO 4217 code as string.
    def code : String
      self.to_s
    end
  end

  # Type-safe monetary value stored in minor units.
  struct Money
    getter minor_units : Int64
    getter currency : CurrencyCode

    def initialize(@minor_units : Int64, @currency : CurrencyCode)
    end

    # Create from major units (dollars, euros, etc.).
    def self.from_major(amount : Int64, currency : CurrencyCode) : Money
      multiplier = 10_i64 ** currency.decimals
      Money.new(amount * multiplier, currency)
    end

    # Create from minor units (cents, satoshis, etc.).
    def self.from_minor(amount : Int64, currency : CurrencyCode) : Money
      Money.new(amount, currency)
    end

    # Create zero amount for a currency.
    def self.zero(currency : CurrencyCode) : Money
      Money.new(0_i64, currency)
    end

    # Get major units (truncated).
    def major : Int64
      divisor = 10_i64 ** @currency.decimals
      @minor_units // divisor
    end

    # Get the fractional part in minor units.
    def fractional : Int64
      divisor = 10_i64 ** @currency.decimals
      @minor_units.abs % divisor
    end

    # Add two monetary values (must be same currency).
    def add(other : Money) : Money?
      return nil unless @currency == other.currency
      Money.new(@minor_units + other.minor_units, @currency)
    end

    # Subtract two monetary values (must be same currency).
    def sub(other : Money) : Money?
      return nil unless @currency == other.currency
      Money.new(@minor_units - other.minor_units, @currency)
    end

    # Multiply by a scalar value.
    def mul(scalar : Int64) : Money
      Money.new(@minor_units * scalar, @currency)
    end

    # Divide by a scalar value.
    def div(scalar : Int64) : Money?
      return nil if scalar == 0
      Money.new(@minor_units // scalar, @currency)
    end

    # Negate the amount.
    def negate : Money
      Money.new(-@minor_units, @currency)
    end

    # Get absolute value.
    def abs : Money
      Money.new(@minor_units.abs, @currency)
    end

    # Check if zero.
    def zero? : Bool
      @minor_units == 0
    end

    # Check if positive.
    def positive? : Bool
      @minor_units > 0
    end

    # Check if negative.
    def negative? : Bool
      @minor_units < 0
    end

    # Compare two monetary values (must be same currency).
    def <=>(other : Money) : Int32?
      return nil unless @currency == other.currency
      @minor_units <=> other.minor_units
    end

    # Check equality.
    def ==(other : Money) : Bool
      @currency == other.currency && @minor_units == other.minor_units
    end

    # Format as string with symbol.
    def to_s : String
      dec = @currency.decimals
      divisor = 10_i64 ** dec
      abs_units = @minor_units.abs
      major_part = abs_units // divisor
      minor_part = abs_units % divisor
      sign = @minor_units < 0 ? "-" : ""

      if dec == 0
        "#{sign}#{@currency.symbol}#{major_part}"
      else
        "#{sign}#{@currency.symbol}#{major_part}.#{minor_part.to_s.rjust(dec.to_i32, '0')}"
      end
    end

    # Format as string with currency code.
    def to_s_code : String
      dec = @currency.decimals
      divisor = 10_i64 ** dec
      abs_units = @minor_units.abs
      major_part = abs_units // divisor
      minor_part = abs_units % divisor
      sign = @minor_units < 0 ? "-" : ""

      if dec == 0
        "#{sign}#{major_part} #{@currency.code}"
      else
        "#{sign}#{major_part}.#{minor_part.to_s.rjust(dec.to_i32, '0')} #{@currency.code}"
      end
    end
  end

  # Safe currency operations.
  module SafeCurrency
    # Mapping of string codes to CurrencyCode enum.
    CURRENCY_MAP = {
      "USD" => CurrencyCode::USD,
      "EUR" => CurrencyCode::EUR,
      "GBP" => CurrencyCode::GBP,
      "JPY" => CurrencyCode::JPY,
      "CHF" => CurrencyCode::CHF,
      "CAD" => CurrencyCode::CAD,
      "AUD" => CurrencyCode::AUD,
      "NZD" => CurrencyCode::NZD,
      "CNY" => CurrencyCode::CNY,
      "INR" => CurrencyCode::INR,
      "BRL" => CurrencyCode::BRL,
      "MXN" => CurrencyCode::MXN,
      "KRW" => CurrencyCode::KRW,
      "SGD" => CurrencyCode::SGD,
      "HKD" => CurrencyCode::HKD,
      "SEK" => CurrencyCode::SEK,
      "NOK" => CurrencyCode::NOK,
      "DKK" => CurrencyCode::DKK,
      "PLN" => CurrencyCode::PLN,
      "RUB" => CurrencyCode::RUB,
      "ZAR" => CurrencyCode::ZAR,
      "TRY" => CurrencyCode::TRY,
      "THB" => CurrencyCode::THB,
      "MYR" => CurrencyCode::MYR,
      "IDR" => CurrencyCode::IDR,
      "PHP" => CurrencyCode::PHP,
      "VND" => CurrencyCode::VND,
      "AED" => CurrencyCode::AED,
      "SAR" => CurrencyCode::SAR,
      "ILS" => CurrencyCode::ILS,
      "CZK" => CurrencyCode::CZK,
      "HUF" => CurrencyCode::HUF,
      "RON" => CurrencyCode::RON,
      "BGN" => CurrencyCode::BGN,
      "HRK" => CurrencyCode::HRK,
      "ISK" => CurrencyCode::ISK,
      "CLP" => CurrencyCode::CLP,
      "COP" => CurrencyCode::COP,
      "PEN" => CurrencyCode::PEN,
      "ARS" => CurrencyCode::ARS,
      "BTC" => CurrencyCode::BTC,
      "ETH" => CurrencyCode::ETH,
    }

    # Parse currency code from string.
    def self.parse_code(input : String) : CurrencyCode?
      CURRENCY_MAP[input.upcase]?
    end

    # Check if valid currency code.
    def self.valid_code?(input : String) : Bool
      CURRENCY_MAP.has_key?(input.upcase)
    end

    # Parse money from string (e.g., "123.45 USD" or "$123.45").
    def self.parse(input : String) : Money?
      trimmed = input.strip
      return nil if trimmed.empty?

      # Try format: "123.45 USD"
      if match = trimmed.match(/^(-?\d+(?:\.\d+)?)\s+([A-Z]{3})$/i)
        amount_str = match[1]
        code_str = match[2]
        currency = parse_code(code_str)
        return nil if currency.nil?
        return parse_amount(amount_str, currency)
      end

      # Try format: "$123.45" (common symbols)
      if match = trimmed.match(/^([^\d-]*)?(-?\d+(?:\.\d+)?)$/)
        symbol_str = match[1]? || ""
        amount_str = match[2]
        currency = symbol_to_currency(symbol_str.strip)
        return nil if currency.nil?
        return parse_amount(amount_str, currency)
      end

      nil
    end

    # Parse a numeric amount string into Money.
    private def self.parse_amount(amount_str : String, currency : CurrencyCode) : Money?
      parts = amount_str.split('.')
      return nil if parts.size > 2

      major_str = parts[0]
      major = major_str.to_i64?
      return nil if major.nil?

      if parts.size == 2
        minor_str = parts[1]
        dec = currency.decimals.to_i32
        # Pad or truncate to match currency decimals
        if minor_str.size < dec
          minor_str = minor_str.ljust(dec, '0')
        elsif minor_str.size > dec
          minor_str = minor_str[0, dec]
        end
        minor = minor_str.to_i64?
        return nil if minor.nil?

        divisor = 10_i64 ** dec
        minor_units = major * divisor + (major < 0 ? -minor : minor)
        Money.new(minor_units, currency)
      else
        Money.from_major(major, currency)
      end
    end

    # Map common symbols to currency codes.
    private def self.symbol_to_currency(symbol : String) : CurrencyCode?
      case symbol
      when "$"                    then CurrencyCode::USD
      when "\u20AC"               then CurrencyCode::EUR # Euro
      when "\u00A3"               then CurrencyCode::GBP # Pound
      when "\u00A5"               then CurrencyCode::JPY # Yen
      when "\u20B9"               then CurrencyCode::INR # Rupee
      when "\u20A9"               then CurrencyCode::KRW # Won
      when "\u20BD"               then CurrencyCode::RUB # Ruble
      when "\u20BF"               then CurrencyCode::BTC # Bitcoin
      when "Fr"                   then CurrencyCode::CHF
      else                             nil
      end
    end

    # Get all supported currency codes.
    def self.all_codes : Array(CurrencyCode)
      CurrencyCode.values
    end

    # Compare two money values (returns nil if different currencies).
    def self.compare(a : Money, b : Money) : Int32?
      a <=> b
    end

    # Sum an array of money values (must all be same currency).
    def self.sum(values : Array(Money)) : Money?
      return nil if values.empty?

      currency = values[0].currency
      total = 0_i64

      values.each do |value|
        return nil unless value.currency == currency
        total += value.minor_units
      end

      Money.new(total, currency)
    end

    # Allocate money proportionally (for splitting bills, etc.).
    def self.allocate(amount : Money, ratios : Array(Int32)) : Array(Money)?
      return nil if ratios.empty?
      return nil if ratios.any? { |r| r < 0 }

      total_ratio = ratios.sum
      return nil if total_ratio == 0

      remaining = amount.minor_units
      results = [] of Money

      ratios.each_with_index do |ratio, index|
        if index == ratios.size - 1
          # Last allocation gets the remainder
          results << Money.new(remaining, amount.currency)
        else
          share = (amount.minor_units * ratio) // total_ratio
          results << Money.new(share, amount.currency)
          remaining -= share
        end
      end

      results
    end
  end
end
