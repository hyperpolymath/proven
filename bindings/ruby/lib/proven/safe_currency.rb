# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe currency operations with type-safe monetary values.
  module SafeCurrency
    # ISO 4217 currency codes.
    module CurrencyCode
      USD = :USD
      EUR = :EUR
      GBP = :GBP
      JPY = :JPY
      CHF = :CHF
      CAD = :CAD
      AUD = :AUD
      NZD = :NZD
      CNY = :CNY
      INR = :INR
      BRL = :BRL
      MXN = :MXN
      KRW = :KRW
      SGD = :SGD
      HKD = :HKD
      SEK = :SEK
      NOK = :NOK
      DKK = :DKK
      PLN = :PLN
      RUB = :RUB
      ZAR = :ZAR
      TRY = :TRY
      THB = :THB
      MYR = :MYR
      IDR = :IDR
      PHP = :PHP
      VND = :VND
      AED = :AED
      SAR = :SAR
      ILS = :ILS
      CZK = :CZK
      HUF = :HUF
      RON = :RON
      BGN = :BGN
      HRK = :HRK
      ISK = :ISK
      CLP = :CLP
      COP = :COP
      PEN = :PEN
      ARS = :ARS
      BTC = :BTC
      ETH = :ETH

      # All known currency codes.
      ALL = [
        USD, EUR, GBP, JPY, CHF, CAD, AUD, NZD, CNY, INR,
        BRL, MXN, KRW, SGD, HKD, SEK, NOK, DKK, PLN, RUB,
        ZAR, TRY, THB, MYR, IDR, PHP, VND, AED, SAR, ILS,
        CZK, HUF, RON, BGN, HRK, ISK, CLP, COP, PEN, ARS,
        BTC, ETH
      ].freeze

      # Decimal places for each currency.
      DECIMALS = {
        JPY => 0, KRW => 0, VND => 0,
        BTC => 8, ETH => 8
      }.freeze

      # Currency symbols.
      SYMBOLS = {
        USD => "$", EUR => "€", GBP => "£",
        JPY => "¥", CNY => "¥", CHF => "Fr",
        INR => "₹", KRW => "₩", RUB => "₽",
        BTC => "₿", ETH => "Ξ"
      }.freeze

      # Currency names.
      NAMES = {
        USD => "US Dollar",
        EUR => "Euro",
        GBP => "British Pound",
        JPY => "Japanese Yen",
        CHF => "Swiss Franc",
        CAD => "Canadian Dollar",
        AUD => "Australian Dollar",
        NZD => "New Zealand Dollar",
        CNY => "Chinese Yuan",
        INR => "Indian Rupee",
        BRL => "Brazilian Real",
        MXN => "Mexican Peso",
        KRW => "South Korean Won",
        BTC => "Bitcoin",
        ETH => "Ethereum"
      }.freeze

      class << self
        # Get number of decimal places for currency.
        #
        # @param code [Symbol]
        # @return [Integer]
        def decimals(code)
          DECIMALS.fetch(code, 2)
        end

        # Get currency symbol.
        #
        # @param code [Symbol]
        # @return [String]
        def symbol(code)
          SYMBOLS.fetch(code, "")
        end

        # Get currency name.
        #
        # @param code [Symbol]
        # @return [String]
        def name(code)
          NAMES.fetch(code, "Currency")
        end

        # Check if code is valid.
        #
        # @param code [Symbol]
        # @return [Boolean]
        def valid?(code)
          ALL.include?(code)
        end
      end
    end

    # Type-safe monetary value.
    class Money
      attr_reader :minor_units, :currency

      # Create from major units (dollars, euros, etc.).
      #
      # @param amount [Integer]
      # @param currency [Symbol]
      # @return [Money]
      def self.from_major(amount, currency)
        decimals = CurrencyCode.decimals(currency)
        multiplier = 10**decimals
        new(amount * multiplier, currency)
      end

      # Create from minor units (cents, satoshis, etc.).
      #
      # @param amount [Integer]
      # @param currency [Symbol]
      # @return [Money]
      def self.from_minor(amount, currency)
        new(amount, currency)
      end

      # Create zero amount.
      #
      # @param currency [Symbol]
      # @return [Money]
      def self.zero(currency)
        new(0, currency)
      end

      # Initialize money with minor units.
      #
      # @param minor_units [Integer]
      # @param currency [Symbol]
      def initialize(minor_units, currency)
        @minor_units = minor_units
        @currency = currency
      end

      # Get major units (truncated).
      #
      # @return [Integer]
      def major
        decimals = CurrencyCode.decimals(@currency)
        divisor = 10**decimals
        @minor_units / divisor
      end

      # Get minor units.
      #
      # @return [Integer]
      def minor
        @minor_units
      end

      # Add two monetary values.
      #
      # @param other [Money]
      # @return [Money, nil] nil if currency mismatch
      def add(other)
        return nil unless @currency == other.currency

        Money.new(@minor_units + other.minor_units, @currency)
      end

      # Subtract two monetary values.
      #
      # @param other [Money]
      # @return [Money, nil] nil if currency mismatch
      def sub(other)
        return nil unless @currency == other.currency

        Money.new(@minor_units - other.minor_units, @currency)
      end

      # Multiply by scalar.
      #
      # @param scalar [Integer]
      # @return [Money]
      def mul(scalar)
        Money.new(@minor_units * scalar, @currency)
      end

      # Divide by scalar.
      #
      # @param scalar [Integer]
      # @return [Money, nil] nil if division by zero
      def div(scalar)
        return nil if scalar.zero?

        Money.new(@minor_units / scalar, @currency)
      end

      # Check if zero.
      #
      # @return [Boolean]
      def zero?
        @minor_units.zero?
      end

      # Check if positive.
      #
      # @return [Boolean]
      def positive?
        @minor_units.positive?
      end

      # Check if negative.
      #
      # @return [Boolean]
      def negative?
        @minor_units.negative?
      end

      # Absolute value.
      #
      # @return [Money]
      def abs
        Money.new(@minor_units.abs, @currency)
      end

      # Format as string.
      #
      # @return [String]
      def to_s
        decimals = CurrencyCode.decimals(@currency)
        divisor = 10**decimals
        absolute_units = @minor_units.abs
        major_part = absolute_units / divisor
        minor_part = absolute_units % divisor
        sign = @minor_units.negative? ? "-" : ""
        symbol = CurrencyCode.symbol(@currency)

        if decimals.zero?
          "#{sign}#{symbol}#{major_part}"
        else
          "#{sign}#{symbol}#{major_part}.#{format("%0#{decimals}d", minor_part)}"
        end
      end

      # Equality check.
      #
      # @param other [Money]
      # @return [Boolean]
      def ==(other)
        return false unless other.is_a?(Money)

        @minor_units == other.minor_units && @currency == other.currency
      end

      alias eql? ==

      # Hash for use in collections.
      #
      # @return [Integer]
      def hash
        [@minor_units, @currency].hash
      end
    end

    class << self
      # Parse currency code from string.
      #
      # @param code_string [String]
      # @return [Symbol, nil]
      def parse_code(code_string)
        code = code_string.upcase.to_sym
        CurrencyCode.valid?(code) ? code : nil
      end

      # Check if valid currency code.
      #
      # @param code_string [String]
      # @return [Boolean]
      def valid_code?(code_string)
        !parse_code(code_string).nil?
      end

      # Get decimals for currency code string.
      #
      # @param code_string [String]
      # @return [Integer, nil]
      def decimals(code_string)
        code = parse_code(code_string)
        code ? CurrencyCode.decimals(code) : nil
      end

      # Get symbol for currency code string.
      #
      # @param code_string [String]
      # @return [String, nil]
      def symbol(code_string)
        code = parse_code(code_string)
        code ? CurrencyCode.symbol(code) : nil
      end
    end
  end
end
