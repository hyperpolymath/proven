# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe phone number validation following E.164.
  module SafePhone
    # Country calling codes.
    module CountryCode
      US = 1    # USA, Canada
      RU = 7    # Russia
      EG = 20   # Egypt
      ZA = 27   # South Africa
      FR = 33   # France
      ES = 34   # Spain
      IT = 39   # Italy
      UK = 44   # UK
      DE = 49   # Germany
      MX = 52   # Mexico
      BR = 55   # Brazil
      AU = 61   # Australia
      JP = 81   # Japan
      KR = 82   # South Korea
      CN = 86   # China
      IN = 91   # India
      UNKNOWN = 0

      # Mapping from numeric values to symbols.
      VALUE_TO_CODE = {
        1 => :US, 7 => :RU, 20 => :EG, 27 => :ZA, 33 => :FR,
        34 => :ES, 39 => :IT, 44 => :UK, 49 => :DE, 52 => :MX,
        55 => :BR, 61 => :AU, 81 => :JP, 82 => :KR, 86 => :CN,
        91 => :IN
      }.freeze

      # Mapping from symbols to numeric values.
      CODE_TO_VALUE = VALUE_TO_CODE.invert.freeze

      class << self
        # Get numeric value for country code.
        #
        # @param code [Symbol]
        # @return [Integer]
        def value(code)
          CODE_TO_VALUE.fetch(code, 0)
        end

        # Get country code from numeric value.
        #
        # @param numeric_value [Integer]
        # @return [Symbol]
        def from_value(numeric_value)
          VALUE_TO_CODE.fetch(numeric_value, :UNKNOWN)
        end

        # Check if country code is known.
        #
        # @param code [Symbol]
        # @return [Boolean]
        def known?(code)
          CODE_TO_VALUE.key?(code)
        end
      end
    end

    # Validated phone number.
    class PhoneNumber
      attr_reader :country_code, :national_number

      # Create a new phone number.
      #
      # @param country_code [Symbol]
      # @param national_number [String]
      def initialize(country_code, national_number)
        @country_code = country_code
        @national_number = national_number.freeze
      end

      # Format in E.164 format.
      #
      # @return [String]
      def to_e164
        "+#{CountryCode.value(@country_code)}#{@national_number}"
      end

      # Format with spaces (international format).
      #
      # @return [String]
      def to_international
        country_code_value = CountryCode.value(@country_code)
        national_digits = @national_number
        national_length = national_digits.length

        if national_length <= 4
          "+#{country_code_value} #{national_digits}"
        elsif national_length <= 7
          "+#{country_code_value} #{national_digits[0, 3]} #{national_digits[3..]}"
        elsif national_length <= 10
          "+#{country_code_value} #{national_digits[0, 3]} #{national_digits[3, 3]} #{national_digits[6..]}"
        else
          "+#{country_code_value} #{national_digits}"
        end
      end

      # Get total digit count.
      #
      # @return [Integer]
      def digit_count
        country_code_value = CountryCode.value(@country_code)
        country_code_digits = if country_code_value >= 100
                                3
                              elsif country_code_value >= 10
                                2
                              else
                                1
                              end
        country_code_digits + @national_number.length
      end

      # Format as string (E.164 format).
      #
      # @return [String]
      def to_s
        to_e164
      end

      # Equality check.
      #
      # @param other [PhoneNumber]
      # @return [Boolean]
      def ==(other)
        return false unless other.is_a?(PhoneNumber)

        @country_code == other.country_code && @national_number == other.national_number
      end

      alias eql? ==

      # Hash for use in collections.
      #
      # @return [Integer]
      def hash
        [@country_code, @national_number].hash
      end
    end

    class << self
      # Parse phone number from string.
      #
      # @param input [String]
      # @return [PhoneNumber, nil]
      def parse(input)
        return nil if input.nil?

        trimmed = input.strip
        return nil if trimmed.empty?

        # Extract digits only
        digits = trimmed.gsub(/\D/, "")

        return nil if digits.length < 7
        return nil if digits.length > 15

        # Try to parse country code
        country_code_result = parse_country_code(digits)
        return nil if country_code_result.nil?

        country_code, national_start = country_code_result
        national_number = digits[national_start..]

        return nil if national_number.length < 4

        PhoneNumber.new(country_code, national_number)
      end

      # Parse phone number, raising on failure.
      #
      # @param input [String]
      # @return [PhoneNumber]
      # @raise [ArgumentError]
      def parse!(input)
        result = parse(input)
        raise ArgumentError, "Invalid phone number: #{input}" if result.nil?

        result
      end

      # Check if valid phone number.
      #
      # @param input [String]
      # @return [Boolean]
      def valid?(input)
        !parse(input).nil?
      end

      # Format phone number in E.164 format.
      #
      # @param phone [PhoneNumber]
      # @return [String]
      def format_e164(phone)
        phone.to_e164
      end

      # Format phone number in international format.
      #
      # @param phone [PhoneNumber]
      # @return [String]
      def format_international(phone)
        phone.to_international
      end

      private

      # Parse country code from digit string.
      #
      # @param digits [String]
      # @return [Array(Symbol, Integer), nil] [country_code, start_index]
      def parse_country_code(digits)
        # Try 3-digit codes first, then 2, then 1
        [3, 2, 1].each do |length|
          next unless digits.length >= length

          begin
            numeric_value = digits[0, length].to_i
            country_code = CountryCode.from_value(numeric_value)
            return [country_code, length] if country_code != :UNKNOWN
          rescue StandardError
            next
          end
        end

        nil
      end
    end
  end
end
