# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe email validation and parsing operations.
  module SafeEmail
    # Represents the parts of an email address.
    EmailParts = Struct.new(:local_part, :domain, keyword_init: true)

    class << self
      # Check if an email address is valid (basic check).
      #
      # @param email [String]
      # @return [Boolean]
      def valid?(email)
        parts = email.split("@")
        return false unless parts.length == 2

        local_part, domain = parts
        return false if local_part.empty?
        return false if domain.length < 3
        return false unless domain.include?(".")
        return false if domain.start_with?(".")
        return false if domain.end_with?(".")

        true
      end

      # Split an email into local part and domain.
      #
      # @param email [String]
      # @return [EmailParts, nil]
      def split(email)
        return nil unless valid?(email)

        parts = email.split("@")
        EmailParts.new(local_part: parts[0], domain: parts[1])
      end

      # Extract the domain from an email address.
      #
      # @param email [String]
      # @return [String, nil]
      def domain(email)
        split(email)&.domain
      end

      # Extract the local part from an email address.
      #
      # @param email [String]
      # @return [String, nil]
      def local_part(email)
        split(email)&.local_part
      end

      # Normalize an email address (lowercase domain).
      #
      # @param email [String]
      # @return [String, nil]
      def normalize(email)
        parts = split(email)
        return nil unless parts

        "#{parts.local_part}@#{parts.domain.downcase}"
      end
    end
  end
end
