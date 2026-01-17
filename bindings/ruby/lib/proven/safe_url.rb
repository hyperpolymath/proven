# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "uri"

module Proven
  # Safe URL operations with validation and parsing.
  #
  # Provides validated URL handling with proper security checks
  # for scheme validation, host validation, and safe construction.
  module SafeUrl
    # Allowed URL schemes for web applications.
    ALLOWED_SCHEMES = %w[http https].freeze

    # Parse a URL string into components with validation.
    #
    # @param url_string [String] the URL to parse
    # @return [Result] containing parsed URL hash or error
    def self.parse(url_string)
      return Result.error(InvalidInputError.new("URL cannot be nil")) if url_string.nil?
      return Result.error(InvalidInputError.new("URL cannot be empty")) if url_string.strip.empty?
      return Result.error(TooLongError.new("URL too long")) if url_string.length > 2048

      begin
        uri = URI.parse(url_string.strip)

        Result.ok({
          scheme: uri.scheme,
          host: uri.host,
          port: uri.port,
          path: uri.path,
          query: uri.query,
          fragment: uri.fragment,
          user: uri.user,
          password: uri.password
        })
      rescue URI::InvalidURIError => e
        Result.error(InvalidFormatError.new("Invalid URL: #{e.message}"))
      end
    end

    # Validate that a URL has allowed scheme.
    #
    # @param url_string [String]
    # @param allowed_schemes [Array<String>]
    # @return [Result]
    def self.validate_scheme(url_string, allowed_schemes: ALLOWED_SCHEMES)
      parse(url_string).and_then do |parsed|
        if allowed_schemes.include?(parsed[:scheme]&.downcase)
          Result.ok(parsed)
        else
          Result.error(ValidationError.new("Scheme not allowed: #{parsed[:scheme]}"))
        end
      end
    end

    # Check if URL is valid HTTP/HTTPS.
    #
    # @param url_string [String]
    # @return [Boolean]
    def self.valid?(url_string)
      validate_scheme(url_string).ok?
    end

    # Extract the domain from a URL.
    #
    # @param url_string [String]
    # @return [Result] containing domain string or error
    def self.domain(url_string)
      parse(url_string).and_then do |parsed|
        if parsed[:host]
          Result.ok(parsed[:host])
        else
          Result.error(ValidationError.new("No host in URL"))
        end
      end
    end

    # Safely join a base URL with a path.
    #
    # @param base [String] the base URL
    # @param path [String] the path to join
    # @return [Result]
    def self.join(base, path)
      return Result.error(InvalidInputError.new("Base URL required")) if base.nil? || base.empty?
      return Result.error(InvalidInputError.new("Path required")) if path.nil?

      begin
        base_uri = URI.parse(base.strip)
        joined = base_uri + path.strip
        Result.ok(joined.to_s)
      rescue URI::InvalidURIError => e
        Result.error(InvalidFormatError.new("Failed to join URLs: #{e.message}"))
      end
    end

    # Safely encode query parameters.
    #
    # @param params [Hash] the parameters to encode
    # @return [String]
    def self.encode_query(params)
      return "" if params.nil? || params.empty?

      URI.encode_www_form(params)
    end

    # Safely decode query string into hash.
    #
    # @param query_string [String]
    # @return [Hash]
    def self.decode_query(query_string)
      return {} if query_string.nil? || query_string.empty?

      URI.decode_www_form(query_string).to_h
    rescue ArgumentError
      {}
    end

    # Build a URL from components.
    #
    # @param scheme [String]
    # @param host [String]
    # @param port [Integer, nil]
    # @param path [String]
    # @param query [String, Hash, nil]
    # @return [Result]
    def self.build(scheme:, host:, port: nil, path: "/", query: nil)
      return Result.error(InvalidInputError.new("Scheme required")) if scheme.nil? || scheme.empty?
      return Result.error(InvalidInputError.new("Host required")) if host.nil? || host.empty?

      begin
        uri = URI::Generic.build(
          scheme: scheme,
          host: host,
          port: port,
          path: path,
          query: query.is_a?(Hash) ? encode_query(query) : query
        )
        Result.ok(uri.to_s)
      rescue URI::InvalidComponentError => e
        Result.error(InvalidFormatError.new("Invalid URL component: #{e.message}"))
      end
    end

    # Check if URL is a data URL.
    #
    # @param url_string [String]
    # @return [Boolean]
    def self.data_url?(url_string)
      return false if url_string.nil?

      url_string.strip.downcase.start_with?("data:")
    end

    # Check if URL is a JavaScript URL (security risk).
    #
    # @param url_string [String]
    # @return [Boolean]
    def self.javascript_url?(url_string)
      return false if url_string.nil?

      url_string.strip.downcase.start_with?("javascript:")
    end

    # Normalize a URL for consistent comparison.
    #
    # @param url_string [String]
    # @return [Result]
    def self.normalize(url_string)
      parse(url_string).and_then do |parsed|
        scheme = parsed[:scheme]&.downcase || "https"
        host = parsed[:host]&.downcase
        return Result.error(ValidationError.new("No host")) unless host

        # Remove default ports
        port = parsed[:port]
        port = nil if (scheme == "http" && port == 80) || (scheme == "https" && port == 443)

        path = parsed[:path]
        path = "/" if path.nil? || path.empty?
        # Remove trailing slash except for root
        path = path.chomp("/") if path.length > 1

        build(
          scheme: scheme,
          host: host,
          port: port,
          path: path,
          query: parsed[:query]
        )
      end
    end

    class << self
      private

      def result_ok(value)
        Result.ok(value)
      end

      def result_error(error)
        Result.error(error)
      end
    end
  end
end
