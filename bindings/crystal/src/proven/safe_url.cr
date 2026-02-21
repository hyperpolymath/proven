# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

require "uri"

module Proven
  # Safe URL operations with validation and sanitization.
  module SafeUrl
    # Parsed URL components.
    record ParsedUrl,
      scheme : String,
      host : String,
      port : Int32?,
      path : String,
      query : String?,
      fragment : String?,
      userinfo : String?

    # Parse and validate a URL.
    def self.parse(input : String) : ParsedUrl?
      return nil if input.empty? || input.size > 2048

      begin
        uri = URI.parse(input)
        scheme = uri.scheme
        host = uri.host

        return nil if scheme.nil? || host.nil?
        return nil if scheme.empty? || host.empty?

        # Validate scheme
        return nil unless valid_scheme?(scheme)

        ParsedUrl.new(
          scheme: scheme.downcase,
          host: host.downcase,
          port: uri.port,
          path: uri.path.empty? ? "/" : uri.path,
          query: uri.query,
          fragment: uri.fragment,
          userinfo: uri.userinfo
        )
      rescue
        nil
      end
    end

    # Check if a URL is valid.
    def self.valid?(input : String) : Bool
      !parse(input).nil?
    end

    # Check if scheme is valid.
    def self.valid_scheme?(scheme : String) : Bool
      normalized = scheme.downcase
      ["http", "https", "ftp", "ftps", "mailto", "tel", "file", "data", "wss", "ws"].includes?(normalized)
    end

    # Check if URL uses HTTPS.
    def self.is_https?(input : String) : Bool
      parsed = parse(input)
      return false if parsed.nil?
      parsed.scheme == "https"
    end

    # Check if URL uses secure scheme.
    def self.is_secure?(input : String) : Bool
      parsed = parse(input)
      return false if parsed.nil?
      ["https", "ftps", "wss"].includes?(parsed.scheme)
    end

    # Extract domain from URL.
    def self.domain(input : String) : String?
      parsed = parse(input)
      parsed.try(&.host)
    end

    # Extract TLD from URL.
    def self.tld(input : String) : String?
      host = domain(input)
      return nil if host.nil?

      parts = host.split('.')
      return nil if parts.size < 2
      parts.last
    end

    # Check if domain matches (exact or subdomain).
    def self.domain_matches?(url : String, domain : String) : Bool
      url_domain = self.domain(url)
      return false if url_domain.nil?

      url_domain == domain.downcase || url_domain.ends_with?(".#{domain.downcase}")
    end

    # Normalize a URL.
    def self.normalize(input : String) : String?
      parsed = parse(input)
      return nil if parsed.nil?

      result = String.build do |str|
        str << parsed.scheme << "://"
        if userinfo = parsed.userinfo
          str << userinfo << "@"
        end
        str << parsed.host
        if port = parsed.port
          # Only include port if non-default
          unless (parsed.scheme == "http" && port == 80) || (parsed.scheme == "https" && port == 443)
            str << ":" << port
          end
        end
        str << parsed.path
        if query = parsed.query
          str << "?" << query
        end
        if fragment = parsed.fragment
          str << "#" << fragment
        end
      end

      result
    end

    # Join base URL with relative path.
    def self.join(base : String, relative : String) : String?
      begin
        base_uri = URI.parse(base)
        resolved = base_uri.resolve(relative)
        resolved.to_s
      rescue
        nil
      end
    end

    # Extract query parameters as hash.
    def self.query_params(input : String) : Hash(String, String)
      parsed = parse(input)
      return {} of String => String if parsed.nil?

      query = parsed.query
      return {} of String => String if query.nil? || query.empty?

      result = {} of String => String
      query.split('&').each do |pair|
        if idx = pair.index('=')
          key = URI.decode_www_form(pair[0...idx])
          value = URI.decode_www_form(pair[(idx + 1)..])
          result[key] = value
        else
          result[URI.decode_www_form(pair)] = ""
        end
      end
      result
    end

    # Build URL from components.
    def self.build(
      scheme : String,
      host : String,
      path : String = "/",
      port : Int32? = nil,
      query : Hash(String, String)? = nil,
      fragment : String? = nil
    ) : String?
      return nil unless valid_scheme?(scheme)
      return nil if host.empty?

      String.build do |str|
        str << scheme.downcase << "://" << host.downcase
        if port && !((scheme.downcase == "http" && port == 80) || (scheme.downcase == "https" && port == 443))
          str << ":" << port
        end
        str << (path.starts_with?('/') ? path : "/#{path}")
        if query && !query.empty?
          str << "?"
          query.each_with_index do |(k, v), i|
            str << "&" if i > 0
            str << URI.encode_www_form(k) << "=" << URI.encode_www_form(v)
          end
        end
        if fragment && !fragment.empty?
          str << "#" << fragment
        end
      end
    end

    # Check if URL contains dangerous patterns.
    def self.contains_dangerous_pattern?(input : String) : Bool
      dangerous = [
        "javascript:",
        "data:text/html",
        "vbscript:",
        "<script",
        "onerror=",
        "onload=",
        "onclick=",
      ]
      lower = input.downcase
      dangerous.any? { |pattern| lower.includes?(pattern) }
    end

    # Sanitize URL by removing dangerous patterns.
    def self.sanitize(input : String) : String?
      return nil if contains_dangerous_pattern?(input)
      normalize(input)
    end
  end
end
