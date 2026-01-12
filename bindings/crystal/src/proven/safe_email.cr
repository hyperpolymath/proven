# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Result type for email parsing.
  struct EmailResult
    getter local_part : String?
    getter domain : String?
    getter error : String?

    def self.ok(local_part : String, domain : String) : EmailResult
      new(local_part, domain, nil)
    end

    def self.error(message : String) : EmailResult
      new(nil, nil, message)
    end

    def initialize(@local_part : String?, @domain : String?, @error : String?)
    end

    def ok? : Bool
      !@local_part.nil? && !@domain.nil?
    end

    def error? : Bool
      !@error.nil?
    end
  end

  # Safe email validation and parsing.
  module SafeEmail
    # Common disposable email domains.
    DISPOSABLE_DOMAINS = [
      "mailinator.com",
      "guerrillamail.com",
      "tempmail.com",
      "throwaway.email",
      "10minutemail.com",
      "fakeinbox.com",
      "trashmail.com",
      "maildrop.cc",
      "yopmail.com",
      "sharklasers.com",
    ]

    # Email regex pattern (RFC 5322 simplified).
    EMAIL_PATTERN = /^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/

    # Check if email format is valid.
    def self.valid?(email : String) : Bool
      return false if email.empty?
      return false if email.size > 254
      return false unless email.includes?('@')

      EMAIL_PATTERN.matches?(email)
    end

    # Parse and validate email address.
    def self.parse(email : String) : EmailResult
      return EmailResult.error("Email is empty") if email.empty?
      return EmailResult.error("Email too long") if email.size > 254

      at_index = email.index('@')
      return EmailResult.error("Missing @ symbol") if at_index.nil?

      local_part = email[0...at_index]
      domain = email[(at_index + 1)..]

      return EmailResult.error("Local part is empty") if local_part.empty?
      return EmailResult.error("Local part too long") if local_part.size > 64
      return EmailResult.error("Domain is empty") if domain.empty?
      return EmailResult.error("Domain too long") if domain.size > 253

      unless valid?(email)
        return EmailResult.error("Invalid email format")
      end

      EmailResult.ok(local_part, domain)
    end

    # Check if domain is a disposable email provider.
    def self.disposable?(email : String) : Bool
      at_index = email.index('@')
      return false if at_index.nil?

      domain = email[(at_index + 1)..].downcase
      DISPOSABLE_DOMAINS.includes?(domain)
    end

    # Normalize email address.
    def self.normalize(email : String) : String?
      result = parse(email)
      return nil unless result.ok?

      local = result.local_part.not_nil!
      domain = result.domain.not_nil!.downcase

      "#{local}@#{domain}"
    end

    # Get domain from email.
    def self.get_domain(email : String) : String?
      at_index = email.index('@')
      return nil if at_index.nil?
      email[(at_index + 1)..]
    end

    # Get local part from email.
    def self.get_local_part(email : String) : String?
      at_index = email.index('@')
      return nil if at_index.nil?
      email[0...at_index]
    end

    # Mask email for display (user@example.com -> u***@example.com).
    def self.mask(email : String) : String?
      result = parse(email)
      return nil unless result.ok?

      local = result.local_part.not_nil!
      domain = result.domain.not_nil!

      if local.size <= 1
        "#{local}***@#{domain}"
      else
        "#{local[0]}***@#{domain}"
      end
    end

    # Check if two emails are equal (case-insensitive domain).
    def self.equal?(email1 : String, email2 : String) : Bool
      norm1 = normalize(email1)
      norm2 = normalize(email2)
      return false if norm1.nil? || norm2.nil?
      norm1 == norm2
    end
  end
end
