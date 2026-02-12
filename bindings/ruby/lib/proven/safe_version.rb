# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe semantic versioning operations.
  #
  # Provides parsing, comparison, and manipulation of SemVer strings
  # with validation and safe operations.
  module SafeVersion
    # Semantic version representation.
    class SemVer
      include Comparable

      attr_reader :major, :minor, :patch, :prerelease, :build_metadata

      def initialize(major:, minor:, patch:, prerelease: nil, build_metadata: nil)
        @major = major
        @minor = minor
        @patch = patch
        @prerelease = prerelease
        @build_metadata = build_metadata
      end

      def <=>(other)
        return nil unless other.is_a?(SemVer)

        result = major <=> other.major
        return result unless result.zero?

        result = minor <=> other.minor
        return result unless result.zero?

        result = patch <=> other.patch
        return result unless result.zero?

        # Pre-release versions have lower precedence
        case [prerelease.nil?, other.prerelease.nil?]
        when [true, true]
          0
        when [true, false]
          1
        when [false, true]
          -1
        else
          compare_prerelease(prerelease, other.prerelease)
        end
      end

      def to_s
        version = "#{major}.#{minor}.#{patch}"
        version += "-#{prerelease}" if prerelease
        version += "+#{build_metadata}" if build_metadata
        version
      end

      def stable?
        prerelease.nil?
      end

      def prerelease?
        !prerelease.nil?
      end

      private

      def compare_prerelease(pre1, pre2)
        parts1 = pre1.split(".")
        parts2 = pre2.split(".")

        [parts1.length, parts2.length].max.times do |i|
          p1 = parts1[i]
          p2 = parts2[i]

          return 1 if p1.nil?
          return -1 if p2.nil?

          # Numeric identifiers have lower precedence than non-numeric
          num1 = Integer(p1) rescue nil
          num2 = Integer(p2) rescue nil

          if num1 && num2
            result = num1 <=> num2
            return result unless result.zero?
          elsif num1
            return -1
          elsif num2
            return 1
          else
            result = p1 <=> p2
            return result unless result.zero?
          end
        end

        0
      end
    end

    # SemVer regex pattern
    SEMVER_REGEX = /\A
      v?                           # Optional 'v' prefix
      (0|[1-9]\d*)                 # Major
      \.
      (0|[1-9]\d*)                 # Minor
      \.
      (0|[1-9]\d*)                 # Patch
      (?:-([\da-zA-Z-]+(?:\.[\da-zA-Z-]+)*))?  # Pre-release
      (?:\+([\da-zA-Z-]+(?:\.[\da-zA-Z-]+)*))? # Build metadata
    \z/x.freeze

    # Parse a version string.
    #
    # @param version_string [String]
    # @return [Result]
    def self.parse(version_string)
      return Result.error(InvalidInputError.new("Version cannot be nil")) if version_string.nil?

      trimmed = version_string.strip
      match = SEMVER_REGEX.match(trimmed)

      return Result.error(InvalidFormatError.new("Invalid version format")) unless match

      Result.ok(SemVer.new(
        major: match[1].to_i,
        minor: match[2].to_i,
        patch: match[3].to_i,
        prerelease: match[4],
        build_metadata: match[5]
      ))
    end

    # Check if a string is a valid SemVer.
    #
    # @param version_string [String]
    # @return [Boolean]
    def self.valid?(version_string)
      parse(version_string).ok?
    end

    # Compare two version strings.
    #
    # @param v1 [String]
    # @param v2 [String]
    # @return [Result] containing -1, 0, or 1
    def self.compare(v1, v2)
      parse(v1).and_then do |ver1|
        parse(v2).and_then do |ver2|
          Result.ok(ver1 <=> ver2)
        end
      end
    end

    # Check if v1 > v2.
    #
    # @param v1 [String]
    # @param v2 [String]
    # @return [Boolean]
    def self.greater?(v1, v2)
      result = compare(v1, v2)
      result.ok? && result.value == 1
    end

    # Check if v1 < v2.
    #
    # @param v1 [String]
    # @param v2 [String]
    # @return [Boolean]
    def self.less?(v1, v2)
      result = compare(v1, v2)
      result.ok? && result.value == -1
    end

    # Check if v1 == v2 (ignoring build metadata).
    #
    # @param v1 [String]
    # @param v2 [String]
    # @return [Boolean]
    def self.equal?(v1, v2)
      result = compare(v1, v2)
      result.ok? && result.value.zero?
    end

    # Increment major version.
    #
    # @param version [SemVer, String]
    # @return [Result]
    def self.increment_major(version)
      ver = version.is_a?(SemVer) ? version : parse(version).value
      return Result.error(InvalidInputError.new("Invalid version")) if ver.nil?

      Result.ok(SemVer.new(major: ver.major + 1, minor: 0, patch: 0))
    end

    # Increment minor version.
    #
    # @param version [SemVer, String]
    # @return [Result]
    def self.increment_minor(version)
      ver = version.is_a?(SemVer) ? version : parse(version).value
      return Result.error(InvalidInputError.new("Invalid version")) if ver.nil?

      Result.ok(SemVer.new(major: ver.major, minor: ver.minor + 1, patch: 0))
    end

    # Increment patch version.
    #
    # @param version [SemVer, String]
    # @return [Result]
    def self.increment_patch(version)
      ver = version.is_a?(SemVer) ? version : parse(version).value
      return Result.error(InvalidInputError.new("Invalid version")) if ver.nil?

      Result.ok(SemVer.new(major: ver.major, minor: ver.minor, patch: ver.patch + 1))
    end

    # Check if version satisfies a constraint.
    #
    # @param version [String]
    # @param constraint [String] e.g., ">=1.0.0", "^2.0.0", "~1.2.3"
    # @return [Boolean]
    def self.satisfies?(version, constraint)
      ver = parse(version)
      return false unless ver.ok?

      ver = ver.value

      case constraint
      when /\A>=\s*(.+)\z/
        other = parse(Regexp.last_match(1))
        other.ok? && ver >= other.value
      when /\A<=\s*(.+)\z/
        other = parse(Regexp.last_match(1))
        other.ok? && ver <= other.value
      when /\A>\s*(.+)\z/
        other = parse(Regexp.last_match(1))
        other.ok? && ver > other.value
      when /\A<\s*(.+)\z/
        other = parse(Regexp.last_match(1))
        other.ok? && ver < other.value
      when /\A=\s*(.+)\z/
        other = parse(Regexp.last_match(1))
        other.ok? && ver == other.value
      when /\A\^\s*(.+)\z/
        # Caret: compatible with version (same major)
        other = parse(Regexp.last_match(1))
        other.ok? && ver.major == other.value.major && ver >= other.value
      when /\A~\s*(.+)\z/
        # Tilde: same major and minor
        other = parse(Regexp.last_match(1))
        other.ok? && ver.major == other.value.major && ver.minor == other.value.minor && ver >= other.value
      else
        false
      end
    end
  end
end
