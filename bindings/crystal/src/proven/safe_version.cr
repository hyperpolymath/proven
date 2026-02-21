# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Semantic version representation.
  record SemanticVersion,
    major : Int32,
    minor : Int32,
    patch : Int32,
    prerelease : String? = nil,
    build_metadata : String? = nil do
    include Comparable(SemanticVersion)

    def <=>(other : SemanticVersion) : Int32
      cmp = major <=> other.major
      return cmp unless cmp == 0

      cmp = minor <=> other.minor
      return cmp unless cmp == 0

      cmp = patch <=> other.patch
      return cmp unless cmp == 0

      # Pre-release versions have lower precedence
      case {prerelease, other.prerelease}
      when {nil, nil}    then 0
      when {nil, String} then 1
      when {String, nil} then -1
      else
        prerelease.not_nil! <=> other.prerelease.not_nil!
      end
    end

    def to_s : String
      result = "#{major}.#{minor}.#{patch}"
      if pr = prerelease
        result += "-#{pr}"
      end
      if bm = build_metadata
        result += "+#{bm}"
      end
      result
    end
  end

  # Safe semantic version operations.
  module SafeVersion
    VERSION_REGEX = /^(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z\-\.]+))?(?:\+([0-9A-Za-z\-\.]+))?$/

    # Parse semantic version string.
    def self.parse(input : String) : SemanticVersion?
      return nil if input.empty? || input.size > 100

      match = VERSION_REGEX.match(input)
      return nil if match.nil?

      major = match[1].to_i?
      minor = match[2].to_i?
      patch = match[3].to_i?

      return nil if major.nil? || minor.nil? || patch.nil?
      return nil if major < 0 || minor < 0 || patch < 0

      prerelease = match[4]?.try { |s| s.empty? ? nil : s }
      build_metadata = match[5]?.try { |s| s.empty? ? nil : s }

      SemanticVersion.new(major, minor, patch, prerelease, build_metadata)
    end

    # Check if string is valid semantic version.
    def self.valid?(input : String) : Bool
      !parse(input).nil?
    end

    # Compare two version strings.
    def self.compare(a : String, b : String) : Int32?
      va = parse(a)
      vb = parse(b)
      return nil if va.nil? || vb.nil?
      va <=> vb
    end

    # Check if version satisfies constraint.
    def self.satisfies?(version : String, constraint : String) : Bool?
      v = parse(version)
      return nil if v.nil?

      # Parse constraint (e.g., ">=1.0.0", "^1.2.3", "~1.2.0")
      if constraint.starts_with?(">=")
        c = parse(constraint[2..])
        return nil if c.nil?
        (v <=> c) >= 0
      elsif constraint.starts_with?(">")
        c = parse(constraint[1..])
        return nil if c.nil?
        (v <=> c) > 0
      elsif constraint.starts_with?("<=")
        c = parse(constraint[2..])
        return nil if c.nil?
        (v <=> c) <= 0
      elsif constraint.starts_with?("<")
        c = parse(constraint[1..])
        return nil if c.nil?
        (v <=> c) < 0
      elsif constraint.starts_with?("=")
        c = parse(constraint[1..])
        return nil if c.nil?
        (v <=> c) == 0
      elsif constraint.starts_with?("^")
        # Caret: compatible with (same major)
        c = parse(constraint[1..])
        return nil if c.nil?
        v.major == c.major && (v <=> c) >= 0
      elsif constraint.starts_with?("~")
        # Tilde: patch-level changes allowed
        c = parse(constraint[1..])
        return nil if c.nil?
        v.major == c.major && v.minor == c.minor && (v <=> c) >= 0
      else
        c = parse(constraint)
        return nil if c.nil?
        (v <=> c) == 0
      end
    end

    # Increment major version.
    def self.increment_major(version : SemanticVersion) : SemanticVersion
      SemanticVersion.new(version.major + 1, 0, 0)
    end

    # Increment minor version.
    def self.increment_minor(version : SemanticVersion) : SemanticVersion
      SemanticVersion.new(version.major, version.minor + 1, 0)
    end

    # Increment patch version.
    def self.increment_patch(version : SemanticVersion) : SemanticVersion
      SemanticVersion.new(version.major, version.minor, version.patch + 1)
    end

    # Set prerelease tag.
    def self.with_prerelease(version : SemanticVersion, prerelease : String) : SemanticVersion
      SemanticVersion.new(
        version.major,
        version.minor,
        version.patch,
        prerelease.empty? ? nil : prerelease,
        version.build_metadata
      )
    end

    # Set build metadata.
    def self.with_build_metadata(version : SemanticVersion, metadata : String) : SemanticVersion
      SemanticVersion.new(
        version.major,
        version.minor,
        version.patch,
        version.prerelease,
        metadata.empty? ? nil : metadata
      )
    end

    # Check if version is stable (no prerelease).
    def self.stable?(version : SemanticVersion) : Bool
      version.prerelease.nil?
    end

    # Check if version is prerelease.
    def self.prerelease?(version : SemanticVersion) : Bool
      !version.prerelease.nil?
    end

    # Find maximum version in list.
    def self.max(versions : Array(String)) : SemanticVersion?
      parsed = versions.compact_map { |v| parse(v) }
      return nil if parsed.empty?
      parsed.max
    end

    # Find minimum version in list.
    def self.min(versions : Array(String)) : SemanticVersion?
      parsed = versions.compact_map { |v| parse(v) }
      return nil if parsed.empty?
      parsed.min
    end

    # Sort versions.
    def self.sort(versions : Array(String)) : Array(SemanticVersion)
      versions.compact_map { |v| parse(v) }.sort
    end
  end
end
