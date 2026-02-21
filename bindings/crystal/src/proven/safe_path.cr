# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Result type for path operations.
  struct PathResult
    getter path : String?
    getter error : String?

    def self.ok(path : String) : PathResult
      new(path, nil)
    end

    def self.error(message : String) : PathResult
      new(nil, message)
    end

    def initialize(@path : String?, @error : String?)
    end

    def ok? : Bool
      !@path.nil?
    end

    def error? : Bool
      !@error.nil?
    end
  end

  # Safe path operations for directory traversal prevention.
  module SafePath
    # Dangerous path patterns.
    TRAVERSAL_PATTERNS = [
      "..",
      "./",
      ".\\",
      "%2e%2e",
      "%2e.",
      ".%2e",
      "%00",
    ]

    # Check if path contains traversal sequences.
    def self.has_traversal?(path : String) : Bool
      normalized = path.downcase
      TRAVERSAL_PATTERNS.any? { |pattern| normalized.includes?(pattern) }
    end

    # Sanitize a filename (remove directory separators and dangerous chars).
    def self.sanitize_filename(filename : String) : String
      # Remove directory separators
      result = filename.gsub(/[\/\\]/, "_")
      # Remove null bytes
      result = result.gsub("\0", "")
      # Remove leading dots
      result = result.lstrip('.')
      # Replace other dangerous chars
      result = result.gsub(/[<>:"|?*]/, "_")
      # Collapse multiple underscores
      result = result.gsub(/_+/, "_")
      # Trim
      result = result.strip("_")
      # Return safe default if empty
      result.empty? ? "unnamed" : result
    end

    # Join paths safely.
    def self.join(base : String, *components : String) : PathResult
      components.each do |component|
        if has_traversal?(component)
          return PathResult.error("Path traversal detected in component: #{component}")
        end
      end

      result = base
      components.each do |component|
        # Remove leading slashes from component
        clean = component.lstrip('/')
        clean = clean.lstrip('\\')
        result = File.join(result, clean)
      end

      PathResult.ok(result)
    end

    # Resolve path within base directory.
    def self.resolve_within(base : String, path : String) : PathResult
      if has_traversal?(path)
        return PathResult.error("Path traversal detected")
      end

      # Get absolute paths
      abs_base = File.expand_path(base)

      # Clean the path
      clean_path = path.lstrip('/').lstrip('\\')
      full_path = File.expand_path(File.join(abs_base, clean_path))

      # Verify it's within base
      unless full_path.starts_with?(abs_base)
        return PathResult.error("Path escapes base directory")
      end

      PathResult.ok(full_path)
    end

    # Get file extension safely.
    def self.get_extension(path : String) : String?
      return nil if path.empty?

      basename = File.basename(path)
      return nil if basename.starts_with?('.')

      dot_index = basename.rindex('.')
      return nil if dot_index.nil? || dot_index == 0

      basename[dot_index..]
    end

    # Check if extension is allowed.
    def self.extension_allowed?(path : String, allowed : Array(String)) : Bool
      ext = get_extension(path)
      return false if ext.nil?
      allowed.any? { |a| a.downcase == ext.downcase }
    end

    # Normalize path separators.
    def self.normalize_separators(path : String) : String
      path.gsub('\\', '/')
    end

    # Check if path is absolute.
    def self.absolute?(path : String) : Bool
      path.starts_with?('/') || (path.size >= 2 && path[1] == ':')
    end

    # Check if path is relative.
    def self.relative?(path : String) : Bool
      !absolute?(path)
    end

    # Get parent directory.
    def self.parent(path : String) : String?
      return nil if path.empty?
      File.dirname(path)
    end

    # Get filename from path.
    def self.filename(path : String) : String
      File.basename(path)
    end

    # Check if filename is hidden (starts with dot).
    def self.hidden?(path : String) : Bool
      File.basename(path).starts_with?('.')
    end
  end
end
