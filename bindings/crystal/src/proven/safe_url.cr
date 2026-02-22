# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeUrl -- thin FFI wrapper around libproven's URL parsing.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  # Parsed URL components returned by SafeUrl.parse.
  record UrlParts,
    scheme : String,
    host : String,
    port : UInt16?,
    path : String,
    query : String?,
    fragment : String?

  module SafeUrl
    # Helper: extract a Crystal String from a C pointer + length, returning nil
    # for null pointers or zero-length strings.
    private def self.extract_string(ptr : Pointer(UInt8), len : LibC::SizeT) : String?
      return nil if ptr.null? || len == 0
      String.new(ptr, len)
    end

    # Parse a URL into its components.  Returns nil on invalid input.
    # The caller does NOT need to free anything -- this method handles cleanup.
    def self.parse(input : String) : UrlParts?
      slice = input.to_slice
      result = LibProven.proven_url_parse(slice.to_unsafe, slice.size)
      return nil unless result.status == LibProven::ProvenStatus::Ok

      c = result.components
      parts = UrlParts.new(
        scheme:   extract_string(c.scheme, c.scheme_len) || "",
        host:     extract_string(c.host, c.host_len) || "",
        port:     c.has_port ? c.port : nil,
        path:     extract_string(c.path, c.path_len) || "/",
        query:    extract_string(c.query, c.query_len),
        fragment: extract_string(c.fragment, c.fragment_len)
      )

      # Free the C-allocated strings inside the components struct.
      LibProven.proven_url_free(pointerof(result.components))

      parts
    end
  end
end
