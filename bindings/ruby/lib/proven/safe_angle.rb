# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe angle operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeAngle
    class << self
      # Convert degrees to radians.
      #
      # @param degrees [Float]
      # @return [Float, nil]
      def deg_to_rad(degrees)
        FFI.invoke_f64(
          "proven_angle_deg_to_rad",
          [Fiddle::TYPE_DOUBLE],
          [degrees.to_f]
        )
      end

      # Convert radians to degrees.
      #
      # @param radians [Float]
      # @return [Float, nil]
      def rad_to_deg(radians)
        FFI.invoke_f64(
          "proven_angle_rad_to_deg",
          [Fiddle::TYPE_DOUBLE],
          [radians.to_f]
        )
      end

      # Normalize degrees to [0, 360).
      #
      # @param degrees [Float]
      # @return [Float, nil]
      def normalize_degrees(degrees)
        FFI.invoke_f64(
          "proven_angle_normalize_degrees",
          [Fiddle::TYPE_DOUBLE],
          [degrees.to_f]
        )
      end

      # Normalize radians to [0, 2*pi).
      #
      # @param radians [Float]
      # @return [Float, nil]
      def normalize_radians(radians)
        FFI.invoke_f64(
          "proven_angle_normalize_radians",
          [Fiddle::TYPE_DOUBLE],
          [radians.to_f]
        )
      end
    end
  end
end
