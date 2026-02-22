# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeAngle -- thin FFI wrapper around libproven's angle operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeAngle
    # Convert degrees to radians.
    def self.deg_to_rad(degrees : Float64) : Float64
      LibProven.proven_angle_deg_to_rad(degrees)
    end

    # Convert radians to degrees.
    def self.rad_to_deg(radians : Float64) : Float64
      LibProven.proven_angle_rad_to_deg(radians)
    end

    # Normalize angle to [0, 360) degrees.
    def self.normalize_degrees(degrees : Float64) : Float64
      LibProven.proven_angle_normalize_degrees(degrees)
    end

    # Normalize angle to [0, 2*pi) radians.
    def self.normalize_radians(radians : Float64) : Float64
      LibProven.proven_angle_normalize_radians(radians)
    end
  end
end
