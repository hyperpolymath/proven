;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeAngle - JNA wrapper for proven_angle_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.angle
  "Safe angle conversion and normalization via libproven JNA FFI."
  (:require [proven.native :as n]))

(defn deg->rad
  "Convert degrees to radians."
  [^double degrees]
  (n/call-double "proven_angle_deg_to_rad" (double degrees)))

(defn rad->deg
  "Convert radians to degrees."
  [^double radians]
  (n/call-double "proven_angle_rad_to_deg" (double radians)))

(defn normalize-degrees
  "Normalize an angle in degrees to [0, 360)."
  [^double degrees]
  (n/call-double "proven_angle_normalize_degrees" (double degrees)))

(defn normalize-radians
  "Normalize an angle in radians to [0, 2*pi)."
  [^double radians]
  (n/call-double "proven_angle_normalize_radians" (double radians)))
