;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeChecksum - JNA wrapper for proven_checksum_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.checksum
  "Safe checksum operations via libproven JNA FFI."
  (:require [proven.native :as n]))

(defn crc32
  "Compute CRC32 checksum of data. Returns the CRC32 value or nil."
  [^bytes data]
  (let [mem (n/to-native-bytes data)]
    (when mem
      (n/call-int-result "proven_checksum_crc32" mem (long (alength data))))))

(defn verify-crc32
  "Verify that data matches an expected CRC32 checksum."
  [^bytes data ^int expected]
  (let [mem (n/to-native-bytes data)]
    (when mem
      (n/call-bool-result "proven_checksum_verify_crc32"
                          mem (long (alength data)) (int expected)))))
