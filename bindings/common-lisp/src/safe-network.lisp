;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeNetwork - Thin CFFI wrapper for libproven network operations.

(in-package #:proven)

;;; IPv4 address struct matching the C ABI
(defcstruct ipv4-address-ffi
  (octets :uint8 :count 4))

(defcstruct ipv4-result
  (status :int32)
  (address (:struct ipv4-address-ffi)))

(defcfun ("proven_network_parse_ipv4" %network-parse-ipv4) (:struct ipv4-result) (ptr :pointer) (len :size))
(defcfun ("proven_network_ipv4_is_private" %network-ipv4-is-private) :boolean (addr (:struct ipv4-address-ffi)))
(defcfun ("proven_network_ipv4_is_loopback" %network-ipv4-is-loopback) :boolean (addr (:struct ipv4-address-ffi)))

(defun network-parse-ipv4 (ip-string)
  "Parse an IPv4 address string. Returns (values octets-list ok-p) where octets-list is (a b c d)."
  (with-foreign-string-buf (ptr len ip-string)
    (let ((result (%network-parse-ipv4 ptr len)))
      (if (zerop (getf result 'status))
          (let ((addr (getf result 'address)))
            (values (getf addr 'octets) t))
          (values nil nil)))))

(defun network-ipv4-is-private (octets)
  "Check if IPv4 address OCTETS (list of 4 bytes) is a private address."
  (with-foreign-object (addr '(:struct ipv4-address-ffi))
    (loop for i below 4
          for byte in octets
          do (setf (mem-aref (foreign-slot-pointer addr '(:struct ipv4-address-ffi) 'octets) :uint8 i) byte))
    (%network-ipv4-is-private addr)))

(defun network-ipv4-is-loopback (octets)
  "Check if IPv4 address OCTETS (list of 4 bytes) is a loopback address."
  (with-foreign-object (addr '(:struct ipv4-address-ffi))
    (loop for i below 4
          for byte in octets
          do (setf (mem-aref (foreign-slot-pointer addr '(:struct ipv4-address-ffi) 'octets) :uint8 i) byte))
    (%network-ipv4-is-loopback addr)))
