;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeCrypto - Thin CFFI wrapper for libproven cryptographic operations.

(in-package #:proven)

(defcfun ("proven_crypto_constant_time_eq" %crypto-constant-time-eq) (:struct bool-result)
  (ptr1 :pointer) (len1 :size) (ptr2 :pointer) (len2 :size))
(defcfun ("proven_crypto_random_bytes" %crypto-random-bytes) :int32 (ptr :pointer) (len :size))

(defun crypto-constant-time-eq (a b)
  "Constant-time byte comparison of strings A and B. Returns (values bool ok-p)."
  (with-foreign-string-buf (ptr1 len1 a)
    (with-foreign-string-buf (ptr2 len2 b)
      (extract-bool-result (%crypto-constant-time-eq ptr1 len1 ptr2 len2)))))

(defun crypto-random-bytes (length)
  "Fill a buffer with LENGTH cryptographically secure random bytes.
   Returns (values byte-vector ok-p)."
  (with-foreign-object (buf :uint8 length)
    (let ((status (%crypto-random-bytes buf length)))
      (if (zerop status)
          (let ((result (make-array length :element-type '(unsigned-byte 8))))
            (loop for i below length
                  do (setf (aref result i) (mem-aref buf :uint8 i)))
            (values result t))
          (values nil nil)))))
