;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeCurrency - Thin CFFI wrapper for libproven currency operations.

(in-package #:proven)

(defcstruct currency-result-ffi
  (status :int32)
  (amount-minor :int64)
  (currency-code :uint8 :count 3)
  (decimal-places :uint8))

(defcfun ("proven_currency_parse" %currency-parse) (:struct currency-result-ffi) (ptr :pointer) (len :size))
(defcfun ("proven_currency_format" %currency-format) (:struct string-result)
  (amount-minor :int64) (code :uint8 :count 3) (decimal-places :uint8))

(defun currency-parse (str)
  "Parse currency string (e.g. 'USD 123.45'). Returns (values amount-minor code decimal-places ok-p)."
  (with-foreign-string-buf (ptr len str)
    (let ((result (%currency-parse ptr len)))
      (if (zerop (getf result 'status))
          (values (getf result 'amount-minor)
                  (coerce (mapcar #'code-char (coerce (getf result 'currency-code) 'list)) 'string)
                  (getf result 'decimal-places)
                  t)
          (values nil nil nil nil)))))

(defun currency-format (amount-minor code decimal-places)
  "Format currency amount. CODE is a 3-char string. Returns (values string ok-p)."
  (with-foreign-object (code-arr :uint8 3)
    (loop for i below 3
          do (setf (mem-aref code-arr :uint8 i) (char-code (char code i))))
    (extract-string-result (%currency-format amount-minor code-arr decimal-places))))
