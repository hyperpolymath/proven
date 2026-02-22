;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafePhone - Thin CFFI wrapper for libproven phone number operations.

(in-package #:proven)

(defcstruct phone-result-ffi
  (status :int32)
  (country-code :uint16)
  (national-number :uint64)
  (is-valid :boolean))

(defcfun ("proven_phone_parse" %phone-parse) (:struct phone-result-ffi) (ptr :pointer) (len :size))
(defcfun ("proven_phone_format_e164" %phone-format-e164) (:struct string-result) (country-code :uint16) (national-number :uint64))

(defun phone-parse (str)
  "Parse phone number string. Returns (values country-code national-number is-valid ok-p)."
  (with-foreign-string-buf (ptr len str)
    (let ((result (%phone-parse ptr len)))
      (if (zerop (getf result 'status))
          (values (getf result 'country-code)
                  (getf result 'national-number)
                  (getf result 'is-valid)
                  t)
          (values nil nil nil nil)))))

(defun phone-format-e164 (country-code national-number)
  "Format phone number as E.164 string. Returns (values string ok-p)."
  (extract-string-result (%phone-format-e164 country-code national-number)))
