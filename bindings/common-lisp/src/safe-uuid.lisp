;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeUUID - Thin CFFI wrapper for libproven UUID operations.

(in-package #:proven)

(defcstruct uuid-ffi
  (bytes :uint8 :count 16))

(defcstruct uuid-result-ffi
  (status :int32)
  (uuid (:struct uuid-ffi)))

(defcfun ("proven_uuid_v4" %uuid-v4) (:struct uuid-result-ffi))
(defcfun ("proven_uuid_to_string" %uuid-to-string) (:struct string-result) (uuid (:struct uuid-ffi)))
(defcfun ("proven_uuid_parse" %uuid-parse) (:struct uuid-result-ffi) (ptr :pointer) (len :size))
(defcfun ("proven_uuid_is_nil" %uuid-is-nil) :boolean (uuid (:struct uuid-ffi)))
(defcfun ("proven_uuid_version" %uuid-version) :uint8 (uuid (:struct uuid-ffi)))

(defun uuid-v4 ()
  "Generate a random UUID v4. Returns (values byte-vector ok-p)."
  (let ((result (%uuid-v4)))
    (if (zerop (getf result 'status))
        (values (getf (getf result 'uuid) 'bytes) t)
        (values nil nil))))

(defun uuid-to-string (bytes)
  "Format UUID BYTES (16-element vector) as string. Returns (values string ok-p)."
  (with-foreign-object (uuid '(:struct uuid-ffi))
    (loop for i below 16
          do (setf (mem-aref (foreign-slot-pointer uuid '(:struct uuid-ffi) 'bytes) :uint8 i) (aref bytes i)))
    (extract-string-result (%uuid-to-string uuid))))

(defun uuid-parse (str)
  "Parse UUID from string. Returns (values byte-vector ok-p)."
  (with-foreign-string-buf (ptr len str)
    (let ((result (%uuid-parse ptr len)))
      (if (zerop (getf result 'status))
          (values (getf (getf result 'uuid) 'bytes) t)
          (values nil nil)))))

(defun uuid-is-nil (bytes)
  "Check if UUID BYTES are all zeros."
  (with-foreign-object (uuid '(:struct uuid-ffi))
    (loop for i below 16
          do (setf (mem-aref (foreign-slot-pointer uuid '(:struct uuid-ffi) 'bytes) :uint8 i) (aref bytes i)))
    (%uuid-is-nil uuid)))

(defun uuid-version (bytes)
  "Get UUID version from BYTES."
  (with-foreign-object (uuid '(:struct uuid-ffi))
    (loop for i below 16
          do (setf (mem-aref (foreign-slot-pointer uuid '(:struct uuid-ffi) 'bytes) :uint8 i) (aref bytes i)))
    (%uuid-version uuid)))
