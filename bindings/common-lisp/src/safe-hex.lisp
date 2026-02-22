;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeHex - Thin CFFI wrapper for libproven hex encoding/decoding.

(in-package #:proven)

(defcstruct hex-decode-result-ffi
  (status :int32)
  (data :pointer)
  (length :size))

(defcfun ("proven_hex_encode" %hex-encode) (:struct string-result) (ptr :pointer) (len :size) (uppercase :boolean))
(defcfun ("proven_hex_decode" %hex-decode) (:struct hex-decode-result-ffi) (ptr :pointer) (len :size))
(defcfun ("proven_hex_free" %hex-free) :void (result :pointer))

(defun hex-encode (bytes &key (uppercase nil))
  "Hex encode BYTES (vector of unsigned-byte 8). Returns (values hex-string ok-p)."
  (let ((len (length bytes)))
    (with-foreign-object (buf :uint8 len)
      (loop for i below len
            do (setf (mem-aref buf :uint8 i) (aref bytes i)))
      (extract-string-result (%hex-encode buf len uppercase)))))

(defun hex-decode (hex-string)
  "Decode hex string to bytes. Returns (values byte-vector ok-p)."
  (with-foreign-string-buf (ptr len hex-string)
    (let ((result (%hex-decode ptr len)))
      (if (zerop (getf result 'status))
          (let* ((data-ptr (getf result 'data))
                 (data-len (getf result 'length))
                 (bytes (make-array data-len :element-type '(unsigned-byte 8))))
            (loop for i below data-len
                  do (setf (aref bytes i) (mem-aref data-ptr :uint8 i)))
            ;; Free the C-allocated data
            (with-foreign-object (res-ptr '(:struct hex-decode-result-ffi))
              (setf (foreign-slot-value res-ptr '(:struct hex-decode-result-ffi) 'status) 0)
              (setf (foreign-slot-value res-ptr '(:struct hex-decode-result-ffi) 'data) data-ptr)
              (setf (foreign-slot-value res-ptr '(:struct hex-decode-result-ffi) 'length) data-len)
              (%hex-free res-ptr))
            (values bytes t))
          (values nil nil)))))
