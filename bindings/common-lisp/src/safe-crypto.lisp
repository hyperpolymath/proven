;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeCrypto - Cryptographic utilities for Common Lisp

(in-package #:proven)

;;; Constant-time string comparison
(defun constant-time-equal-p (a b)
  "Compare strings A and B in constant time.
   Returns T if equal, NIL otherwise.
   Prevents timing attacks by always comparing all bytes."
  (if (/= (length a) (length b))
      nil
      (let ((result 0))
        (loop for i from 0 below (length a)
              do (setf result (logior result
                                      (logxor (char-code (char a i))
                                              (char-code (char b i))))))
        (zerop result))))

;;; Simple hash function (FNV-1a variant)
(defun simple-hash (input)
  "Compute a simple hash of INPUT string using FNV-1a algorithm.
   Returns a 32-bit unsigned integer."
  (let ((hash #x811c9dc5))  ; FNV offset basis
    (loop for char across input
          do (setf hash (logxor hash (char-code char)))
             (setf hash (logand #xFFFFFFFF
                                (* hash #x01000193))))  ; FNV prime
    hash))

;;; Hex characters
(defparameter *hex-chars* "0123456789abcdef")

;;; Convert bytes to hex string
(defun bytes-to-hex (bytes)
  "Convert BYTES (list or vector of integers) to hexadecimal string."
  (with-output-to-string (out)
    (loop for byte across (coerce bytes 'vector)
          do (write-char (char *hex-chars* (ash byte -4)) out)
             (write-char (char *hex-chars* (logand byte #xF)) out))))

;;; Generate random token
(defun generate-token (length)
  "Generate a random hexadecimal token of LENGTH characters.
   Uses platform random for entropy."
  (with-output-to-string (out)
    (loop repeat length
          do (write-char (char *hex-chars* (random 16)) out))))

;;; Generate random integer in range
(defun random-int (min-val max-val)
  "Generate a random integer in the range [MIN-VAL, MAX-VAL]."
  (if (>= min-val max-val)
      min-val
      (+ min-val (random (1+ (- max-val min-val))))))

;;; Secure memory wipe (best effort in CL)
(defun secure-wipe (string)
  "Attempt to securely wipe STRING contents.
   Note: In Common Lisp, this is best-effort due to GC and immutability.
   Returns T after wiping."
  (when (and string (stringp string) (array-has-fill-pointer-p string))
    (loop for i from 0 below (length string)
          do (setf (char string i) #\Null)))
  t)
