;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeUUID - UUID validation and generation for Common Lisp

(in-package #:proven)

;;; UUID Version Types
(deftype uuid-version ()
  "Valid UUID version numbers."
  '(member 1 2 3 4 5))

;;; UUID Variant Types
(deftype uuid-variant ()
  "UUID variant identifiers."
  '(member :ncs :rfc4122 :microsoft :future))

;;; UUID Class
(defclass uuid ()
  ((bytes
    :initarg :bytes
    :accessor uuid-bytes
    :type (simple-array (unsigned-byte 8) (16))
    :documentation "The 16-byte UUID value.")
   (version
    :initarg :version
    :accessor uuid-version
    :type (or null uuid-version)
    :initform nil
    :documentation "UUID version (1-5) or NIL if invalid.")
   (variant
    :initarg :variant
    :accessor uuid-variant
    :type (or null uuid-variant)
    :initform nil
    :documentation "UUID variant identifier."))
  (:documentation "Represents a UUID (Universally Unique Identifier)."))

;;; UUID Result Class
(defclass uuid-result ()
  ((uuid
    :initarg :uuid
    :accessor uuid-result-uuid
    :type (or null uuid)
    :initform nil
    :documentation "The parsed UUID if successful.")
   (error-message
    :initarg :error
    :accessor uuid-result-error
    :type string
    :initform ""
    :documentation "Error message if parsing failed.")
   (ok-p
    :initarg :ok-p
    :accessor uuid-result-ok-p
    :type boolean
    :initform nil
    :documentation "T if parsing succeeded."))
  (:documentation "Result of UUID parsing operation."))

;;; Nil UUID constant
(defparameter +nil-uuid-bytes+
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)
  "Bytes representing the nil UUID (all zeros).")

;;; Generic Functions
(defgeneric parse-uuid (input)
  (:documentation "Parse INPUT into a UUID object. Returns a UUID-RESULT."))

(defgeneric format-uuid (uuid &key case)
  (:documentation "Format UUID as a string. CASE can be :lower or :upper."))

(defgeneric uuid-valid-p (uuid)
  (:documentation "Return T if UUID is valid (not nil UUID and has valid structure)."))

(defgeneric uuid-nil-p (uuid)
  (:documentation "Return T if UUID is the nil UUID (all zeros)."))

(defgeneric uuid-equal-p (uuid1 uuid2)
  (:documentation "Return T if UUID1 and UUID2 are equal."))

;;; Helper Functions
(defun hex-char-value (char)
  "Return numeric value of hex character, or NIL if invalid."
  (cond
    ((char<= #\0 char #\9) (- (char-code char) (char-code #\0)))
    ((char<= #\a char #\f) (+ 10 (- (char-code char) (char-code #\a))))
    ((char<= #\A char #\F) (+ 10 (- (char-code char) (char-code #\A))))
    (t nil)))

(defun hex-char-p (char)
  "Return T if CHAR is a valid hexadecimal character."
  (not (null (hex-char-value char))))

(defun parse-hex-byte (str pos)
  "Parse two hex characters from STR starting at POS. Returns byte value or NIL."
  (let ((high (hex-char-value (char str pos)))
        (low (hex-char-value (char str (1+ pos)))))
    (when (and high low)
      (logior (ash high 4) low))))

(defun determine-version (bytes)
  "Determine UUID version from BYTES."
  (let ((version-nibble (ash (aref bytes 6) -4)))
    (if (<= 1 version-nibble 5)
        version-nibble
        nil)))

(defun determine-variant (bytes)
  "Determine UUID variant from BYTES."
  (let ((variant-byte (aref bytes 8)))
    (cond
      ((zerop (logand variant-byte #x80)) :ncs)
      ((= (logand variant-byte #xC0) #x80) :rfc4122)
      ((= (logand variant-byte #xE0) #xC0) :microsoft)
      (t :future))))

;;; Method Implementations
(defmethod parse-uuid ((input string))
  "Parse a UUID string in standard 8-4-4-4-12 format."
  (let ((len (length input)))
    ;; Standard format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars)
    ;; Or without dashes: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx (32 chars)
    (cond
      ((= len 36)
       ;; Validate dash positions
       (if (and (char= (char input 8) #\-)
                (char= (char input 13) #\-)
                (char= (char input 18) #\-)
                (char= (char input 23) #\-))
           (let ((bytes (make-array 16 :element-type '(unsigned-byte 8)))
                 (hex-positions '(0 2 4 6 9 11 14 16 19 21 24 26 28 30 32 34)))
             (loop for hex-pos in hex-positions
                   for byte-idx from 0
                   do (let ((byte-val (parse-hex-byte input hex-pos)))
                        (if byte-val
                            (setf (aref bytes byte-idx) byte-val)
                            (return-from parse-uuid
                              (make-instance 'uuid-result
                                             :error "Invalid hex character"
                                             :ok-p nil)))))
             (let ((uuid (make-instance 'uuid
                                        :bytes bytes
                                        :version (determine-version bytes)
                                        :variant (determine-variant bytes))))
               (make-instance 'uuid-result
                              :uuid uuid
                              :ok-p t)))
           (make-instance 'uuid-result
                          :error "Invalid dash positions"
                          :ok-p nil)))
      ((= len 32)
       ;; No dashes format
       (if (every #'hex-char-p input)
           (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
             (loop for i from 0 below 32 by 2
                   for byte-idx from 0
                   do (let ((byte-val (parse-hex-byte input i)))
                        (setf (aref bytes byte-idx) byte-val)))
             (let ((uuid (make-instance 'uuid
                                        :bytes bytes
                                        :version (determine-version bytes)
                                        :variant (determine-variant bytes))))
               (make-instance 'uuid-result
                              :uuid uuid
                              :ok-p t)))
           (make-instance 'uuid-result
                          :error "Invalid hex character"
                          :ok-p nil)))
      (t
       (make-instance 'uuid-result
                      :error (format nil "Invalid UUID length: ~D" len)
                      :ok-p nil)))))

(defmethod format-uuid ((uuid uuid) &key (case :lower))
  "Format UUID as standard 8-4-4-4-12 string."
  (let ((bytes (uuid-bytes uuid))
        (hex-chars (if (eq case :upper)
                       "0123456789ABCDEF"
                       "0123456789abcdef")))
    (with-output-to-string (out)
      (flet ((write-hex (byte)
               (write-char (char hex-chars (ash byte -4)) out)
               (write-char (char hex-chars (logand byte #xF)) out)))
        ;; xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
        (loop for i from 0 below 4 do (write-hex (aref bytes i)))
        (write-char #\- out)
        (loop for i from 4 below 6 do (write-hex (aref bytes i)))
        (write-char #\- out)
        (loop for i from 6 below 8 do (write-hex (aref bytes i)))
        (write-char #\- out)
        (loop for i from 8 below 10 do (write-hex (aref bytes i)))
        (write-char #\- out)
        (loop for i from 10 below 16 do (write-hex (aref bytes i)))))))

(defmethod uuid-valid-p ((uuid uuid))
  "Return T if UUID has valid version and variant."
  (and (uuid-version uuid)
       (uuid-variant uuid)
       (not (uuid-nil-p uuid))))

(defmethod uuid-nil-p ((uuid uuid))
  "Return T if UUID is the nil UUID."
  (let ((bytes (uuid-bytes uuid)))
    (loop for i from 0 below 16
          always (zerop (aref bytes i)))))

(defmethod uuid-equal-p ((uuid1 uuid) (uuid2 uuid))
  "Return T if both UUIDs have identical bytes."
  (let ((bytes1 (uuid-bytes uuid1))
        (bytes2 (uuid-bytes uuid2)))
    (loop for i from 0 below 16
          always (= (aref bytes1 i) (aref bytes2 i)))))

;;; UUID Generation (Version 4 - Random)
(defun generate-uuid-v4 ()
  "Generate a random UUID (Version 4, RFC 4122 variant)."
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Fill with random bytes
    (loop for i from 0 below 16
          do (setf (aref bytes i) (random 256)))
    ;; Set version to 4 (random)
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0F) #x40))
    ;; Set variant to RFC 4122
    (setf (aref bytes 8) (logior (logand (aref bytes 8) #x3F) #x80))
    (make-instance 'uuid
                   :bytes bytes
                   :version 4
                   :variant :rfc4122)))

;;; Create nil UUID
(defun make-nil-uuid ()
  "Create the nil UUID (all zeros)."
  (make-instance 'uuid
                 :bytes (make-array 16 :element-type '(unsigned-byte 8)
                                       :initial-element 0)
                 :version nil
                 :variant :ncs))
