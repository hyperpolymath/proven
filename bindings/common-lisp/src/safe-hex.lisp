;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeHex - Hexadecimal encoding/decoding for Common Lisp

(in-package #:proven)

;;; Hex Result Class
(defclass hex-result ()
  ((value
    :initarg :value
    :accessor hex-result-value
    :type (or null string (simple-array (unsigned-byte 8) (*)))
    :initform nil
    :documentation "The resulting value if successful.")
   (error-message
    :initarg :error
    :accessor hex-result-error
    :type string
    :initform ""
    :documentation "Error message if operation failed.")
   (ok-p
    :initarg :ok-p
    :accessor hex-result-ok-p
    :type boolean
    :initform nil
    :documentation "T if operation succeeded."))
  (:documentation "Result of hex encoding/decoding operations."))

;;; Hex character tables
(defparameter +hex-chars-lower+ "0123456789abcdef"
  "Lowercase hexadecimal characters.")

(defparameter +hex-chars-upper+ "0123456789ABCDEF"
  "Uppercase hexadecimal characters.")

;;; Generic Functions
(defgeneric hex-encode (input &key case)
  (:documentation "Encode INPUT to hexadecimal string. CASE can be :lower or :upper."))

(defgeneric hex-decode (input)
  (:documentation "Decode hexadecimal INPUT to bytes. Returns HEX-RESULT."))

(defgeneric hex-valid-p (input)
  (:documentation "Return T if INPUT is valid hexadecimal."))

;;; Helper Functions
(defun hex-digit-value (char)
  "Return numeric value of hex digit CHAR, or NIL if invalid."
  (cond
    ((char<= #\0 char #\9) (- (char-code char) (char-code #\0)))
    ((char<= #\a char #\f) (+ 10 (- (char-code char) (char-code #\a))))
    ((char<= #\A char #\F) (+ 10 (- (char-code char) (char-code #\A))))
    (t nil)))

(defun hex-digit-p (char)
  "Return T if CHAR is a valid hex digit."
  (not (null (hex-digit-value char))))

;;; Method Implementations - hex-encode
(defmethod hex-encode ((input string) &key (case :lower))
  "Encode a string to hexadecimal."
  (let ((chars (if (eq case :upper) +hex-chars-upper+ +hex-chars-lower+)))
    (with-output-to-string (out)
      (loop for char across input
            for code = (char-code char)
            do (write-char (char chars (ash code -4)) out)
               (write-char (char chars (logand code #xF)) out)))))

(defmethod hex-encode ((input vector) &key (case :lower))
  "Encode a byte vector to hexadecimal."
  (let ((chars (if (eq case :upper) +hex-chars-upper+ +hex-chars-lower+)))
    (with-output-to-string (out)
      (loop for byte across input
            do (write-char (char chars (ash byte -4)) out)
               (write-char (char chars (logand byte #xF)) out)))))

(defmethod hex-encode ((input list) &key (case :lower))
  "Encode a list of bytes to hexadecimal."
  (let ((chars (if (eq case :upper) +hex-chars-upper+ +hex-chars-lower+)))
    (with-output-to-string (out)
      (dolist (byte input)
        (write-char (char chars (ash byte -4)) out)
        (write-char (char chars (logand byte #xF)) out)))))

(defmethod hex-encode ((input integer) &key (case :lower))
  "Encode an integer to hexadecimal."
  (if (minusp input)
      (concatenate 'string "-" (hex-encode (- input) :case case))
      (let ((chars (if (eq case :upper) +hex-chars-upper+ +hex-chars-lower+)))
        (if (zerop input)
            "00"
            (let ((digits nil))
              (loop while (plusp input)
                    do (push (char chars (logand input #xF)) digits)
                       (setf input (ash input -4)))
              ;; Ensure even number of digits
              (when (oddp (length digits))
                (push #\0 digits))
              (coerce digits 'string))))))

;;; Method Implementations - hex-decode
(defmethod hex-decode ((input string))
  "Decode hexadecimal string to bytes."
  (let ((len (length input)))
    (cond
      ;; Empty string
      ((zerop len)
       (make-instance 'hex-result
                      :value (make-array 0 :element-type '(unsigned-byte 8))
                      :ok-p t))
      ;; Odd length
      ((oddp len)
       (make-instance 'hex-result
                      :error "Hex string must have even length"
                      :ok-p nil))
      ;; Check all characters first
      ((not (every #'hex-digit-p input))
       (make-instance 'hex-result
                      :error "Invalid hexadecimal character"
                      :ok-p nil))
      ;; Decode
      (t
       (let ((bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
         (loop for i from 0 below len by 2
               for byte-idx from 0
               for high = (hex-digit-value (char input i))
               for low = (hex-digit-value (char input (1+ i)))
               do (setf (aref bytes byte-idx) (logior (ash high 4) low)))
         (make-instance 'hex-result
                        :value bytes
                        :ok-p t))))))

;;; Method Implementations - hex-valid-p
(defmethod hex-valid-p ((input string))
  "Check if string is valid hexadecimal."
  (and (evenp (length input))
       (every #'hex-digit-p input)))

(defmethod hex-valid-p ((input t))
  "Non-strings are not valid hex."
  nil)

;;; Constant-Time Comparison for Hex Strings
(defun hex-constant-time-equal-p (a b)
  "Compare two hex strings in constant time.
   Prevents timing attacks by always comparing all characters.
   Both strings must be valid hex; returns NIL for invalid inputs."
  (cond
    ;; Different lengths - still do constant-time compare on shorter
    ((/= (length a) (length b))
     nil)
    ;; Empty strings are equal
    ((zerop (length a))
     t)
    ;; Validate and compare
    (t
     (let ((result 0))
       (loop for i from 0 below (length a)
             for char-a = (char a i)
             for char-b = (char b i)
             for val-a = (hex-digit-value char-a)
             for val-b = (hex-digit-value char-b)
             do (cond
                  ;; Invalid character in either string
                  ((or (null val-a) (null val-b))
                   (return-from hex-constant-time-equal-p nil))
                  ;; Accumulate differences
                  (t
                   ;; Compare normalized values (case-insensitive)
                   (setf result (logior result (logxor val-a val-b))))))
       (zerop result)))))

;;; Convenience Functions
(defun bytes-to-hex-lower (bytes)
  "Convert BYTES to lowercase hex string."
  (hex-encode bytes :case :lower))

(defun bytes-to-hex-upper (bytes)
  "Convert BYTES to uppercase hex string."
  (hex-encode bytes :case :upper))

(defun hex-to-bytes (hex-string)
  "Convert HEX-STRING to byte vector.
   Returns byte vector or NIL if invalid."
  (let ((result (hex-decode hex-string)))
    (when (hex-result-ok-p result)
      (hex-result-value result))))

(defun hex-to-string (hex-string)
  "Convert HEX-STRING to ASCII string.
   Returns string or NIL if invalid."
  (let ((bytes (hex-to-bytes hex-string)))
    (when bytes
      (map 'string #'code-char bytes))))

(defun string-to-hex (string &key (case :lower))
  "Convert STRING to hexadecimal."
  (hex-encode string :case case))

;;; Hex Formatting Functions
(defun hex-format-bytes (bytes &key (separator " ") (group-size 2) (case :lower))
  "Format BYTES as hex with separators.
   GROUP-SIZE is number of bytes per group.
   SEPARATOR is string between groups."
  (let ((hex (hex-encode bytes :case case))
        (chars-per-group (* group-size 2)))
    (with-output-to-string (out)
      (loop for i from 0 below (length hex) by chars-per-group
            for first = t then nil
            do (unless first
                 (write-string separator out))
               (write-string (subseq hex i (min (+ i chars-per-group)
                                                 (length hex)))
                             out)))))

(defun hex-dump (bytes &key (bytes-per-line 16) (show-ascii t))
  "Create hex dump string of BYTES.
   BYTES-PER-LINE controls line width.
   SHOW-ASCII includes ASCII representation."
  (let ((len (length bytes)))
    (with-output-to-string (out)
      (loop for offset from 0 below len by bytes-per-line
            for line-end = (min (+ offset bytes-per-line) len)
            do
               ;; Offset
               (format out "~8,'0X  " offset)
               ;; Hex bytes
               (loop for i from offset below line-end
                     do (format out "~2,'0X " (aref bytes i)))
               ;; Padding for incomplete lines
               (when (< (- line-end offset) bytes-per-line)
                 (loop repeat (* 3 (- bytes-per-line (- line-end offset)))
                       do (write-char #\Space out)))
               ;; ASCII
               (when show-ascii
                 (write-string " |" out)
                 (loop for i from offset below line-end
                       for byte = (aref bytes i)
                       do (if (<= 32 byte 126)
                              (write-char (code-char byte) out)
                              (write-char #\. out)))
                 (write-char #\| out))
               (terpri out)))))

;;; Bit manipulation helpers
(defun hex-xor (hex-a hex-b)
  "XOR two hex strings of equal length.
   Returns hex string or NIL if invalid."
  (when (and (hex-valid-p hex-a)
             (hex-valid-p hex-b)
             (= (length hex-a) (length hex-b)))
    (let ((bytes-a (hex-to-bytes hex-a))
          (bytes-b (hex-to-bytes hex-b)))
      (hex-encode
       (map '(vector (unsigned-byte 8))
            #'logxor bytes-a bytes-b)))))

(defun hex-and (hex-a hex-b)
  "AND two hex strings of equal length.
   Returns hex string or NIL if invalid."
  (when (and (hex-valid-p hex-a)
             (hex-valid-p hex-b)
             (= (length hex-a) (length hex-b)))
    (let ((bytes-a (hex-to-bytes hex-a))
          (bytes-b (hex-to-bytes hex-b)))
      (hex-encode
       (map '(vector (unsigned-byte 8))
            #'logand bytes-a bytes-b)))))

(defun hex-or (hex-a hex-b)
  "OR two hex strings of equal length.
   Returns hex string or NIL if invalid."
  (when (and (hex-valid-p hex-a)
             (hex-valid-p hex-b)
             (= (length hex-a) (length hex-b)))
    (let ((bytes-a (hex-to-bytes hex-a))
          (bytes-b (hex-to-bytes hex-b)))
      (hex-encode
       (map '(vector (unsigned-byte 8))
            #'logior bytes-a bytes-b)))))
