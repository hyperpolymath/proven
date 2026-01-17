;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeJSON - Safe JSON parsing and formatting for Common Lisp

(in-package #:proven)

;;; JSON value types
(deftype json-value ()
  "Type for JSON values."
  '(or null hash-table list string number (member t :false :null)))

(deftype json-object ()
  "Type for JSON objects (represented as hash tables)."
  'hash-table)

(deftype json-array ()
  "Type for JSON arrays (represented as lists)."
  'list)

(deftype json-string ()
  "Type for JSON strings."
  'string)

(deftype json-number ()
  "Type for JSON numbers."
  'number)

(deftype json-boolean ()
  "Type for JSON booleans."
  '(member t :false))

(deftype json-null ()
  "Type for JSON null."
  '(eql :null))

;;; JSON result structure
(defstruct json-result
  "Result of JSON parsing operation."
  (value nil)
  (error nil :type (or null string))
  (ok-p nil :type boolean))

;;; JSON type checking
(defun json-null-p (json-value)
  "Check if JSON value is null."
  (or (null json-value) (eq json-value :null)))

(defun json-type (json-value)
  "Return the JSON type of a value as a keyword."
  (cond
    ((json-null-p json-value) :null)
    ((eq json-value t) :boolean)
    ((eq json-value :false) :boolean)
    ((stringp json-value) :string)
    ((numberp json-value) :number)
    ((hash-table-p json-value) :object)
    ((listp json-value) :array)
    (t :unknown)))

;;; JSON string escaping
(defun json-escape-string (input-string)
  "Escape a string for JSON output."
  (with-output-to-string (output-stream)
    (loop for char-value across input-string
          do (case char-value
               (#\" (write-string "\\\"" output-stream))
               (#\\ (write-string "\\\\" output-stream))
               (#\Backspace (write-string "\\b" output-stream))
               (#\Page (write-string "\\f" output-stream))
               (#\Newline (write-string "\\n" output-stream))
               (#\Return (write-string "\\r" output-stream))
               (#\Tab (write-string "\\t" output-stream))
               (otherwise
                (if (< (char-code char-value) 32)
                    (format output-stream "\\u~4,'0X" (char-code char-value))
                    (write-char char-value output-stream)))))))

;;; JSON formatting
(defun format-json (json-value &optional (indent-level 0) (pretty-print nil))
  "Format a Lisp value as a JSON string."
  (let ((indent-string (if pretty-print
                           (make-string (* indent-level 2) :initial-element #\Space)
                           "")))
    (cond
      ;; Null
      ((json-null-p json-value) "null")
      ;; Boolean true
      ((eq json-value t) "true")
      ;; Boolean false
      ((eq json-value :false) "false")
      ;; String
      ((stringp json-value)
       (format nil "\"~A\"" (json-escape-string json-value)))
      ;; Number
      ((integerp json-value)
       (format nil "~D" json-value))
      ((floatp json-value)
       (format nil "~F" json-value))
      ;; Object (hash table)
      ((hash-table-p json-value)
       (if (zerop (hash-table-count json-value))
           "{}"
           (with-output-to-string (output-stream)
             (write-char #\{ output-stream)
             (when pretty-print (write-char #\Newline output-stream))
             (let ((first-entry t))
               (maphash (lambda (key value)
                          (unless first-entry
                            (write-char #\, output-stream)
                            (when pretty-print (write-char #\Newline output-stream)))
                          (setf first-entry nil)
                          (when pretty-print
                            (write-string (make-string (* (1+ indent-level) 2) :initial-element #\Space) output-stream))
                          (format output-stream "\"~A\":" (json-escape-string (if (stringp key) key (format nil "~A" key))))
                          (when pretty-print (write-char #\Space output-stream))
                          (write-string (format-json value (1+ indent-level) pretty-print) output-stream))
                        json-value))
             (when pretty-print
               (write-char #\Newline output-stream)
               (write-string indent-string output-stream))
             (write-char #\} output-stream))))
      ;; Array (list)
      ((listp json-value)
       (if (null json-value)
           "[]"
           (with-output-to-string (output-stream)
             (write-char #\[ output-stream)
             (when pretty-print (write-char #\Newline output-stream))
             (loop for remaining-elements on json-value
                   for element = (car remaining-elements)
                   do (when pretty-print
                        (write-string (make-string (* (1+ indent-level) 2) :initial-element #\Space) output-stream))
                      (write-string (format-json element (1+ indent-level) pretty-print) output-stream)
                      (when (cdr remaining-elements)
                        (write-char #\, output-stream))
                      (when pretty-print (write-char #\Newline output-stream)))
             (when pretty-print
               (write-string indent-string output-stream))
             (write-char #\] output-stream))))
      ;; Unknown type
      (t "null"))))

(defun json-pretty-print (json-value)
  "Format JSON with pretty printing."
  (format-json json-value 0 t))

;;; Simple JSON parser
(defun skip-whitespace (input-string position)
  "Skip whitespace characters and return new position."
  (loop while (and (< position (length input-string))
                   (member (char input-string position) '(#\Space #\Tab #\Newline #\Return)))
        do (incf position))
  position)

(defun parse-json-string (input-string position)
  "Parse a JSON string starting at position. Returns (value . new-position)."
  (when (and (< position (length input-string))
             (char= (char input-string position) #\"))
    (incf position)
    (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop while (< position (length input-string))
            for current-char = (char input-string position)
            do (cond
                 ((char= current-char #\")
                  (return-from parse-json-string (cons result (1+ position))))
                 ((char= current-char #\\)
                  (incf position)
                  (when (< position (length input-string))
                    (let ((escaped-char (char input-string position)))
                      (case escaped-char
                        (#\" (vector-push-extend #\" result))
                        (#\\ (vector-push-extend #\\ result))
                        (#\/ (vector-push-extend #\/ result))
                        (#\b (vector-push-extend #\Backspace result))
                        (#\f (vector-push-extend #\Page result))
                        (#\n (vector-push-extend #\Newline result))
                        (#\r (vector-push-extend #\Return result))
                        (#\t (vector-push-extend #\Tab result))
                        (#\u
                         (when (< (+ position 4) (length input-string))
                           (let ((hex-string (subseq input-string (1+ position) (+ position 5))))
                             (handler-case
                                 (progn
                                   (vector-push-extend (code-char (parse-integer hex-string :radix 16)) result)
                                   (incf position 4))
                               (error () nil)))))
                        (otherwise (vector-push-extend escaped-char result)))
                      (incf position))))
                 (t
                  (vector-push-extend current-char result)
                  (incf position))))
      nil)))

(defun parse-json-number (input-string position)
  "Parse a JSON number starting at position. Returns (value . new-position)."
  (let ((start-position position)
        (has-decimal nil)
        (has-exponent nil))
    ;; Handle negative
    (when (and (< position (length input-string))
               (char= (char input-string position) #\-))
      (incf position))
    ;; Parse digits
    (loop while (and (< position (length input-string))
                     (or (digit-char-p (char input-string position))
                         (and (char= (char input-string position) #\.)
                              (not has-decimal)
                              (setf has-decimal t))
                         (and (member (char input-string position) '(#\e #\E))
                              (not has-exponent)
                              (setf has-exponent t))
                         (and has-exponent
                              (member (char input-string position) '(#\+ #\-)))))
          do (incf position))
    (when (> position start-position)
      (let ((number-string (subseq input-string start-position position)))
        (handler-case
            (cons (if has-decimal
                      (read-from-string number-string)
                      (parse-integer number-string))
                  position)
          (error () nil))))))

(defun parse-json-value (input-string position)
  "Parse a JSON value starting at position. Returns (value . new-position) or NIL."
  (setf position (skip-whitespace input-string position))
  (when (>= position (length input-string))
    (return-from parse-json-value nil))
  (let ((current-char (char input-string position)))
    (cond
      ;; String
      ((char= current-char #\")
       (parse-json-string input-string position))
      ;; Object
      ((char= current-char #\{)
       (parse-json-object input-string position))
      ;; Array
      ((char= current-char #\[)
       (parse-json-array input-string position))
      ;; Number
      ((or (digit-char-p current-char) (char= current-char #\-))
       (parse-json-number input-string position))
      ;; Literals
      ((and (<= (+ position 4) (length input-string))
            (string= (subseq input-string position (+ position 4)) "true"))
       (cons t (+ position 4)))
      ((and (<= (+ position 5) (length input-string))
            (string= (subseq input-string position (+ position 5)) "false"))
       (cons :false (+ position 5)))
      ((and (<= (+ position 4) (length input-string))
            (string= (subseq input-string position (+ position 4)) "null"))
       (cons :null (+ position 4)))
      (t nil))))

(defun parse-json-object (input-string position)
  "Parse a JSON object starting at position. Returns (value . new-position)."
  (when (and (< position (length input-string))
             (char= (char input-string position) #\{))
    (incf position)
    (let ((result (make-hash-table :test 'equal)))
      (setf position (skip-whitespace input-string position))
      (when (and (< position (length input-string))
                 (char= (char input-string position) #\}))
        (return-from parse-json-object (cons result (1+ position))))
      (loop
        (setf position (skip-whitespace input-string position))
        ;; Parse key
        (let ((key-result (parse-json-string input-string position)))
          (unless key-result (return nil))
          (setf position (cdr key-result))
          (setf position (skip-whitespace input-string position))
          ;; Expect colon
          (unless (and (< position (length input-string))
                       (char= (char input-string position) #\:))
            (return nil))
          (incf position)
          ;; Parse value
          (let ((value-result (parse-json-value input-string position)))
            (unless value-result (return nil))
            (setf (gethash (car key-result) result) (car value-result))
            (setf position (cdr value-result))
            (setf position (skip-whitespace input-string position))
            ;; Check for comma or end
            (when (>= position (length input-string)) (return nil))
            (cond
              ((char= (char input-string position) #\})
               (return-from parse-json-object (cons result (1+ position))))
              ((char= (char input-string position) #\,)
               (incf position))
              (t (return nil)))))))))

(defun parse-json-array (input-string position)
  "Parse a JSON array starting at position. Returns (value . new-position)."
  (when (and (< position (length input-string))
             (char= (char input-string position) #\[))
    (incf position)
    (let ((result '()))
      (setf position (skip-whitespace input-string position))
      (when (and (< position (length input-string))
                 (char= (char input-string position) #\]))
        (return-from parse-json-array (cons (nreverse result) (1+ position))))
      (loop
        (let ((value-result (parse-json-value input-string position)))
          (unless value-result (return nil))
          (push (car value-result) result)
          (setf position (cdr value-result))
          (setf position (skip-whitespace input-string position))
          ;; Check for comma or end
          (when (>= position (length input-string)) (return nil))
          (cond
            ((char= (char input-string position) #\])
             (return-from parse-json-array (cons (nreverse result) (1+ position))))
            ((char= (char input-string position) #\,)
             (incf position))
            (t (return nil))))))))

(defun parse-json (input-string)
  "Parse a JSON string into Lisp values.
   Returns a JSON-RESULT with VALUE and OK-P."
  (handler-case
      (let ((result (parse-json-value input-string 0)))
        (if result
            (make-json-result :value (car result) :ok-p t)
            (make-json-result :error "Invalid JSON" :ok-p nil)))
    (error (condition-object)
      (make-json-result :error (format nil "Parse error: ~A" condition-object) :ok-p nil))))

;;; JSON path access
(defun json-get (json-value key)
  "Get a value from a JSON object by key."
  (when (hash-table-p json-value)
    (gethash key json-value)))

(defun json-get-path (json-value path)
  "Get a value from nested JSON using a list of keys/indices."
  (reduce (lambda (current-value key)
            (cond
              ((and (hash-table-p current-value) (stringp key))
               (gethash key current-value))
              ((and (listp current-value) (integerp key) (>= key 0) (< key (length current-value)))
               (nth key current-value))
              (t nil)))
          path
          :initial-value json-value))
