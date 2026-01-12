;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafePhone - Phone number validation for Common Lisp

(in-package #:proven)

;;; Country Code Type
(deftype country-code ()
  "ISO 3166-1 alpha-2 country code (2-letter uppercase string)."
  '(satisfies country-code-valid-p))

(defun country-code-valid-p (obj)
  "Return T if OBJ is a valid country code format."
  (and (stringp obj)
       (= (length obj) 2)
       (every #'upper-case-p obj)))

;;; Country Phone Info Class
(defclass country-phone-info ()
  ((country-code
    :initarg :country-code
    :accessor cpi-country-code
    :type string
    :documentation "ISO 3166-1 alpha-2 country code.")
   (calling-code
    :initarg :calling-code
    :accessor cpi-calling-code
    :type string
    :documentation "International calling code (e.g., +1, +44).")
   (name
    :initarg :name
    :accessor cpi-name
    :type string
    :initform ""
    :documentation "Country name.")
   (min-length
    :initarg :min-length
    :accessor cpi-min-length
    :type (integer 1 20)
    :initform 7
    :documentation "Minimum national number length.")
   (max-length
    :initarg :max-length
    :accessor cpi-max-length
    :type (integer 1 20)
    :initform 15
    :documentation "Maximum national number length."))
  (:documentation "Phone number metadata for a country."))

;;; Phone Number Class
(defclass phone-number ()
  ((country
    :initarg :country
    :accessor phone-country
    :type country-phone-info
    :documentation "Country information for this phone number.")
   (national-number
    :initarg :national-number
    :accessor phone-national-number
    :type string
    :documentation "National number without country code.")
   (extension
    :initarg :extension
    :accessor phone-extension
    :type (or null string)
    :initform nil
    :documentation "Optional extension number.")
   (raw-input
    :initarg :raw-input
    :accessor phone-raw-input
    :type string
    :initform ""
    :documentation "Original input string."))
  (:documentation "Represents a validated phone number."))

;;; Phone Result Class
(defclass phone-result ()
  ((phone
    :initarg :phone
    :accessor phone-result-phone
    :type (or null phone-number)
    :initform nil
    :documentation "Parsed phone number if successful.")
   (error-message
    :initarg :error
    :accessor phone-result-error
    :type string
    :initform ""
    :documentation "Error message if parsing failed.")
   (ok-p
    :initarg :ok-p
    :accessor phone-result-ok-p
    :type boolean
    :initform nil
    :documentation "T if parsing succeeded."))
  (:documentation "Result of phone number parsing."))

;;; Country Phone Database
(defparameter *country-phones*
  (let ((table (make-hash-table :test 'equal)))
    (flet ((add-country (code calling name min-len max-len)
             (setf (gethash code table)
                   (make-instance 'country-phone-info
                                  :country-code code
                                  :calling-code calling
                                  :name name
                                  :min-length min-len
                                  :max-length max-len))))
      ;; North America (+1)
      (add-country "US" "+1" "United States" 10 10)
      (add-country "CA" "+1" "Canada" 10 10)
      ;; Europe
      (add-country "GB" "+44" "United Kingdom" 10 10)
      (add-country "DE" "+49" "Germany" 10 11)
      (add-country "FR" "+33" "France" 9 9)
      (add-country "IT" "+39" "Italy" 9 11)
      (add-country "ES" "+34" "Spain" 9 9)
      (add-country "NL" "+31" "Netherlands" 9 9)
      (add-country "BE" "+32" "Belgium" 8 9)
      (add-country "CH" "+41" "Switzerland" 9 9)
      (add-country "AT" "+43" "Austria" 10 13)
      (add-country "SE" "+46" "Sweden" 9 9)
      (add-country "NO" "+47" "Norway" 8 8)
      (add-country "DK" "+45" "Denmark" 8 8)
      (add-country "FI" "+358" "Finland" 9 11)
      (add-country "PL" "+48" "Poland" 9 9)
      (add-country "IE" "+353" "Ireland" 9 9)
      (add-country "PT" "+351" "Portugal" 9 9)
      ;; Asia Pacific
      (add-country "JP" "+81" "Japan" 10 10)
      (add-country "CN" "+86" "China" 11 11)
      (add-country "IN" "+91" "India" 10 10)
      (add-country "AU" "+61" "Australia" 9 9)
      (add-country "NZ" "+64" "New Zealand" 8 10)
      (add-country "SG" "+65" "Singapore" 8 8)
      (add-country "HK" "+852" "Hong Kong" 8 8)
      (add-country "KR" "+82" "South Korea" 9 10)
      (add-country "TW" "+886" "Taiwan" 9 9)
      ;; Other
      (add-country "BR" "+55" "Brazil" 10 11)
      (add-country "MX" "+52" "Mexico" 10 10)
      (add-country "RU" "+7" "Russia" 10 10)
      (add-country "ZA" "+27" "South Africa" 9 9)
      (add-country "AE" "+971" "United Arab Emirates" 9 9)
      (add-country "IL" "+972" "Israel" 9 9))
    table)
  "Hash table of country phone info by country code.")

;;; Generic Functions
(defgeneric parse-phone (input &key default-country)
  (:documentation "Parse INPUT into a phone number. Returns PHONE-RESULT."))

(defgeneric format-phone (phone &key format)
  (:documentation "Format PHONE as a string. FORMAT can be :e164, :national, or :international."))

(defgeneric phone-valid-p (phone)
  (:documentation "Return T if PHONE is valid for its country."))

(defgeneric phone-equal-p (p1 p2)
  (:documentation "Return T if P1 and P2 represent the same phone number."))

;;; Helper Functions
(defun strip-non-digits (str)
  "Remove all non-digit characters from STR, except leading +."
  (let ((has-plus (and (> (length str) 0) (char= (char str 0) #\+))))
    (let ((digits (remove-if-not #'digit-char-p str)))
      (if has-plus
          (concatenate 'string "+" digits)
          digits))))

(defun get-country-info (code)
  "Look up country phone info by country code."
  (gethash (string-upcase code) *country-phones*))

(defun find-country-by-calling-code (calling-code)
  "Find first country matching CALLING-CODE."
  (maphash (lambda (code info)
             (declare (ignore code))
             (when (string= (cpi-calling-code info) calling-code)
               (return-from find-country-by-calling-code info)))
           *country-phones*)
  nil)

(defun extract-calling-code (number)
  "Try to extract calling code from NUMBER starting with +.
   Returns (VALUES country-info remaining-number) or (VALUES NIL NIL)."
  (when (and (> (length number) 1) (char= (char number 0) #\+))
    (let ((digits (subseq number 1)))
      ;; Try 1, 2, 3, then 4 digit calling codes
      (loop for len from 1 to (min 4 (length digits))
            for code = (format nil "+~A" (subseq digits 0 len))
            for info = (find-country-by-calling-code code)
            when info
              do (return-from extract-calling-code
                   (values info (subseq digits len))))))
  (values nil nil))

;;; Method Implementations
(defmethod parse-phone ((input string) &key (default-country "US"))
  "Parse a phone number string."
  (let* ((cleaned (strip-non-digits input))
         (len (length cleaned)))
    (when (zerop len)
      (return-from parse-phone
        (make-instance 'phone-result
                       :error "Empty phone number"
                       :ok-p nil)))

    ;; Check for extension (common separators: x, ext, #)
    (let* ((ext-pos (or (search "x" (string-downcase input))
                        (search "ext" (string-downcase input))
                        (position #\# input)))
           (extension (when ext-pos
                        (strip-non-digits (subseq input ext-pos))))
           (main-input (if ext-pos (subseq input 0 ext-pos) input))
           (main-cleaned (strip-non-digits main-input)))

      ;; Try to parse with country code
      (if (and (> (length main-cleaned) 0) (char= (char main-cleaned 0) #\+))
          ;; Has international prefix
          (multiple-value-bind (country-info national)
              (extract-calling-code main-cleaned)
            (if country-info
                (let ((nat-len (length national)))
                  (if (and (>= nat-len (cpi-min-length country-info))
                           (<= nat-len (cpi-max-length country-info)))
                      (make-instance 'phone-result
                                     :phone (make-instance 'phone-number
                                                           :country country-info
                                                           :national-number national
                                                           :extension extension
                                                           :raw-input input)
                                     :ok-p t)
                      (make-instance 'phone-result
                                     :error (format nil "Invalid number length for ~A"
                                                    (cpi-name country-info))
                                     :ok-p nil)))
                (make-instance 'phone-result
                               :error "Unknown country calling code"
                               :ok-p nil)))
          ;; No international prefix - use default country
          (let ((country-info (get-country-info default-country)))
            (if country-info
                (let* ((national (if (and (> (length main-cleaned) 0)
                                          (char= (char main-cleaned 0) #\0))
                                     (subseq main-cleaned 1) ; Strip trunk prefix
                                     main-cleaned))
                       (nat-len (length national)))
                  (if (and (>= nat-len (cpi-min-length country-info))
                           (<= nat-len (cpi-max-length country-info)))
                      (make-instance 'phone-result
                                     :phone (make-instance 'phone-number
                                                           :country country-info
                                                           :national-number national
                                                           :extension extension
                                                           :raw-input input)
                                     :ok-p t)
                      (make-instance 'phone-result
                                     :error (format nil "Invalid number length for ~A"
                                                    (cpi-name country-info))
                                     :ok-p nil)))
                (make-instance 'phone-result
                               :error (format nil "Unknown country: ~A" default-country)
                               :ok-p nil)))))))

(defmethod format-phone ((phone phone-number) &key (format :e164))
  "Format phone number according to FORMAT."
  (let ((country (phone-country phone))
        (national (phone-national-number phone))
        (extension (phone-extension phone)))
    (with-output-to-string (out)
      (case format
        (:e164
         ;; +14155551234
         (write-string (cpi-calling-code country) out)
         (write-string national out))
        (:international
         ;; +1 415 555 1234
         (write-string (cpi-calling-code country) out)
         (write-char #\Space out)
         (write-string national out))
        (:national
         ;; (415) 555-1234 or 0415 555 1234 depending on country
         ;; Simplified: just the national number
         (write-string national out))
        (otherwise
         (write-string (cpi-calling-code country) out)
         (write-string national out)))
      (when extension
        (format out " x~A" extension)))))

(defmethod phone-valid-p ((phone phone-number))
  "Check if phone number length is valid for its country."
  (let* ((country (phone-country phone))
         (len (length (phone-national-number phone))))
    (and (>= len (cpi-min-length country))
         (<= len (cpi-max-length country)))))

(defmethod phone-equal-p ((p1 phone-number) (p2 phone-number))
  "Check if two phone numbers are equal."
  (and (string= (cpi-calling-code (phone-country p1))
                (cpi-calling-code (phone-country p2)))
       (string= (phone-national-number p1)
                (phone-national-number p2))))

;;; Convenience functions
(defun valid-phone-p (input &key (default-country "US"))
  "Check if INPUT is a valid phone number."
  (let ((result (parse-phone input :default-country default-country)))
    (and (phone-result-ok-p result)
         (phone-valid-p (phone-result-phone result)))))

(defun normalize-phone (input &key (default-country "US") (format :e164))
  "Parse and normalize a phone number to standard format.
   Returns formatted string or NIL if invalid."
  (let ((result (parse-phone input :default-country default-country)))
    (when (phone-result-ok-p result)
      (format-phone (phone-result-phone result) :format format))))

;;; Country info functions
(defun list-countries ()
  "Return list of all supported country codes."
  (let ((codes nil))
    (maphash (lambda (code info)
               (declare (ignore info))
               (push code codes))
             *country-phones*)
    (sort codes #'string<)))

(defun register-country (country-code calling-code name &key (min-length 7) (max-length 15))
  "Register or update a country's phone number info."
  (setf (gethash (string-upcase country-code) *country-phones*)
        (make-instance 'country-phone-info
                       :country-code (string-upcase country-code)
                       :calling-code calling-code
                       :name name
                       :min-length min-length
                       :max-length max-length)))
