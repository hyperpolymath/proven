;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeCurrency - Currency and money handling for Common Lisp

(in-package #:proven)

;;; Currency Code Type (ISO 4217)
(deftype currency-code ()
  "ISO 4217 currency code (3-letter uppercase string)."
  '(satisfies currency-code-p))

(defun currency-code-p (obj)
  "Return T if OBJ is a valid currency code format."
  (and (stringp obj)
       (= (length obj) 3)
       (every #'upper-case-p obj)))

;;; Currency Information Class
(defclass currency ()
  ((code
    :initarg :code
    :accessor currency-code
    :type string
    :documentation "ISO 4217 currency code (e.g., USD, EUR).")
   (minor-units
    :initarg :minor-units
    :accessor currency-minor-units
    :type (integer 0 4)
    :initform 2
    :documentation "Number of decimal places (0-4).")
   (name
    :initarg :name
    :accessor currency-name
    :type string
    :initform ""
    :documentation "Human-readable currency name.")
   (symbol
    :initarg :symbol
    :accessor currency-symbol
    :type string
    :initform ""
    :documentation "Currency symbol (e.g., $, EUR)."))
  (:documentation "Represents a currency with its properties."))

;;; Money Class
(defclass money ()
  ((amount
    :initarg :amount
    :accessor money-amount
    :type integer
    :documentation "Amount in minor units (cents, pence, etc.).")
   (currency
    :initarg :currency
    :accessor money-currency
    :type currency
    :documentation "The currency of this money value."))
  (:documentation "Represents a monetary amount with its currency."))

;;; Money Result Class
(defclass money-result ()
  ((money
    :initarg :money
    :accessor money-result-value
    :type (or null money)
    :initform nil
    :documentation "The resulting money value if successful.")
   (error-message
    :initarg :error
    :accessor money-result-error
    :type string
    :initform ""
    :documentation "Error message if operation failed.")
   (ok-p
    :initarg :ok-p
    :accessor money-result-ok-p
    :type boolean
    :initform nil
    :documentation "T if operation succeeded."))
  (:documentation "Result of money operations."))

;;; Known Currencies Database
(defparameter *currencies*
  (let ((table (make-hash-table :test 'equal)))
    (flet ((add-currency (code minor-units name symbol)
             (setf (gethash code table)
                   (make-instance 'currency
                                  :code code
                                  :minor-units minor-units
                                  :name name
                                  :symbol symbol))))
      ;; Major currencies
      (add-currency "USD" 2 "US Dollar" "$")
      (add-currency "EUR" 2 "Euro" "EUR")
      (add-currency "GBP" 2 "British Pound" "GBP")
      (add-currency "JPY" 0 "Japanese Yen" "JPY")
      (add-currency "CHF" 2 "Swiss Franc" "CHF")
      (add-currency "CAD" 2 "Canadian Dollar" "CA$")
      (add-currency "AUD" 2 "Australian Dollar" "A$")
      (add-currency "NZD" 2 "New Zealand Dollar" "NZ$")
      (add-currency "CNY" 2 "Chinese Yuan" "CNY")
      (add-currency "INR" 2 "Indian Rupee" "INR")
      (add-currency "BRL" 2 "Brazilian Real" "R$")
      (add-currency "RUB" 2 "Russian Ruble" "RUB")
      (add-currency "KRW" 0 "South Korean Won" "KRW")
      (add-currency "MXN" 2 "Mexican Peso" "MX$")
      (add-currency "SGD" 2 "Singapore Dollar" "S$")
      (add-currency "HKD" 2 "Hong Kong Dollar" "HK$")
      (add-currency "SEK" 2 "Swedish Krona" "kr")
      (add-currency "NOK" 2 "Norwegian Krone" "kr")
      (add-currency "DKK" 2 "Danish Krone" "kr")
      (add-currency "PLN" 2 "Polish Zloty" "zl")
      ;; Cryptocurrencies (higher precision)
      (add-currency "BTC" 8 "Bitcoin" "BTC")
      (add-currency "ETH" 18 "Ethereum" "ETH")
      ;; Zero decimal currencies
      (add-currency "KWD" 3 "Kuwaiti Dinar" "KWD")
      (add-currency "BHD" 3 "Bahraini Dinar" "BHD")
      (add-currency "OMR" 3 "Omani Rial" "OMR"))
    table)
  "Hash table of known currencies by code.")

;;; Generic Functions
(defgeneric money-add (m1 m2)
  (:documentation "Add two money values. Must have same currency."))

(defgeneric money-subtract (m1 m2)
  (:documentation "Subtract M2 from M1. Must have same currency."))

(defgeneric money-multiply (money factor)
  (:documentation "Multiply MONEY by FACTOR (integer or ratio)."))

(defgeneric money-divide (money divisor)
  (:documentation "Divide MONEY by DIVISOR."))

(defgeneric money-negate (money)
  (:documentation "Return negation of MONEY."))

(defgeneric money-abs (money)
  (:documentation "Return absolute value of MONEY."))

(defgeneric money-compare (m1 m2)
  (:documentation "Compare two money values. Returns -1, 0, or 1."))

(defgeneric money-zero-p (money)
  (:documentation "Return T if MONEY amount is zero."))

(defgeneric money-positive-p (money)
  (:documentation "Return T if MONEY amount is positive."))

(defgeneric money-negative-p (money)
  (:documentation "Return T if MONEY amount is negative."))

(defgeneric format-money (money &key show-symbol show-code)
  (:documentation "Format MONEY as a human-readable string."))

(defgeneric parse-money (amount currency-code)
  (:documentation "Parse AMOUNT into money with given CURRENCY-CODE."))

;;; Helper Functions
(defun get-currency (code)
  "Look up currency by CODE. Returns NIL if not found."
  (gethash (string-upcase code) *currencies*))

(defun minor-unit-factor (currency)
  "Return the factor to convert major units to minor units."
  (expt 10 (currency-minor-units currency)))

(defun same-currency-p (m1 m2)
  "Return T if M1 and M2 have the same currency."
  (string= (currency-code (money-currency m1))
           (currency-code (money-currency m2))))

;;; Method Implementations
(defmethod money-add ((m1 money) (m2 money))
  "Add two money values."
  (if (same-currency-p m1 m2)
      (make-instance 'money-result
                     :money (make-instance 'money
                                           :amount (+ (money-amount m1)
                                                      (money-amount m2))
                                           :currency (money-currency m1))
                     :ok-p t)
      (make-instance 'money-result
                     :error "Currency mismatch"
                     :ok-p nil)))

(defmethod money-subtract ((m1 money) (m2 money))
  "Subtract M2 from M1."
  (if (same-currency-p m1 m2)
      (make-instance 'money-result
                     :money (make-instance 'money
                                           :amount (- (money-amount m1)
                                                      (money-amount m2))
                                           :currency (money-currency m1))
                     :ok-p t)
      (make-instance 'money-result
                     :error "Currency mismatch"
                     :ok-p nil)))

(defmethod money-multiply ((money money) (factor integer))
  "Multiply money by integer factor."
  (make-instance 'money-result
                 :money (make-instance 'money
                                       :amount (* (money-amount money) factor)
                                       :currency (money-currency money))
                 :ok-p t))

(defmethod money-multiply ((money money) (factor ratio))
  "Multiply money by ratio, rounding to nearest minor unit."
  (make-instance 'money-result
                 :money (make-instance 'money
                                       :amount (round (* (money-amount money) factor))
                                       :currency (money-currency money))
                 :ok-p t))

(defmethod money-divide ((money money) (divisor integer))
  "Divide money by integer divisor."
  (if (zerop divisor)
      (make-instance 'money-result
                     :error "Division by zero"
                     :ok-p nil)
      (make-instance 'money-result
                     :money (make-instance 'money
                                           :amount (round (money-amount money) divisor)
                                           :currency (money-currency money))
                     :ok-p t)))

(defmethod money-negate ((money money))
  "Return negation of money."
  (make-instance 'money
                 :amount (- (money-amount money))
                 :currency (money-currency money)))

(defmethod money-abs ((money money))
  "Return absolute value of money."
  (make-instance 'money
                 :amount (abs (money-amount money))
                 :currency (money-currency money)))

(defmethod money-compare ((m1 money) (m2 money))
  "Compare two money values. Returns -1, 0, or 1."
  (if (same-currency-p m1 m2)
      (let ((diff (- (money-amount m1) (money-amount m2))))
        (cond
          ((minusp diff) -1)
          ((plusp diff) 1)
          (t 0)))
      (error "Cannot compare money with different currencies")))

(defmethod money-zero-p ((money money))
  "Return T if amount is zero."
  (zerop (money-amount money)))

(defmethod money-positive-p ((money money))
  "Return T if amount is positive."
  (plusp (money-amount money)))

(defmethod money-negative-p ((money money))
  "Return T if amount is negative."
  (minusp (money-amount money)))

(defmethod format-money ((money money) &key (show-symbol t) (show-code nil))
  "Format money as human-readable string."
  (let* ((currency (money-currency money))
         (minor-units (currency-minor-units currency))
         (factor (minor-unit-factor currency))
         (amount (money-amount money))
         (major (truncate amount factor))
         (minor (abs (mod amount factor))))
    (with-output-to-string (out)
      (when show-symbol
        (write-string (currency-symbol currency) out))
      (if (zerop minor-units)
          (format out "~D" major)
          (format out "~D.~V,'0D" major minor-units minor))
      (when show-code
        (format out " ~A" (currency-code currency))))))

(defmethod parse-money ((amount number) (currency-code string))
  "Parse a numeric amount into money."
  (let ((currency (get-currency currency-code)))
    (if currency
        (let* ((factor (minor-unit-factor currency))
               (minor-amount (round (* amount factor))))
          (make-instance 'money-result
                         :money (make-instance 'money
                                               :amount minor-amount
                                               :currency currency)
                         :ok-p t))
        (make-instance 'money-result
                       :error (format nil "Unknown currency: ~A" currency-code)
                       :ok-p nil))))

(defmethod parse-money ((amount string) (currency-code string))
  "Parse a string amount into money."
  (let ((num (ignore-errors (read-from-string amount))))
    (if (numberp num)
        (parse-money num currency-code)
        (make-instance 'money-result
                       :error "Invalid amount format"
                       :ok-p nil))))

;;; Convenience constructors
(defun make-money (amount currency-code)
  "Create money from major units (e.g., dollars, not cents).
   Returns MONEY-RESULT."
  (parse-money amount currency-code))

(defun make-money-minor (amount currency-code)
  "Create money from minor units (e.g., cents).
   Returns MONEY-RESULT."
  (let ((currency (get-currency currency-code)))
    (if currency
        (make-instance 'money-result
                       :money (make-instance 'money
                                             :amount amount
                                             :currency currency)
                       :ok-p t)
        (make-instance 'money-result
                       :error (format nil "Unknown currency: ~A" currency-code)
                       :ok-p nil))))

;;; Currency registration
(defun register-currency (code minor-units &key (name "") (symbol ""))
  "Register a new currency or update existing one."
  (let ((code-upper (string-upcase code)))
    (setf (gethash code-upper *currencies*)
          (make-instance 'currency
                         :code code-upper
                         :minor-units minor-units
                         :name name
                         :symbol (if (string= symbol "") code-upper symbol)))))

;;; List all currencies
(defun list-currencies ()
  "Return list of all registered currency codes."
  (let ((codes nil))
    (maphash (lambda (code currency)
               (declare (ignore currency))
               (push code codes))
             *currencies*)
    (sort codes #'string<)))
