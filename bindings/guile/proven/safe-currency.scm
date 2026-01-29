;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeCurrency - Safe currency operations for Guile Scheme
;;;
;;; Provides overflow-safe money arithmetic and validation.
;;; All operations use integer cents to avoid floating point errors.

(define-module (proven safe-currency)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (;; Money creation
            make-money
            money-from-cents
            money-from-major

            ;; Money accessors
            money-cents
            money-currency
            money-major
            money-minor

            ;; Arithmetic
            money-add
            money-sub
            money-mul
            money-div
            money-negate
            money-abs

            ;; Comparison
            money-equal?
            money-less?
            money-greater?
            money-zero?
            money-positive?
            money-negative?

            ;; Formatting
            money-format
            money-parse

            ;; Validation
            currency-code-valid?
            money-valid?

            ;; Currency info
            currency-decimals
            currency-symbol

            ;; Constants
            *max-money-cents*
            *min-money-cents*))

;;; Maximum/minimum safe money values in cents (i64 limits)
(define *max-money-cents* 9223372036854775807)
(define *min-money-cents* -9223372036854775808)

;;; Make a result record
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Currency decimal places
(define *currency-decimals*
  '(("USD" . 2) ("EUR" . 2) ("GBP" . 2) ("JPY" . 0) ("CHF" . 2)
    ("AUD" . 2) ("CAD" . 2) ("CNY" . 2) ("HKD" . 2) ("NZD" . 2)
    ("SGD" . 2) ("KRW" . 0) ("NOK" . 2) ("SEK" . 2) ("DKK" . 2)
    ("BTC" . 8) ("ETH" . 18) ("BHD" . 3) ("KWD" . 3) ("OMR" . 3)))

;;; Currency symbols
(define *currency-symbols*
  '(("USD" . "$") ("EUR" . "\u20AC") ("GBP" . "\u00A3") ("JPY" . "\u00A5")
    ("CHF" . "CHF") ("AUD" . "A$") ("CAD" . "C$") ("CNY" . "\u00A5")
    ("BTC" . "\u20BF") ("ETH" . "\u039E")))

;;; Get decimal places for currency
(define (currency-decimals currency-code)
  (or (assoc-ref *currency-decimals* (string-upcase currency-code))
      2))

;;; Get symbol for currency
(define (currency-symbol currency-code)
  (or (assoc-ref *currency-symbols* (string-upcase currency-code))
      currency-code))

;;; Validate ISO 4217 currency code
(define (currency-code-valid? currency-code)
  (and (string? currency-code)
       (= (string-length currency-code) 3)
       (string-every char-upper-case?
                    (string-upcase currency-code))))

;;; Create money record
(define (make-money cents currency-code)
  (if (and (integer? cents)
           (currency-code-valid? currency-code)
           (<= cents *max-money-cents*)
           (>= cents *min-money-cents*))
      `((cents . ,cents)
        (currency . ,(string-upcase currency-code)))
      #f))

;;; Create money from cents
(define (money-from-cents cents currency-code)
  (make-money cents currency-code))

;;; Create money from major units (dollars, euros, etc.)
(define (money-from-major amount currency-code)
  (let ((decimals (currency-decimals currency-code))
        (multiplier (expt 10 (currency-decimals currency-code))))
    (make-money (inexact->exact (round (* amount multiplier)))
                currency-code)))

;;; Get cents from money
(define (money-cents money)
  (and money (assoc-ref money 'cents)))

;;; Get currency from money
(define (money-currency money)
  (and money (assoc-ref money 'currency)))

;;; Get major units (dollars, euros)
(define (money-major money)
  (let ((cents (money-cents money))
        (decimals (currency-decimals (money-currency money))))
    (if cents
        (quotient cents (expt 10 decimals))
        #f)))

;;; Get minor units (cents, pence)
(define (money-minor money)
  (let ((cents (money-cents money))
        (decimals (currency-decimals (money-currency money))))
    (if cents
        (remainder (abs cents) (expt 10 decimals))
        #f)))

;;; Validate money record
(define (money-valid? money)
  (and money
       (assoc-ref money 'cents)
       (assoc-ref money 'currency)))

;;; Check currency match
(define (same-currency? m1 m2)
  (string=? (money-currency m1) (money-currency m2)))

;;; Safe money addition
(define (money-add m1 m2)
  (cond
   ((not (and (money-valid? m1) (money-valid? m2)))
    (make-result #f #f))
   ((not (same-currency? m1 m2))
    (make-result #f #f))
   (else
    (let ((c1 (money-cents m1))
          (c2 (money-cents m2)))
      ;; Check overflow
      (if (or (and (> c2 0) (> c1 (- *max-money-cents* c2)))
              (and (< c2 0) (< c1 (- *min-money-cents* c2))))
          (make-result #f #f)
          (make-result (make-money (+ c1 c2) (money-currency m1)) #t))))))

;;; Safe money subtraction
(define (money-sub m1 m2)
  (cond
   ((not (and (money-valid? m1) (money-valid? m2)))
    (make-result #f #f))
   ((not (same-currency? m1 m2))
    (make-result #f #f))
   (else
    (let ((c1 (money-cents m1))
          (c2 (money-cents m2)))
      ;; Check overflow
      (if (or (and (< c2 0) (> c1 (+ *max-money-cents* c2)))
              (and (> c2 0) (< c1 (+ *min-money-cents* c2))))
          (make-result #f #f)
          (make-result (make-money (- c1 c2) (money-currency m1)) #t))))))

;;; Safe money multiplication by scalar
(define (money-mul money scalar)
  (if (not (money-valid? money))
      (make-result #f #f)
      (let* ((cents (money-cents money))
             (result (* cents scalar)))
        (if (or (> result *max-money-cents*)
                (< result *min-money-cents*))
            (make-result #f #f)
            (make-result (make-money (inexact->exact (round result))
                                    (money-currency money))
                        #t)))))

;;; Safe money division by scalar
(define (money-div money divisor)
  (cond
   ((not (money-valid? money))
    (make-result #f #f))
   ((= divisor 0)
    (make-result #f #f))
   (else
    (let ((cents (money-cents money)))
      (make-result (make-money (quotient cents divisor)
                              (money-currency money))
                  #t)))))

;;; Negate money
(define (money-negate money)
  (if (not (money-valid? money))
      (make-result #f #f)
      (let ((cents (money-cents money)))
        (if (= cents *min-money-cents*)
            (make-result #f #f)
            (make-result (make-money (- cents) (money-currency money)) #t)))))

;;; Absolute value
(define (money-abs money)
  (if (not (money-valid? money))
      (make-result #f #f)
      (let ((cents (money-cents money)))
        (if (= cents *min-money-cents*)
            (make-result #f #f)
            (make-result (make-money (abs cents) (money-currency money)) #t)))))

;;; Money comparison
(define (money-equal? m1 m2)
  (and (money-valid? m1)
       (money-valid? m2)
       (same-currency? m1 m2)
       (= (money-cents m1) (money-cents m2))))

(define (money-less? m1 m2)
  (and (money-valid? m1)
       (money-valid? m2)
       (same-currency? m1 m2)
       (< (money-cents m1) (money-cents m2))))

(define (money-greater? m1 m2)
  (and (money-valid? m1)
       (money-valid? m2)
       (same-currency? m1 m2)
       (> (money-cents m1) (money-cents m2))))

(define (money-zero? money)
  (and (money-valid? money)
       (= (money-cents money) 0)))

(define (money-positive? money)
  (and (money-valid? money)
       (> (money-cents money) 0)))

(define (money-negative? money)
  (and (money-valid? money)
       (< (money-cents money) 0)))

;;; Format money as string
(define (money-format money)
  (if (not (money-valid? money))
      "#<invalid-money>"
      (let* ((currency (money-currency money))
             (decimals (currency-decimals currency))
             (symbol (currency-symbol currency))
             (cents (money-cents money))
             (negative? (< cents 0))
             (abs-cents (abs cents))
             (major (quotient abs-cents (expt 10 decimals)))
             (minor (remainder abs-cents (expt 10 decimals))))
        (string-append
         (if negative? "-" "")
         symbol
         (number->string major)
         (if (> decimals 0)
             (string-append "." (string-pad-right
                                (number->string minor)
                                decimals
                                #\0))
             "")))))

;;; Parse money from string (simple format: "100.50 USD")
(define (money-parse str)
  (let* ((parts (string-split str #\space))
         (amount-str (if (>= (length parts) 1) (car parts) ""))
         (currency (if (>= (length parts) 2) (cadr parts) "USD")))
    (let ((amount (string->number amount-str)))
      (if amount
          (money-from-major amount currency)
          #f))))
