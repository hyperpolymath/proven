#lang racket

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeCurrency - Currency and money handling for Racket
;;

(require racket/contract
         racket/format
         racket/match)

(provide
 ;; Struct with contract
 (contract-out
  [struct money-value ([amount exact-integer?]
                       [currency currency-code?]
                       [decimal-places exact-nonnegative-integer?])])

 ;; Currency code predicate
 currency-code?
 valid-currency-code?

 ;; Currency info lookup
 currency-decimal-places
 currency-symbol
 currency-name

 ;; Money construction
 make-money
 money-from-decimal
 money-from-string

 ;; Money formatting
 format-money
 money->decimal
 money->string

 ;; Arithmetic operations (return money-result)
 (contract-out
  [struct money-result ([value (or/c money-value? #f)]
                        [ok? boolean?]
                        [error (or/c string? #f)])])
 money-add
 money-sub
 money-mul
 money-div

 ;; Comparison
 money=?
 money<?
 money>?
 money<=?
 money>=?
 money-compare

 ;; Validation
 money-zero?
 money-positive?
 money-negative?
 money-abs
 money-negate

 ;; Conversion
 money-convert

 ;; List of all supported currency codes
 all-currency-codes)

;; ============================================================================
;; Currency Code Definitions
;; ============================================================================

;; Map of currency code -> (decimal-places symbol name)
(define currency-info-table
  (hash
   ;; Major world currencies
   'USD '(2 "$" "US Dollar")
   'EUR '(2 "\u20AC" "Euro")
   'GBP '(2 "\u00A3" "British Pound")
   'JPY '(0 "\u00A5" "Japanese Yen")
   'CHF '(2 "CHF" "Swiss Franc")
   'CAD '(2 "C$" "Canadian Dollar")
   'AUD '(2 "A$" "Australian Dollar")
   'NZD '(2 "NZ$" "New Zealand Dollar")
   'CNY '(2 "\u00A5" "Chinese Yuan")
   'HKD '(2 "HK$" "Hong Kong Dollar")
   'SGD '(2 "S$" "Singapore Dollar")
   'SEK '(2 "kr" "Swedish Krona")
   'NOK '(2 "kr" "Norwegian Krone")
   'DKK '(2 "kr" "Danish Krone")
   'MXN '(2 "$" "Mexican Peso")
   'BRL '(2 "R$" "Brazilian Real")
   'INR '(2 "\u20B9" "Indian Rupee")
   'RUB '(2 "\u20BD" "Russian Ruble")
   'ZAR '(2 "R" "South African Rand")
   'KRW '(0 "\u20A9" "South Korean Won")
   'TRY '(2 "\u20BA" "Turkish Lira")
   'PLN '(2 "z\u0142" "Polish Zloty")
   'THB '(2 "\u0E3F" "Thai Baht")
   'IDR '(2 "Rp" "Indonesian Rupiah")
   'MYR '(2 "RM" "Malaysian Ringgit")
   'PHP '(2 "\u20B1" "Philippine Peso")
   'CZK '(2 "K\u010D" "Czech Koruna")
   'ILS '(2 "\u20AA" "Israeli Shekel")
   'CLP '(0 "$" "Chilean Peso")
   'AED '(2 "AED" "UAE Dirham")
   'SAR '(2 "SAR" "Saudi Riyal")
   'TWD '(2 "NT$" "Taiwan Dollar")
   'ARS '(2 "$" "Argentine Peso")
   'COP '(2 "$" "Colombian Peso")
   'EGP '(2 "E\u00A3" "Egyptian Pound")
   'PKR '(2 "\u20A8" "Pakistani Rupee")
   'VND '(0 "\u20AB" "Vietnamese Dong")
   'NGN '(2 "\u20A6" "Nigerian Naira")
   'BDT '(2 "\u09F3" "Bangladeshi Taka")
   'UAH '(2 "\u20B4" "Ukrainian Hryvnia")
   'PEN '(2 "S/" "Peruvian Sol")
   'RON '(2 "lei" "Romanian Leu")
   'HUF '(2 "Ft" "Hungarian Forint")

   ;; Cryptocurrencies (8 decimal places standard)
   'BTC '(8 "\u20BF" "Bitcoin")
   'ETH '(18 "\u039E" "Ethereum")
   'XRP '(6 "XRP" "Ripple")
   'LTC '(8 "\u0141" "Litecoin")
   'DOGE '(8 "\u00D0" "Dogecoin")
   'USDT '(6 "USDT" "Tether")
   'USDC '(6 "USDC" "USD Coin")
   'DAI '(18 "DAI" "Dai")

   ;; Precious metals (6 decimal places)
   'XAU '(6 "XAU" "Gold (troy oz)")
   'XAG '(6 "XAG" "Silver (troy oz)")
   'XPT '(6 "XPT" "Platinum (troy oz)")
   'XPD '(6 "XPD" "Palladium (troy oz)")

   ;; Special codes
   'XXX '(0 "" "No currency")))

;; List of all currency codes
(define all-currency-codes
  (hash-keys currency-info-table))

;; ============================================================================
;; Currency Code Predicate
;; ============================================================================

(define (currency-code? symbol)
  (and (symbol? symbol)
       (hash-has-key? currency-info-table symbol)))

(define (valid-currency-code? symbol)
  (currency-code? symbol))

;; ============================================================================
;; Currency Info Lookup
;; ============================================================================

;; Get decimal places for a currency (default 2)
(define (currency-decimal-places currency-code)
  (define info (hash-ref currency-info-table currency-code #f))
  (if info (first info) 2))

;; Get symbol for a currency
(define (currency-symbol currency-code)
  (define info (hash-ref currency-info-table currency-code #f))
  (if info (second info) (symbol->string currency-code)))

;; Get full name of a currency
(define (currency-name currency-code)
  (define info (hash-ref currency-info-table currency-code #f))
  (if info (third info) "Unknown Currency"))

;; ============================================================================
;; Money Struct
;; ============================================================================

;; Money value stores amount in smallest units (e.g., cents for USD)
(struct money-value (amount currency decimal-places) #:transparent)

;; Result struct for arithmetic operations
(struct money-result (value ok? error) #:transparent)

;; ============================================================================
;; Money Construction
;; ============================================================================

;; Create money from smallest units (e.g., cents)
(define (make-money amount currency-code)
  (unless (currency-code? currency-code)
    (error 'make-money "Invalid currency code: ~a" currency-code))
  (money-value amount
               currency-code
               (currency-decimal-places currency-code)))

;; Create money from decimal amount (e.g., 10.50 -> 1050 cents)
(define (money-from-decimal decimal-amount currency-code)
  (unless (currency-code? currency-code)
    (error 'money-from-decimal "Invalid currency code: ~a" currency-code))
  (define decimal-places (currency-decimal-places currency-code))
  (define multiplier (expt 10 decimal-places))
  (define amount (inexact->exact (round (* decimal-amount multiplier))))
  (money-value amount currency-code decimal-places))

;; Parse money from string (e.g., "10.50" or "10,50")
(define (money-from-string amount-string currency-code)
  (unless (currency-code? currency-code)
    (error 'money-from-string "Invalid currency code: ~a" currency-code))
  (define normalized-string
    (regexp-replace* #rx"[, ]" amount-string ""))
  (define decimal-amount
    (string->number (regexp-replace #rx"," normalized-string ".")))
  (if decimal-amount
      (money-from-decimal decimal-amount currency-code)
      #f))

;; ============================================================================
;; Money Formatting
;; ============================================================================

;; Convert money to decimal representation
(define (money->decimal money)
  (define divisor (expt 10 (money-value-decimal-places money)))
  (/ (money-value-amount money) divisor))

;; Format money as string with currency symbol
(define (format-money money #:symbol? [show-symbol? #t] #:code? [show-code? #f])
  (define amount (money-value-amount money))
  (define currency-code (money-value-currency money))
  (define decimal-places (money-value-decimal-places money))

  ;; Calculate whole and fractional parts
  (define divisor (expt 10 decimal-places))
  (define whole-part (quotient (abs amount) divisor))
  (define fractional-part (remainder (abs amount) divisor))

  ;; Build formatted string
  (define sign (if (negative? amount) "-" ""))
  (define symbol-str (if show-symbol? (currency-symbol currency-code) ""))
  (define code-str (if show-code? (string-append " " (symbol->string currency-code)) ""))
  (define decimal-str
    (if (> decimal-places 0)
        (string-append "." (~a fractional-part #:width decimal-places #:align 'right #:pad-string "0"))
        ""))

  (string-append sign symbol-str (number->string whole-part) decimal-str code-str))

;; Convert money to string (alias)
(define (money->string money)
  (format-money money #:symbol? #f #:code? #t))

;; ============================================================================
;; Arithmetic Operations
;; ============================================================================

;; Helper: ensure same currency
(define (same-currency? money-a money-b)
  (eq? (money-value-currency money-a) (money-value-currency money-b)))

;; Add two money values
(define (money-add money-a money-b)
  (cond
    [(not (same-currency? money-a money-b))
     (money-result #f #f "Cannot add different currencies")]
    [else
     (define result-amount (+ (money-value-amount money-a)
                              (money-value-amount money-b)))
     (money-result (money-value result-amount
                                (money-value-currency money-a)
                                (money-value-decimal-places money-a))
                   #t
                   #f)]))

;; Subtract two money values
(define (money-sub money-a money-b)
  (cond
    [(not (same-currency? money-a money-b))
     (money-result #f #f "Cannot subtract different currencies")]
    [else
     (define result-amount (- (money-value-amount money-a)
                              (money-value-amount money-b)))
     (money-result (money-value result-amount
                                (money-value-currency money-a)
                                (money-value-decimal-places money-a))
                   #t
                   #f)]))

;; Multiply money by a scalar
(define (money-mul money scalar)
  (define result-amount (inexact->exact (round (* (money-value-amount money) scalar))))
  (money-result (money-value result-amount
                             (money-value-currency money)
                             (money-value-decimal-places money))
                #t
                #f))

;; Divide money by a scalar
(define (money-div money divisor)
  (cond
    [(= divisor 0)
     (money-result #f #f "Division by zero")]
    [else
     (define result-amount (inexact->exact (round (/ (money-value-amount money) divisor))))
     (money-result (money-value result-amount
                                (money-value-currency money)
                                (money-value-decimal-places money))
                   #t
                   #f)]))

;; ============================================================================
;; Comparison Functions
;; ============================================================================

;; Check equality (same currency and amount)
(define (money=? money-a money-b)
  (and (same-currency? money-a money-b)
       (= (money-value-amount money-a) (money-value-amount money-b))))

;; Less than (same currency required)
(define (money<? money-a money-b)
  (unless (same-currency? money-a money-b)
    (error 'money<? "Cannot compare different currencies"))
  (< (money-value-amount money-a) (money-value-amount money-b)))

;; Greater than
(define (money>? money-a money-b)
  (unless (same-currency? money-a money-b)
    (error 'money>? "Cannot compare different currencies"))
  (> (money-value-amount money-a) (money-value-amount money-b)))

;; Less than or equal
(define (money<=? money-a money-b)
  (unless (same-currency? money-a money-b)
    (error 'money<=? "Cannot compare different currencies"))
  (<= (money-value-amount money-a) (money-value-amount money-b)))

;; Greater than or equal
(define (money>=? money-a money-b)
  (unless (same-currency? money-a money-b)
    (error 'money>=? "Cannot compare different currencies"))
  (>= (money-value-amount money-a) (money-value-amount money-b)))

;; Compare: returns -1, 0, or 1
(define (money-compare money-a money-b)
  (unless (same-currency? money-a money-b)
    (error 'money-compare "Cannot compare different currencies"))
  (define amount-a (money-value-amount money-a))
  (define amount-b (money-value-amount money-b))
  (cond
    [(< amount-a amount-b) -1]
    [(> amount-a amount-b) 1]
    [else 0]))

;; ============================================================================
;; Validation Functions
;; ============================================================================

;; Check if money is zero
(define (money-zero? money)
  (= (money-value-amount money) 0))

;; Check if money is positive
(define (money-positive? money)
  (> (money-value-amount money) 0))

;; Check if money is negative
(define (money-negative? money)
  (< (money-value-amount money) 0))

;; Get absolute value of money
(define (money-abs money)
  (money-value (abs (money-value-amount money))
               (money-value-currency money)
               (money-value-decimal-places money)))

;; Negate money value
(define (money-negate money)
  (money-value (- (money-value-amount money))
               (money-value-currency money)
               (money-value-decimal-places money)))

;; ============================================================================
;; Currency Conversion
;; ============================================================================

;; Convert money from one currency to another using an exchange rate
;; The rate is: 1 source currency = rate target currency
(define (money-convert money target-currency exchange-rate)
  (unless (currency-code? target-currency)
    (error 'money-convert "Invalid target currency code: ~a" target-currency))

  ;; Convert to decimal, apply rate, convert to target smallest units
  (define source-decimal (money->decimal money))
  (define target-decimal (* source-decimal exchange-rate))
  (money-from-decimal target-decimal target-currency))
