;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-currency
  "Safe currency operations with type-safe monetary values."
  (:require [clojure.string :as str]))

;; ISO 4217 currency codes with metadata
(def currency-codes
  "Set of supported ISO 4217 currency codes."
  #{:USD :EUR :GBP :JPY :CHF :CAD :AUD :NZD :CNY :INR
    :BRL :MXN :KRW :SGD :HKD :SEK :NOK :DKK :PLN :RUB
    :ZAR :TRY :THB :MYR :IDR :PHP :VND :AED :SAR :ILS
    :CZK :HUF :RON :BGN :HRK :ISK :CLP :COP :PEN :ARS
    :BTC :ETH})

(def currency-metadata
  "Metadata for currency codes including decimals, symbol, and name."
  {:USD {:decimals 2 :symbol "$" :name "US Dollar"}
   :EUR {:decimals 2 :symbol "€" :name "Euro"}
   :GBP {:decimals 2 :symbol "£" :name "British Pound"}
   :JPY {:decimals 0 :symbol "¥" :name "Japanese Yen"}
   :CHF {:decimals 2 :symbol "Fr" :name "Swiss Franc"}
   :CAD {:decimals 2 :symbol "C$" :name "Canadian Dollar"}
   :AUD {:decimals 2 :symbol "A$" :name "Australian Dollar"}
   :NZD {:decimals 2 :symbol "NZ$" :name "New Zealand Dollar"}
   :CNY {:decimals 2 :symbol "¥" :name "Chinese Yuan"}
   :INR {:decimals 2 :symbol "₹" :name "Indian Rupee"}
   :BRL {:decimals 2 :symbol "R$" :name "Brazilian Real"}
   :MXN {:decimals 2 :symbol "Mex$" :name "Mexican Peso"}
   :KRW {:decimals 0 :symbol "₩" :name "South Korean Won"}
   :SGD {:decimals 2 :symbol "S$" :name "Singapore Dollar"}
   :HKD {:decimals 2 :symbol "HK$" :name "Hong Kong Dollar"}
   :SEK {:decimals 2 :symbol "kr" :name "Swedish Krona"}
   :NOK {:decimals 2 :symbol "kr" :name "Norwegian Krone"}
   :DKK {:decimals 2 :symbol "kr" :name "Danish Krone"}
   :PLN {:decimals 2 :symbol "zł" :name "Polish Zloty"}
   :RUB {:decimals 2 :symbol "₽" :name "Russian Ruble"}
   :ZAR {:decimals 2 :symbol "R" :name "South African Rand"}
   :TRY {:decimals 2 :symbol "₺" :name "Turkish Lira"}
   :THB {:decimals 2 :symbol "฿" :name "Thai Baht"}
   :MYR {:decimals 2 :symbol "RM" :name "Malaysian Ringgit"}
   :IDR {:decimals 2 :symbol "Rp" :name "Indonesian Rupiah"}
   :PHP {:decimals 2 :symbol "₱" :name "Philippine Peso"}
   :VND {:decimals 0 :symbol "₫" :name "Vietnamese Dong"}
   :AED {:decimals 2 :symbol "د.إ" :name "UAE Dirham"}
   :SAR {:decimals 2 :symbol "﷼" :name "Saudi Riyal"}
   :ILS {:decimals 2 :symbol "₪" :name "Israeli Shekel"}
   :CZK {:decimals 2 :symbol "Kč" :name "Czech Koruna"}
   :HUF {:decimals 2 :symbol "Ft" :name "Hungarian Forint"}
   :RON {:decimals 2 :symbol "lei" :name "Romanian Leu"}
   :BGN {:decimals 2 :symbol "лв" :name "Bulgarian Lev"}
   :HRK {:decimals 2 :symbol "kn" :name "Croatian Kuna"}
   :ISK {:decimals 0 :symbol "kr" :name "Icelandic Krona"}
   :CLP {:decimals 0 :symbol "$" :name "Chilean Peso"}
   :COP {:decimals 2 :symbol "$" :name "Colombian Peso"}
   :PEN {:decimals 2 :symbol "S/" :name "Peruvian Sol"}
   :ARS {:decimals 2 :symbol "$" :name "Argentine Peso"}
   :BTC {:decimals 8 :symbol "₿" :name "Bitcoin"}
   :ETH {:decimals 8 :symbol "Ξ" :name "Ethereum"}})

(defrecord Money [minor-units currency])

(defn get-decimals
  "Get the number of decimal places for a currency."
  [currency-code]
  (get-in currency-metadata [currency-code :decimals] 2))

(defn get-symbol
  "Get the symbol for a currency."
  [currency-code]
  (get-in currency-metadata [currency-code :symbol] ""))

(defn get-currency-name
  "Get the full name of a currency."
  [currency-code]
  (get-in currency-metadata [currency-code :name] "Unknown Currency"))

(defn valid-code?
  "Check if a currency code is valid."
  [currency-code]
  (contains? currency-codes currency-code))

(defn parse-code
  "Parse a currency code from string.
   Returns {:ok keyword} or {:error message}."
  [code-string]
  (if (str/blank? code-string)
    {:error "empty_currency_code"}
    (let [currency-keyword (keyword (str/upper-case (str/trim code-string)))]
      (if (valid-code? currency-keyword)
        {:ok currency-keyword}
        {:error "unknown_currency_code"}))))

(defn parse-code!
  "Parse a currency code from string. Throws ex-info on error."
  [code-string]
  (let [result (parse-code code-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid currency code: " (:error result))
                      {:type :invalid-currency-code
                       :error (:error result)
                       :input code-string})))))

(defn- multiplier
  "Get the multiplier for converting major to minor units."
  [currency-code]
  (long (Math/pow 10 (get-decimals currency-code))))

(defn from-major
  "Create Money from major units (e.g., dollars, euros)."
  [amount currency-code]
  (when (valid-code? currency-code)
    (->Money (* (long amount) (multiplier currency-code)) currency-code)))

(defn from-minor
  "Create Money from minor units (e.g., cents, satoshis)."
  [amount currency-code]
  (when (valid-code? currency-code)
    (->Money (long amount) currency-code)))

(defn zero
  "Create zero Money for a currency."
  [currency-code]
  (from-minor 0 currency-code))

(defn get-currency
  "Get the currency code of a Money value."
  [money]
  (:currency money))

(defn get-minor
  "Get the amount in minor units."
  [money]
  (:minor-units money))

(defn get-major
  "Get the amount in major units (truncated)."
  [money]
  (quot (:minor-units money) (multiplier (:currency money))))

(defn get-major-decimal
  "Get the amount in major units as a BigDecimal."
  [money]
  (let [decimal-places (get-decimals (:currency money))]
    (bigdec (/ (bigdec (:minor-units money))
               (bigdec (multiplier (:currency money)))))))

(defn add
  "Add two Money values. Returns {:ok Money} or {:error message}."
  [money-a money-b]
  (if (not= (:currency money-a) (:currency money-b))
    {:error "currency_mismatch"}
    {:ok (->Money (+ (:minor-units money-a) (:minor-units money-b))
                  (:currency money-a))}))

(defn add!
  "Add two Money values. Throws ex-info on currency mismatch."
  [money-a money-b]
  (let [result (add money-a money-b)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info "Currency mismatch"
                      {:type :currency-mismatch
                       :currency-a (:currency money-a)
                       :currency-b (:currency money-b)})))))

(defn sub
  "Subtract two Money values. Returns {:ok Money} or {:error message}."
  [money-a money-b]
  (if (not= (:currency money-a) (:currency money-b))
    {:error "currency_mismatch"}
    {:ok (->Money (- (:minor-units money-a) (:minor-units money-b))
                  (:currency money-a))}))

(defn sub!
  "Subtract two Money values. Throws ex-info on currency mismatch."
  [money-a money-b]
  (let [result (sub money-a money-b)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info "Currency mismatch"
                      {:type :currency-mismatch
                       :currency-a (:currency money-a)
                       :currency-b (:currency money-b)})))))

(defn mul
  "Multiply Money by a scalar."
  [money scalar]
  (->Money (* (:minor-units money) (long scalar))
           (:currency money)))

(defn div
  "Divide Money by a scalar. Returns {:ok Money} or {:error message}."
  [money scalar]
  (if (zero? scalar)
    {:error "division_by_zero"}
    {:ok (->Money (quot (:minor-units money) (long scalar))
                  (:currency money))}))

(defn div!
  "Divide Money by a scalar. Throws ex-info on division by zero."
  [money scalar]
  (let [result (div money scalar)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info "Division by zero"
                      {:type :division-by-zero})))))

(defn zero?-money
  "Check if Money is zero."
  [money]
  (zero? (:minor-units money)))

(defn positive?
  "Check if Money is positive."
  [money]
  (pos? (:minor-units money)))

(defn negative?
  "Check if Money is negative."
  [money]
  (neg? (:minor-units money)))

(defn abs
  "Get absolute value of Money."
  [money]
  (->Money (Math/abs ^long (:minor-units money))
           (:currency money)))

(defn negate
  "Negate a Money value."
  [money]
  (->Money (- (:minor-units money))
           (:currency money)))

(defn compare-money
  "Compare two Money values. Returns -1, 0, or 1.
   Returns {:ok result} or {:error message}."
  [money-a money-b]
  (if (not= (:currency money-a) (:currency money-b))
    {:error "currency_mismatch"}
    {:ok (compare (:minor-units money-a) (:minor-units money-b))}))

(defn format-money
  "Format Money for display."
  [money]
  (let [currency-code (:currency money)
        decimal-places (get-decimals currency-code)
        divisor (multiplier currency-code)
        abs-units (Math/abs ^long (:minor-units money))
        major-part (quot abs-units divisor)
        minor-part (rem abs-units divisor)
        sign-prefix (if (neg? (:minor-units money)) "-" "")
        currency-symbol (get-symbol currency-code)]
    (if (zero? decimal-places)
      (str sign-prefix currency-symbol major-part)
      (str sign-prefix currency-symbol major-part "."
           (format (str "%0" decimal-places "d") minor-part)))))

(defn format-iso
  "Format Money in ISO format (amount currency-code)."
  [money]
  (let [decimal-places (get-decimals (:currency money))
        divisor (multiplier (:currency money))
        amount (/ (double (:minor-units money)) (double divisor))]
    (str (format (str "%." decimal-places "f") amount) " " (name (:currency money)))))

(defn allocate
  "Allocate Money according to ratios. Returns vector of Money values."
  [money ratios]
  (let [total-ratio (reduce + ratios)
        base-amount (quot (:minor-units money) total-ratio)
        remainder (rem (:minor-units money) total-ratio)
        allocations (mapv #(* base-amount %) ratios)]
    ;; Distribute remainder to first n allocations
    (mapv (fn [allocation-amount idx]
            (->Money (+ allocation-amount (if (< idx remainder) 1 0))
                     (:currency money)))
          allocations
          (range))))

(defn split
  "Split Money equally into n parts. Returns vector of Money values."
  [money num-parts]
  (when (pos? num-parts)
    (let [base-amount (quot (:minor-units money) num-parts)
          remainder (rem (:minor-units money) num-parts)]
      (mapv (fn [idx]
              (->Money (+ base-amount (if (< idx remainder) 1 0))
                       (:currency money)))
            (range num-parts)))))
