;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-phone
  "Safe phone number validation following E.164 standard."
  (:require [clojure.string :as str]))

;; Country calling codes (ITU-T E.164)
(def country-codes
  "Map of country code values to country identifiers."
  {1   :US    ; USA, Canada, Caribbean
   7   :RU    ; Russia, Kazakhstan
   20  :EG    ; Egypt
   27  :ZA    ; South Africa
   30  :GR    ; Greece
   31  :NL    ; Netherlands
   32  :BE    ; Belgium
   33  :FR    ; France
   34  :ES    ; Spain
   36  :HU    ; Hungary
   39  :IT    ; Italy
   40  :RO    ; Romania
   41  :CH    ; Switzerland
   43  :AT    ; Austria
   44  :UK    ; United Kingdom
   45  :DK    ; Denmark
   46  :SE    ; Sweden
   47  :NO    ; Norway
   48  :PL    ; Poland
   49  :DE    ; Germany
   51  :PE    ; Peru
   52  :MX    ; Mexico
   53  :CU    ; Cuba
   54  :AR    ; Argentina
   55  :BR    ; Brazil
   56  :CL    ; Chile
   57  :CO    ; Colombia
   58  :VE    ; Venezuela
   60  :MY    ; Malaysia
   61  :AU    ; Australia
   62  :ID    ; Indonesia
   63  :PH    ; Philippines
   64  :NZ    ; New Zealand
   65  :SG    ; Singapore
   66  :TH    ; Thailand
   81  :JP    ; Japan
   82  :KR    ; South Korea
   84  :VN    ; Vietnam
   86  :CN    ; China
   90  :TR    ; Turkey
   91  :IN    ; India
   92  :PK    ; Pakistan
   93  :AF    ; Afghanistan
   94  :LK    ; Sri Lanka
   95  :MM    ; Myanmar
   98  :IR    ; Iran
   212 :MA    ; Morocco
   213 :DZ    ; Algeria
   216 :TN    ; Tunisia
   218 :LY    ; Libya
   220 :GM    ; Gambia
   221 :SN    ; Senegal
   234 :NG    ; Nigeria
   254 :KE    ; Kenya
   255 :TZ    ; Tanzania
   256 :UG    ; Uganda
   260 :ZM    ; Zambia
   263 :ZW    ; Zimbabwe
   351 :PT    ; Portugal
   352 :LU    ; Luxembourg
   353 :IE    ; Ireland
   354 :IS    ; Iceland
   355 :AL    ; Albania
   356 :MT    ; Malta
   357 :CY    ; Cyprus
   358 :FI    ; Finland
   359 :BG    ; Bulgaria
   370 :LT    ; Lithuania
   371 :LV    ; Latvia
   372 :EE    ; Estonia
   380 :UA    ; Ukraine
   381 :RS    ; Serbia
   385 :HR    ; Croatia
   386 :SI    ; Slovenia
   420 :CZ    ; Czech Republic
   421 :SK    ; Slovakia
   852 :HK    ; Hong Kong
   853 :MO    ; Macau
   855 :KH    ; Cambodia
   856 :LA    ; Laos
   880 :BD    ; Bangladesh
   886 :TW    ; Taiwan
   960 :MV    ; Maldives
   961 :LB    ; Lebanon
   962 :JO    ; Jordan
   963 :SY    ; Syria
   964 :IQ    ; Iraq
   965 :KW    ; Kuwait
   966 :SA    ; Saudi Arabia
   967 :YE    ; Yemen
   968 :OM    ; Oman
   970 :PS    ; Palestine
   971 :AE    ; UAE
   972 :IL    ; Israel
   973 :BH    ; Bahrain
   974 :QA    ; Qatar
   975 :BT    ; Bhutan
   976 :MN    ; Mongolia
   977 :NP    ; Nepal
   992 :TJ    ; Tajikistan
   993 :TM    ; Turkmenistan
   994 :AZ    ; Azerbaijan
   995 :GE    ; Georgia
   996 :KG    ; Kyrgyzstan
   998 :UZ    ; Uzbekistan})

(def country-code-lengths
  "Set of valid country code lengths for parsing."
  #{1 2 3})

(defrecord PhoneNumber [country-code national-number raw-input])

(defn- extract-digits
  "Extract only digit characters from a string."
  [phone-string]
  (apply str (filter #(Character/isDigit ^char %) phone-string)))

(defn- parse-country-code
  "Try to parse country code from digits. Returns [country-code remaining-digits] or nil."
  [digits]
  (loop [code-length 3]
    (when (>= code-length 1)
      (if (>= (count digits) code-length)
        (let [potential-code (Long/parseLong (subs digits 0 code-length))]
          (if (contains? country-codes potential-code)
            [potential-code (subs digits code-length)]
            (recur (dec code-length))))
        (recur (dec code-length))))))

(defn parse
  "Parse a phone number string.
   Returns {:ok PhoneNumber} or {:error message}."
  [phone-string]
  (cond
    (str/blank? phone-string)
    {:error "empty_phone_number"}

    :else
    (let [trimmed-input (str/trim phone-string)
          digits (extract-digits trimmed-input)]
      (cond
        (< (count digits) 7)
        {:error "phone_number_too_short"}

        (> (count digits) 15)
        {:error "phone_number_too_long"}

        :else
        (if-let [[country-code-value national-digits] (parse-country-code digits)]
          (if (< (count national-digits) 4)
            {:error "national_number_too_short"}
            {:ok (->PhoneNumber country-code-value national-digits trimmed-input)})
          {:error "unknown_country_code"})))))

(defn parse!
  "Parse a phone number string. Throws ex-info on error."
  [phone-string]
  (let [result (parse phone-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid phone number: " (:error result))
                      {:type :invalid-phone-number
                       :error (:error result)
                       :input phone-string})))))

(defn valid?
  "Check if a string is a valid phone number."
  [phone-string]
  (contains? (parse phone-string) :ok))

(defn get-country-code
  "Get the numeric country code."
  [phone-number]
  (:country-code phone-number))

(defn get-country
  "Get the country identifier keyword."
  [phone-number]
  (get country-codes (:country-code phone-number) :unknown))

(defn get-national-number
  "Get the national number (without country code)."
  [phone-number]
  (:national-number phone-number))

(defn format-e164
  "Format phone number in E.164 format (+CC...)."
  [phone-number]
  (str "+" (:country-code phone-number) (:national-number phone-number)))

(defn format-international
  "Format phone number with spaces for readability."
  [phone-number]
  (let [country-code-str (str (:country-code phone-number))
        national-number (:national-number phone-number)
        national-length (count national-number)]
    (cond
      (<= national-length 4)
      (str "+" country-code-str " " national-number)

      (<= national-length 7)
      (str "+" country-code-str " "
           (subs national-number 0 3) " "
           (subs national-number 3))

      (<= national-length 10)
      (str "+" country-code-str " "
           (subs national-number 0 3) " "
           (subs national-number 3 6) " "
           (subs national-number 6))

      :else
      (str "+" country-code-str " " national-number))))

(defn format-national
  "Format as national number (without country code)."
  [phone-number]
  (let [national-number (:national-number phone-number)
        national-length (count national-number)]
    (cond
      (<= national-length 4)
      national-number

      (<= national-length 7)
      (str (subs national-number 0 3) "-"
           (subs national-number 3))

      (<= national-length 10)
      (str "(" (subs national-number 0 3) ") "
           (subs national-number 3 6) "-"
           (subs national-number 6))

      :else
      national-number)))

(defn format-rfc3966
  "Format phone number as RFC 3966 tel: URI."
  [phone-number]
  (str "tel:+" (:country-code phone-number) (:national-number phone-number)))

(defn digit-count
  "Get total digit count (country code + national number)."
  [phone-number]
  (let [country-code-digits (count (str (:country-code phone-number)))]
    (+ country-code-digits (count (:national-number phone-number)))))

(defn from-country?
  "Check if phone number is from a specific country."
  [phone-number country-keyword]
  (= (get-country phone-number) country-keyword))

(defn from-countries?
  "Check if phone number is from one of the specified countries."
  [phone-number country-keywords]
  (contains? (set country-keywords) (get-country phone-number)))

(defn normalize
  "Normalize a phone number to E.164 format string."
  [phone-string]
  (when-let [parsed (:ok (parse phone-string))]
    (format-e164 parsed)))

(defn equivalent?
  "Check if two phone numbers are equivalent (same E.164)."
  [phone-a phone-b]
  (let [normalized-a (normalize phone-a)
        normalized-b (normalize phone-b)]
    (and normalized-a normalized-b (= normalized-a normalized-b))))

(defn obfuscate
  "Obfuscate phone number for display (e.g., +1 ***-***-4567)."
  [phone-number]
  (let [national-number (:national-number phone-number)
        national-length (count national-number)]
    (if (<= national-length 4)
      (str "+" (:country-code phone-number) " " national-number)
      (let [visible-suffix (subs national-number (- national-length 4))
            masked-prefix (apply str (repeat (- national-length 4) "*"))]
        (str "+" (:country-code phone-number) " " masked-prefix visible-suffix)))))
