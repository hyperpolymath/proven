;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-hex
  "Safe hexadecimal encoding and decoding operations."
  (:require [clojure.string :as str]))

(def ^:private hex-chars-lower "0123456789abcdef")
(def ^:private hex-chars-upper "0123456789ABCDEF")

(defn hex-char?
  "Check if a character is a valid hexadecimal digit."
  [character]
  (let [character-code (int character)]
    (or (<= (int \0) character-code (int \9))
        (<= (int \a) character-code (int \f))
        (<= (int \A) character-code (int \F)))))

(defn hex-char-to-nibble
  "Convert a hex character to its nibble value (0-15). Returns nil for invalid chars."
  [hex-char]
  (let [character-code (int hex-char)]
    (cond
      (<= (int \0) character-code (int \9)) (- character-code (int \0))
      (<= (int \a) character-code (int \f)) (+ 10 (- character-code (int \a)))
      (<= (int \A) character-code (int \F)) (+ 10 (- character-code (int \A)))
      :else nil)))

(defn nibble-to-hex-char
  "Convert a nibble value (0-15) to lowercase hex character."
  [nibble-value]
  (nth hex-chars-lower (bit-and nibble-value 0x0F)))

(defn nibble-to-hex-char-upper
  "Convert a nibble value (0-15) to uppercase hex character."
  [nibble-value]
  (nth hex-chars-upper (bit-and nibble-value 0x0F)))

(defn encode
  "Encode byte array to lowercase hex string."
  [byte-array]
  (let [bytes-seq (if (bytes? byte-array)
                    (map #(bit-and % 0xFF) (seq byte-array))
                    byte-array)]
    (apply str
           (mapcat (fn [byte-value]
                     [(nibble-to-hex-char (bit-shift-right byte-value 4))
                      (nibble-to-hex-char byte-value)])
                   bytes-seq))))

(defn encode-upper
  "Encode byte array to uppercase hex string."
  [byte-array]
  (let [bytes-seq (if (bytes? byte-array)
                    (map #(bit-and % 0xFF) (seq byte-array))
                    byte-array)]
    (apply str
           (mapcat (fn [byte-value]
                     [(nibble-to-hex-char-upper (bit-shift-right byte-value 4))
                      (nibble-to-hex-char-upper byte-value)])
                   bytes-seq))))

(defn decode
  "Decode hex string to byte vector. Returns {:ok bytes} or {:error message}."
  [hex-string]
  (cond
    (odd? (count hex-string))
    {:error "odd_length_hex_string"}

    (not (every? hex-char? hex-string))
    {:error "invalid_hex_character"}

    :else
    (let [hex-chars (seq hex-string)
          byte-pairs (partition 2 hex-chars)]
      {:ok (vec (map (fn [[high-char low-char]]
                       (let [high-nibble (hex-char-to-nibble high-char)
                             low-nibble (hex-char-to-nibble low-char)]
                         (bit-or (bit-shift-left high-nibble 4) low-nibble)))
                     byte-pairs))})))

(defn decode!
  "Decode hex string to byte vector. Throws ex-info on error."
  [hex-string]
  (let [result (decode hex-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid hex string: " (:error result))
                      {:type :invalid-hex-string
                       :error (:error result)
                       :input hex-string})))))

(defn decode-to-bytes
  "Decode hex string to Java byte array. Returns {:ok bytes} or {:error message}."
  [hex-string]
  (let [result (decode hex-string)]
    (if (:ok result)
      {:ok (byte-array (:ok result))}
      result)))

(defn valid?
  "Check if string contains only valid hex characters."
  [hex-string]
  (every? hex-char? hex-string))

(defn valid-bytes?
  "Check if string is valid hex with even length (represents complete bytes)."
  [hex-string]
  (and (even? (count hex-string))
       (valid? hex-string)))

(defn format-spaced
  "Format hex string with spaces between bytes. Returns {:ok formatted} or {:error message}."
  [hex-string]
  (cond
    (odd? (count hex-string))
    {:error "odd_length_hex_string"}

    (empty? hex-string)
    {:ok ""}

    :else
    (let [byte-pairs (partition 2 hex-string)]
      {:ok (str/join " " (map #(apply str %) byte-pairs))})))

(defn format-spaced!
  "Format hex string with spaces. Throws ex-info on error."
  [hex-string]
  (let [result (format-spaced hex-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid hex string: " (:error result))
                      {:type :invalid-hex-string
                       :error (:error result)
                       :input hex-string})))))

(defn format-colons
  "Format hex string with colons between bytes. Returns {:ok formatted} or {:error message}."
  [hex-string]
  (cond
    (odd? (count hex-string))
    {:error "odd_length_hex_string"}

    (empty? hex-string)
    {:ok ""}

    :else
    (let [byte-pairs (partition 2 hex-string)]
      {:ok (str/join ":" (map #(apply str %) byte-pairs))})))

(defn format-colons!
  "Format hex string with colons. Throws ex-info on error."
  [hex-string]
  (let [result (format-colons hex-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid hex string: " (:error result))
                      {:type :invalid-hex-string
                       :error (:error result)
                       :input hex-string})))))

(defn format-groups
  "Format hex string in groups of specified size. Returns {:ok formatted} or {:error message}."
  [hex-string group-size separator]
  (cond
    (<= group-size 0)
    {:error "invalid_group_size"}

    (empty? hex-string)
    {:ok ""}

    :else
    (let [groups (partition-all group-size hex-string)]
      {:ok (str/join separator (map #(apply str %) groups))})))

(defn constant-time-equal
  "Constant-time comparison of two hex strings to prevent timing attacks.
   Both strings are compared in their entirety regardless of differences."
  [hex-a hex-b]
  (if (not= (count hex-a) (count hex-b))
    false
    (let [lower-a (str/lower-case hex-a)
          lower-b (str/lower-case hex-b)]
      (loop [idx 0
             diff 0]
        (if (>= idx (count lower-a))
          (zero? diff)
          (recur (inc idx)
                 (bit-or diff (bit-xor (int (nth lower-a idx))
                                       (int (nth lower-b idx))))))))))

(defn int-to-hex
  "Convert integer to hex string with optional minimum width."
  ([integer-value]
   (int-to-hex integer-value 0))
  ([integer-value min-width]
   (let [hex-str (Long/toHexString integer-value)]
     (if (>= (count hex-str) min-width)
       hex-str
       (str (apply str (repeat (- min-width (count hex-str)) \0)) hex-str)))))

(defn hex-to-int
  "Parse hex string to integer. Returns {:ok value} or {:error message}."
  [hex-string]
  (if (str/blank? hex-string)
    {:error "empty_hex_string"}
    (try
      {:ok (Long/parseLong hex-string 16)}
      (catch NumberFormatException _
        {:error "invalid_hex_number"}))))

(defn hex-to-int!
  "Parse hex string to integer. Throws ex-info on error."
  [hex-string]
  (let [result (hex-to-int hex-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid hex number: " (:error result))
                      {:type :invalid-hex-number
                       :error (:error result)
                       :input hex-string})))))

(defn strip-prefix
  "Remove 0x or 0X prefix from hex string if present."
  [hex-string]
  (cond
    (str/starts-with? hex-string "0x") (subs hex-string 2)
    (str/starts-with? hex-string "0X") (subs hex-string 2)
    :else hex-string))

(defn add-prefix
  "Add 0x prefix to hex string."
  [hex-string]
  (str "0x" hex-string))

(defn normalize
  "Normalize hex string to lowercase without prefix."
  [hex-string]
  (str/lower-case (strip-prefix hex-string)))

(defn xor-hex
  "XOR two hex strings. Returns {:ok result} or {:error message}."
  [hex-a hex-b]
  (let [decoded-a (decode hex-a)
        decoded-b (decode hex-b)]
    (cond
      (:error decoded-a) decoded-a
      (:error decoded-b) decoded-b
      (not= (count (:ok decoded-a)) (count (:ok decoded-b)))
      {:error "length_mismatch"}
      :else
      {:ok (encode (mapv bit-xor (:ok decoded-a) (:ok decoded-b)))})))

(defn and-hex
  "AND two hex strings. Returns {:ok result} or {:error message}."
  [hex-a hex-b]
  (let [decoded-a (decode hex-a)
        decoded-b (decode hex-b)]
    (cond
      (:error decoded-a) decoded-a
      (:error decoded-b) decoded-b
      (not= (count (:ok decoded-a)) (count (:ok decoded-b)))
      {:error "length_mismatch"}
      :else
      {:ok (encode (mapv bit-and (:ok decoded-a) (:ok decoded-b)))})))

(defn or-hex
  "OR two hex strings. Returns {:ok result} or {:error message}."
  [hex-a hex-b]
  (let [decoded-a (decode hex-a)
        decoded-b (decode hex-b)]
    (cond
      (:error decoded-a) decoded-a
      (:error decoded-b) decoded-b
      (not= (count (:ok decoded-a)) (count (:ok decoded-b)))
      {:error "length_mismatch"}
      :else
      {:ok (encode (mapv bit-or (:ok decoded-a) (:ok decoded-b)))})))
