;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-uuid
  "Safe UUID generation and validation following RFC 4122."
  (:require [clojure.string :as str])
  (:import [java.util UUID]))

;; UUID version types
(def uuid-versions
  "UUID version identifiers."
  {:v1 1  ; Time-based
   :v2 2  ; DCE Security
   :v3 3  ; Name-based (MD5)
   :v4 4  ; Random
   :v5 5  ; Name-based (SHA-1)
   :nil 0})

;; UUID variant types
(def uuid-variants
  "UUID variant identifiers."
  {:ncs 0
   :rfc4122 1
   :microsoft 2
   :future 3})

;; Namespace UUIDs per RFC 4122
(def namespace-dns
  "DNS namespace UUID."
  {:bytes [0x6b 0xa7 0xb8 0x10 0x9d 0xad 0x11 0xd1
           0x80 0xb4 0x00 0xc0 0x4f 0xd4 0x30 0xc8]})

(def namespace-url
  "URL namespace UUID."
  {:bytes [0x6b 0xa7 0xb8 0x11 0x9d 0xad 0x11 0xd1
           0x80 0xb4 0x00 0xc0 0x4f 0xd4 0x30 0xc8]})

(def namespace-oid
  "OID namespace UUID."
  {:bytes [0x6b 0xa7 0xb8 0x12 0x9d 0xad 0x11 0xd1
           0x80 0xb4 0x00 0xc0 0x4f 0xd4 0x30 0xc8]})

(def namespace-x500
  "X.500 namespace UUID."
  {:bytes [0x6b 0xa7 0xb8 0x14 0x9d 0xad 0x11 0xd1
           0x80 0xb4 0x00 0xc0 0x4f 0xd4 0x30 0xc8]})

(def nil-uuid
  "The nil UUID (all zeros)."
  {:bytes (vec (repeat 16 0))})

(defrecord SafeUuid [bytes])

(defn- byte-to-hex
  "Convert a byte to two hex characters."
  [byte-value]
  (format "%02x" (bit-and byte-value 0xFF)))

(defn- hex-char-to-nibble
  "Convert a hex character to its nibble value."
  [hex-char]
  (let [character-code (int hex-char)]
    (cond
      (<= (int \0) character-code (int \9)) (- character-code (int \0))
      (<= (int \a) character-code (int \f)) (+ 10 (- character-code (int \a)))
      (<= (int \A) character-code (int \F)) (+ 10 (- character-code (int \A)))
      :else nil)))

(defn from-bytes
  "Create a SafeUuid from a 16-byte vector."
  [byte-vector]
  (when (and (vector? byte-vector) (= 16 (count byte-vector)))
    (->SafeUuid byte-vector)))

(defn get-bytes
  "Get the raw bytes of a UUID."
  [uuid]
  (:bytes uuid))

(defn get-version
  "Get the UUID version."
  [uuid]
  (let [version-byte (nth (:bytes uuid) 6)
        version-nibble (bit-and (bit-shift-right version-byte 4) 0x0F)]
    (case version-nibble
      1 :v1
      2 :v2
      3 :v3
      4 :v4
      5 :v5
      :nil)))

(defn get-variant
  "Get the UUID variant."
  [uuid]
  (let [variant-byte (nth (:bytes uuid) 8)]
    (cond
      (zero? (bit-and variant-byte 0x80)) :ncs
      (= 0x80 (bit-and variant-byte 0xC0)) :rfc4122
      (= 0xC0 (bit-and variant-byte 0xE0)) :microsoft
      :else :future)))

(defn nil?-uuid
  "Check if the UUID is the nil UUID."
  [uuid]
  (every? zero? (:bytes uuid)))

(defn format-canonical
  "Format UUID as canonical string (8-4-4-4-12)."
  [uuid]
  (let [bytes-vec (:bytes uuid)]
    (str
     (apply str (map byte-to-hex (subvec bytes-vec 0 4)))
     "-"
     (apply str (map byte-to-hex (subvec bytes-vec 4 6)))
     "-"
     (apply str (map byte-to-hex (subvec bytes-vec 6 8)))
     "-"
     (apply str (map byte-to-hex (subvec bytes-vec 8 10)))
     "-"
     (apply str (map byte-to-hex (subvec bytes-vec 10 16))))))

(defn format-urn
  "Format UUID as URN (urn:uuid:...)."
  [uuid]
  (str "urn:uuid:" (format-canonical uuid)))

(defn format-braced
  "Format UUID with braces ({...})."
  [uuid]
  (str "{" (format-canonical uuid) "}"))

(defn format-compact
  "Format UUID without hyphens."
  [uuid]
  (apply str (map byte-to-hex (:bytes uuid))))

(defn parse
  "Parse a UUID from canonical string format.
   Returns {:ok SafeUuid} or {:error message}."
  [uuid-string]
  (cond
    (str/blank? uuid-string)
    {:error "empty_uuid_string"}

    (not= 36 (count uuid-string))
    {:error "invalid_uuid_length"}

    (or (not= \- (nth uuid-string 8))
        (not= \- (nth uuid-string 13))
        (not= \- (nth uuid-string 18))
        (not= \- (nth uuid-string 23)))
    {:error "invalid_uuid_format"}

    :else
    (let [hex-string (str/replace uuid-string "-" "")]
      (if (not= 32 (count hex-string))
        {:error "invalid_uuid_hex_length"}
        (let [hex-chars (seq hex-string)
              nibbles (map hex-char-to-nibble hex-chars)]
          (if (some nil? nibbles)
            {:error "invalid_hex_character"}
            (let [byte-values (mapv (fn [[high-nibble low-nibble]]
                                      (bit-or (bit-shift-left high-nibble 4) low-nibble))
                                    (partition 2 nibbles))]
              {:ok (->SafeUuid byte-values)})))))))

(defn parse!
  "Parse a UUID from string. Throws ex-info on error."
  [uuid-string]
  (let [result (parse uuid-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (str "Invalid UUID: " (:error result))
                      {:type :invalid-uuid
                       :error (:error result)
                       :input uuid-string})))))

(defn valid?
  "Check if a string is a valid UUID format."
  [uuid-string]
  (contains? (parse uuid-string) :ok))

(defn generate-v4
  "Generate a random v4 UUID using Java's UUID generator."
  []
  (let [java-uuid (UUID/randomUUID)
        most-significant-bits (.getMostSignificantBits java-uuid)
        least-significant-bits (.getLeastSignificantBits java-uuid)
        bytes-vec (vec (concat
                        (for [shift-amount [56 48 40 32 24 16 8 0]]
                          (bit-and (bit-shift-right most-significant-bits shift-amount) 0xFF))
                        (for [shift-amount [56 48 40 32 24 16 8 0]]
                          (bit-and (bit-shift-right least-significant-bits shift-amount) 0xFF))))]
    (->SafeUuid bytes-vec)))

(defn v4-from-bytes
  "Create a v4 UUID from 16 random bytes (sets version and variant bits)."
  [random-bytes]
  (when (and (sequential? random-bytes) (= 16 (count random-bytes)))
    (let [mutable-bytes (vec random-bytes)
          ;; Set version to 4
          byte-6 (bit-or (bit-and (nth mutable-bytes 6) 0x0F) 0x40)
          ;; Set variant to RFC 4122
          byte-8 (bit-or (bit-and (nth mutable-bytes 8) 0x3F) 0x80)]
      (->SafeUuid (-> mutable-bytes
                      (assoc 6 byte-6)
                      (assoc 8 byte-8))))))

(defn equal?
  "Check if two UUIDs are equal."
  [uuid-a uuid-b]
  (= (:bytes uuid-a) (:bytes uuid-b)))

(defn compare-uuids
  "Compare two UUIDs lexicographically."
  [uuid-a uuid-b]
  (compare (:bytes uuid-a) (:bytes uuid-b)))

(defn to-java-uuid
  "Convert SafeUuid to java.util.UUID."
  [uuid]
  (UUID/fromString (format-canonical uuid)))

(defn from-java-uuid
  "Convert java.util.UUID to SafeUuid."
  [java-uuid]
  (parse (.toString java-uuid)))
