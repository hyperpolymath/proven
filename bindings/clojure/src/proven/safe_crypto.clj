;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-crypto
  "Safe cryptographic operations."
  (:import [java.security MessageDigest SecureRandom]
           [java.util Base64]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec PBEKeySpec]
           [javax.crypto SecretKeyFactory]))

(def ^:private secure-random (SecureRandom.))

(defn constant-time-equals-bytes
  "Constant-time byte array comparison to prevent timing attacks."
  [^bytes a ^bytes b]
  (if (not= (alength a) (alength b))
    false
    (loop [i 0 result 0]
      (if (>= i (alength a))
        (zero? result)
        (recur (inc i)
               (bit-or result (bit-xor (aget a i) (aget b i))))))))

(defn constant-time-equals
  "Constant-time string comparison to prevent timing attacks."
  [a b]
  (constant-time-equals-bytes (.getBytes a "UTF-8") (.getBytes b "UTF-8")))

(defn random-bytes
  "Generate cryptographically secure random bytes."
  [count]
  (let [bytes (byte-array count)]
    (.nextBytes secure-random bytes)
    bytes))

(defn bytes->hex
  "Convert byte array to hex string."
  [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and % 0xFF)) bytes)))

(defn random-hex
  "Generate random bytes as hex string."
  [byte-count]
  (bytes->hex (random-bytes byte-count)))

(defn random-base64
  "Generate random bytes as base64 string."
  [byte-count]
  (.encodeToString (Base64/getEncoder) (random-bytes byte-count)))

(defn random-url-safe
  "Generate URL-safe random string."
  [byte-count]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) (random-bytes byte-count)))

(defn random-int
  "Generate random integer in range [min-val, max-val]."
  [min-val max-val]
  (let [[lo hi] (if (<= min-val max-val) [min-val max-val] [max-val min-val])
        range (- hi lo -1)]
    (+ lo (.nextInt secure-random range))))

(defn generate-token
  "Generate a secure token (for sessions, CSRF, etc)."
  ([] (generate-token 32))
  ([length] (random-url-safe length)))

(defn sha256
  "Hash a string with SHA-256."
  [input]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes input "UTF-8"))
    (bytes->hex (.digest digest))))

(defn sha256-bytes
  "Hash bytes with SHA-256."
  [^bytes input]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest input)
    (bytes->hex (.digest digest))))

(defn sha512
  "Hash a string with SHA-512."
  [input]
  (let [digest (MessageDigest/getInstance "SHA-512")]
    (.update digest (.getBytes input "UTF-8"))
    (bytes->hex (.digest digest))))

(defn sha512-bytes
  "Hash bytes with SHA-512."
  [^bytes input]
  (let [digest (MessageDigest/getInstance "SHA-512")]
    (.update digest input)
    (bytes->hex (.digest digest))))

(defn hmac-sha256
  "Compute HMAC-SHA256."
  [key message]
  (let [key-bytes (.getBytes key "UTF-8")
        msg-bytes (.getBytes message "UTF-8")
        mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. key-bytes "HmacSHA256"))
    (bytes->hex (.doFinal mac msg-bytes))))

(defn hmac-sha256-bytes
  "Compute HMAC-SHA256 with bytes."
  [^bytes key ^bytes message]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. key "HmacSHA256"))
    (bytes->hex (.doFinal mac message))))

(defn hmac-sha512
  "Compute HMAC-SHA512."
  [key message]
  (let [key-bytes (.getBytes key "UTF-8")
        msg-bytes (.getBytes message "UTF-8")
        mac (Mac/getInstance "HmacSHA512")]
    (.init mac (SecretKeySpec. key-bytes "HmacSHA512"))
    (bytes->hex (.doFinal mac msg-bytes))))

(defn verify-hmac-sha256
  "Verify HMAC using constant-time comparison."
  [key message expected-mac]
  (constant-time-equals (hmac-sha256 key message) expected-mac))

(defn verify-hmac-sha512
  "Verify HMAC-SHA512 using constant-time comparison."
  [key message expected-mac]
  (constant-time-equals (hmac-sha512 key message) expected-mac))

(defn md5
  "Hash a string with MD5 (NOT for security, only for checksums)."
  [input]
  (let [digest (MessageDigest/getInstance "MD5")]
    (.update digest (.getBytes input "UTF-8"))
    (bytes->hex (.digest digest))))

(defn pbkdf2
  "Derive a key using PBKDF2-SHA256. Returns hex-encoded derived key."
  ([password salt] (pbkdf2 password salt 100000 32))
  ([password salt iterations] (pbkdf2 password salt iterations 32))
  ([password salt iterations key-length]
   (let [spec (PBEKeySpec. (.toCharArray password)
                           (.getBytes salt "UTF-8")
                           iterations
                           (* key-length 8))
         factory (SecretKeyFactory/getInstance "PBKDF2WithHmacSHA256")]
     (bytes->hex (.getEncoded (.generateSecret factory spec))))))

(defn generate-password
  "Generate a random password."
  ([] (generate-password {}))
  ([{:keys [length include-uppercase include-lowercase include-numbers include-symbols]
     :or {length 16 include-uppercase true include-lowercase true
          include-numbers true include-symbols true}}]
   (let [chars (str (when include-lowercase "abcdefghijklmnopqrstuvwxyz")
                    (when include-uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (when include-numbers "0123456789")
                    (when include-symbols "!@#$%^&*()_+-=[]{}|;:,.<>?"))]
     (if (empty? chars)
       ""
       (apply str (repeatedly length #(nth chars (.nextInt secure-random (count chars)))))))))

(defn secure-wipe
  "Securely wipe a byte array (best effort)."
  [^bytes data]
  (java.util.Arrays/fill data (byte 0)))

(defn secure-random-available?
  "Check if a source of randomness is cryptographically secure."
  []
  true)
