;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-string
  "Safe string operations with XSS and injection prevention."
  (:require [clojure.string :as str])
  (:import [java.net URLEncoder URLDecoder]
           [java.util Base64]
           [java.nio.charset StandardCharsets]))

(def ^:private html-entities
  {\& "&amp;"
   \< "&lt;"
   \> "&gt;"
   \" "&quot;"
   \' "&#x27;"})

(defn escape-html
  "Escape string for safe HTML output (XSS prevention)."
  [input]
  (apply str (map #(get html-entities % (str %)) input)))

(defn escape-sql
  "Escape string for SQL single quotes.
   Note: PREFER PARAMETERIZED QUERIES! This is for edge cases only."
  [input]
  (str/replace input "'" "''"))

(defn escape-js
  "Escape string for JavaScript string context."
  [input]
  (apply str
         (map (fn [c]
                (case c
                  \\ "\\\\"
                  \' "\\'"
                  \" "\\\""
                  \newline "\\n"
                  \return "\\r"
                  \tab "\\t"
                  \< "\\x3C"
                  \> "\\x3E"
                  (str c)))
              input)))

(defn escape-shell
  "Escape string for shell command (single-quote wrapping)."
  [input]
  (str "'" (str/replace input "'" "'\\''") "'"))

(defn escape-regex
  "Escape special characters for use in regex patterns."
  [input]
  (java.util.regex.Pattern/quote input))

(defn url-encode
  "URL-encode a string."
  [input]
  (URLEncoder/encode input (.name StandardCharsets/UTF_8)))

(defn url-decode
  "URL-decode a string. Returns nil on error."
  [input]
  (try
    (URLDecoder/decode input (.name StandardCharsets/UTF_8))
    (catch Exception _ nil)))

(defn base64-encode
  "Base64-encode a string."
  [input]
  (.encodeToString (Base64/getEncoder) (.getBytes input StandardCharsets/UTF_8)))

(defn base64-decode
  "Base64-decode a string. Returns nil on error."
  [input]
  (try
    (String. (.decode (Base64/getDecoder) input) StandardCharsets/UTF_8)
    (catch Exception _ nil)))

(defn truncate
  "Safely truncate a string to a maximum length."
  ([input max-length] (truncate input max-length "..."))
  ([input max-length suffix]
   (cond
     (<= max-length 0) ""
     (<= (count input) max-length) input
     (<= max-length (count suffix)) (subs input 0 max-length)
     :else (str (subs input 0 (- max-length (count suffix))) suffix))))

(defn strip-html
  "Strip HTML tags from string (basic - not a full parser)."
  [input]
  (str/replace input #"<[^>]*>" ""))

(defn alphanumeric?
  "Check if string contains only alphanumeric characters."
  [input]
  (and (not (str/blank? input))
       (re-matches #"^[a-zA-Z0-9]+$" input)))

(defn ascii?
  "Check if string contains only ASCII characters."
  [input]
  (every? #(<= 0 (int %) 127) input))

(defn sanitize
  "Sanitize string to contain only specified allowed characters."
  ([input] (sanitize input "a-zA-Z0-9_-"))
  ([input allowed]
   (let [pattern (re-pattern (str "[^" allowed "]"))]
     (str/replace input pattern ""))))

(defn remove-control-chars
  "Remove control characters from string."
  [input]
  (str/replace input #"[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" ""))

(defn normalize-whitespace
  "Normalize whitespace (collapse multiple spaces, trim)."
  [input]
  (-> input
      (str/replace #"\s+" " ")
      str/trim))

(defn contains-suspicious-patterns?
  "Check if string looks like it contains injection attempts."
  [input]
  (let [lower-input (str/lower-case input)]
    (or (str/includes? lower-input "<script")
        (str/includes? lower-input "javascript:")
        (some? (re-find #"on\w+\s*=" lower-input))
        (some? (re-find #"union\s+select" lower-input))
        (some? (re-find #";\s*drop\s+table" lower-input)))))

(defn slugify
  "Convert string to slug (URL-friendly format)."
  [input]
  (-> input
      str/lower-case
      (str/replace #"[^a-z0-9\s-]" "")
      (str/replace #"\s+" "-")
      (str/replace #"-+" "-")
      (str/replace #"^-|-$" "")))

(defn escape-xml
  "Escape XML special characters."
  [input]
  (escape-html input))
