;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-json
  "Safe JSON parsing with depth and size limits."
  (:require [clojure.string :as str])
  (:import [java.io StringReader PushbackReader]))

(def ^:dynamic *max-depth* 64)
(def ^:dynamic *max-string-length* 1000000)
(def ^:dynamic *max-array-length* 10000)
(def ^:dynamic *max-object-keys* 10000)

(declare parse-value)

(defn- skip-whitespace [^PushbackReader reader]
  (loop []
    (let [char-code (.read reader)]
      (when (>= char-code 0)
        (let [character (char char-code)]
          (if (Character/isWhitespace character)
            (recur)
            (.unread reader char-code)))))))

(defn- peek-char [^PushbackReader reader]
  (let [char-code (.read reader)]
    (when (>= char-code 0)
      (.unread reader char-code)
      (char char-code))))

(defn- read-char [^PushbackReader reader]
  (let [char-code (.read reader)]
    (when (>= char-code 0)
      (char char-code))))

(defn- expect-char [^PushbackReader reader expected]
  (let [actual (read-char reader)]
    (when (not= actual expected)
      (throw (ex-info "Unexpected character" {:expected expected :actual actual})))))

(defn- parse-string [^PushbackReader reader]
  (expect-char reader \")
  (let [sb (StringBuilder.)]
    (loop []
      (let [character (read-char reader)]
        (when (nil? character)
          (throw (ex-info "Unterminated string" {})))
        (cond
          (= character \")
          (let [result (.toString sb)]
            (when (> (count result) *max-string-length*)
              (throw (ex-info "String too long" {:length (count result) :max *max-string-length*})))
            result)

          (= character \\)
          (let [escape-char (read-char reader)]
            (case escape-char
              \" (.append sb \")
              \\ (.append sb \\)
              \/ (.append sb \/)
              \b (.append sb \backspace)
              \f (.append sb \formfeed)
              \n (.append sb \newline)
              \r (.append sb \return)
              \t (.append sb \tab)
              \u (let [hex-chars (apply str (repeatedly 4 #(read-char reader)))
                       code-point (Integer/parseInt hex-chars 16)]
                   (.append sb (char code-point)))
              (throw (ex-info "Invalid escape sequence" {:char escape-char})))
            (recur))

          :else
          (do
            (.append sb character)
            (recur)))))))

(defn- parse-number [^PushbackReader reader]
  (let [sb (StringBuilder.)]
    (loop []
      (let [char-code (.read reader)]
        (if (< char-code 0)
          nil
          (let [character (char char-code)]
            (if (or (Character/isDigit character)
                    (= character \-)
                    (= character \+)
                    (= character \.)
                    (= character \e)
                    (= character \E))
              (do (.append sb character)
                  (recur))
              (.unread reader char-code))))))
    (let [num-str (.toString sb)]
      (if (str/includes? num-str ".")
        (Double/parseDouble num-str)
        (Long/parseLong num-str)))))

(defn- parse-array [^PushbackReader reader depth]
  (when (>= depth *max-depth*)
    (throw (ex-info "Maximum depth exceeded" {:depth depth :max *max-depth*})))
  (expect-char reader \[)
  (skip-whitespace reader)
  (if (= (peek-char reader) \])
    (do (read-char reader) [])
    (loop [elements []
           count 0]
      (when (>= count *max-array-length*)
        (throw (ex-info "Array too long" {:length count :max *max-array-length*})))
      (let [element (parse-value reader (inc depth))
            new-elements (conj elements element)]
        (skip-whitespace reader)
        (let [next-char (read-char reader)]
          (cond
            (= next-char \]) new-elements
            (= next-char \,) (do (skip-whitespace reader)
                                 (recur new-elements (inc count)))
            :else (throw (ex-info "Expected , or ]" {:char next-char}))))))))

(defn- parse-object [^PushbackReader reader depth]
  (when (>= depth *max-depth*)
    (throw (ex-info "Maximum depth exceeded" {:depth depth :max *max-depth*})))
  (expect-char reader \{)
  (skip-whitespace reader)
  (if (= (peek-char reader) \})
    (do (read-char reader) {})
    (loop [obj {}
           count 0]
      (when (>= count *max-object-keys*)
        (throw (ex-info "Object has too many keys" {:count count :max *max-object-keys*})))
      (skip-whitespace reader)
      (let [key (parse-string reader)]
        (skip-whitespace reader)
        (expect-char reader \:)
        (skip-whitespace reader)
        (let [value (parse-value reader (inc depth))
              new-obj (assoc obj key value)]
          (skip-whitespace reader)
          (let [next-char (read-char reader)]
            (cond
              (= next-char \}) new-obj
              (= next-char \,) (recur new-obj (inc count))
              :else (throw (ex-info "Expected , or }" {:char next-char})))))))))

(defn- parse-literal [^PushbackReader reader]
  (let [sb (StringBuilder.)]
    (loop []
      (let [char-code (.read reader)]
        (if (< char-code 0)
          nil
          (let [character (char char-code)]
            (if (Character/isLetter character)
              (do (.append sb character)
                  (recur))
              (.unread reader char-code))))))
    (let [literal (.toString sb)]
      (case literal
        "true" true
        "false" false
        "null" nil
        (throw (ex-info "Invalid literal" {:literal literal}))))))

(defn- parse-value [^PushbackReader reader depth]
  (skip-whitespace reader)
  (let [character (peek-char reader)]
    (cond
      (nil? character) (throw (ex-info "Unexpected end of input" {}))
      (= character \") (parse-string reader)
      (= character \{) (parse-object reader depth)
      (= character \[) (parse-array reader depth)
      (or (Character/isDigit character) (= character \-)) (parse-number reader)
      :else (parse-literal reader))))

(defn parse
  "Parse a JSON string.
   Returns {:ok value} or {:error message}.
   Options:
   - :max-depth - Maximum nesting depth (default 64)
   - :max-string-length - Maximum string length (default 1000000)
   - :max-array-length - Maximum array elements (default 10000)
   - :max-object-keys - Maximum object keys (default 10000)"
  ([json-string] (parse json-string {}))
  ([json-string options]
   (if (str/blank? json-string)
     {:error "empty_json"}
     (binding [*max-depth* (get options :max-depth *max-depth*)
               *max-string-length* (get options :max-string-length *max-string-length*)
               *max-array-length* (get options :max-array-length *max-array-length*)
               *max-object-keys* (get options :max-object-keys *max-object-keys*)]
       (try
         (let [reader (PushbackReader. (StringReader. json-string))
               result (parse-value reader 0)]
           (skip-whitespace reader)
           (if (>= (.read reader) 0)
             {:error "trailing_content"}
             {:ok result}))
         (catch Exception e
           {:error (str "parse_error: " (.getMessage e))}))))))

(defn valid?
  "Check if a string is valid JSON."
  [json-string]
  (contains? (parse json-string) :ok))

(defn parse!
  "Parse JSON string. Throws ex-info on error."
  ([json-string] (parse! json-string {}))
  ([json-string options]
   (let [result (parse json-string options)]
     (if (:ok result)
       (:ok result)
       (throw (ex-info (:error result) {:type :json-parse-error}))))))

(defn get-path
  "Get value at a path in parsed JSON. Path is a vector of keys/indices."
  [json-value path]
  (reduce (fn [current segment]
            (cond
              (nil? current) nil
              (and (map? current) (string? segment)) (get current segment)
              (and (vector? current) (integer? segment)) (get current segment)
              :else nil))
          json-value
          path))

(defn safe-get
  "Safely get a value with type checking."
  [json-value key expected-type]
  (let [value (get json-value key)]
    (when (instance? expected-type value)
      value)))
