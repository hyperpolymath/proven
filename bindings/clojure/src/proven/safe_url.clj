;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-url
  "Safe URL parsing and validation."
  (:require [clojure.string :as str])
  (:import [java.net URI URL URLEncoder URLDecoder]
           [java.nio.charset StandardCharsets]))

(defrecord ParsedUrl [scheme host port path query fragment userinfo])

(def default-ports
  "Default ports for common schemes."
  {"http" 80
   "https" 443
   "ftp" 21
   "ssh" 22
   "ws" 80
   "wss" 443})

(defn parse
  "Parse a URL string into components.
   Returns {:ok ParsedUrl} or {:error message}."
  [url-string]
  (if (str/blank? url-string)
    {:error "empty_url"}
    (try
      (let [uri (URI. url-string)
            scheme (.getScheme uri)
            host (.getHost uri)
            port (.getPort uri)
            path (.getPath uri)
            query (.getQuery uri)
            fragment (.getFragment uri)
            userinfo (.getUserInfo uri)]
        (if (nil? scheme)
          {:error "missing_scheme"}
          {:ok (->ParsedUrl
                scheme
                host
                (if (= port -1) nil port)
                (if (str/blank? path) "/" path)
                query
                fragment
                userinfo)}))
      (catch Exception e
        {:error (str "invalid_url: " (.getMessage e))}))))

(defn valid?
  "Check if a string is a valid URL."
  [url-string]
  (contains? (parse url-string) :ok))

(defn valid-http?
  "Check if URL is a valid HTTP or HTTPS URL."
  [url-string]
  (when-let [parsed (:ok (parse url-string))]
    (contains? #{"http" "https"} (:scheme parsed))))

(defn get-scheme
  "Get the scheme (protocol) of a URL."
  [url-string]
  (:scheme (:ok (parse url-string))))

(defn get-host
  "Get the host of a URL."
  [url-string]
  (:host (:ok (parse url-string))))

(defn get-port
  "Get the port of a URL (explicit or default for scheme)."
  [url-string]
  (when-let [parsed (:ok (parse url-string))]
    (or (:port parsed)
        (get default-ports (:scheme parsed)))))

(defn get-path
  "Get the path of a URL."
  [url-string]
  (:path (:ok (parse url-string))))

(defn get-query
  "Get the query string of a URL."
  [url-string]
  (:query (:ok (parse url-string))))

(defn get-fragment
  "Get the fragment of a URL."
  [url-string]
  (:fragment (:ok (parse url-string))))

(defn parse-query-string
  "Parse a query string into a map of key-value pairs."
  [query-string]
  (when-not (str/blank? query-string)
    (into {}
          (for [pair (str/split query-string #"&")
                :let [[k v] (str/split pair #"=" 2)]
                :when (not (str/blank? k))]
            [(URLDecoder/decode k (.name StandardCharsets/UTF_8))
             (when v (URLDecoder/decode v (.name StandardCharsets/UTF_8)))]))))

(defn build-query-string
  "Build a query string from a map of key-value pairs."
  [params]
  (when (seq params)
    (str/join "&"
              (for [[k v] params]
                (str (URLEncoder/encode (str k) (.name StandardCharsets/UTF_8))
                     "="
                     (URLEncoder/encode (str v) (.name StandardCharsets/UTF_8)))))))

(defn normalize
  "Normalize a URL (lowercase scheme and host, remove default port)."
  [url-string]
  (when-let [parsed (:ok (parse url-string))]
    (let [scheme (str/lower-case (:scheme parsed))
          host (when (:host parsed) (str/lower-case (:host parsed)))
          port (:port parsed)
          default-port (get default-ports scheme)
          port-str (when (and port (not= port default-port))
                     (str ":" port))
          path (or (:path parsed) "/")
          query-str (when (:query parsed) (str "?" (:query parsed)))
          fragment-str (when (:fragment parsed) (str "#" (:fragment parsed)))]
      (str scheme "://" host port-str path query-str fragment-str))))

(defn join
  "Join a base URL with a relative path."
  [base-url relative-path]
  (try
    (let [base-uri (URI. base-url)
          resolved (.resolve base-uri relative-path)]
      {:ok (str resolved)})
    (catch Exception _
      {:error "invalid_url_join"})))

(defn is-same-origin?
  "Check if two URLs have the same origin (scheme, host, port)."
  [url-a url-b]
  (let [parsed-a (:ok (parse url-a))
        parsed-b (:ok (parse url-b))]
    (when (and parsed-a parsed-b)
      (and (= (str/lower-case (:scheme parsed-a))
              (str/lower-case (:scheme parsed-b)))
           (= (when (:host parsed-a) (str/lower-case (:host parsed-a)))
              (when (:host parsed-b) (str/lower-case (:host parsed-b))))
           (= (or (:port parsed-a) (get default-ports (:scheme parsed-a)))
              (or (:port parsed-b) (get default-ports (:scheme parsed-b))))))))

(defn is-subdomain?
  "Check if URL host is a subdomain of the given domain."
  [url-string parent-domain]
  (when-let [host (get-host url-string)]
    (let [lower-host (str/lower-case host)
          lower-parent (str/lower-case parent-domain)]
      (or (= lower-host lower-parent)
          (str/ends-with? lower-host (str "." lower-parent))))))

(defn encode-component
  "URL-encode a string component."
  [component]
  (URLEncoder/encode component (.name StandardCharsets/UTF_8)))

(defn decode-component
  "URL-decode a string component. Returns nil on error."
  [component]
  (try
    (URLDecoder/decode component (.name StandardCharsets/UTF_8))
    (catch Exception _ nil)))

(defn has-dangerous-protocol?
  "Check if URL has a dangerous protocol (javascript:, data:, etc)."
  [url-string]
  (when-let [parsed (:ok (parse url-string))]
    (contains? #{"javascript" "data" "vbscript"} (str/lower-case (:scheme parsed)))))

(defn safe-redirect?
  "Check if URL is safe for redirect (not javascript:, etc)."
  [url-string]
  (and (valid? url-string)
       (not (has-dangerous-protocol? url-string))))

(defn extract-domain
  "Extract the registrable domain from URL (simplified, no public suffix list)."
  [url-string]
  (when-let [host (get-host url-string)]
    (let [parts (str/split host #"\.")]
      (when (>= (count parts) 2)
        (str/join "." (take-last 2 parts))))))
