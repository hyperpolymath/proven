;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-network
  "Safe network validation and operations."
  (:require [clojure.string :as str])
  (:import [java.net InetAddress URI]))

(defrecord IPv4Address [a b c d])

(defn parse-ipv4
  "Parse an IPv4 address string. Returns IPv4Address or nil."
  [address]
  (let [parts (str/split address #"\.")]
    (when (= 4 (count parts))
      (let [octets (map (fn [part]
                          (when (and (not (str/blank? part))
                                     (re-matches #"^[0-9]+$" part)
                                     (or (= part "0")
                                         (not (str/starts-with? part "0"))))
                            (try
                              (let [n (Long/parseLong part)]
                                (when (<= 0 n 255) n))
                              (catch NumberFormatException _ nil))))
                        parts)]
        (when (every? some? octets)
          (->IPv4Address (nth octets 0) (nth octets 1) (nth octets 2) (nth octets 3)))))))

(defn ipv4->int
  "Convert IPv4Address to integer representation."
  [{:keys [a b c d]}]
  (bit-or (bit-shift-left (bit-and a 0xFF) 24)
          (bit-shift-left (bit-and b 0xFF) 16)
          (bit-shift-left (bit-and c 0xFF) 8)
          (bit-and d 0xFF)))

(defn loopback?
  "Check if IPv4 is a loopback address (127.0.0.0/8)."
  [{:keys [a]}]
  (= a 127))

(defn private?
  "Check if IPv4 is a private address (RFC 1918)."
  [{:keys [a b]}]
  (or (= a 10)
      (and (= a 172) (<= 16 b 31))
      (and (= a 192) (= b 168))))

(defn reserved?
  "Check if IPv4 is a reserved address."
  [{:keys [a b c]}]
  (or (= a 0)
      (and (= a 100) (<= 64 b 127))
      (and (= a 169) (= b 254))
      (and (= a 192) (= b 0) (= c 0))
      (and (= a 192) (= b 0) (= c 2))
      (and (= a 198) (= b 51) (= c 100))
      (and (= a 203) (= b 0) (= c 113))
      (<= 224 a 239)
      (>= a 240)))

(defn public?
  "Check if IPv4 is a public address."
  [ip]
  (not (or (loopback? ip) (private? ip) (reserved? ip))))

(defn classify
  "Get classification of IPv4 address."
  [ip]
  (cond
    (loopback? ip) :loopback
    (private? ip) :private
    (reserved? ip) :reserved
    :else :public))

(defn in-range?
  "Check if IPv4 address is in a CIDR range."
  [ip network prefix-length]
  (when (and (>= prefix-length 0) (<= prefix-length 32))
    (let [mask (if (= prefix-length 0)
                 0
                 (bit-shift-left 0xFFFFFFFF (- 32 prefix-length)))]
      (= (bit-and (ipv4->int ip) mask)
         (bit-and (ipv4->int network) mask)))))

(defn valid-ipv4?
  "Check if string is a valid IPv4 address."
  [address]
  (some? (parse-ipv4 address)))

(defn valid-ipv6?
  "Check if string is a valid IPv6 address."
  [address]
  (try
    (let [inet (InetAddress/getByName address)]
      (and (= 16 (count (.getAddress inet)))
           (str/includes? address ":")))
    (catch Exception _ false)))

(defn valid-ip?
  "Check if string is any valid IP address."
  [address]
  (or (valid-ipv4? address) (valid-ipv6? address)))

(defn classify-ipv4
  "Classify an IPv4 address string."
  [address]
  (if-let [ip (parse-ipv4 address)]
    (classify ip)
    :invalid))

(defn valid-port?
  "Check if port number is valid (1-65535)."
  [port]
  (<= 1 port 65535))

(defn privileged-port?
  "Check if port is privileged (< 1024)."
  [port]
  (and (>= port 1) (< port 1024)))

(defn valid-hostname?
  "Check if string is a valid hostname."
  [hostname]
  (when (and (not (str/blank? hostname))
             (<= (count hostname) 253))
    (let [labels (str/split hostname #"\.")]
      (every? (fn [label]
                (and (not (str/blank? label))
                     (<= (count label) 63)
                     (re-matches #"^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$" label)))
              labels))))

(defn valid-host-port?
  "Check if host:port string is valid."
  [host-port]
  (let [last-colon (str/last-index-of host-port ":")]
    (when last-colon
      (let [host (subs host-port 0 last-colon)
            port-str (subs host-port (inc last-colon))]
        (when-let [port (try (Long/parseLong port-str) (catch Exception _ nil))]
          (and (valid-port? port)
               (or (valid-ip? host) (valid-hostname? host))))))))

(defn valid-url?
  "Check if string is a valid URL."
  [url]
  (try
    (let [uri (URI. url)]
      (and (.getScheme uri)
           (#{"http" "https"} (.getScheme uri))))
    (catch Exception _ false)))

(defn parse-url
  "Parse a URL safely. Returns URI or nil."
  [url]
  (try
    (let [uri (URI. url)]
      (when (.getScheme uri) uri))
    (catch Exception _ nil)))

(defn private-url?
  "Check if URL host is a private IP (SSRF protection)."
  [url]
  (when-let [uri (parse-url url)]
    (let [host (.getHost uri)]
      (or (= host "localhost")
          (= host "127.0.0.1")
          (= host "::1")
          (when-let [ip (parse-ipv4 host)]
            (or (private? ip) (loopback? ip) (reserved? ip)))))))

(defn format-ipv4
  "Format IPv4 from octets."
  [a b c d]
  (str a "." b "." c "." d))
