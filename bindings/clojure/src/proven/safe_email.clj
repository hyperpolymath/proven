;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-email
  "Safe email validation and manipulation."
  (:require [clojure.string :as str]))

(def disposable-domains
  #{"tempmail.com" "throwaway.email" "guerrillamail.com" "mailinator.com"
    "10minutemail.com" "temp-mail.org" "fakeinbox.com" "trashmail.com"
    "yopmail.com" "sharklasers.com" "getairmail.com" "tempail.com"
    "discard.email" "maildrop.cc"})

(def ^:private local-part-pattern #"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+$")
(def ^:private domain-pattern #"^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$")

(defn parse
  "Parse and validate an email address.
   Returns {:ok {:local-part x :domain y}} or {:error msg}."
  [email]
  (cond
    (str/blank? email)
    {:error "empty_email"}

    (> (count email) 254)
    {:error "email_too_long"}

    (not (str/includes? email "@"))
    {:error "missing_at_symbol"}

    (> (count (filter #{\@} email)) 1)
    {:error "multiple_at_symbols"}

    :else
    (let [at-index (str/index-of email "@")
          local-part (subs email 0 at-index)
          domain (subs email (inc at-index))]
      (cond
        (str/blank? local-part)
        {:error "empty_local_part"}

        (> (count local-part) 64)
        {:error "local_part_too_long"}

        (or (str/starts-with? local-part ".")
            (str/ends-with? local-part "."))
        {:error "local_part_dot_position"}

        (str/includes? local-part "..")
        {:error "local_part_consecutive_dots"}

        (not (re-matches local-part-pattern local-part))
        {:error "invalid_local_part_chars"}

        (str/blank? domain)
        {:error "empty_domain"}

        (> (count domain) 253)
        {:error "domain_too_long"}

        (and (not (str/includes? domain "."))
             (not= (str/lower-case domain) "localhost"))
        {:error "domain_missing_dot"}

        (or (str/starts-with? domain ".")
            (str/ends-with? domain ".")
            (str/starts-with? domain "-")
            (str/ends-with? domain "-"))
        {:error "invalid_domain_format"}

        (not (re-matches domain-pattern domain))
        {:error "invalid_domain_chars"}

        :else
        {:ok {:local-part local-part :domain domain}}))))

(defn valid?
  "Check if email is valid."
  [email]
  (contains? (parse email) :ok))

(defn get-domain
  "Extract domain from email. Returns nil if invalid."
  [email]
  (when-let [result (:ok (parse email))]
    (:domain result)))

(defn get-local-part
  "Extract local part from email. Returns nil if invalid."
  [email]
  (when-let [result (:ok (parse email))]
    (:local-part result)))

(defn normalize
  "Normalize email (lowercase domain, preserve local part case).
   Returns nil if invalid."
  [email]
  (when-let [result (:ok (parse email))]
    (str (:local-part result) "@" (str/lower-case (:domain result)))))

(defn normalize-full
  "Fully normalize email (lowercase everything).
   Returns nil if invalid."
  [email]
  (when-let [result (:ok (parse email))]
    (str (str/lower-case (:local-part result)) "@" (str/lower-case (:domain result)))))

(defn disposable?
  "Check if email is from a disposable service."
  [email]
  (when-let [domain (get-domain email)]
    (contains? disposable-domains (str/lower-case domain))))

(defn from-domain?
  "Check if email is from a specific domain."
  [email expected-domain]
  (when-let [domain (get-domain email)]
    (= (str/lower-case domain) (str/lower-case expected-domain))))

(defn from-allowed-domain?
  "Check if email is from one of a list of allowed domains."
  [email allowed-domains]
  (when-let [domain (get-domain email)]
    (let [lower-domain (str/lower-case domain)]
      (some #(= (str/lower-case %) lower-domain) allowed-domains))))

(defn get-tld
  "Get the TLD (top-level domain) of an email."
  [email]
  (when-let [domain (get-domain email)]
    (let [last-dot (str/last-index-of domain ".")]
      (when last-dot
        (str/lower-case (subs domain (inc last-dot)))))))

(defn obfuscate
  "Obfuscate email for display (e.g., 'u***r@example.com')."
  [email]
  (when-let [result (:ok (parse email))]
    (let [local (:local-part result)
          domain (:domain result)
          obfuscated-local (if (<= (count local) 2)
                            "***"
                            (str (first local) "***" (last local)))]
      (str obfuscated-local "@" domain))))

(defn equivalent?
  "Check if two emails are equivalent (after normalization)."
  [email1 email2]
  (let [n1 (normalize-full email1)
        n2 (normalize-full email2)]
    (and n1 n2 (= n1 n2))))
