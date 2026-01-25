;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-version
  "Semantic versioning (SemVer 2.0.0) parsing and comparison."
  (:require [clojure.string :as str]))

(defrecord SemanticVersion [major minor patch prerelease build-metadata])

(def ^:private semver-pattern
  #"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$")

(defn parse
  "Parse a semantic version string.
   Returns {:ok SemanticVersion} or {:error message}."
  [version-string]
  (cond
    (str/blank? version-string)
    {:error "empty_version"}

    :else
    (if-let [matches (re-matches semver-pattern version-string)]
      (let [[_ major-str minor-str patch-str prerelease build-metadata] matches]
        {:ok (->SemanticVersion
              (Long/parseLong major-str)
              (Long/parseLong minor-str)
              (Long/parseLong patch-str)
              prerelease
              build-metadata)})
      {:error "invalid_semver_format"})))

(defn valid?
  "Check if string is a valid semantic version."
  [version-string]
  (contains? (parse version-string) :ok))

(defn parse!
  "Parse semantic version. Throws ex-info on error."
  [version-string]
  (let [result (parse version-string)]
    (if (:ok result)
      (:ok result)
      (throw (ex-info (:error result) {:type :invalid-version :input version-string})))))

(defn format-version
  "Format a SemanticVersion as string."
  [version]
  (str (:major version) "." (:minor version) "." (:patch version)
       (when (:prerelease version) (str "-" (:prerelease version)))
       (when (:build-metadata version) (str "+" (:build-metadata version)))))

(defn- compare-prerelease-identifiers
  "Compare two prerelease identifiers."
  [id-a id-b]
  (let [num-a (try (Long/parseLong id-a) (catch NumberFormatException _ nil))
        num-b (try (Long/parseLong id-b) (catch NumberFormatException _ nil))]
    (cond
      (and num-a num-b) (compare num-a num-b)
      num-a -1  ; numeric < alphanumeric
      num-b 1   ; alphanumeric > numeric
      :else (compare id-a id-b))))

(defn- compare-prerelease
  "Compare two prerelease strings."
  [pre-a pre-b]
  (cond
    (and (nil? pre-a) (nil? pre-b)) 0
    (nil? pre-a) 1   ; no prerelease > with prerelease
    (nil? pre-b) -1  ; with prerelease < no prerelease
    :else
    (let [ids-a (str/split pre-a #"\.")
          ids-b (str/split pre-b #"\.")]
      (loop [a-remaining ids-a
             b-remaining ids-b]
        (cond
          (and (empty? a-remaining) (empty? b-remaining)) 0
          (empty? a-remaining) -1  ; fewer fields < more fields
          (empty? b-remaining) 1   ; more fields > fewer fields
          :else
          (let [cmp (compare-prerelease-identifiers (first a-remaining) (first b-remaining))]
            (if (not= cmp 0)
              cmp
              (recur (rest a-remaining) (rest b-remaining)))))))))

(defn compare-versions
  "Compare two semantic versions. Returns -1, 0, or 1.
   Build metadata is ignored per SemVer spec."
  [version-a version-b]
  (let [major-cmp (compare (:major version-a) (:major version-b))]
    (if (not= major-cmp 0)
      major-cmp
      (let [minor-cmp (compare (:minor version-a) (:minor version-b))]
        (if (not= minor-cmp 0)
          minor-cmp
          (let [patch-cmp (compare (:patch version-a) (:patch version-b))]
            (if (not= patch-cmp 0)
              patch-cmp
              (compare-prerelease (:prerelease version-a) (:prerelease version-b)))))))))

(defn compare-version-strings
  "Compare two version strings. Returns -1, 0, 1, or nil if invalid."
  [str-a str-b]
  (let [parsed-a (:ok (parse str-a))
        parsed-b (:ok (parse str-b))]
    (when (and parsed-a parsed-b)
      (compare-versions parsed-a parsed-b))))

(defn newer?
  "Check if version-a is newer than version-b."
  [version-a version-b]
  (pos? (compare-versions version-a version-b)))

(defn older?
  "Check if version-a is older than version-b."
  [version-a version-b]
  (neg? (compare-versions version-a version-b)))

(defn equal?
  "Check if two versions are equal (ignoring build metadata)."
  [version-a version-b]
  (zero? (compare-versions version-a version-b)))

(defn compatible?
  "Check if two versions are compatible (same major version, minor >= other).
   For major version 0, requires same minor version."
  [version-a version-b]
  (if (zero? (:major version-a))
    (= (:minor version-a) (:minor version-b))
    (= (:major version-a) (:major version-b))))

(defn satisfies?
  "Check if version satisfies a simple constraint.
   Constraint formats: '=1.2.3', '>1.2.3', '>=1.2.3', '<1.2.3', '<=1.2.3', '^1.2.3', '~1.2.3'"
  [version constraint-string]
  (let [constraint-string (str/trim constraint-string)]
    (cond
      (str/starts-with? constraint-string ">=")
      (when-let [constraint (:ok (parse (subs constraint-string 2)))]
        (>= (compare-versions version constraint) 0))

      (str/starts-with? constraint-string "<=")
      (when-let [constraint (:ok (parse (subs constraint-string 2)))]
        (<= (compare-versions version constraint) 0))

      (str/starts-with? constraint-string ">")
      (when-let [constraint (:ok (parse (subs constraint-string 1)))]
        (pos? (compare-versions version constraint)))

      (str/starts-with? constraint-string "<")
      (when-let [constraint (:ok (parse (subs constraint-string 1)))]
        (neg? (compare-versions version constraint)))

      (str/starts-with? constraint-string "=")
      (when-let [constraint (:ok (parse (subs constraint-string 1)))]
        (equal? version constraint))

      (str/starts-with? constraint-string "^")
      (when-let [constraint (:ok (parse (subs constraint-string 1)))]
        (and (>= (compare-versions version constraint) 0)
             (= (:major version) (:major constraint))))

      (str/starts-with? constraint-string "~")
      (when-let [constraint (:ok (parse (subs constraint-string 1)))]
        (and (>= (compare-versions version constraint) 0)
             (= (:major version) (:major constraint))
             (= (:minor version) (:minor constraint))))

      :else
      (when-let [constraint (:ok (parse constraint-string))]
        (equal? version constraint)))))

(defn increment-major
  "Increment major version, reset minor and patch to 0."
  [version]
  (->SemanticVersion (inc (:major version)) 0 0 nil nil))

(defn increment-minor
  "Increment minor version, reset patch to 0."
  [version]
  (->SemanticVersion (:major version) (inc (:minor version)) 0 nil nil))

(defn increment-patch
  "Increment patch version."
  [version]
  (->SemanticVersion (:major version) (:minor version) (inc (:patch version)) nil nil))

(defn with-prerelease
  "Set prerelease identifier."
  [version prerelease]
  (->SemanticVersion (:major version) (:minor version) (:patch version)
                     prerelease (:build-metadata version)))

(defn with-build-metadata
  "Set build metadata."
  [version build-metadata]
  (->SemanticVersion (:major version) (:minor version) (:patch version)
                     (:prerelease version) build-metadata))

(defn is-prerelease?
  "Check if version is a prerelease."
  [version]
  (some? (:prerelease version)))

(defn is-stable?
  "Check if version is stable (major > 0 and no prerelease)."
  [version]
  (and (pos? (:major version))
       (nil? (:prerelease version))))
