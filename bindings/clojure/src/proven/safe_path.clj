;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-path
  "Safe filesystem path operations with traversal protection."
  (:require [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files Paths]))

(def ^:private dangerous-chars
  #{\< \> \: \" \/ \\ \| \? \*})

(defn has-traversal?
  "Check if a path contains directory traversal sequences."
  [path]
  (or (str/includes? path "..")
      (str/starts-with? path "~")))

(defn safe-path?
  "Check if a path is safe (no traversal)."
  [path]
  (not (has-traversal? path)))

(defn sanitize-filename
  "Sanitize a filename by removing dangerous characters."
  [filename]
  (-> filename
      (str/replace ".." "_")
      (str/replace #"[<>:\"/\\|?*]" "_")
      (str/replace "\u0000" "_")
      (str/replace #"^[.\s]+" "")
      (str/replace #"[.\s]+$" "")))

(defn join
  "Safely join path components, checking for traversal.
   Returns {:ok path} or {:error msg}."
  [base & parts]
  (let [sep File/separator]
    (loop [path (str/replace base (re-pattern (str sep "+$")) "")
           remaining parts]
      (if (empty? remaining)
        {:ok path}
        (let [part (first remaining)]
          (if (has-traversal? part)
            {:error "traversal_detected"}
            (let [safe-part (sanitize-filename part)]
              (recur (str path sep safe-part)
                     (rest remaining)))))))))

(defn resolve-within
  "Resolve path and verify it's within a base directory.
   Returns {:ok path} or {:error msg}."
  [base-path user-path]
  (try
    (let [base (.toRealPath (Paths/get base-path (into-array String [])))
          full (.normalize (.resolve base user-path))
          resolved (if (Files/exists full (into-array java.nio.file.LinkOption []))
                     (.toRealPath full (into-array java.nio.file.LinkOption []))
                     full)]
      (if (.startsWith resolved base)
        {:ok (str resolved)}
        {:error "path_escapes_base"}))
    (catch Exception _
      {:error "resolution_failed"})))

(defn safe-basename
  "Get safe basename (strip directory components)."
  [path]
  (sanitize-filename (.getName (File. path))))

(defn has-allowed-extension?
  "Check if filename has an allowed extension."
  [filename allowed-extensions]
  (let [last-dot (str/last-index-of filename ".")]
    (if (or (nil? last-dot) (= last-dot (dec (count filename))))
      false
      (let [ext (str/lower-case (subs filename (inc last-dot)))]
        (some #(= (str/lower-case %) ext) allowed-extensions)))))

(defn get-extension
  "Get file extension (lowercase). Returns nil if no extension."
  [filename]
  (let [last-dot (str/last-index-of filename ".")]
    (when (and last-dot (< last-dot (dec (count filename))))
      (str/lower-case (subs filename (inc last-dot))))))

(defn absolute-path?
  "Check if path is absolute."
  [path]
  (.isAbsolute (Paths/get path (into-array String []))))

(defn readable?
  "Check if path exists and is readable."
  [path]
  (try
    (let [p (Paths/get path (into-array String []))]
      (and (Files/exists p (into-array java.nio.file.LinkOption []))
           (Files/isReadable p)))
    (catch Exception _ false)))

(defn regular-file?
  "Check if path is a regular file (not symlink, device, etc)."
  [path]
  (try
    (Files/isRegularFile (Paths/get path (into-array String []))
                         (into-array java.nio.file.LinkOption []))
    (catch Exception _ false)))

(defn directory?
  "Check if path is a directory."
  [path]
  (try
    (Files/isDirectory (Paths/get path (into-array String []))
                       (into-array java.nio.file.LinkOption []))
    (catch Exception _ false)))

(defn symlink?
  "Check if path is a symbolic link."
  [path]
  (try
    (Files/isSymbolicLink (Paths/get path (into-array String [])))
    (catch Exception _ false)))

(defn safe-mkdir
  "Create directory safely (checks for traversal first).
   Returns true on success, false on failure."
  [base name]
  (if (has-traversal? name)
    false
    (try
      (let [safe-name (sanitize-filename name)
            full-path (Paths/get base (into-array String [safe-name]))]
        (Files/createDirectories full-path (into-array java.nio.file.attribute.FileAttribute []))
        true)
      (catch Exception _ false))))

(defn normalize-path
  "Normalize a path by resolving redundant separators."
  [path]
  (str (.normalize (Paths/get path (into-array String [])))))
