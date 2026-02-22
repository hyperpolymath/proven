;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafePath - Filesystem traversal prevention.
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error. NEVER reimplements logic.

(ns proven.safe-path
  "Safe path operations: directory traversal prevention.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


(defn has-traversal?
  "Check if path contains directory traversal sequences ('..').
  Returns true if traversal detected, false if safe, nil on error.

  Example:
    (has-traversal? \"../etc/passwd\")  ;=> true
    (has-traversal? \"safe/file.txt\")  ;=> false"
  [path-str]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_path_has_traversal lib path-str (count path-str))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn sanitize-filename
  "Sanitize a filename by removing dangerous characters. Returns nil on error.

  Example:
    (sanitize-filename \"../../etc/passwd\")  ;=> sanitized name"
  [filename]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_path_sanitize_filename lib filename (count filename))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-path?
  "Convenience: returns true if path has no traversal. Returns nil on error.

  Example:
    (safe-path? \"data/file.txt\")          ;=> true
    (safe-path? \"../../../etc/shadow\")    ;=> false"
  [path-str]
  (when-let [traversal (has-traversal? path-str)]
    (not traversal)))
