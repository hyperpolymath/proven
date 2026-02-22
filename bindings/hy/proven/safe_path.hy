; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafePath - Filesystem traversal prevention.
;;
;; All functions delegate to libproven via FFI.
;; Returns None on error. NEVER reimplements logic.

(import proven.ffi [get-lib ok? encode-str decode-string-result])


(defn has-traversal [path-str]
  "Check if path contains directory traversal sequences ('..').
  Returns True if traversal detected, False if safe, None on error.

  Example:
    (has-traversal \"../etc/passwd\")    ; => True
    (has-traversal \"safe/file.txt\")    ; => False
  "
  (setv #(b n) (encode-str path-str))
  (setv result (.proven_path_has_traversal (get-lib) b n))
  (when (ok? result.status)
    (return result.value))
  None)


(defn sanitize-filename [filename]
  "Sanitize a filename by removing dangerous characters. Returns None on error.

  Example:
    (sanitize-filename \"../../etc/passwd\")  ; => \"etc_passwd\" (or similar safe name)
  "
  (setv #(b n) (encode-str filename))
  (setv result (.proven_path_sanitize_filename (get-lib) b n))
  (decode-string-result result))


(defn is-safe-path [path-str]
  "Convenience: returns True if path has no traversal. Returns None on error.

  Example:
    (is-safe-path \"data/file.txt\")     ; => True
    (is-safe-path \"../../../etc/shadow\")  ; => False
  "
  (setv traversal (has-traversal path-str))
  (when (is traversal None)
    (return None))
  (not traversal))
