;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; FFI bridge to libproven via JavaScript interop.
;;
;; ClojureScript compiles to JavaScript, so this module uses js interop
;; to call the Deno/Node JS binding which in turn calls the native
;; libproven shared library. This is transitive FFI:
;;
;;   ClojureScript -> JS interop -> Deno FFI -> libproven.so -> Idris 2
;;
;; This module MUST NOT reimplement any logic. It is a pure FFI bridge.

(ns proven.ffi
  "FFI bridge to libproven. Loads the JS binding and provides
   raw access to all proven FFI functions.")


;; ---------------------------------------------------------------------------
;; Status codes
;; ---------------------------------------------------------------------------

(def PROVEN-OK                     0)
(def PROVEN-ERR-NULL-POINTER      -1)
(def PROVEN-ERR-INVALID-ARGUMENT  -2)
(def PROVEN-ERR-OVERFLOW          -3)
(def PROVEN-ERR-UNDERFLOW         -4)
(def PROVEN-ERR-DIVISION-BY-ZERO  -5)
(def PROVEN-ERR-PARSE-FAILURE     -6)
(def PROVEN-ERR-VALIDATION-FAILED -7)
(def PROVEN-ERR-OUT-OF-BOUNDS     -8)
(def PROVEN-ERR-ENCODING-ERROR    -9)
(def PROVEN-ERR-ALLOCATION-FAILED -10)
(def PROVEN-ERR-NOT-IMPLEMENTED   -99)


;; ---------------------------------------------------------------------------
;; Library loading
;; ---------------------------------------------------------------------------

(defonce ^:private lib-atom (atom nil))

(defn- try-require
  "Attempt to load the JS proven module via require or dynamic import."
  []
  (try
    ;; Node.js / shadow-cljs :node-library target
    (js/require "../deno/src/ffi")
    (catch :default _e1
      (try
        (js/require "proven")
        (catch :default _e2
          nil)))))

(defn get-lib
  "Return the loaded JS proven module.
   Loads it on first call. Returns nil if unavailable."
  []
  (when (nil? @lib-atom)
    (reset! lib-atom (try-require)))
  @lib-atom)


;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn ok?
  "Return true if status code indicates success."
  [status]
  (zero? status))

(defn status->error
  "Convert a status code to a human-readable error string."
  [status]
  (case status
    0   "Ok"
    -1  "Null pointer"
    -2  "Invalid argument"
    -3  "Integer overflow"
    -4  "Integer underflow"
    -5  "Division by zero"
    -6  "Parse failure"
    -7  "Validation failed"
    -8  "Out of bounds"
    -9  "Encoding error"
    -10 "Allocation failed"
    -99 "Not implemented"
    (str "Unknown error (" status ")")))
