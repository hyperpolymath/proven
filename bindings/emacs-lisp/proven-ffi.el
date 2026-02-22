;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-ffi.el - FFI declarations for libproven via Emacs dynamic modules.
;; All computation is performed in Idris 2 via the Zig FFI layer.
;; This file provides low-level C function bindings. Do NOT reimplement logic.
;;
;; Requires Emacs 28+ with dynamic module support (--with-modules).
;; The native C module (proven-module.so/dylib) bridges Emacs Lisp to libproven.

;;; Commentary:
;;
;; This file loads the compiled dynamic module `proven-module' which exposes
;; libproven's C ABI as Emacs Lisp functions.  Each `proven--ffi-*' function
;; is a thin trampoline into the Idris 2 + Zig verified implementation.
;;
;; C ABI result types:
;;   IntResult:    { int32_t status; int64_t value; }
;;   BoolResult:   { int32_t status; bool value; }
;;   StringResult: { int32_t status; char* ptr; size_t len; }
;;   FloatResult:  { int32_t status; double value; }
;;
;; Status codes:
;;   PROVEN_OK                    =   0
;;   PROVEN_ERR_NULL_POINTER      =  -1
;;   PROVEN_ERR_INVALID_ARGUMENT  =  -2
;;   PROVEN_ERR_OVERFLOW          =  -3
;;   PROVEN_ERR_UNDERFLOW         =  -4
;;   PROVEN_ERR_DIVISION_BY_ZERO  =  -5
;;   PROVEN_ERR_PARSE_FAILURE     =  -6
;;   PROVEN_ERR_VALIDATION_FAILED =  -7
;;   PROVEN_ERR_OUT_OF_BOUNDS     =  -8
;;   PROVEN_ERR_ENCODING_ERROR    =  -9
;;   PROVEN_ERR_ALLOCATION_FAILED = -10
;;   PROVEN_ERR_NOT_IMPLEMENTED   = -99

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Module loading
;;; ============================================================================

(defvar proven-ffi--module-loaded nil
  "Non-nil if the proven dynamic module has been loaded.")

(defvar proven-ffi--library-path nil
  "Path to the proven dynamic module (proven-module.so or proven-module.dylib).
If nil, `module-load' searches the standard module path.")

(defun proven-ffi--find-module ()
  "Locate the proven dynamic module, searching common paths.
Returns the path to the module file, or nil if not found."
  (let ((module-name (cond
                      ((eq system-type 'gnu/linux) "proven-module.so")
                      ((eq system-type 'darwin) "proven-module.dylib")
                      ((eq system-type 'windows-nt) "proven-module.dll")
                      (t "proven-module.so")))
        (search-dirs (list
                      (when load-file-name (file-name-directory load-file-name))
                      (expand-file-name "proven" user-emacs-directory)
                      "/usr/local/lib/proven/"
                      "/usr/lib/proven/")))
    (cl-loop for dir in search-dirs
             when dir
             for path = (expand-file-name module-name dir)
             when (file-exists-p path)
             return path)))

(defun proven-ffi-load-module (&optional path)
  "Load the proven dynamic module from PATH.
If PATH is nil, search standard locations.  The dynamic module provides
all `proven--ffi-*' functions that call into libproven.
Returns non-nil on success, nil on failure."
  (when proven-ffi--module-loaded
    (message "proven-ffi: module already loaded")
    (cl-return-from proven-ffi-load-module t))
  (let ((module-path (or path
                        proven-ffi--library-path
                        (proven-ffi--find-module))))
    (if module-path
        (condition-case err
            (progn
              (module-load module-path)
              (setq proven-ffi--module-loaded t)
              (message "proven-ffi: loaded module from %s" module-path)
              t)
          (error
           (message "proven-ffi: failed to load module from %s: %s"
                    module-path (error-message-string err))
           nil))
      (message "proven-ffi: module not found; FFI functions unavailable")
      nil)))

;;; ============================================================================
;;; Status code constants
;;; ============================================================================

(defconst proven-ffi-OK 0
  "Operation succeeded.")

(defconst proven-ffi-ERR-NULL-POINTER -1
  "Null pointer error.")

(defconst proven-ffi-ERR-INVALID-ARGUMENT -2
  "Invalid argument error.")

(defconst proven-ffi-ERR-OVERFLOW -3
  "Arithmetic overflow error.")

(defconst proven-ffi-ERR-UNDERFLOW -4
  "Arithmetic underflow error.")

(defconst proven-ffi-ERR-DIVISION-BY-ZERO -5
  "Division by zero error.")

(defconst proven-ffi-ERR-PARSE-FAILURE -6
  "Parse failure error.")

(defconst proven-ffi-ERR-VALIDATION-FAILED -7
  "Validation failed error.")

(defconst proven-ffi-ERR-OUT-OF-BOUNDS -8
  "Out of bounds error.")

(defconst proven-ffi-ERR-ENCODING-ERROR -9
  "Encoding error.")

(defconst proven-ffi-ERR-ALLOCATION-FAILED -10
  "Memory allocation failed error.")

(defconst proven-ffi-ERR-NOT-IMPLEMENTED -99
  "Not implemented error.")

;;; ============================================================================
;;; Result extraction helpers
;;; ============================================================================

(defun proven-ffi--extract-int-result (result)
  "Extract value from an IntResult cons cell (STATUS . VALUE).
Returns VALUE on success (status 0), nil on error.
RESULT is a cons cell as returned by the dynamic module."
  (when (and (consp result) (= (car result) 0))
    (cdr result)))

(defun proven-ffi--extract-bool-result (result)
  "Extract value from a BoolResult cons cell (STATUS . VALUE).
Returns the boolean VALUE on success (status 0), nil on error.
RESULT is a cons cell as returned by the dynamic module."
  (when (and (consp result) (= (car result) 0))
    (cdr result)))

(defun proven-ffi--extract-float-result (result)
  "Extract value from a FloatResult cons cell (STATUS . VALUE).
Returns VALUE (a float) on success (status 0), nil on error.
RESULT is a cons cell as returned by the dynamic module."
  (when (and (consp result) (= (car result) 0))
    (cdr result)))

(defun proven-ffi--extract-string-result (result)
  "Extract value from a StringResult cons cell (STATUS . STRING).
Returns STRING on success (status 0), nil on error.
The dynamic module handles freeing the C-allocated string after copying.
RESULT is a cons cell as returned by the dynamic module."
  (when (and (consp result) (= (car result) 0))
    (cdr result)))

(defun proven-ffi--status-name (status)
  "Return a human-readable name for a numeric STATUS code."
  (pcase status
    (0   "OK")
    (-1  "ERR_NULL_POINTER")
    (-2  "ERR_INVALID_ARGUMENT")
    (-3  "ERR_OVERFLOW")
    (-4  "ERR_UNDERFLOW")
    (-5  "ERR_DIVISION_BY_ZERO")
    (-6  "ERR_PARSE_FAILURE")
    (-7  "ERR_VALIDATION_FAILED")
    (-8  "ERR_OUT_OF_BOUNDS")
    (-9  "ERR_ENCODING_ERROR")
    (-10 "ERR_ALLOCATION_FAILED")
    (-99 "ERR_NOT_IMPLEMENTED")
    (_   (format "UNKNOWN(%d)" status))))

;;; ============================================================================
;;; Stub declarations for dynamic module functions
;;; ============================================================================
;;
;; The following `declare-function' forms document the functions provided
;; by the compiled dynamic module (proven-module.so).  They do not exist
;; until `proven-ffi-load-module' is called.  Each function is a direct
;; trampoline into the corresponding libproven C function.
;;
;; Lifecycle:
;;   proven--ffi-init          () -> int
;;   proven--ffi-deinit        () -> nil
;;   proven--ffi-is-initialized() -> bool
;;   proven--ffi-abi-version   () -> int
;;   proven--ffi-version-major () -> int
;;   proven--ffi-version-minor () -> int
;;   proven--ffi-version-patch () -> int
;;   proven--ffi-module-count  () -> int
;;
;; SafeMath:
;;   proven--ffi-math-add-checked (a b) -> (status . value)
;;   proven--ffi-math-sub-checked (a b) -> (status . value)
;;   proven--ffi-math-mul-checked (a b) -> (status . value)
;;   proven--ffi-math-div         (a b) -> (status . value)
;;   proven--ffi-math-mod         (a b) -> (status . value)
;;   proven--ffi-math-abs-safe    (n)   -> (status . value)
;;   proven--ffi-math-clamp       (lo hi value) -> int
;;   proven--ffi-math-pow-checked (base exp) -> (status . value)
;;
;; SafeString:
;;   proven--ffi-string-is-valid-utf8 (str) -> (status . bool)
;;   proven--ffi-string-escape-sql    (str) -> (status . string)
;;   proven--ffi-string-escape-html   (str) -> (status . string)
;;   proven--ffi-string-escape-js     (str) -> (status . string)
;;
;; SafePath:
;;   proven--ffi-path-has-traversal      (str) -> (status . bool)
;;   proven--ffi-path-sanitize-filename  (str) -> (status . string)
;;
;; SafeEmail:
;;   proven--ffi-email-is-valid (str) -> (status . bool)
;;
;; SafeUrl:
;;   proven--ffi-url-parse (str) -> (status . alist) or nil
;;
;; SafeCrypto:
;;   proven--ffi-crypto-constant-time-eq (a b) -> (status . bool)
;;   proven--ffi-crypto-random-bytes     (n)   -> string or nil
;;
;; SafeJson:
;;   proven--ffi-json-is-valid (str) -> (status . bool)
;;   proven--ffi-json-get-type (str) -> int
;;
;; SafeFloat:
;;   proven--ffi-float-div       (a b) -> (status . float)
;;   proven--ffi-float-is-finite (x)   -> bool
;;   proven--ffi-float-is-nan    (x)   -> bool
;;   proven--ffi-float-sqrt      (x)   -> (status . float)
;;   proven--ffi-float-ln        (x)   -> (status . float)

(provide 'proven-ffi)

;;; proven-ffi.el ends here
