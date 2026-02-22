;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven.el - Emacs Lisp bindings for libproven (formally verified safety library).
;; All computation is performed in Idris 2 via the Zig FFI layer.
;; This package is a thin wrapper; it does NOT reimplement any logic.

;;; Commentary:
;;
;; `proven' provides safe arithmetic, string escaping, path sanitization,
;; email/URL validation, JSON validation, cryptographic primitives, and more,
;; backed by the formally verified Idris 2 core of libproven.
;;
;; Usage:
;;   (require 'proven)
;;   (proven-init)
;;   (proven-safe-math-add 1000000000000 2000000000000)  ;=> 3000000000000
;;   (proven-safe-math-div 10 0)                          ;=> nil (safe!)
;;   (proven-deinit)
;;
;; All functions return nil on error instead of signaling.  This is
;; intentional: proven is about total safety, and nil propagation is
;; the Emacs Lisp idiom for safe failure.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Package metadata
;;; ============================================================================

(defgroup proven nil
  "Emacs bindings for libproven, a formally verified safety library."
  :group 'tools
  :prefix "proven-")

(defconst proven-version "0.9.0"
  "Version of the proven Emacs Lisp binding.")

;;; ============================================================================
;;; Runtime lifecycle
;;; ============================================================================

(defun proven-init ()
  "Initialize the Proven runtime (Idris 2 + Zig FFI).
Must be called before any other proven function.
Returns non-nil on success, nil on failure."
  (when (proven-ffi-load-module)
    (condition-case nil
        (let ((status (proven--ffi-init)))
          (= status 0))
      (error nil))))

(defun proven-deinit ()
  "Shut down the Proven runtime.
Call when done using proven functions.  Safe to call if not initialized."
  (condition-case nil
      (proven--ffi-deinit)
    (error nil)))

(defun proven-initialized-p ()
  "Return non-nil if the Proven runtime is initialized."
  (condition-case nil
      (proven--ffi-is-initialized)
    (error nil)))

(defun proven-abi-version ()
  "Return the FFI ABI version number, or nil on error."
  (condition-case nil
      (proven--ffi-abi-version)
    (error nil)))

(defun proven-library-version ()
  "Return the libproven version as a string \"MAJOR.MINOR.PATCH\", or nil."
  (condition-case nil
      (format "%d.%d.%d"
              (proven--ffi-version-major)
              (proven--ffi-version-minor)
              (proven--ffi-version-patch))
    (error nil)))

(defun proven-module-count ()
  "Return the number of modules in libproven, or nil on error."
  (condition-case nil
      (proven--ffi-module-count)
    (error nil)))

;;; ============================================================================
;;; Feature loading
;;; ============================================================================

(require 'proven-safe-math)
(require 'proven-safe-string)
(require 'proven-safe-path)
(require 'proven-safe-email)
(require 'proven-safe-url)
(require 'proven-safe-crypto)
(require 'proven-safe-json)

(provide 'proven)

;;; proven.el ends here
