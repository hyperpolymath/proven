;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; Proven FFI - CFFI library loading and foreign type definitions.
;;;; All functions delegate to libproven (Idris 2 + Zig).

(in-package #:proven)

;;; ============================================================================
;;; Library loading
;;; ============================================================================

(define-foreign-library libproven
  (:unix "libproven.so")
  (:darwin "libproven.dylib")
  (:windows "proven.dll")
  (t (:default "libproven")))

(use-foreign-library libproven)

;;; ============================================================================
;;; C struct types matching the Zig FFI ABI
;;; ============================================================================

;;; IntResult: { status: i32, value: i64 }
(defcstruct int-result
  (status :int32)
  (value :int64))

;;; BoolResult: { status: i32, value: bool }
(defcstruct bool-result
  (status :int32)
  (value :boolean))

;;; StringResult: { status: i32, value: pointer, length: size_t }
(defcstruct string-result
  (status :int32)
  (value :pointer)
  (length :size))

;;; FloatResult: { status: i32, value: f64 }
(defcstruct float-result
  (status :int32)
  (value :double))

;;; ============================================================================
;;; Runtime lifecycle FFI declarations
;;; ============================================================================

(defcfun ("proven_init" %proven-init) :int32)
(defcfun ("proven_deinit" %proven-deinit) :void)
(defcfun ("proven_is_initialized" %proven-is-initialized) :boolean)
(defcfun ("proven_ffi_abi_version" %proven-ffi-abi-version) :uint32)
(defcfun ("proven_free_string" %proven-free-string) :void (ptr :pointer))

;;; Version info
(defcfun ("proven_version_major" %proven-version-major) :uint32)
(defcfun ("proven_version_minor" %proven-version-minor) :uint32)
(defcfun ("proven_version_patch" %proven-version-patch) :uint32)
(defcfun ("proven_module_count" %proven-module-count) :uint32)

;;; ============================================================================
;;; Helper functions
;;; ============================================================================

(defun init ()
  "Initialize the Proven runtime. Returns 0 on success."
  (%proven-init))

(defun deinit ()
  "Shut down the Proven runtime."
  (%proven-deinit))

(defun initialized-p ()
  "Return T if the Proven runtime is initialized."
  (%proven-is-initialized))

(defun ffi-abi-version ()
  "Return the FFI ABI version number."
  (%proven-ffi-abi-version))

(defun free-proven-string (ptr)
  "Free a string allocated by libproven."
  (%proven-free-string ptr))

(defun version-major ()
  "Return the library major version."
  (%proven-version-major))

(defun version-minor ()
  "Return the library minor version."
  (%proven-version-minor))

(defun version-patch ()
  "Return the library patch version."
  (%proven-version-patch))

(defun module-count ()
  "Return the number of modules in libproven."
  (%proven-module-count))

;;; ============================================================================
;;; Marshaling helpers
;;; ============================================================================

(defun string-to-foreign-buf (str)
  "Convert a Lisp string to a foreign UTF-8 buffer. Returns (values pointer length).
   Caller is responsible for freeing the pointer with CFFI:FOREIGN-FREE."
  (let* ((octets (babel:string-to-octets str :encoding :utf-8))
         (len (length octets))
         (buf (foreign-alloc :uint8 :count len)))
    (loop for i below len
          do (setf (mem-aref buf :uint8 i) (aref octets i)))
    (values buf len)))

(defmacro with-foreign-string-buf ((ptr-var len-var str) &body body)
  "Bind PTR-VAR and LEN-VAR to a foreign UTF-8 buffer of STR, execute BODY,
   then free the buffer."
  `(multiple-value-bind (,ptr-var ,len-var) (string-to-foreign-buf ,str)
     (unwind-protect (progn ,@body)
       (foreign-free ,ptr-var))))

(defun extract-int-result (result)
  "Extract an int-result struct. Returns (values value ok-p) where OK-P is T on status 0."
  (let ((status (getf result 'status))
        (value (getf result 'value)))
    (if (zerop status)
        (values value t)
        (values nil nil))))

(defun extract-bool-result (result)
  "Extract a bool-result struct. Returns (values value ok-p)."
  (let ((status (getf result 'status))
        (value (getf result 'value)))
    (if (zerop status)
        (values value t)
        (values nil nil))))

(defun extract-float-result (result)
  "Extract a float-result struct. Returns (values value ok-p)."
  (let ((status (getf result 'status))
        (value (getf result 'value)))
    (if (zerop status)
        (values value t)
        (values nil nil))))

(defun extract-string-result (result)
  "Extract a string-result struct. Returns (values string ok-p).
   Frees the C-allocated string after copying."
  (let ((status (getf result 'status))
        (ptr (getf result 'value))
        (length (getf result 'length)))
    (if (and (zerop status) (not (null-pointer-p ptr)))
        (let ((str (foreign-string-to-lisp ptr :count length :encoding :utf-8)))
          (%proven-free-string ptr)
          (values str t))
        (values nil nil))))
