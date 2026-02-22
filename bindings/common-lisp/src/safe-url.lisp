;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeURL - Thin CFFI wrapper for libproven URL parsing and validation.
;;;; All computation delegates to Idris 2 via the Zig FFI layer.

(in-package #:proven)

;;; ============================================================================
;;; C struct types matching the Zig FFI ABI
;;; ============================================================================

;;; UrlComponents: { scheme, scheme_len, host, host_len, port, has_port,
;;;                  path, path_len, query, query_len, fragment, fragment_len }
(defcstruct url-components-ffi
  (scheme :pointer)
  (scheme-len :size)
  (host :pointer)
  (host-len :size)
  (port :uint16)
  (has-port :boolean)
  (path :pointer)
  (path-len :size)
  (query :pointer)
  (query-len :size)
  (fragment :pointer)
  (fragment-len :size))

;;; UrlResult: { status: i32, components: UrlComponents }
(defcstruct url-result-ffi
  (status :int32)
  (components (:struct url-components-ffi)))

;;; ============================================================================
;;; FFI declarations
;;; ============================================================================

(defcfun ("proven_url_parse" %url-parse) (:struct url-result-ffi)
  (ptr :pointer) (len :size))

(defcfun ("proven_url_free" %url-free) :void
  (components :pointer))

;;; ============================================================================
;;; Internal helpers
;;; ============================================================================

(defun %extract-url-string (ptr len)
  "Extract a Lisp string from a foreign pointer with given length.
   Returns NIL if the pointer is null or length is zero."
  (if (and (not (null-pointer-p ptr)) (plusp len))
      (foreign-string-to-lisp ptr :count len :encoding :utf-8)
      nil))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun url-parse (url-string)
  "Parse a URL string into its components via libproven.
   Returns (values plist ok-p) where plist has keys :SCHEME :HOST :PORT
   :PATH :QUERY :FRAGMENT, or (values nil nil) on error.
   :PORT is NIL if not explicitly specified in the URL."
  (with-foreign-string-buf (ptr len url-string)
    (let ((result (%url-parse ptr len)))
      (if (zerop (getf result 'status))
          (let* ((components (getf result 'components))
                 (scheme (%extract-url-string
                          (getf components 'scheme)
                          (getf components 'scheme-len)))
                 (host (%extract-url-string
                        (getf components 'host)
                        (getf components 'host-len)))
                 (port (when (getf components 'has-port)
                         (getf components 'port)))
                 (path (%extract-url-string
                        (getf components 'path)
                        (getf components 'path-len)))
                 (query (%extract-url-string
                         (getf components 'query)
                         (getf components 'query-len)))
                 (fragment (%extract-url-string
                            (getf components 'fragment)
                            (getf components 'fragment-len))))
            ;; Free the C-allocated component strings by writing the
            ;; components struct into a foreign object and calling url_free
            (with-foreign-object (comp-ptr '(:struct url-components-ffi))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'scheme)
                    (getf components 'scheme))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'scheme-len)
                    (getf components 'scheme-len))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'host)
                    (getf components 'host))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'host-len)
                    (getf components 'host-len))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'port)
                    (getf components 'port))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'has-port)
                    (getf components 'has-port))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'path)
                    (getf components 'path))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'path-len)
                    (getf components 'path-len))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'query)
                    (getf components 'query))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'query-len)
                    (getf components 'query-len))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'fragment)
                    (getf components 'fragment))
              (setf (foreign-slot-value comp-ptr '(:struct url-components-ffi) 'fragment-len)
                    (getf components 'fragment-len))
              (%url-free comp-ptr))
            (values (list :scheme scheme
                          :host host
                          :port port
                          :path path
                          :query query
                          :fragment fragment)
                    t))
          (values nil nil)))))
