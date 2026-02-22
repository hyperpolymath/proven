;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-url.el - Safe URL operation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any URL logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides URL parsing, URL encoding/decoding, and HTTP header utilities.
;; Every function returns nil on error instead of signaling.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Safe URL operations
;;; ============================================================================

(defun proven-safe-url-parse (url-string)
  "Parse URL-STRING into an alist of components.
Returns an alist with keys `scheme', `host', `port', `path', `query',
`fragment', or nil if parsing fails.
Calls proven_url_parse via FFI."
  (condition-case nil
      (let ((result (proven--ffi-url-parse url-string)))
        (when (and (consp result) (= (car result) 0))
          (cdr result)))
    (error nil)))

(defun proven-safe-url-encode (str)
  "URL-encode STR per RFC 3986 (percent encoding).
Returns the encoded string, or nil on error.
Calls proven_http_url_encode via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result (proven--ffi-http-url-encode str))
    (error nil)))

(defun proven-safe-url-decode (str)
  "URL-decode a percent-encoded STR.
Returns the decoded string, or nil on error.
Calls proven_http_url_decode via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result (proven--ffi-http-url-decode str))
    (error nil)))

(provide 'proven-safe-url)

;;; proven-safe-url.el ends here
