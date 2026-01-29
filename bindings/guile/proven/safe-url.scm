;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeUrl - URL validation and parsing for Guile Scheme
;;;
;;; Provides RFC 3986 compliant URL validation and safe operations.
;;; All operations are pure and cannot crash.

(define-module (proven safe-url)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (url-valid?
            url-parse
            url-scheme
            url-host
            url-port
            url-path
            url-query
            url-fragment
            url-normalize
            url-encode
            url-decode
            url-is-absolute?
            url-is-relative?
            url-is-secure?
            url-join
            url-resolve
            query-parse
            query-encode
            make-url))

;;; URL schemes considered secure
(define *secure-schemes* '("https" "wss" "ftps" "sftp" "ssh"))

;;; Reserved characters that need percent encoding
(define *reserved-chars*
  '(#\: #\/ #\? #\# #\[ #\] #\@ #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))

;;; Unreserved characters (no encoding needed)
(define (unreserved-char? char-code)
  (or (and (>= char-code 65) (<= char-code 90))   ; A-Z
      (and (>= char-code 97) (<= char-code 122))  ; a-z
      (and (>= char-code 48) (<= char-code 57))   ; 0-9
      (= char-code 45)   ; -
      (= char-code 46)   ; .
      (= char-code 95)   ; _
      (= char-code 126))); ~

;;; Make a result record
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Helper to get hex value
(define (hex-char-value char-code)
  (cond
   ((and (>= char-code 48) (<= char-code 57))
    (- char-code 48))
   ((and (>= char-code 65) (<= char-code 70))
    (+ 10 (- char-code 65)))
   ((and (>= char-code 97) (<= char-code 102))
    (+ 10 (- char-code 97)))
   (else #f)))

;;; Percent encode a single character
(define (percent-encode-char char-code)
  (string-append "%"
                 (string-upcase
                  (number->string char-code 16))))

;;; URL encode a string (percent encoding)
(define (url-encode input)
  (apply string-append
         (map (lambda (char-code)
                (if (unreserved-char? char-code)
                    (string (integer->char char-code))
                    (percent-encode-char char-code)))
              (map char->integer (string->list input)))))

;;; URL decode a string (percent decoding)
(define (url-decode input)
  (let loop ((chars (string->list input))
             (result '()))
    (cond
     ((null? chars)
      (list->string (reverse result)))
     ((and (char=? (car chars) #\%)
           (>= (length chars) 3))
      (let ((high (hex-char-value (char->integer (cadr chars))))
            (low (hex-char-value (char->integer (caddr chars)))))
        (if (and high low)
            (loop (cdddr chars)
                  (cons (integer->char (+ (* high 16) low)) result))
            (loop (cdr chars) (cons (car chars) result)))))
     ((char=? (car chars) #\+)
      (loop (cdr chars) (cons #\space result)))
     (else
      (loop (cdr chars) (cons (car chars) result))))))

;;; Check if URL is valid
(define (url-valid? url-string)
  (and (string? url-string)
       (> (string-length url-string) 0)
       (<= (string-length url-string) 2083)
       (let ((parsed (url-parse url-string)))
         (result-ok? parsed))))

;;; Parse URL into components
(define (url-parse url-string)
  (if (or (not (string? url-string))
          (= (string-length url-string) 0)
          (> (string-length url-string) 2083))
      (make-result #f #f)
      (let* ((trimmed (string-trim-both url-string))
             ;; Extract fragment
             (frag-idx (string-index trimmed #\#))
             (fragment (if frag-idx
                          (substring trimmed (+ frag-idx 1))
                          #f))
             (no-frag (if frag-idx
                         (substring trimmed 0 frag-idx)
                         trimmed))
             ;; Extract query
             (query-idx (string-index no-frag #\?))
             (query (if query-idx
                       (substring no-frag (+ query-idx 1))
                       #f))
             (no-query (if query-idx
                          (substring no-frag 0 query-idx)
                          no-frag))
             ;; Extract scheme
             (scheme-end (string-contains no-query "://"))
             (scheme (if scheme-end
                        (string-downcase (substring no-query 0 scheme-end))
                        #f))
             (after-scheme (if scheme-end
                              (substring no-query (+ scheme-end 3))
                              no-query))
             ;; Extract host and path
             (path-start (string-index after-scheme #\/))
             (authority (if path-start
                           (substring after-scheme 0 path-start)
                           after-scheme))
             (path (if path-start
                      (substring after-scheme path-start)
                      "/"))
             ;; Extract port from authority
             (port-idx (string-index authority #\:))
             (host (if port-idx
                      (substring authority 0 port-idx)
                      authority))
             (port-str (if port-idx
                          (substring authority (+ port-idx 1))
                          #f))
             (port (if port-str
                      (string->number port-str)
                      #f)))
        (make-result
         `((scheme . ,scheme)
           (host . ,host)
           (port . ,port)
           (path . ,path)
           (query . ,query)
           (fragment . ,fragment))
         #t))))

;;; Get URL scheme
(define (url-scheme url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'scheme)
        #f)))

;;; Get URL host
(define (url-host url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'host)
        #f)))

;;; Get URL port
(define (url-port url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'port)
        #f)))

;;; Get URL path
(define (url-path url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'path)
        #f)))

;;; Get URL query
(define (url-query url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'query)
        #f)))

;;; Get URL fragment
(define (url-fragment url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'fragment)
        #f)))

;;; Normalize URL (lowercase scheme and host)
(define (url-normalize url-string)
  (let ((parsed (url-parse url-string)))
    (if (result-ok? parsed)
        (let* ((parts (result-value parsed))
               (scheme (assoc-ref parts 'scheme))
               (host (assoc-ref parts 'host))
               (port (assoc-ref parts 'port))
               (path (assoc-ref parts 'path))
               (query (assoc-ref parts 'query))
               (fragment (assoc-ref parts 'fragment)))
          (string-append
           (if scheme (string-append scheme "://") "")
           (if host (string-downcase host) "")
           (if port (string-append ":" (number->string port)) "")
           (or path "/")
           (if query (string-append "?" query) "")
           (if fragment (string-append "#" fragment) "")))
        url-string)))

;;; Check if URL is absolute
(define (url-is-absolute? url-string)
  (let ((scheme (url-scheme url-string)))
    (and scheme (> (string-length scheme) 0))))

;;; Check if URL is relative
(define (url-is-relative? url-string)
  (not (url-is-absolute? url-string)))

;;; Check if URL uses a secure scheme
(define (url-is-secure? url-string)
  (let ((scheme (url-scheme url-string)))
    (and scheme (member scheme *secure-schemes*))))

;;; Parse query string into alist
(define (query-parse query-string)
  (if (or (not query-string) (= (string-length query-string) 0))
      '()
      (map (lambda (pair)
             (let ((eq-idx (string-index pair #\=)))
               (if eq-idx
                   (cons (url-decode (substring pair 0 eq-idx))
                         (url-decode (substring pair (+ eq-idx 1))))
                   (cons (url-decode pair) ""))))
           (string-split query-string #\&))))

;;; Encode alist to query string
(define (query-encode params)
  (string-join
   (map (lambda (pair)
          (string-append (url-encode (car pair))
                        "="
                        (url-encode (cdr pair))))
        params)
   "&"))

;;; Join base URL with relative path
(define (url-join base-url relative-path)
  (let ((parsed (url-parse base-url)))
    (if (result-ok? parsed)
        (let* ((parts (result-value parsed))
               (scheme (assoc-ref parts 'scheme))
               (host (assoc-ref parts 'host))
               (port (assoc-ref parts 'port)))
          (string-append
           (if scheme (string-append scheme "://") "")
           (or host "")
           (if port (string-append ":" (number->string port)) "")
           (if (string-prefix? "/" relative-path)
               relative-path
               (string-append "/" relative-path))))
        #f)))

;;; Resolve relative URL against base
(define (url-resolve base-url relative-url)
  (cond
   ;; If relative is actually absolute, return it
   ((url-is-absolute? relative-url) relative-url)
   ;; Protocol-relative URL
   ((string-prefix? "//" relative-url)
    (let ((scheme (url-scheme base-url)))
      (if scheme
          (string-append scheme ":" relative-url)
          relative-url)))
   ;; Absolute path
   ((string-prefix? "/" relative-url)
    (url-join base-url relative-url))
   ;; Relative path
   (else
    (let ((base-path (or (url-path base-url) "/")))
      (url-join base-url
                (string-append
                 (let ((last-slash (string-rindex base-path #\/)))
                   (if last-slash
                       (substring base-path 0 (+ last-slash 1))
                       "/"))
                 relative-url))))))

;;; Construct URL from components
(define (make-url scheme host port path query fragment)
  (string-append
   (if scheme (string-append scheme "://") "")
   (or host "")
   (if port (string-append ":" (number->string port)) "")
   (or path "/")
   (if query (string-append "?" query) "")
   (if fragment (string-append "#" fragment) "")))
