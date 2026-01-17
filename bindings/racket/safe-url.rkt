#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeUrl - URL validation and parsing for Racket
;;

(require racket/string
         racket/match
         racket/contract)

(provide
 ;; URL struct
 (struct-out url-parts)

 ;; Validation
 valid-url?
 valid-scheme?
 valid-host?

 ;; Parsing
 parse-url

 ;; Building
 build-url
 url-encode-component
 url-decode-component

 ;; Query string operations
 parse-query-string
 build-query-string

 ;; Normalization
 normalize-url)

;; URL components structure
(struct url-parts (scheme host port path query fragment) #:transparent)

;; Valid URL schemes
(define valid-schemes '("http" "https" "ftp" "ftps" "mailto" "file" "data"))

;; Check if scheme is valid
(define (valid-scheme? scheme)
  (and (string? scheme)
       (member (string-downcase scheme) valid-schemes)
       #t))

;; Check if host is valid (basic validation)
(define (valid-host? host)
  (and (string? host)
       (> (string-length host) 0)
       (<= (string-length host) 253)
       (not (string-contains? host " "))
       (not (string-contains? host "\n"))
       (not (string-contains? host "\r"))))

;; Check if a URL string is valid
(define (valid-url? url-string)
  (and (string? url-string)
       (> (string-length url-string) 0)
       (<= (string-length url-string) 2048)
       (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9+.-]*://" url-string)))

;; URL encode a component
(define (url-encode-component str)
  (define (encode-char c)
    (cond
      [(or (char-alphabetic? c)
           (char-numeric? c)
           (member c '(#\- #\_ #\. #\~)))
       (string c)]
      [else
       (let ([bytes (string->bytes/utf-8 (string c))])
         (apply string-append
                (for/list ([b (in-bytes bytes)])
                  (format "%~a" (string-upcase
                                 (let ([hex (number->string b 16)])
                                   (if (= (string-length hex) 1)
                                       (string-append "0" hex)
                                       hex)))))))]))
  (apply string-append (map encode-char (string->list str))))

;; URL decode a component
(define (url-decode-component str)
  (define (hex-char->num c)
    (cond
      [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
      [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
      [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
      [else 0]))
  (let loop ([chars (string->list str)] [result '()])
    (match chars
      ['() (list->string (reverse result))]
      [(list #\% c1 c2 rest ...)
       (let ([byte (+ (* 16 (hex-char->num c1)) (hex-char->num c2))])
         (loop rest (cons (integer->char byte) result)))]
      [(list #\+ rest ...)
       (loop rest (cons #\space result))]
      [(list c rest ...)
       (loop rest (cons c result))])))

;; Parse URL string into components
(define (parse-url url-string)
  (if (not (valid-url? url-string))
      #f
      (let* ([match-result (regexp-match
                            #rx"^([a-zA-Z][a-zA-Z0-9+.-]*)://([^/:?#]*)(:[0-9]+)?([^?#]*)?([?][^#]*)?(#.*)?"
                            url-string)])
        (if match-result
            (let* ([scheme (string-downcase (list-ref match-result 1))]
                   [host (list-ref match-result 2)]
                   [port-str (list-ref match-result 3)]
                   [port (if port-str
                             (string->number (substring port-str 1))
                             #f)]
                   [path (or (list-ref match-result 4) "/")]
                   [query-str (list-ref match-result 5)]
                   [query (if query-str (substring query-str 1) #f)]
                   [fragment-str (list-ref match-result 6)]
                   [fragment (if fragment-str (substring fragment-str 1) #f)])
              (url-parts scheme host port path query fragment))
            #f))))

;; Build URL from components
(define (build-url parts)
  (string-append
   (url-parts-scheme parts) "://"
   (url-parts-host parts)
   (if (url-parts-port parts)
       (format ":~a" (url-parts-port parts))
       "")
   (or (url-parts-path parts) "/")
   (if (url-parts-query parts)
       (format "?~a" (url-parts-query parts))
       "")
   (if (url-parts-fragment parts)
       (format "#~a" (url-parts-fragment parts))
       "")))

;; Parse query string into association list
(define (parse-query-string query-string)
  (if (or (not query-string) (string=? query-string ""))
      '()
      (for/list ([pair (string-split query-string "&")])
        (let ([parts (string-split pair "=" #:repeat? #f)])
          (if (>= (length parts) 2)
              (cons (url-decode-component (car parts))
                    (url-decode-component (cadr parts)))
              (cons (url-decode-component (car parts)) ""))))))

;; Build query string from association list
(define (build-query-string params)
  (string-join
   (for/list ([pair params])
     (format "~a=~a"
             (url-encode-component (car pair))
             (url-encode-component (cdr pair))))
   "&"))

;; Normalize URL (lowercase scheme/host, default port removal)
(define (normalize-url url-string)
  (let ([parts (parse-url url-string)])
    (if parts
        (let* ([scheme (string-downcase (url-parts-scheme parts))]
               [host (string-downcase (url-parts-host parts))]
               [port (url-parts-port parts)]
               [default-port (cond
                              [(string=? scheme "http") 80]
                              [(string=? scheme "https") 443]
                              [(string=? scheme "ftp") 21]
                              [else #f])]
               [normalized-port (if (and port (equal? port default-port))
                                    #f
                                    port)]
               [path (let ([p (url-parts-path parts)])
                       (if (or (not p) (string=? p ""))
                           "/"
                           p))])
          (build-url (url-parts scheme host normalized-port path
                               (url-parts-query parts)
                               (url-parts-fragment parts))))
        url-string)))
