;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeNetwork - Network primitive validation for Guile Scheme
;;;
;;; Validates IPv4, IPv6, CIDR notation, ports, and hostnames.
;;; All operations are pure and cannot crash.

(define-module (proven safe-network)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (ipv4-valid?
            ipv6-valid?
            ip-valid?
            port-valid?
            hostname-valid?
            cidr-valid?
            cidr-parse
            ip-private?
            ip-loopback?
            make-cidr-result
            cidr-result-ip
            cidr-result-prefix
            cidr-result-ok?))

;;; CIDR result constructor
(define (make-cidr-result ip prefix ok)
  `((ip . ,ip) (prefix . ,prefix) (ok . ,ok)))

(define (cidr-result-ip result)
  (assoc-ref result 'ip))

(define (cidr-result-prefix result)
  (assoc-ref result 'prefix))

(define (cidr-result-ok? result)
  (assoc-ref result 'ok))

;;; IPv4 address pattern
(define ipv4-pattern
  (make-regexp "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"))

;;; Simplified IPv6 pattern
(define ipv6-pattern
  (make-regexp "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$"))

;;; Hostname pattern (RFC 1123)
(define hostname-pattern
  (make-regexp "^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"))

;;; Check if string is a valid IPv4 address
(define (ipv4-valid? ip)
  (regexp-exec ipv4-pattern ip))

;;; Check if string is a valid IPv6 address
(define (ipv6-valid? ip)
  (or (regexp-exec ipv6-pattern ip)
      (string=? ip "::")))

;;; Check if string is a valid IP address (v4 or v6)
(define (ip-valid? ip)
  (or (ipv4-valid? ip) (ipv6-valid? ip)))

;;; Check if number is a valid port (0-65535)
(define (port-valid? port)
  (and (integer? port)
       (>= port 0)
       (<= port 65535)))

;;; Check if string is a valid hostname
(define (hostname-valid? host)
  (and (regexp-exec hostname-pattern host)
       (<= (string-length host) 253)))

;;; Check if string is a valid CIDR notation (IPv4)
(define (cidr-valid? cidr)
  (let ((slash-pos (string-index cidr #\/)))
    (if (not slash-pos)
        #f
        (let ((ip (substring cidr 0 slash-pos))
              (prefix-str (substring cidr (+ slash-pos 1))))
          (and (ipv4-valid? ip)
               (regexp-exec (make-regexp "^[0-9]+$") prefix-str)
               (let ((prefix (string->number prefix-str)))
                 (and prefix (>= prefix 0) (<= prefix 32))))))))

;;; Parse CIDR into IP and prefix length
(define (cidr-parse cidr)
  (let ((slash-pos (string-index cidr #\/)))
    (if (not slash-pos)
        (make-cidr-result "" 0 #f)
        (let ((ip (substring cidr 0 slash-pos))
              (prefix-str (substring cidr (+ slash-pos 1))))
          (if (not (ipv4-valid? ip))
              (make-cidr-result "" 0 #f)
              (let ((prefix (string->number prefix-str)))
                (if (not prefix)
                    (make-cidr-result "" 0 #f)
                    (make-cidr-result ip prefix #t))))))))

;;; Check if IP is in private range (RFC 1918)
(define (ip-private? ip)
  (or (string-prefix? "10." ip)
      (regexp-exec (make-regexp "^172\\.(1[6-9]|2[0-9]|3[01])\\.") ip)
      (string-prefix? "192.168." ip)))

;;; Check if IP is loopback
(define (ip-loopback? ip)
  (or (string-prefix? "127." ip)
      (string=? ip "::1")))
