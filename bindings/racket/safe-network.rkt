#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeNetwork - IP validation for Racket
;;

(require racket/string
         racket/format)

(provide
 ;; IPv4 struct
 (struct-out ipv4-address)

 ;; IP classification
 ip-class-invalid
 ip-class-loopback
 ip-class-private
 ip-class-reserved
 ip-class-public

 ;; Functions
 parse-ipv4
 format-ipv4
 loopback?
 private-ip?
 reserved-ip?
 public-ip?
 classify-ip

 ;; Port validation
 valid-port?
 privileged-port?)

;; IP classification constants
(define ip-class-invalid 0)
(define ip-class-loopback 1)
(define ip-class-private 2)
(define ip-class-reserved 3)
(define ip-class-public 4)

;; IPv4 address struct
(struct ipv4-address (octets valid?) #:transparent)

;; Parse IPv4 address string
(define (parse-ipv4 address)
  (define parts (string-split address "."))

  (cond
    [(not (= (length parts) 4))
     (ipv4-address '(0 0 0 0) #f)]

    [else
     (define (parse-octet str)
       (cond
         [(= (string-length str) 0) #f]
         ;; Check for leading zeros
         [(and (> (string-length str) 1)
               (char=? (string-ref str 0) #\0))
          #f]
         ;; Check all digits
         [(not (andmap char-numeric? (string->list str))) #f]
         [else
          (define val (string->number str))
          (and val (>= val 0) (<= val 255) val)]))

     (define octets (map parse-octet parts))

     (if (andmap number? octets)
         (ipv4-address octets #t)
         (ipv4-address '(0 0 0 0) #f))]))

;; Format IPv4 address as string
(define (format-ipv4 ip)
  (string-join (map number->string (ipv4-address-octets ip)) "."))

;; Check if IP is loopback (127.x.x.x)
(define (loopback? ip)
  (= (first (ipv4-address-octets ip)) 127))

;; Check if IP is private (RFC 1918)
(define (private-ip? ip)
  (define octets (ipv4-address-octets ip))
  (define o1 (first octets))
  (define o2 (second octets))

  (or
   ;; 10.0.0.0/8
   (= o1 10)
   ;; 172.16.0.0/12
   (and (= o1 172) (>= o2 16) (<= o2 31))
   ;; 192.168.0.0/16
   (and (= o1 192) (= o2 168))))

;; Check if IP is reserved
(define (reserved-ip? ip)
  (define octets (ipv4-address-octets ip))
  (define o1 (first octets))
  (define o2 (second octets))
  (define o3 (third octets))

  (or
   ;; 0.0.0.0/8
   (= o1 0)
   ;; 100.64.0.0/10 (CGNAT)
   (and (= o1 100) (>= o2 64) (<= o2 127))
   ;; 169.254.0.0/16 (link-local)
   (and (= o1 169) (= o2 254))
   ;; 192.0.0.0/24
   (and (= o1 192) (= o2 0) (= o3 0))
   ;; 192.0.2.0/24
   (and (= o1 192) (= o2 0) (= o3 2))
   ;; 198.51.100.0/24
   (and (= o1 198) (= o2 51) (= o3 100))
   ;; 203.0.113.0/24
   (and (= o1 203) (= o2 0) (= o3 113))
   ;; 224-239.x.x.x (multicast)
   (and (>= o1 224) (<= o1 239))
   ;; 240+ (reserved/broadcast)
   (>= o1 240)))

;; Check if IP is public
(define (public-ip? ip)
  (and (ipv4-address-valid? ip)
       (not (loopback? ip))
       (not (private-ip? ip))
       (not (reserved-ip? ip))))

;; Classify IP address
(define (classify-ip ip)
  (cond
    [(not (ipv4-address-valid? ip)) ip-class-invalid]
    [(loopback? ip) ip-class-loopback]
    [(private-ip? ip) ip-class-private]
    [(reserved-ip? ip) ip-class-reserved]
    [else ip-class-public]))

;; Check if port is valid (1-65535)
(define (valid-port? port)
  (and (>= port 1) (<= port 65535)))

;; Check if port is privileged (<1024)
(define (privileged-port? port)
  (and (>= port 1) (< port 1024)))
