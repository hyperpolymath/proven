;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeNetwork - Network validation for Common Lisp

(in-package #:proven)

;;; IP classification constants
(defconstant +ip-class-invalid+ 0)
(defconstant +ip-class-loopback+ 1)
(defconstant +ip-class-private+ 2)
(defconstant +ip-class-reserved+ 3)
(defconstant +ip-class-public+ 4)

;;; IPv4 address struct
(defstruct ipv4-address
  (octets #(0 0 0 0) :type (simple-vector 4))
  (valid-p nil :type boolean))

;;; Parse IPv4 address
(defun parse-ipv4 (ip-string)
  "Parse IP-STRING into an IPv4-ADDRESS struct."
  (handler-case
      (let* ((parts (split-string ip-string #\.))
             (octets (make-array 4 :initial-element 0)))
        (if (= (length parts) 4)
            (progn
              (dotimes (i 4)
                (let ((val (parse-integer (nth i parts))))
                  (if (and (>= val 0) (<= val 255))
                      (setf (aref octets i) val)
                      (return-from parse-ipv4
                        (make-ipv4-address :valid-p nil)))))
              (make-ipv4-address :octets octets :valid-p t))
            (make-ipv4-address :valid-p nil)))
    (error () (make-ipv4-address :valid-p nil))))

;;; Split string by delimiter
(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
            do (push (subseq string start i) result)
               (setf start (1+ i)))
    (push (subseq string start) result)
    (nreverse result)))

;;; Format IPv4 address
(defun format-ipv4 (addr)
  "Format IPV4-ADDRESS struct as dotted-decimal string."
  (if (ipv4-address-valid-p addr)
      (let ((octets (ipv4-address-octets addr)))
        (format nil "~A.~A.~A.~A"
                (aref octets 0)
                (aref octets 1)
                (aref octets 2)
                (aref octets 3)))
      nil))

;;; Check if loopback address
(defun loopback-p (addr)
  "Return T if ADDR is a loopback address (127.x.x.x)."
  (and (ipv4-address-valid-p addr)
       (= (aref (ipv4-address-octets addr) 0) 127)))

;;; Check if private IP
(defun private-ip-p (addr)
  "Return T if ADDR is a private IP address."
  (when (ipv4-address-valid-p addr)
    (let ((octets (ipv4-address-octets addr)))
      (let ((a (aref octets 0))
            (b (aref octets 1)))
        (or
         ;; 10.0.0.0/8
         (= a 10)
         ;; 172.16.0.0/12
         (and (= a 172) (>= b 16) (<= b 31))
         ;; 192.168.0.0/16
         (and (= a 192) (= b 168)))))))

;;; Check if reserved IP
(defun reserved-ip-p (addr)
  "Return T if ADDR is a reserved IP address."
  (when (ipv4-address-valid-p addr)
    (let ((octets (ipv4-address-octets addr)))
      (let ((a (aref octets 0))
            (b (aref octets 1)))
        (or
         ;; 0.0.0.0/8 - Current network
         (= a 0)
         ;; 100.64.0.0/10 - Shared address space
         (and (= a 100) (>= b 64) (<= b 127))
         ;; 169.254.0.0/16 - Link-local
         (and (= a 169) (= b 254))
         ;; 192.0.0.0/24 - IETF Protocol Assignments
         (and (= a 192) (= b 0) (= (aref octets 2) 0))
         ;; 192.0.2.0/24 - TEST-NET-1
         (and (= a 192) (= b 0) (= (aref octets 2) 2))
         ;; 198.51.100.0/24 - TEST-NET-2
         (and (= a 198) (= b 51) (= (aref octets 2) 100))
         ;; 203.0.113.0/24 - TEST-NET-3
         (and (= a 203) (= b 0) (= (aref octets 2) 113))
         ;; 224.0.0.0/4 - Multicast
         (and (>= a 224) (<= a 239))
         ;; 240.0.0.0/4 - Reserved for future use
         (>= a 240)
         ;; 255.255.255.255 - Broadcast
         (and (= a 255) (= b 255)
              (= (aref octets 2) 255)
              (= (aref octets 3) 255)))))))

;;; Check if public IP
(defun public-ip-p (addr)
  "Return T if ADDR is a public IP address."
  (and (ipv4-address-valid-p addr)
       (not (loopback-p addr))
       (not (private-ip-p addr))
       (not (reserved-ip-p addr))))

;;; Classify IP address
(defun classify-ip (addr)
  "Classify IPV4-ADDRESS and return classification constant."
  (cond
    ((not (ipv4-address-valid-p addr)) +ip-class-invalid+)
    ((loopback-p addr) +ip-class-loopback+)
    ((private-ip-p addr) +ip-class-private+)
    ((reserved-ip-p addr) +ip-class-reserved+)
    (t +ip-class-public+)))

;;; Validate port number
(defun valid-port-p (port)
  "Return T if PORT is a valid port number (1-65535)."
  (and (integerp port)
       (>= port 1)
       (<= port 65535)))

;;; Check if privileged port
(defun privileged-port-p (port)
  "Return T if PORT is a privileged port (1-1023)."
  (and (valid-port-p port)
       (<= port 1023)))
