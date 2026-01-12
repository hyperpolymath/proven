;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeEmail - Email validation for Common Lisp

(in-package #:proven)

;;; Email result struct
(defstruct email-result
  (local-part "" :type string)
  (domain "" :type string)
  (error "" :type string)
  (ok-p nil :type boolean))

;;; Common disposable email domains
(defparameter *disposable-domains*
  '("tempmail.com" "throwaway.com" "mailinator.com" "guerrillamail.com"
    "10minutemail.com" "trashmail.com" "fakeinbox.com" "tempinbox.com"))

;;; Check if character is valid for local part
(defun local-char-p (char)
  "Return T if CHAR is valid in email local part."
  (or (alpha-num-p char)
      (member char '(#\. #\_ #\- #\+ #\=))))

;;; Check if character is valid for domain
(defun domain-char-p (char)
  "Return T if CHAR is valid in email domain."
  (or (alpha-num-p char)
      (member char '(#\. #\-))))

;;; Validate local part
(defun valid-local-part-p (local)
  "Return T if LOCAL is a valid email local part."
  (and (> (length local) 0)
       (<= (length local) 64)
       (not (char= (char local 0) #\.))
       (not (char= (char local (1- (length local))) #\.))
       (not (search ".." local))
       (every #'local-char-p local)))

;;; Validate domain
(defun valid-domain-p (domain)
  "Return T if DOMAIN is a valid email domain."
  (and (> (length domain) 0)
       (<= (length domain) 253)
       (position #\. domain)  ; Must have at least one dot
       (not (char= (char domain 0) #\.))
       (not (char= (char domain (1- (length domain))) #\.))
       (not (char= (char domain 0) #\-))
       (not (char= (char domain (1- (length domain))) #\-))
       (not (search ".." domain))
       (not (search ".-" domain))
       (not (search "-." domain))
       (every #'domain-char-p domain)))

;;; Check if email is valid
(defun valid-email-p (email)
  "Return T if EMAIL is a valid email address."
  (let ((at-pos (position #\@ email)))
    (and at-pos
         (= (count #\@ email) 1)
         (valid-local-part-p (subseq email 0 at-pos))
         (valid-domain-p (subseq email (1+ at-pos))))))

;;; Parse email address
(defun parse-email (email)
  "Parse EMAIL into local part and domain.
   Returns an EMAIL-RESULT struct."
  (let ((at-pos (position #\@ email)))
    (cond
      ((null at-pos)
       (make-email-result :error "No @ symbol found" :ok-p nil))
      ((/= (count #\@ email) 1)
       (make-email-result :error "Multiple @ symbols" :ok-p nil))
      (t
       (let ((local (subseq email 0 at-pos))
             (domain (subseq email (1+ at-pos))))
         (cond
           ((not (valid-local-part-p local))
            (make-email-result :error "Invalid local part" :ok-p nil))
           ((not (valid-domain-p domain))
            (make-email-result :error "Invalid domain" :ok-p nil))
           (t
            (make-email-result :local-part local
                               :domain domain
                               :error ""
                               :ok-p t))))))))

;;; Check if email is from a disposable domain
(defun disposable-email-p (email)
  "Return T if EMAIL is from a known disposable email provider."
  (let ((result (parse-email email)))
    (when (email-result-ok-p result)
      (member (string-downcase (email-result-domain result))
              *disposable-domains*
              :test #'string=))))

;;; Normalize email address
(defun normalize-email (email)
  "Normalize EMAIL to lowercase.
   Returns normalized email or NIL if invalid."
  (let ((result (parse-email email)))
    (when (email-result-ok-p result)
      (format nil "~A@~A"
              (email-result-local-part result)
              (string-downcase (email-result-domain result))))))
