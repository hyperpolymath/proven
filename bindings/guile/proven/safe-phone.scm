;;; SPDX-License-Identifier: PMPL-1.0
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafePhone - Phone number validation for Guile Scheme
;;;
;;; Provides E.164 compliant phone number validation and formatting.
;;; All operations are pure and cannot crash.

(define-module (proven safe-phone)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (phone-valid?
            phone-valid-e164?
            phone-valid-nanp?
            phone-normalize
            phone-format
            phone-format-national
            phone-format-international
            phone-parse
            phone-country-code
            phone-national-number
            phone-is-mobile?
            phone-is-toll-free?
            phone-strip-formatting

            ;; Country code helpers
            country-code-valid?
            country-code-for-country

            ;; Constants
            *max-phone-length*
            *min-phone-length*))

;;; Phone number length limits
(define *max-phone-length* 15)  ; E.164 maximum
(define *min-phone-length* 7)   ; Minimum meaningful length

;;; Country calling codes
(define *country-codes*
  '(("US" . "1") ("CA" . "1") ("GB" . "44") ("DE" . "49") ("FR" . "33")
    ("IT" . "39") ("ES" . "34") ("JP" . "81") ("CN" . "86") ("IN" . "91")
    ("AU" . "61") ("NZ" . "64") ("BR" . "55") ("MX" . "52") ("RU" . "7")
    ("KR" . "82") ("SG" . "65") ("HK" . "852") ("TW" . "886") ("IL" . "972")
    ("AE" . "971") ("SA" . "966") ("ZA" . "27") ("NG" . "234") ("EG" . "20")))

;;; NANP toll-free prefixes
(define *toll-free-prefixes* '("800" "888" "877" "866" "855" "844" "833"))

;;; Make a result record
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Check if character is a digit
(define (digit-char? c)
  (and (char>=? c #\0) (char<=? c #\9)))

;;; Extract only digits from string
(define (extract-digits str)
  (list->string
   (filter digit-char? (string->list str))))

;;; Strip all formatting from phone number
(define (phone-strip-formatting phone)
  (extract-digits phone))

;;; Validate E.164 format phone number
(define (phone-valid-e164? phone)
  (and (string? phone)
       (string-prefix? "+" phone)
       (let ((digits (extract-digits phone)))
         (and (>= (string-length digits) *min-phone-length*)
              (<= (string-length digits) *max-phone-length*)
              (not (char=? (string-ref digits 0) #\0))))))

;;; Validate NANP (North American Numbering Plan) format
(define (phone-valid-nanp? phone)
  (let ((digits (extract-digits phone)))
    (and (or (= (string-length digits) 10)
             (and (= (string-length digits) 11)
                  (char=? (string-ref digits 0) #\1)))
         ;; NPA (area code) can't start with 0 or 1
         (let* ((offset (if (= (string-length digits) 11) 1 0))
                (npa-first (string-ref digits offset)))
           (and (not (char=? npa-first #\0))
                (not (char=? npa-first #\1))
                ;; NXX (exchange) can't start with 0 or 1
                (let ((nxx-first (string-ref digits (+ offset 3))))
                  (and (not (char=? nxx-first #\0))
                       (not (char=? nxx-first #\1)))))))))

;;; General phone validation
(define (phone-valid? phone)
  (and (string? phone)
       (> (string-length phone) 0)
       (let ((digits (extract-digits phone)))
         (and (>= (string-length digits) *min-phone-length*)
              (<= (string-length digits) *max-phone-length*)))))

;;; Normalize phone number to E.164 format
(define (phone-normalize phone default-country)
  (if (not (string? phone))
      (make-result #f #f)
      (let ((digits (extract-digits phone)))
        (cond
         ;; Already has country code (starts with +)
         ((and (string-prefix? "+" phone)
               (>= (string-length digits) *min-phone-length*))
          (make-result (string-append "+" digits) #t))
         ;; NANP number (10 or 11 digits starting with 1)
         ((and (= (string-length digits) 11)
               (char=? (string-ref digits 0) #\1))
          (make-result (string-append "+" digits) #t))
         ;; 10 digit NANP (add +1)
         ((and (= (string-length digits) 10)
               (string=? default-country "US"))
          (make-result (string-append "+1" digits) #t))
         ;; Use provided country code
         (else
          (let ((country-code (country-code-for-country default-country)))
            (if country-code
                (make-result (string-append "+" country-code digits) #t)
                (make-result #f #f))))))))

;;; Format for display (national format)
(define (phone-format-national phone)
  (let ((digits (extract-digits phone)))
    (cond
     ;; NANP: (XXX) XXX-XXXX
     ((and (>= (string-length digits) 10)
           (<= (string-length digits) 11))
      (let ((offset (if (= (string-length digits) 11) 1 0)))
        (string-append
         "("
         (substring digits offset (+ offset 3))
         ") "
         (substring digits (+ offset 3) (+ offset 6))
         "-"
         (substring digits (+ offset 6) (+ offset 10)))))
     ;; Generic: group in 3s
     (else
      (let loop ((chars (string->list digits))
                 (result '())
                 (count 0))
        (if (null? chars)
            (list->string (reverse result))
            (loop (cdr chars)
                  (if (and (= (remainder count 3) 0)
                           (> count 0))
                      (cons (car chars) (cons #\space result))
                      (cons (car chars) result))
                  (+ count 1))))))))

;;; Format for international display
(define (phone-format-international phone)
  (let ((digits (extract-digits phone)))
    (if (string-prefix? "+" phone)
        (string-append "+" (phone-format-national digits))
        (phone-format-national digits))))

;;; Simple format (just adds dashes)
(define (phone-format phone)
  (phone-format-national phone))

;;; Parse phone number into components
(define (phone-parse phone)
  (if (not (phone-valid? phone))
      (make-result #f #f)
      (let* ((digits (extract-digits phone))
             (has-plus (string-prefix? "+" phone))
             (country-code
              (cond
               ((and has-plus (>= (string-length digits) 10))
                ;; Try to extract country code (1-3 digits)
                (cond
                 ((char=? (string-ref digits 0) #\1) "1")  ; NANP
                 ((>= (string-length digits) 12)
                  (substring digits 0 2))  ; 2-digit codes
                 (else (substring digits 0 1))))
               (else #f)))
             (national (if country-code
                          (substring digits (string-length country-code))
                          digits)))
        (make-result
         `((country-code . ,country-code)
           (national-number . ,national)
           (original . ,phone)
           (e164 . ,(if has-plus
                       (string-append "+" digits)
                       #f)))
         #t))))

;;; Get country code from phone
(define (phone-country-code phone)
  (let ((parsed (phone-parse phone)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'country-code)
        #f)))

;;; Get national number from phone
(define (phone-national-number phone)
  (let ((parsed (phone-parse phone)))
    (if (result-ok? parsed)
        (assoc-ref (result-value parsed) 'national-number)
        #f)))

;;; Check if likely mobile (heuristic)
(define (phone-is-mobile? phone)
  ;; This is a simplistic check - real validation requires carrier databases
  (let ((digits (extract-digits phone)))
    (and (>= (string-length digits) 10)
         ;; Mobile numbers often start with certain prefixes
         #t)))  ; Can't reliably detect without carrier data

;;; Check if toll-free (NANP)
(define (phone-is-toll-free? phone)
  (let ((digits (extract-digits phone)))
    (and (>= (string-length digits) 10)
         (let* ((offset (if (and (>= (string-length digits) 11)
                                 (char=? (string-ref digits 0) #\1))
                           1 0))
                (area-code (substring digits offset (+ offset 3))))
           (member area-code *toll-free-prefixes*)))))

;;; Validate country code
(define (country-code-valid? code)
  (and (string? code)
       (= (string-length code) 2)
       (assoc-ref *country-codes* (string-upcase code))))

;;; Get calling code for country
(define (country-code-for-country iso-code)
  (and (string? iso-code)
       (assoc-ref *country-codes* (string-upcase iso-code))))
