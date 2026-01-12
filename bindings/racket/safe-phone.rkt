#lang racket

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafePhone - Phone number validation and formatting for Racket
;;

(require racket/contract
         racket/string
         racket/format)

(provide
 ;; Struct with contract
 (contract-out
  [struct phone-number ([country-code string?]
                        [national-number string?]
                        [extension (or/c string? #f)]
                        [type phone-type?]
                        [country-iso (or/c symbol? #f)])])

 ;; Phone type predicate
 phone-type?

 ;; Parse and format functions
 parse-phone
 format-phone
 phone->e164
 phone->national
 phone->international

 ;; Validation predicates
 valid-phone?
 valid-phone-string?
 possible-phone?

 ;; Country code lookup
 country-code->iso
 iso->country-code
 country-name

 ;; Utility functions
 phone-digits-only
 normalize-phone
 phones-match?

 ;; List of supported countries
 all-country-codes)

;; ============================================================================
;; Phone Type Definitions
;; ============================================================================

(define phone-types
  '(unknown fixed-line mobile fixed-line-or-mobile toll-free
    premium-rate shared-cost voip personal-number pager uan
    voicemail emergency short-code))

(define (phone-type? symbol)
  (and (symbol? symbol)
       (member symbol phone-types)
       #t))

;; ============================================================================
;; Country Code Database
;; ============================================================================

;; Map of country code -> (iso-code name national-prefix trunk-prefix)
(define country-info-table
  (hash
   ;; North America (NANP)
   "1" '(US "United States" #f "1")
   "1242" '(BS "Bahamas" #f "1")
   "1246" '(BB "Barbados" #f "1")
   "1264" '(AI "Anguilla" #f "1")
   "1268" '(AG "Antigua and Barbuda" #f "1")
   "1284" '(VG "British Virgin Islands" #f "1")
   "1340" '(VI "US Virgin Islands" #f "1")
   "1345" '(KY "Cayman Islands" #f "1")
   "1441" '(BM "Bermuda" #f "1")
   "1473" '(GD "Grenada" #f "1")
   "1649" '(TC "Turks and Caicos" #f "1")
   "1664" '(MS "Montserrat" #f "1")
   "1670" '(MP "Northern Mariana Islands" #f "1")
   "1671" '(GU "Guam" #f "1")
   "1684" '(AS "American Samoa" #f "1")
   "1721" '(SX "Sint Maarten" #f "1")
   "1758" '(LC "Saint Lucia" #f "1")
   "1767" '(DM "Dominica" #f "1")
   "1784" '(VC "St Vincent and Grenadines" #f "1")
   "1787" '(PR "Puerto Rico" #f "1")
   "1809" '(DO "Dominican Republic" #f "1")
   "1868" '(TT "Trinidad and Tobago" #f "1")
   "1869" '(KN "Saint Kitts and Nevis" #f "1")
   "1876" '(JM "Jamaica" #f "1")

   ;; Europe
   "7" '(RU "Russia" "8" #f)
   "20" '(EG "Egypt" "0" #f)
   "27" '(ZA "South Africa" "0" #f)
   "30" '(GR "Greece" #f #f)
   "31" '(NL "Netherlands" "0" #f)
   "32" '(BE "Belgium" "0" #f)
   "33" '(FR "France" "0" #f)
   "34" '(ES "Spain" #f #f)
   "36" '(HU "Hungary" "06" #f)
   "39" '(IT "Italy" #f #f)
   "40" '(RO "Romania" "0" #f)
   "41" '(CH "Switzerland" "0" #f)
   "43" '(AT "Austria" "0" #f)
   "44" '(GB "United Kingdom" "0" #f)
   "45" '(DK "Denmark" #f #f)
   "46" '(SE "Sweden" "0" #f)
   "47" '(NO "Norway" #f #f)
   "48" '(PL "Poland" #f #f)
   "49" '(DE "Germany" "0" #f)
   "51" '(PE "Peru" "0" #f)
   "52" '(MX "Mexico" "01" #f)
   "53" '(CU "Cuba" "0" #f)
   "54" '(AR "Argentina" "0" #f)
   "55" '(BR "Brazil" "0" #f)
   "56" '(CL "Chile" #f #f)
   "57" '(CO "Colombia" "0" #f)
   "58" '(VE "Venezuela" "0" #f)
   "60" '(MY "Malaysia" "0" #f)
   "61" '(AU "Australia" "0" #f)
   "62" '(ID "Indonesia" "0" #f)
   "63" '(PH "Philippines" "0" #f)
   "64" '(NZ "New Zealand" "0" #f)
   "65" '(SG "Singapore" #f #f)
   "66" '(TH "Thailand" "0" #f)
   "81" '(JP "Japan" "0" #f)
   "82" '(KR "South Korea" "0" #f)
   "84" '(VN "Vietnam" "0" #f)
   "86" '(CN "China" "0" #f)
   "90" '(TR "Turkey" "0" #f)
   "91" '(IN "India" "0" #f)
   "92" '(PK "Pakistan" "0" #f)
   "93" '(AF "Afghanistan" "0" #f)
   "94" '(LK "Sri Lanka" "0" #f)
   "95" '(MM "Myanmar" #f #f)
   "98" '(IR "Iran" "0" #f)

   ;; Additional European
   "350" '(GI "Gibraltar" #f #f)
   "351" '(PT "Portugal" #f #f)
   "352" '(LU "Luxembourg" #f #f)
   "353" '(IE "Ireland" "0" #f)
   "354" '(IS "Iceland" #f #f)
   "355" '(AL "Albania" "0" #f)
   "356" '(MT "Malta" #f #f)
   "357" '(CY "Cyprus" #f #f)
   "358" '(FI "Finland" "0" #f)
   "359" '(BG "Bulgaria" "0" #f)
   "370" '(LT "Lithuania" "8" #f)
   "371" '(LV "Latvia" #f #f)
   "372" '(EE "Estonia" #f #f)
   "373" '(MD "Moldova" "0" #f)
   "374" '(AM "Armenia" "0" #f)
   "375" '(BY "Belarus" "8" #f)
   "376" '(AD "Andorra" #f #f)
   "377" '(MC "Monaco" #f #f)
   "378" '(SM "San Marino" #f #f)
   "380" '(UA "Ukraine" "0" #f)
   "381" '(RS "Serbia" "0" #f)
   "382" '(ME "Montenegro" "0" #f)
   "383" '(XK "Kosovo" "0" #f)
   "385" '(HR "Croatia" "0" #f)
   "386" '(SI "Slovenia" "0" #f)
   "387" '(BA "Bosnia and Herzegovina" "0" #f)
   "389" '(MK "North Macedonia" "0" #f)
   "420" '(CZ "Czech Republic" #f #f)
   "421" '(SK "Slovakia" "0" #f)
   "423" '(LI "Liechtenstein" #f #f)

   ;; Middle East
   "961" '(LB "Lebanon" "0" #f)
   "962" '(JO "Jordan" "0" #f)
   "963" '(SY "Syria" "0" #f)
   "964" '(IQ "Iraq" "0" #f)
   "965" '(KW "Kuwait" #f #f)
   "966" '(SA "Saudi Arabia" "0" #f)
   "967" '(YE "Yemen" "0" #f)
   "968" '(OM "Oman" #f #f)
   "970" '(PS "Palestine" "0" #f)
   "971" '(AE "United Arab Emirates" "0" #f)
   "972" '(IL "Israel" "0" #f)
   "973" '(BH "Bahrain" #f #f)
   "974" '(QA "Qatar" #f #f)
   "975" '(BT "Bhutan" #f #f)
   "976" '(MN "Mongolia" "0" #f)
   "977" '(NP "Nepal" "0" #f)))

;; Reverse lookup: ISO code -> country code
(define iso-to-country-code-table
  (for/hash ([(code info) (in-hash country-info-table)])
    (values (first info) code)))

;; Get all country codes
(define all-country-codes
  (hash-keys country-info-table))

;; ============================================================================
;; Phone Number Struct
;; ============================================================================

;; Phone number value struct
(struct phone-number (country-code national-number extension type country-iso)
  #:transparent)

;; ============================================================================
;; Country Code Lookup Functions
;; ============================================================================

;; Get ISO code from country calling code
(define (country-code->iso calling-code)
  (define info (hash-ref country-info-table calling-code #f))
  (and info (first info)))

;; Get country calling code from ISO code
(define (iso->country-code iso-code)
  (hash-ref iso-to-country-code-table iso-code #f))

;; Get country name from calling code
(define (country-name calling-code)
  (define info (hash-ref country-info-table calling-code #f))
  (and info (second info)))

;; ============================================================================
;; Utility Functions
;; ============================================================================

;; Extract only digits from phone string
(define (phone-digits-only phone-string)
  (list->string
   (filter char-numeric? (string->list phone-string))))

;; Remove leading zeros and whitespace
(define (strip-leading-zeros str)
  (define trimmed (string-trim str))
  (regexp-replace #rx"^0+" trimmed ""))

;; ============================================================================
;; Parsing Functions
;; ============================================================================

;; Try to match country code from digits
(define (find-country-code digits)
  (for/first ([len (in-list '(4 3 2 1))]
              #:when (>= (string-length digits) len))
    (define prefix (substring digits 0 len))
    (and (hash-has-key? country-info-table prefix)
         prefix)))

;; Parse phone number string into phone-number struct
(define (parse-phone phone-string [default-country-code #f])
  (define cleaned (phone-digits-only phone-string))

  (cond
    ;; Empty input
    [(= (string-length cleaned) 0)
     #f]

    ;; Starts with + or has enough digits to include country code
    [(or (string-prefix? (string-trim phone-string) "+")
         (> (string-length cleaned) 10))
     ;; Remove leading + if present and extract country code
     (define digits
       (if (string-prefix? (string-trim phone-string) "+")
           cleaned
           cleaned))

     (define found-country-code (find-country-code digits))

     (cond
       [found-country-code
        (define national-part
          (substring digits (string-length found-country-code)))
        (define iso-code (country-code->iso found-country-code))

        ;; Extract extension if present (after x, ext, #)
        (define extension-match
          (regexp-match #rx"(?i)(?:x|ext|#)\\s*(\\d+)$" phone-string))
        (define extension
          (and extension-match (second extension-match)))

        (phone-number found-country-code
                      national-part
                      extension
                      (detect-phone-type found-country-code national-part)
                      iso-code)]
       [else #f])]

    ;; Use default country code if provided
    [default-country-code
     (define national-part (strip-leading-zeros cleaned))
     (phone-number default-country-code
                   national-part
                   #f
                   (detect-phone-type default-country-code national-part)
                   (country-code->iso default-country-code))]

    ;; Cannot determine country code
    [else #f]))

;; Detect phone type based on country code and number pattern
(define (detect-phone-type country-code national-number)
  (define len (string-length national-number))

  (cond
    ;; Short codes (typically 3-5 digits)
    [(< len 5) 'short-code]

    ;; Emergency numbers
    [(member national-number '("911" "999" "112" "110" "119"))
     'emergency]

    ;; US/Canada NANP patterns
    [(equal? country-code "1")
     (cond
       [(string-prefix? national-number "800") 'toll-free]
       [(string-prefix? national-number "888") 'toll-free]
       [(string-prefix? national-number "877") 'toll-free]
       [(string-prefix? national-number "866") 'toll-free]
       [(string-prefix? national-number "855") 'toll-free]
       [(string-prefix? national-number "844") 'toll-free]
       [(string-prefix? national-number "900") 'premium-rate]
       [(string-prefix? national-number "976") 'premium-rate]
       [else 'fixed-line-or-mobile])]

    ;; UK patterns
    [(equal? country-code "44")
     (cond
       [(string-prefix? national-number "7") 'mobile]
       [(string-prefix? national-number "800") 'toll-free]
       [(string-prefix? national-number "808") 'toll-free]
       [(string-prefix? national-number "90") 'premium-rate]
       [(string-prefix? national-number "91") 'premium-rate]
       [else 'fixed-line])]

    ;; Default: unknown
    [else 'unknown]))

;; ============================================================================
;; Validation Functions
;; ============================================================================

;; Check if phone number string appears valid
(define (valid-phone-string? phone-string)
  (define parsed (parse-phone phone-string))
  (and parsed #t))

;; Check if parsed phone number is valid
(define (valid-phone? phone)
  (and (phone-number? phone)
       (string? (phone-number-country-code phone))
       (> (string-length (phone-number-national-number phone)) 4)))

;; Check if phone number is possibly valid (less strict)
(define (possible-phone? phone-string)
  (define digits (phone-digits-only phone-string))
  (and (>= (string-length digits) 6)
       (<= (string-length digits) 15)))

;; ============================================================================
;; Formatting Functions
;; ============================================================================

;; Format as E.164 (international standard: +CCnnnnnnnnn)
(define (phone->e164 phone)
  (string-append "+"
                 (phone-number-country-code phone)
                 (phone-number-national-number phone)))

;; Format as international (with spaces and country code)
(define (phone->international phone)
  (define country-code (phone-number-country-code phone))
  (define national (phone-number-national-number phone))
  (define ext (phone-number-extension phone))

  ;; Format national number with spaces every 3-4 digits
  (define formatted-national
    (format-national-number national))

  (define base
    (string-append "+" country-code " " formatted-national))

  (if ext
      (string-append base " ext. " ext)
      base))

;; Format as national (without country code)
(define (phone->national phone)
  (define national (phone-number-national-number phone))
  (define ext (phone-number-extension phone))

  (define formatted (format-national-number national))

  (if ext
      (string-append formatted " ext. " ext)
      formatted))

;; Helper: format national number with appropriate grouping
(define (format-national-number national)
  (define len (string-length national))
  (cond
    ;; 10 digits: (XXX) XXX-XXXX or XXX XXX XXXX
    [(= len 10)
     (string-append (substring national 0 3) " "
                    (substring national 3 6) " "
                    (substring national 6))]
    ;; 9 digits: XXX XXX XXX
    [(= len 9)
     (string-append (substring national 0 3) " "
                    (substring national 3 6) " "
                    (substring national 6))]
    ;; 8 digits: XXXX XXXX
    [(= len 8)
     (string-append (substring national 0 4) " "
                    (substring national 4))]
    ;; 7 digits: XXX XXXX
    [(= len 7)
     (string-append (substring national 0 3) " "
                    (substring national 3))]
    ;; Default: group in threes from right
    [else
     (define remainder-len (remainder len 3))
     (define first-group
       (if (= remainder-len 0)
           ""
           (string-append (substring national 0 remainder-len) " ")))
     (define rest (substring national remainder-len))
     (string-append
      first-group
      (string-join (for/list ([i (in-range 0 (string-length rest) 3)])
                     (substring rest i (min (+ i 3) (string-length rest))))
                   " "))]))

;; Generic format function with style option
(define (format-phone phone [style 'international])
  (case style
    [(e164) (phone->e164 phone)]
    [(national) (phone->national phone)]
    [(international) (phone->international phone)]
    [else (phone->international phone)]))

;; ============================================================================
;; Comparison Functions
;; ============================================================================

;; Normalize phone number for comparison
(define (normalize-phone phone-string [default-country-code #f])
  (define parsed (parse-phone phone-string default-country-code))
  (and parsed (phone->e164 parsed)))

;; Check if two phone numbers match
(define (phones-match? phone-a phone-b [default-country-code #f])
  (define normalized-a (normalize-phone phone-a default-country-code))
  (define normalized-b (normalize-phone phone-b default-country-code))
  (and normalized-a
       normalized-b
       (string=? normalized-a normalized-b)))
