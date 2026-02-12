#lang racket/base

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeUnit - Safe physical unit conversions for Racket
;;

(provide
 ;; Length conversion
 length-units
 convert-length
 meters->feet
 feet->meters
 meters->miles
 miles->meters
 kilometers->miles
 miles->kilometers

 ;; Mass conversion
 mass-units
 convert-mass
 kilograms->pounds
 pounds->kilograms
 grams->ounces
 ounces->grams

 ;; Temperature conversion
 temperature-units
 convert-temperature
 celsius->fahrenheit
 fahrenheit->celsius
 celsius->kelvin
 kelvin->celsius

 ;; Time conversion
 time-units
 convert-time
 hours->minutes
 minutes->hours
 hours->seconds
 seconds->hours
 days->hours
 hours->days

 ;; Data size conversion
 data-units
 convert-data
 bytes->kilobytes
 kilobytes->bytes
 bytes->megabytes
 megabytes->bytes
 bytes->gigabytes
 gigabytes->bytes
 kibibytes->kilobytes
 kilobytes->kibibytes)

;; Length units enumeration
(define length-units
  '(meters kilometers centimeters millimeters miles yards feet inches))

;; Length conversion factors to meters
(define (length-to-meters value unit)
  (case unit
    [(meters) value]
    [(kilometers) (* value 1000)]
    [(centimeters) (/ value 100)]
    [(millimeters) (/ value 1000)]
    [(miles) (* value 1609.344)]
    [(yards) (* value 0.9144)]
    [(feet) (* value 0.3048)]
    [(inches) (* value 0.0254)]
    [else value]))

;; Length conversion from meters
(define (meters-to-length meters unit)
  (case unit
    [(meters) meters]
    [(kilometers) (/ meters 1000)]
    [(centimeters) (* meters 100)]
    [(millimeters) (* meters 1000)]
    [(miles) (/ meters 1609.344)]
    [(yards) (/ meters 0.9144)]
    [(feet) (/ meters 0.3048)]
    [(inches) (/ meters 0.0254)]
    [else meters]))

;; Convert length between units
(define (convert-length value from-unit to-unit)
  (meters-to-length (length-to-meters value from-unit) to-unit))

;; Convenience functions for length
(define (meters->feet m) (convert-length m 'meters 'feet))
(define (feet->meters f) (convert-length f 'feet 'meters))
(define (meters->miles m) (convert-length m 'meters 'miles))
(define (miles->meters mi) (convert-length mi 'miles 'meters))
(define (kilometers->miles km) (convert-length km 'kilometers 'miles))
(define (miles->kilometers mi) (convert-length mi 'miles 'kilometers))

;; Mass units enumeration
(define mass-units
  '(kilograms grams milligrams pounds ounces stones))

;; Mass conversion factors to kilograms
(define (mass-to-kilograms value unit)
  (case unit
    [(kilograms) value]
    [(grams) (/ value 1000)]
    [(milligrams) (/ value 1000000)]
    [(pounds) (* value 0.453592)]
    [(ounces) (* value 0.0283495)]
    [(stones) (* value 6.35029)]
    [else value]))

;; Mass conversion from kilograms
(define (kilograms-to-mass kg unit)
  (case unit
    [(kilograms) kg]
    [(grams) (* kg 1000)]
    [(milligrams) (* kg 1000000)]
    [(pounds) (/ kg 0.453592)]
    [(ounces) (/ kg 0.0283495)]
    [(stones) (/ kg 6.35029)]
    [else kg]))

;; Convert mass between units
(define (convert-mass value from-unit to-unit)
  (kilograms-to-mass (mass-to-kilograms value from-unit) to-unit))

;; Convenience functions for mass
(define (kilograms->pounds kg) (convert-mass kg 'kilograms 'pounds))
(define (pounds->kilograms lb) (convert-mass lb 'pounds 'kilograms))
(define (grams->ounces g) (convert-mass g 'grams 'ounces))
(define (ounces->grams oz) (convert-mass oz 'ounces 'grams))

;; Temperature units enumeration
(define temperature-units '(celsius fahrenheit kelvin))

;; Convert temperature between units
(define (convert-temperature value from-unit to-unit)
  ;; First convert to Kelvin
  (let ([kelvin (case from-unit
                  [(celsius) (+ value 273.15)]
                  [(fahrenheit) (+ (* (- value 32) (/ 5 9)) 273.15)]
                  [(kelvin) value]
                  [else value])])
    ;; Then convert from Kelvin
    (case to-unit
      [(celsius) (- kelvin 273.15)]
      [(fahrenheit) (+ (* (- kelvin 273.15) (/ 9 5)) 32)]
      [(kelvin) kelvin]
      [else kelvin])))

;; Convenience functions for temperature
(define (celsius->fahrenheit c) (convert-temperature c 'celsius 'fahrenheit))
(define (fahrenheit->celsius f) (convert-temperature f 'fahrenheit 'celsius))
(define (celsius->kelvin c) (convert-temperature c 'celsius 'kelvin))
(define (kelvin->celsius k) (convert-temperature k 'kelvin 'celsius))

;; Time units enumeration
(define time-units
  '(seconds milliseconds microseconds nanoseconds minutes hours days weeks))

;; Time conversion to seconds
(define (time-to-seconds value unit)
  (case unit
    [(seconds) value]
    [(milliseconds) (/ value 1000)]
    [(microseconds) (/ value 1000000)]
    [(nanoseconds) (/ value 1000000000)]
    [(minutes) (* value 60)]
    [(hours) (* value 3600)]
    [(days) (* value 86400)]
    [(weeks) (* value 604800)]
    [else value]))

;; Time conversion from seconds
(define (seconds-to-time secs unit)
  (case unit
    [(seconds) secs]
    [(milliseconds) (* secs 1000)]
    [(microseconds) (* secs 1000000)]
    [(nanoseconds) (* secs 1000000000)]
    [(minutes) (/ secs 60)]
    [(hours) (/ secs 3600)]
    [(days) (/ secs 86400)]
    [(weeks) (/ secs 604800)]
    [else secs]))

;; Convert time between units
(define (convert-time value from-unit to-unit)
  (seconds-to-time (time-to-seconds value from-unit) to-unit))

;; Convenience functions for time
(define (hours->minutes h) (convert-time h 'hours 'minutes))
(define (minutes->hours m) (convert-time m 'minutes 'hours))
(define (hours->seconds h) (convert-time h 'hours 'seconds))
(define (seconds->hours s) (convert-time s 'seconds 'hours))
(define (days->hours d) (convert-time d 'days 'hours))
(define (hours->days h) (convert-time h 'hours 'days))

;; Data units enumeration
(define data-units
  '(bytes kilobytes megabytes gigabytes terabytes
    kibibytes mebibytes gibibytes tebibytes))

;; Data conversion to bytes
(define (data-to-bytes value unit)
  (case unit
    [(bytes) value]
    [(kilobytes) (* value 1000)]
    [(megabytes) (* value 1000000)]
    [(gigabytes) (* value 1000000000)]
    [(terabytes) (* value 1000000000000)]
    [(kibibytes) (* value 1024)]
    [(mebibytes) (* value 1048576)]
    [(gibibytes) (* value 1073741824)]
    [(tebibytes) (* value 1099511627776)]
    [else value]))

;; Data conversion from bytes
(define (bytes-to-data bytes unit)
  (case unit
    [(bytes) bytes]
    [(kilobytes) (/ bytes 1000)]
    [(megabytes) (/ bytes 1000000)]
    [(gigabytes) (/ bytes 1000000000)]
    [(terabytes) (/ bytes 1000000000000)]
    [(kibibytes) (/ bytes 1024)]
    [(mebibytes) (/ bytes 1048576)]
    [(gibibytes) (/ bytes 1073741824)]
    [(tebibytes) (/ bytes 1099511627776)]
    [else bytes]))

;; Convert data size between units
(define (convert-data value from-unit to-unit)
  (bytes-to-data (data-to-bytes value from-unit) to-unit))

;; Convenience functions for data
(define (bytes->kilobytes b) (convert-data b 'bytes 'kilobytes))
(define (kilobytes->bytes kb) (convert-data kb 'kilobytes 'bytes))
(define (bytes->megabytes b) (convert-data b 'bytes 'megabytes))
(define (megabytes->bytes mb) (convert-data mb 'megabytes 'bytes))
(define (bytes->gigabytes b) (convert-data b 'bytes 'gigabytes))
(define (gigabytes->bytes gb) (convert-data gb 'gigabytes 'bytes))
(define (kibibytes->kilobytes kib) (/ (* kib 1024) 1000))
(define (kilobytes->kibibytes kb) (/ (* kb 1000) 1024))
