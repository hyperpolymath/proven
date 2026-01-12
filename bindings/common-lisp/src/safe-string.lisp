;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeString - XSS prevention for Common Lisp

(in-package #:proven)

;;; Escape HTML special characters
(defun escape-html (input)
  "Escape HTML special characters in INPUT string."
  (with-output-to-string (out)
    (loop for char across input
          do (case char
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#x27;" out))
               (otherwise (write-char char out))))))

;;; Escape SQL single quotes
(defun escape-sql (input)
  "Escape SQL single quotes by doubling them."
  (with-output-to-string (out)
    (loop for char across input
          do (if (char= char #\')
                 (write-string "''" out)
                 (write-char char out)))))

;;; Escape JavaScript special characters
(defun escape-js (input)
  "Escape JavaScript special characters in INPUT string."
  (with-output-to-string (out)
    (loop for char across input
          do (case char
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\' (write-string "\\'" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (#\< (write-string "\\u003C" out))
               (#\> (write-string "\\u003E" out))
               (#\/ (write-string "\\/" out))
               (otherwise (write-char char out))))))

;;; Check if character is alphanumeric
(defun alpha-num-p (char)
  "Return T if CHAR is alphanumeric."
  (or (alpha-char-p char)
      (digit-char-p char)))

;;; Sanitize to alphanumeric + underscore + hyphen
(defun sanitize-default (input)
  "Remove all characters except alphanumeric, underscore, and hyphen."
  (remove-if-not (lambda (c)
                   (or (alpha-num-p c)
                       (char= c #\_)
                       (char= c #\-)))
                 input))

;;; URL encode
(defun url-encode (input)
  "URL-encode the INPUT string."
  (with-output-to-string (out)
    (loop for char across input
          do (cond
               ((or (alpha-num-p char)
                    (member char '(#\- #\_ #\. #\~)))
                (write-char char out))
               (t
                (format out "%~2,'0X" (char-code char)))))))

;;; Convert to URL-safe slug
(defun slugify (input)
  "Convert INPUT to a URL-safe slug."
  (let* ((lower (string-downcase input))
         (cleaned (remove-if-not (lambda (c)
                                   (or (alpha-num-p c)
                                       (char= c #\Space)
                                       (char= c #\-)))
                                 lower)))
    ;; Replace spaces with hyphens
    (let ((result (substitute #\- #\Space cleaned)))
      ;; Collapse multiple hyphens
      (with-output-to-string (out)
        (let ((prev-hyphen nil))
          (loop for char across result
                do (cond
                     ((char= char #\-)
                      (unless prev-hyphen
                        (write-char char out)
                        (setf prev-hyphen t)))
                     (t
                      (write-char char out)
                      (setf prev-hyphen nil)))))))))
