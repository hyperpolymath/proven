;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafePath - Directory traversal prevention for Common Lisp

(in-package #:proven)

;;; Path result struct
(defstruct path-result
  (path "" :type string)
  (error "" :type string)
  (ok-p nil :type boolean))

;;; Check if string contains substring
(defun contains-p (string substring)
  "Return T if STRING contains SUBSTRING."
  (search substring string))

;;; Check for path traversal patterns
(defun has-traversal-p (path)
  "Return T if PATH contains traversal patterns."
  (let ((lower-path (string-downcase path)))
    (or (contains-p path "..")
        (contains-p path "./")
        (contains-p lower-path "%2e%2e")
        (contains-p lower-path "%00"))))

;;; Sanitize filename by removing dangerous characters
(defun sanitize-filename (input)
  "Replace dangerous filename characters with underscores."
  (map 'string
       (lambda (c)
         (if (member c '(#\/ #\\ #\: #\* #\? #\" #\< #\> #\|))
             #\_
             c))
       input))

;;; Safely join paths
(defun safe-path-join (base filename)
  "Safely join BASE path with FILENAME.
   Returns a PATH-RESULT struct."
  (cond
    ((has-traversal-p filename)
     (make-path-result :path ""
                       :error "Path traversal detected"
                       :ok-p nil))
    (t
     (let* ((safe-name (sanitize-filename filename))
            (joined (if (char= (char base (1- (length base))) #\/)
                        (concatenate 'string base safe-name)
                        (concatenate 'string base "/" safe-name))))
       (make-path-result :path joined
                         :error ""
                         :ok-p t)))))
