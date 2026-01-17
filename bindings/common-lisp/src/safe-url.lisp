;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeURL - URL parsing and validation for Common Lisp

(in-package #:proven)

;;; Default port numbers for common schemes
(defparameter +default-ports+
  '(("http" . 80)
    ("https" . 443)
    ("ftp" . 21)
    ("ssh" . 22)
    ("ws" . 80)
    ("wss" . 443))
  "Association list of scheme to default port mappings.")

;;; URL structure
(defstruct url
  "Represents a parsed URL with all components."
  (scheme nil :type (or null string))
  (userinfo nil :type (or null string))
  (host nil :type (or null string))
  (port nil :type (or null integer))
  (path "" :type string)
  (query nil :type (or null string))
  (fragment nil :type (or null string)))

;;; URL result for parsing
(defstruct url-result
  "Result of URL parsing operation."
  (url nil :type (or null url))
  (error nil :type (or null string))
  (ok-p nil :type boolean))

;;; Reserved characters for URL encoding
(defun unreserved-char-p (char-code)
  "Check if character code represents an unreserved URL character."
  (or (and (>= char-code 65) (<= char-code 90))   ; A-Z
      (and (>= char-code 97) (<= char-code 122))  ; a-z
      (and (>= char-code 48) (<= char-code 57))   ; 0-9
      (= char-code 45)   ; -
      (= char-code 46)   ; .
      (= char-code 95)   ; _
      (= char-code 126))); ~

(defun url-encode-component (component-string)
  "Percent-encode a URL component string."
  (with-output-to-string (output-stream)
    (loop for char-value across component-string
          for char-code = (char-code char-value)
          do (if (unreserved-char-p char-code)
                 (write-char char-value output-stream)
                 (format output-stream "%~2,'0X" char-code)))))

(defun url-decode-component (encoded-string)
  "Decode a percent-encoded URL component string."
  (with-output-to-string (output-stream)
    (let ((length-of-string (length encoded-string))
          (index 0))
      (loop while (< index length-of-string)
            do (let ((current-char (char encoded-string index)))
                 (cond
                   ((and (char= current-char #\%)
                         (< (+ index 2) length-of-string))
                    (let ((hex-string (subseq encoded-string (1+ index) (+ index 3))))
                      (handler-case
                          (progn
                            (write-char (code-char (parse-integer hex-string :radix 16)) output-stream)
                            (incf index 3))
                        (error ()
                          (write-char current-char output-stream)
                          (incf index)))))
                   ((char= current-char #\+)
                    (write-char #\Space output-stream)
                    (incf index))
                   (t
                    (write-char current-char output-stream)
                    (incf index))))))))

(defun parse-query-string (query-string)
  "Parse a query string into an association list of key-value pairs."
  (when (and query-string (plusp (length query-string)))
    (let ((query-pairs '()))
      (dolist (pair-string (split-string query-string #\&))
        (let ((equals-position (position #\= pair-string)))
          (if equals-position
              (push (cons (url-decode-component (subseq pair-string 0 equals-position))
                          (url-decode-component (subseq pair-string (1+ equals-position))))
                    query-pairs)
              (push (cons (url-decode-component pair-string) "") query-pairs))))
      (nreverse query-pairs))))

(defun format-query-string (query-pairs)
  "Format an association list of key-value pairs into a query string."
  (when query-pairs
    (with-output-to-string (output-stream)
      (loop for (key . value) in query-pairs
            for first-pair = t then nil
            do (unless first-pair (write-char #\& output-stream))
               (write-string (url-encode-component key) output-stream)
               (write-char #\= output-stream)
               (write-string (url-encode-component value) output-stream)))))

(defun split-string (input-string delimiter-char)
  "Split a string by delimiter character."
  (let ((result '())
        (current-start 0))
    (loop for position from 0 below (length input-string)
          when (char= (char input-string position) delimiter-char)
            do (push (subseq input-string current-start position) result)
               (setf current-start (1+ position)))
    (push (subseq input-string current-start) result)
    (nreverse result)))

(defun parse-url (url-string)
  "Parse a URL string into its components.
   Returns a URL-RESULT with URL and OK-P."
  (handler-case
      (let* ((working-string url-string)
             (scheme nil)
             (userinfo nil)
             (host nil)
             (port nil)
             (path "")
             (query nil)
             (fragment nil))
        ;; Extract fragment
        (let ((fragment-position (position #\# working-string)))
          (when fragment-position
            (setf fragment (subseq working-string (1+ fragment-position)))
            (setf working-string (subseq working-string 0 fragment-position))))
        ;; Extract query
        (let ((query-position (position #\? working-string)))
          (when query-position
            (setf query (subseq working-string (1+ query-position)))
            (setf working-string (subseq working-string 0 query-position))))
        ;; Extract scheme
        (let ((scheme-position (search "://" working-string)))
          (when scheme-position
            (setf scheme (string-downcase (subseq working-string 0 scheme-position)))
            (setf working-string (subseq working-string (+ scheme-position 3)))))
        ;; Extract path
        (let ((path-position (position #\/ working-string)))
          (when path-position
            (setf path (subseq working-string path-position))
            (setf working-string (subseq working-string 0 path-position))))
        ;; Extract userinfo
        (let ((at-position (position #\@ working-string)))
          (when at-position
            (setf userinfo (subseq working-string 0 at-position))
            (setf working-string (subseq working-string (1+ at-position)))))
        ;; Extract port
        (let ((colon-position (position #\: working-string :from-end t)))
          (when colon-position
            (let ((port-string (subseq working-string (1+ colon-position))))
              (handler-case
                  (progn
                    (setf port (parse-integer port-string))
                    (setf working-string (subseq working-string 0 colon-position)))
                (error () nil)))))
        ;; Remaining is host
        (when (plusp (length working-string))
          (setf host (string-downcase working-string)))
        (make-url-result
         :url (make-url :scheme scheme
                        :userinfo userinfo
                        :host host
                        :port port
                        :path path
                        :query query
                        :fragment fragment)
         :ok-p t))
    (error (condition-object)
      (make-url-result :error (format nil "Parse error: ~A" condition-object) :ok-p nil))))

(defun format-url (url-object)
  "Format a URL structure back into a string."
  (with-output-to-string (output-stream)
    (when (url-scheme url-object)
      (format output-stream "~A://" (url-scheme url-object)))
    (when (url-userinfo url-object)
      (format output-stream "~A@" (url-userinfo url-object)))
    (when (url-host url-object)
      (write-string (url-host url-object) output-stream))
    (when (url-port url-object)
      (format output-stream ":~D" (url-port url-object)))
    (write-string (url-path url-object) output-stream)
    (when (url-query url-object)
      (format output-stream "?~A" (url-query url-object)))
    (when (url-fragment url-object)
      (format output-stream "#~A" (url-fragment url-object)))))

(defun url-valid-p (url-object)
  "Check if a URL structure is valid."
  (and url-object
       (url-scheme url-object)
       (url-host url-object)))

(defun url-http-p (url-object)
  "Check if URL uses HTTP scheme."
  (and url-object
       (url-scheme url-object)
       (string= (url-scheme url-object) "http")))

(defun url-https-p (url-object)
  "Check if URL uses HTTPS scheme."
  (and url-object
       (url-scheme url-object)
       (string= (url-scheme url-object) "https")))

(defun url-secure-p (url-object)
  "Check if URL uses a secure scheme (HTTPS, WSS, etc.)."
  (and url-object
       (url-scheme url-object)
       (member (url-scheme url-object) '("https" "wss" "ftps" "sftp") :test #'string=)))

(defun normalize-url (url-object)
  "Normalize a URL by lowercasing scheme and host, removing default ports."
  (when url-object
    (let ((normalized-url (copy-url url-object)))
      ;; Lowercase scheme
      (when (url-scheme normalized-url)
        (setf (url-scheme normalized-url) (string-downcase (url-scheme normalized-url))))
      ;; Lowercase host
      (when (url-host normalized-url)
        (setf (url-host normalized-url) (string-downcase (url-host normalized-url))))
      ;; Remove default port
      (when (and (url-scheme normalized-url) (url-port normalized-url))
        (let ((default-port (cdr (assoc (url-scheme normalized-url) +default-ports+ :test #'string=))))
          (when (and default-port (= (url-port normalized-url) default-port))
            (setf (url-port normalized-url) nil))))
      ;; Ensure path has leading slash for absolute URLs
      (when (and (url-host normalized-url)
                 (or (zerop (length (url-path normalized-url)))
                     (not (char= (char (url-path normalized-url) 0) #\/))))
        (setf (url-path normalized-url) (concatenate 'string "/" (url-path normalized-url))))
      normalized-url)))

(defun join-url-paths (base-path relative-path)
  "Join two URL paths together."
  (cond
    ;; Absolute path
    ((and (plusp (length relative-path))
          (char= (char relative-path 0) #\/))
     relative-path)
    ;; Empty base
    ((zerop (length base-path))
     relative-path)
    ;; Join paths
    (t
     (let ((base-dir (if (find #\/ base-path :from-end t)
                         (subseq base-path 0 (1+ (position #\/ base-path :from-end t)))
                         "/")))
       (concatenate 'string base-dir relative-path)))))
