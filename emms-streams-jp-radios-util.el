;;; emms-streams-jp-radios-util.el --- Utility functions -*- lexical-binding: t -*-

;; Copyright (C) 2017 momomo5717

;; URL: https://github.com/momomo5717/emms-player-mpv-jp-radios

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utility functions.

;; Setup:
;;
;; (require 'emms-streams-jp-radios-util)

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'xml)
(require 'url)

(cl-defun emms-stream-jpr-get-node
    (node name  &key (test 'identity) (getter 'identity))
  "Collect nodes of NAME from node.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
If NAME is :ignore, all node name will be tested.
Object returned by GETTER is collected.
If GETTER is t, test result will be returned."
  (let ((top node) stack result test-result)
    (while (or top stack)
      (cond ((not (consp top))
             (setq top (pop stack)))
            ((and (or (eq name (car top))
                      (and (eq name :ignore)
                           (symbolp (car top))))
                  (setq test-result (funcall test top)))
             (push (if (eq getter t) test-result (funcall getter top))
                   result)
             (setq top (pop stack)))
            ((symbolp (car top))
             (setq top (xml-node-children top)))
            (t
             (when (cdr top) (push (cdr top) stack))
             (setq top (car top)))))
    (nreverse result)))

(defun emms-stream-jpr-url-to-html (url-or-buf &optional xmlp not-kill-buf)
  "Return html from URL-OR-BUF.
This function assumes a buffer has a response header.
If URL-OR-BUF is a buffer, it will be used.
If XMLP is non-nill, `libxml-parse-xml-region' will be used.
If NOT-KILL-BUF is non-nil, the used buffer will not be killed."
  (let ((buf (if (bufferp url-or-buf)
                 url-or-buf
               (url-retrieve-synchronously url-or-buf))))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (prog1
          (funcall (if xmlp #'libxml-parse-xml-region
                     #'libxml-parse-html-region) (point) (point-max))
        (when (and (not not-kill-buf) (buffer-live-p buf)) (kill-buffer buf))))))

(defun emms-stream-jpr-url-to-json (url-or-buf &optional not-kill-buf)
  "Return a json object from URL-OR-BUF.
This function assumes a buffer has a response header.
If URL-OR-BUF is a buffer, it will be used.
If NOT-KILL-BUF is non-nil, the used buffer will not be killed."
  (let ((buf (if (bufferp url-or-buf)
                 url-or-buf
               (url-retrieve-synchronously url-or-buf))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let ((p (point))
            (p-max (point-max)))
        (prog1 (with-temp-buffer
                 (insert-buffer-substring-no-properties buf p p-max)
                 (decode-coding-region (point-min) (point-max) 'utf-8)
                 (goto-char (point-min))
                 (json-read))
          (when (and (not not-kill-buf) (buffer-live-p buf)) (kill-buffer buf)))))))

(defun emms-stream-jpr-assq-get (keys list)
  "Return the value of KEYS from LIST.
KEYS can be a symbol or list of symbols."
  (dolist (key (if (symbolp keys) (list keys) keys) (cdr list))
    (setq list (assq key list))))

(defun emms-stream-jpr-write-cookies (domain file)
  "Write cookies of DOMAIN to FILE."
  (with-temp-buffer
    (dolist (cookie (assoc-default domain url-cookie-storage))
      (insert (mapconcat
               #'identity
               (list domain "TRUE" (url-cookie-localpart cookie)
                     (if (url-cookie-secure cookie) "TRUE" "FALSE")
                     (number-to-string
                      (floor (float-time (date-to-time (url-cookie-expires cookie)))))
                     (url-cookie-name cookie) (url-cookie-value cookie) "\n")
               "\t")))
    (write-region (point-min) (point-max) file nil 'nomessage)))

(provide 'emms-streams-jp-radios-util)
;;; emms-streams-jp-radios-util.el ends here
