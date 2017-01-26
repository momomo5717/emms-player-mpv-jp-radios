;;; emms-streams-lantis.el --- emms stream list for Lantis -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 momomo5717

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

;; This provides emms stream list for Lantis.

;; (require 'emms-streams-lantis)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-lantis--url "http://lantis-net.com/")

(defvar emms-stream-lantis--stream-list-cache nil
  "Cache for stream list.")

(defvar emms-stream-lantis--url-headers
  '(("User-Agent" .
     "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/600.8.9 (KHTML, like Gecko) Version/8.0.8 Safari/600.8.9"))
  "To get m3u8 link.")

(cl-defun emms-stream-lantis--xml-collect-node
    (name xml-ls &key (test #'identity) (getter #'identity))
  "Collect nodes of NAME from XML-LS.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
Object returned by GETTER is collected."
  (cl-labels
      ((collect-name-node (xml-ls &optional ls)
        (cond ((not (consp xml-ls)) ls)
              ((and (eq name (car xml-ls)) (funcall test xml-ls))
               (cons (funcall getter xml-ls) ls))
              (t (dolist (e xml-ls ls)
                   (when (and (car-safe e) (symbolp (car e)))
                     (setq ls (collect-name-node e ls))))))))
    (nreverse (collect-name-node xml-ls))))

(defun emms-stream-lantis--url-to-html (url &optional xmlp buf)
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (kill-buffer buf)))))

(defun emms-stream-lantis--html-to-stream-list (html)
  "Retrun list which has list of plist from HTML."
  (cl-labels
      ((get-title-and-link
        (node)
        (emms-stream-lantis--xml-collect-node
         'a node
         :test
         (lambda (a-node) (xml-get-attribute-or-nil a-node 'title))
         :getter
         (lambda (a-node)
           (list 'title (xml-get-attribute a-node 'title)
                 'href (xml-get-attribute a-node 'href)))))
       (collect-titles
        (html)
        (emms-stream-lantis--xml-collect-node
         'div html
         :test
         (lambda (node) (equal (xml-get-attribute-or-nil node 'class)
                           "titles"))
         :getter #'get-title-and-link)))
    (cl-loop for (a0 a1) in (collect-titles html)
             for href = (plist-get a1 'href)
             for date = (and (stringp href) (cl-third (split-string href "_")))
             when date
             collect (list (format "%s : %s"
                                   (plist-get a0 'title)
                                   (if (and (stringp date) (eq (length date) 6))
                                       (concat
                                        (cl-loop for c across date
                                                 for i from 0
                                                 collect c
                                                 when (and (< i 5) (cl-oddp i))
                                                 collect ?/))
                                     date))
                           (format "lantis://%s" href)
                           1 'streamlist))))

(defun emms-stream-lantis--fetch-stream-list (&optional updatep)
  "Return lantis stream list.
If UPDATEP is no-nil, cache is updated."
  (if (or updatep (null emms-stream-lantis--stream-list-cache))
      (setq emms-stream-lantis--stream-list-cache
            (emms-stream-lantis--html-to-stream-list
             (let ((url-request-extra-headers
                    emms-stream-lantis--url-headers))
               (emms-stream-lantis--url-to-html emms-stream-lantis--url))))
    emms-stream-lantis--stream-list-cache))

;;;###autoload
(defun emms-stream-lantis-update-cache-async ()
  "Update cache asynchronously."
  (let ((url-request-extra-headers
         emms-stream-lantis--url-headers))
    (url-retrieve
     emms-stream-lantis--url
     (lambda (status &rest _)
       (when (memq :error status)
         (error "Failed to get lantis stream list : %s" (cdr status)))
       (setq emms-stream-lantis--stream-list-cache
             (emms-stream-lantis--html-to-stream-list
              (emms-stream-lantis--url-to-html nil nil (current-buffer))))
       (message "Updated lantis stream list cache")))))

;;;###autoload
(defun emms-stream-lantis-get-stream-list ()
  "Return new stream-list from cache."
  (cl-copy-list emms-stream-lantis--stream-list-cache))

;;;###autoload
(defun emms-stream-lantis-add-bookmark (&optional updatep)
  "Create lantis bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-lantis-update-cache-async)
    (let ((buf (get-buffer emms-stream-buffer-name)))
      (unless (buffer-live-p buf)
        (error "%s is not a live buffer" emms-stream-buffer-name))
      (set-buffer buf))
    (let* ((stream-list (emms-stream-lantis--fetch-stream-list updatep))
           (line        (emms-line-number-at-pos (point)))
           (index       (+ (/ line 2) 1)))
      (dolist (stream stream-list)
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index))
      (emms-stream-redisplay)
      (goto-char (point-min))
      (forward-line (1- line)))))

;; For media player

;;;###autoload
(defun emms-stream-lantis-stream-url-to-m3u8 (stream-url)
  "Return m3u8 from STREAM-URL."
  (replace-regexp-in-string "\\`lantis://" "" stream-url))

(provide 'emms-streams-lantis)
;;; emms-streams-lantis.el ends here
