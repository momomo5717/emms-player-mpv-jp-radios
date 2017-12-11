;;; emms-streams-anitama.el --- emms stream list for アニたまどっとコム -*- lexical-binding: t -*-

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

;; This provides emms stream list for アニたまどっとコム インターネットラジオ.

;; (require 'emms-streams-anitama)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'url-queue)
(require 'url-cookie)
(require 'emms-streams-nico-util)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(cl-defun emms-stream-anitama--xml-collect-node
    (name xml-ls &key (test 'identity) (getter 'identity))
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

(defun emms-stream-anitama--url-to-html (url &optional xmlp buf)
  "Return html from URL.
If XMLP is non-nil, `libxml-parse-xml-region' will be used.
If BUF is non-nil, it will be used instead of URL."
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;; For stream-list

(defvar emms-stream-anitama-stream-list-cache nil)

(defvar emms-stream-anitama--url-video
  "http://ch.nicovideo.jp/anitama-ch/video?sort=f&order=d")

(defun emms-stream-anitama--item-to-url-title (item)
  "Return list of url and title from ITEM."
  (car (emms-stream-anitama--xml-collect-node
        'h6 item
        :test (lambda (node) (equal (xml-get-attribute node 'class) "title"))
        :getter (lambda (node)
                  (let ((a (car (xml-get-children node 'a))))
                    (list (xml-get-attribute a 'href)
                          (xml-get-attribute a 'title)))))))

(defun emms-stream-anitama--description-to-personality (node)
  "Substring a personality from NODE's description."
  (let ((str (car (xml-node-children node))))
    (if (and (stringp str) (string-match "：\\(.+\\)アニたま" str))
        (replace-regexp-in-string "　$" "" (match-string 1 str))
      "")))

(defun emms-stream-anitama--item-to-personality (item)
  "Return personality from ITEM."
  (car (emms-stream-anitama--xml-collect-node
        'p item
        :test (lambda (node) (equal (xml-get-attribute node 'class) "description"))
        :getter #'emms-stream-anitama--description-to-personality)))

(defun emms-stream-anitama--item-to-date (item)
  "Return DATE from ITEM."
  (car (emms-stream-anitama--xml-collect-node
        'var item
        :test (lambda (node) (xml-get-attribute-or-nil node 'title))
        :getter (lambda (node) (xml-get-attribute node 'title)))))

(defun emms-stream-anitama--item-to-streamlist (item)
  "Return streamlist from ITEM."
  (let* ((url-title (emms-stream-anitama--item-to-url-title item))
         (url (cl-first url-title))
         (title (cl-second url-title))
         (personality (emms-stream-anitama--item-to-personality item))
         (date (car (split-string (emms-stream-anitama--item-to-date item)))))
    (list (concat title " : " personality " : " (car (split-string date)))
          (concat "anitama://" url) 1 'streamlist)))

(defun emms-stream-anitama--html-to-stream-list (html)
  "Return stream list from HTML."
  (cl-loop with one-week = 604800
           with ctime = (current-time)
           for item
           in (emms-stream-anitama--xml-collect-node
               'li html
               :test (lambda (node) (equal (xml-get-attribute node 'class) "item")))
           for time = (date-to-time (replace-regexp-in-string
                                     "/" "-"
                                     (emms-stream-anitama--item-to-date item)))
           when (< (float-time (time-subtract ctime time)) one-week)
           collect (emms-stream-anitama--item-to-streamlist item)))

(defun emms-stream-anitama-fetch-stream-list (&optional updatep)
  "Retrun anitama stream list.
If UPDATEP is no-nil, cache is updated."
  (if (and (not updatep) (consp emms-stream-anitama-stream-list-cache))
      emms-stream-anitama-stream-list-cache
    (setq emms-stream-anitama-stream-list-cache
          (emms-stream-anitama--html-to-stream-list
           (emms-stream-anitama--url-to-html emms-stream-anitama--url-video)))))

;;;###autoload
(defun emms-stream-anitama-update-cache-async ()
  "Update cache asynchronously."
  (url-queue-retrieve
     emms-stream-anitama--url-video
     (lambda (status &rest _)
       (if (memq :error status)
           (message "Failed to get anitama stream list : %s" (cdr status))
         (setq emms-stream-anitama-stream-list-cache
               (emms-stream-anitama--html-to-stream-list
                (emms-stream-anitama--url-to-html nil nil (current-buffer))))
         (message "Updated anitama stream list cache")))))

;;;###autoload
(defun emms-stream-anitama-get-stream-list ()
  "Return new stream-list from cache."
  (cl-copy-list emms-stream-anitama-stream-list-cache))

;;;###autoload
(defun emms-stream-anitama-add-bookmark (&optional updatep)
  "Create anitama bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1)
      (emms-stream-anitama-update-cache-async)
   (let ((buf (get-buffer emms-stream-buffer-name)))
     (unless (buffer-live-p buf)
       (error "%s is not a live buffer" emms-stream-buffer-name))
     (set-buffer buf))
   (let* ((stream-list (emms-stream-anitama-fetch-stream-list updatep))
          (line       (emms-line-number-at-pos (point)))
          (index      (+ (/ line 2) 1)))
     (dolist (stream stream-list)
       (setq emms-stream-list (emms-stream-insert-at index stream
                                                     emms-stream-list))
       (cl-incf index))
     (emms-stream-redisplay)
     (goto-char (point-min))
     (forward-line (1- line)))))

;; For media player

;;;###autoload
(defun emms-stream-anitama--stream-url-to-url (stream-url)
  "Replace anitama:// of STREAM-URL with empty string."
  (replace-regexp-in-string "\\`anitama://" "" stream-url))

;;;###autoload
(defun emms-stream-anitama-stream-url-to-nicovideo-url (stream-url)
  "Return nicovideo url for STREAM-URL."
  (if emms-stream-nico-use-old-api
      (emms-stream-nico-old-nico-url-to-nicovideo-url
       (emms-stream-anitama--stream-url-to-url stream-url))
    (emms-stream-nico-url-to-nicovideo stream-url)))

(provide 'emms-streams-anitama)
;;; emms-streams-anitama.el ends here
