;;; emms-streams-onsen.el --- emms stream list for 音泉 -*- lexical-binding: t -*-

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

;; This provides emms stream list for 音泉.

;; (require 'emms-streams-onsen)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'url-queue)
(require 'json)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-onsen--url-top "http://www.onsen.ag")

(defvar emms-stream-onsen--stream-alist-cache nil
  "Cache for stream alist.")

(defvar emms-stream-onsen--days
  '("mon" "tue" "wed" "thu" "fri" "sat" "sun"))

(cl-defun emms-stream-onsen--xml-collect-node
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

(defun emms-stream-onsen--url-to-html (url &optional xmlp buf)
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (kill-buffer buf)))))

(defun emms-stream-onsen--li-to-day-stream (li)
  "Retrun a day of the week and stream from LI."
  (let* ((id (xml-get-attribute li 'id))
         (week (xml-get-attribute li 'data-week))
         (update (xml-get-attribute li 'data-update))
         (title
          (car (emms-stream-onsen--xml-collect-node
                'h4 li
                :test (lambda (node) (equal (xml-get-attribute node 'class)
                                        "listItem"))
                :getter (lambda (node)
                          (car (xml-node-children
                                (car (xml-get-children node 'span))))))))
         (navigator
          (car (emms-stream-onsen--xml-collect-node
                'p li
                :test (lambda (node) (equal (xml-get-attribute node 'class)
                                        "navigator listItem"))
                :getter (lambda (node)
                          (car (xml-node-children
                                (car (xml-get-children node 'span)))))))))
    (list week
          (list (format "%s : %s : update %s (%s)" title navigator update week)
                (format "onsen://%s" id) 1 'streamlist))))

(defun emms-stream-onsen--top-html-to-stream-alist (html)
  "Return onsen stream alist from HTML."
  (let* (stream-alist
         (section-movieList
          (car (emms-stream-onsen--xml-collect-node
                'section html
                :test (lambda (node) (equal (xml-get-attribute-or-nil node 'id)
                                        "movieList")))))
         (div-listWrap
          (car (emms-stream-onsen--xml-collect-node
                'div section-movieList
                :test (lambda (node) (equal (xml-get-attribute-or-nil node 'class)
                                        "listWrap")))))
         (li-ls (xml-get-children
                 (car (xml-get-children div-listWrap 'ul)) 'li)))
    (dolist (li li-ls)
      (let* ((day-stream (emms-stream-onsen--li-to-day-stream li))
             (day-streams (assoc (car day-stream) stream-alist)))
        (if day-streams (nconc (cdr day-streams) (list (cl-second day-stream)))
          (push day-stream stream-alist))))
    stream-alist))

(defun emms-stream-onsen--fetch-stream-alist (&optional updatep)
  "Return onsen stream alist.
If UPDATEP is no-nil, cache is updated."
  (if (or updatep (null emms-stream-onsen--stream-alist-cache))
      (setq emms-stream-onsen--stream-alist-cache
            (emms-stream-onsen--top-html-to-stream-alist
             (emms-stream-onsen--url-to-html emms-stream-onsen--url-top)))
    emms-stream-onsen--stream-alist-cache))

;;;###autoload
(defun emms-stream-onsen-update-cache-async ()
  "Update cache asynchronously."
  (url-queue-retrieve
   emms-stream-onsen--url-top
   (lambda (status &rest _)
     (if (plist-get status :error)
         (message "Failed to get onsen stream list : %s"
                  (plist-get status :error))
       (setq emms-stream-onsen--stream-alist-cache
             (emms-stream-onsen--top-html-to-stream-alist
              (emms-stream-onsen--url-to-html nil nil (current-buffer))))
       (message "Updated onsen stream list cache")))))

(defun emms-stream-onsen--add-bookmark-dows (days &optional updatep)
  "Helper function for `emms-stream-onsen-add-bookmark', etc.
Add stream list of DAYS.
If UPDATEP is non-nil, cache is updated."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((stream-alist (emms-stream-onsen--fetch-stream-alist updatep))
         (line         (emms-line-number-at-pos (point)))
         (index        (+ (/ line 2) 1)))
    (dolist (day days)
      (dolist (stream (assoc-default day stream-alist))
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-onsen-get-stream-list ()
  "Return new stream-list from cache."
  (cl-loop
   with ls = nil
   for day in emms-stream-onsen--days do
   (dolist (stream (assoc-default day emms-stream-onsen--stream-alist-cache))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-onsen-add-bookmark (&optional updatep dow)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
DOW is a number of 0-6 or -1.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-onsen-update-cache-async)
    (unless (integerp dow)
      (let ((msg (concat "[0] All  [1] Mon  [2] Tue  [3] Wed  [4] Thu\n"
                         "         [5] Fri  [6] Sat/Sun\n"
                         "[-1] Update stream list cache asynchronously\n\n"
                         "Input a number of 0-6 or -1: ")))
        (while (not (and (integerp (setq dow (read-number msg)))
                         (<= -1 dow) (<= dow 6))))))
    (if (= dow -1) (emms-stream-onsen-update-cache-async)
      (emms-stream-onsen--add-bookmark-dows
       (cond ((zerop dow) emms-stream-onsen--days)
             ((= dow 6) '("sat" "sun"))
             (t (list (nth (1- dow) emms-stream-onsen--days))))
       updatep))))

;; For media player

(defun emms-stream-onsen--stream-url-to-json-obj (stream-url)
  "Return json obj from STREAM-URL."
  (let* ((id (file-name-nondirectory stream-url))
         (buf (url-retrieve-synchronously
               (url-expand-file-name
                (format "data/api/getMovieInfo/%s" id)
                emms-stream-onsen--url-top))))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (search-forward "callback(")
          (json-read))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;;###autoload
(defun emms-stream-onsen-stream-url-to-moviePath (stream-url)
  "Return pc link of moviePath from STREAM-URL."
  (let* ((json-obj (emms-stream-onsen--stream-url-to-json-obj stream-url))
         (moviePath (cdr (assq 'pc (cdr (assq 'moviePath json-obj))))))
    (unless moviePath (error "Failed to fetch moviePath"))
    moviePath))

(provide 'emms-streams-onsen)
;;; emms-streams-onsen.el ends here
