;;; emms-streams-onsen.el --- emms stream list for 音泉 -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

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

;; (require 'emms-stream-onsen)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)

(defvar emms-stream-onsen--stream-alist-cache nil
  "Cache for stream alist.")

(cl-defun emms-stream-onsen--xml-collect-node
        (name xml-ls &key (test #'identity) (getter #'identity))
  "Collect nodes of NAME from XML-LS.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
Object returned by GETTER is collected."
  (cl-labels ((collect-name-node (xml-ls ls)
                (cond
                 ((atom xml-ls) ls)
                 ((consp (car xml-ls))
                  (collect-name-node (car xml-ls)
                                     (collect-name-node (cdr xml-ls) ls)))

                 ((and (eq (car xml-ls) name)
                       (funcall test xml-ls))
                  (cons (funcall getter xml-ls) ls))
                 ((or (null (car xml-ls))
                      (not (symbolp (car xml-ls))))
                  (collect-name-node (cdr xml-ls) ls))
                 ((symbolp (car xml-ls))
                  (collect-name-node (xml-node-children xml-ls) ls ))
                 (t ls))))
    (collect-name-node xml-ls nil)))

(defun emms-stream-onsen--url-to-html (url &optional xml-p)
  (let ((buf (url-retrieve-synchronously url)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xml-p #'libxml-parse-xml-region
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
             (emms-stream-onsen--url-to-html "http://www.onsen.ag")))
    emms-stream-onsen--stream-alist-cache))

(defun emms-stream-onsen--add-bookmark-dows (days &optional updatep)
  "Helper function for `emms-stream-onsen-add-bookmark', etc.
Add stream list of DAYS.
If UPDATEP is non-nil, cache is updated."
  (set-buffer (get-buffer-create emms-stream-buffer-name))
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
(defun emms-stream-onsen-add-bookmark (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows
   '("mon" "tue" "wed" "thu" "fri" "sat" "sun") updatep))

;;;###autoload
(defun emms-stream-onsen-add-bookmark-mon (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows '("mon") updatep))

;;;###autoload
(defun emms-stream-onsen-add-bookmark-tue (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows '("tue") updatep))

;;;###autoload
(defun emms-stream-onsen-add-bookmark-wed (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows '("wed") updatep))

;;;###autoload
(defun emms-stream-onsen-add-bookmark-thu (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows '("thu") updatep))

;;;###autoload
(defun emms-stream-onsen-add-bookmark-fri (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows '("fri") updatep))

;;;###autoload
(defun emms-stream-onsen-add-bookmark-sat-sun (&optional updatep)
  "Create onsen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (emms-stream-onsen--add-bookmark-dows '("sat" "sun") updatep))

(provide 'emms-streams-onsen)
;;; emms-streams-onsen.el ends here
