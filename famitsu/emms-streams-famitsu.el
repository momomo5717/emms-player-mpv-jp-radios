;;; emms-streams-famitsu.el --- emms stream list for ファミ通.com -*- lexical-binding: t -*-

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

;; This provides emms stream list for ファミ通.com.

;; (require 'emms-streams-famitsu)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'url-queue)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-famitsu--podcast-url-alist
  '((asami_ssg . "http://www.famitsu.com/blog/asami_ssg/asami_ssg_pcas.rdf")
    (haramaru . "http://eb-dl.pod.tv/flv/blog/radio/haramaru/haramaru_pcas.rdf")))

(defvar emms-stream-famitsu--stream-alist-cache
  (cl-loop for (key . _) in emms-stream-famitsu--podcast-url-alist
           collect (cons key nil))
  "Cache for stream alist.")

(defvar emms-stream-famitsu-podcast-ascending-order-p nil)

(defvar emms-stream-famitsu-podcast-collect-last-n 1
  "Collect last n items.")

(defun emms-stream-famitsu--url-to-html (url &optional xmlp buf)
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (kill-buffer buf)))))

(cl-defun emms-stream-famitsu--podcast-n-items (rss &optional (n 1) (reversep nil))
  "Collect last n items"
  (let ((channel (assq 'channel rss)))
    (funcall (if reversep #'nreverse #'identity)
             (cl-loop with count = 0
                      for node in channel
                      while (< count n)
                      when (eq (car-safe node) 'item)
                      collect node
                      and do (cl-incf count)))))

(defun emms-stream-famitsu--item-to-streamlist (item)
  "Return streamlist of ITEM."
  (let* ((title (car (xml-node-children (car (xml-get-children item 'title)))))
         (enclosure (car (xml-get-children item 'enclosure)))
         (url (xml-get-attribute-or-nil enclosure 'url))
         (pubDate (car (xml-node-children (car (xml-get-children item 'pubDate))))))
    (list (format "%s : %s" title
                  (format-time-string "%Y/%m/%d (%a)" (date-to-time pubDate)))
          (format "famitsu://%s" url) 1 'streamlist)))

(cl-defun emms-stream-famitsu--rss-to-stream-list (rss &optional (n 1))
  "Retrun stream list of RSS.
Collect last N items from RSS."
  (mapcar #'emms-stream-famitsu--item-to-streamlist
          (emms-stream-famitsu--podcast-n-items
           rss n emms-stream-famitsu-podcast-ascending-order-p)))

(cl-defun emms-stream-famitsu--url-alist-to-stream-alist (url-alist &optional (n 1))
  "Return stream-alist of URL-ALIST.
Collect last N items from each URL-ALIST."
  (let ((stream-alist (cl-loop for (key . _) in url-alist
                               collect (cons key nil))))
    (cl-loop for (key . rss-url) in url-alist do
             (setcdr
              (assq key stream-alist)
              (emms-stream-famitsu--rss-to-stream-list
               (emms-stream-famitsu--url-to-html rss-url t) n)))
    stream-alist))

(defun emms-stream-famitsu--fetch-stream-alist (&optional updatep)
  "Return famitsu stream list.
If UPDATEP is no-nil, cache is updated."
  (if (or updatep (null (cl-loop for (_ . ls)
                                 in emms-stream-famitsu--stream-alist-cache
                                 never (null ls))))
      (setq emms-stream-famitsu--stream-alist-cache
            (emms-stream-famitsu--url-alist-to-stream-alist
             emms-stream-famitsu--podcast-url-alist
             emms-stream-famitsu-podcast-collect-last-n))
    emms-stream-famitsu--stream-alist-cache))

(defun emms-stream-famitsu-update-cache-async-1 (key rss-url)
  "Set stream-list of KEY from RSS-URL."
  (url-queue-retrieve
   rss-url
   (lambda (status &rest _)
     (if  (plist-get status :error)
         (message "Failed to get famitsu stream list : %s"
                  (plist-get status :error))
       (setcdr (assq key emms-stream-famitsu--stream-alist-cache)
               (emms-stream-famitsu--rss-to-stream-list
                (emms-stream-famitsu--url-to-html nil t (current-buffer))
                emms-stream-famitsu-podcast-collect-last-n))
       (when (cl-loop for (_ . ls)
                      in emms-stream-famitsu--stream-alist-cache
                      never (null ls))
         (message "Updated famitsu stream list cache"))))))

;;;###autoload
(defun emms-stream-famitsu-update-cache-async ()
  "Update cache asynchronously."
  (setq emms-stream-famitsu--stream-alist-cache
        (cl-loop for (key . _) in emms-stream-famitsu--podcast-url-alist
                 collect (cons key nil)))
  (cl-loop for (key . rss-url) in emms-stream-famitsu--podcast-url-alist do
           (emms-stream-famitsu-update-cache-async-1 key rss-url)))

;;;###autoload
(defun emms-stream-famitsu-get-stream-list ()
  "Return new stream-list from cache."
  (cl-loop
   with ls = nil
   for (key . _) in emms-stream-famitsu--podcast-url-alist do
   (dolist (stream (cdr (assq key emms-stream-famitsu--stream-alist-cache)))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-famitsu-add-bookmark (&optional updatep)
  "Create famitsu bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-famitsu-update-cache-async)
    (let ((buf (get-buffer emms-stream-buffer-name)))
      (unless (buffer-live-p buf)
        (error "%s is not a live buffer" emms-stream-buffer-name))
      (set-buffer buf))
    (let* ((line       (emms-line-number-at-pos (point)))
           (index      (+ (/ line 2) 1)))
      (cl-loop for (_ . stream-list)
               in (emms-stream-famitsu--fetch-stream-alist updatep) do
               (dolist (stream stream-list)
                 (setq emms-stream-list (emms-stream-insert-at index stream
                                                               emms-stream-list))
                 (cl-incf index)))
      (emms-stream-redisplay)
      (goto-char (point-min))
      (forward-line (1- line)))))

;; For media player

;;;###autoload
(defun emms-stream-famitsu-stream-url-to-mp3 (stream-url)
  "Return mp3 link from STREAM-URL."
  (replace-regexp-in-string "\\`famitsu://" "" stream-url))

(provide 'emms-streams-famitsu)
;;; emms-streams-famitsu.el ends here
