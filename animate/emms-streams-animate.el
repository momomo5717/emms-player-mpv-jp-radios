;;; emms-streams-animate.el --- emms stream list for animate.tv -*- lexical-binding: t -*-

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

;; This provides emms stream list for animate.tv.

;; (require 'emms-streams-animate)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)

;; Suppress warning messages
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-animate--url-top "http://sp.animatetimes.com")

(defvar emms-stream-animate--url-radio
  (url-expand-file-name "radio"emms-stream-animate--url-top))

(defvar emms-stream-animate--stream-alist-cache nil)

(defvar emms-stream-animate--days
  '("mon" "tue" "wed" "thu" "fri" "irr")
  "Days of the weekdays.")

(defun emms-stream-animate--url-http-user-agent-string ()
  "`http-user-agent-string' for animate."
  "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 3_0 like Mac OS X; en-us) AppleWebKit/528.18 (KHTML, like Gecko) Version/4.0 Mobile/7A341 Safari/528.16\r\n")

(defun emms-stream-animate--url-retrieve-synchronously
    (url &optional silent inhibit-cookies)
  "`url-retrieve-synchronously' for animate."
  (cl-letf (((symbol-function 'url-http-user-agent-string)
             (symbol-function 'emms-stream-animate--url-http-user-agent-string)))
    (url-retrieve-synchronously url silent inhibit-cookies)))

(defun emms-stream-animate--url-retrieve
    (url callback &optional cbargs silent inhibit-cookies)
  "`url-retrieve' for animate."
  (cl-letf (((symbol-function 'url-http-user-agent-string)
             (symbol-function 'emms-stream-animate--url-http-user-agent-string)))
    (url-retrieve url callback cbargs silent inhibit-cookies)))

(cl-defun emms-stream-animate--xml-collect-node
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

(defun emms-stream-animate--url-to-html (url &optional xmlp buf)
  "Return html from URL."
  (let ((buf (or buf (emms-stream-animate--url-retrieve-synchronously url))))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun emms-stream-animate--box-to-stream (box)
  "BOX to streamlist."
  (cl-labels ((get-span-content
               (content box)
               (car (emms-stream-animate--xml-collect-node
                     'span box
                     :test
                     (lambda (node) (equal (xml-get-attribute node 'class)
                                             content))
                     :getter
                     (lambda (node)
                       (cond
                        ((cl-find-if #'stringp (xml-node-children node)))
                        (t "")))))))
    (let ((title (get-span-content "title" box))
          (main  (get-span-content "main" box))
          (date  (car (split-string (get-span-content "date" box))))
          (href  (car (emms-stream-animate--xml-collect-node
                       'a box :test (lambda (node) (xml-get-attribute-or-nil node 'href))
                       :getter (lambda (node) (xml-get-attribute node 'href))))))
      (when (string< "2016" date)
       (list (format "%s : %s : %s" title (mapconcat #'identity (split-string main) " ") date)
             (concat "animate://" (url-expand-file-name href emms-stream-animate--url-top))
             1 'streamlist)))))

(defun emms-stream-animate--html-to-stream-list (day html)
  "Retrun stream list of DAY from HTML."
  (let ((boxes
         (car (emms-stream-animate--xml-collect-node
               'div html
               :test
               (lambda (node) (and (equal (xml-get-attribute node 'class)
                                      "list")
                               (equal (xml-get-attribute node 'id)
                                      day)))
               :getter
               (lambda (node) (emms-stream-animate--xml-collect-node
                           'div node
                           :test
                           (lambda (node) (equal (xml-get-attribute node 'class)
                                             "box"))))))))
    (cl-loop for box in boxes
             for stream = (emms-stream-animate--box-to-stream box)
             when stream collect stream)))

(defun emms-stream-animate--fetch-stream-alist (&optional updatep)
  "Fetch stream alist.
If UPDATEP is non-nil, cache is updated."
  (if (or updatep (null emms-stream-animate--stream-alist-cache))
      (let ((html (emms-stream-animate--url-to-html emms-stream-animate--url-radio)))
        (setq emms-stream-animate--stream-alist-cache
              (cl-loop for day in emms-stream-animate--days collect
                       (cons day (emms-stream-animate--html-to-stream-list day html)))))
    emms-stream-animate--stream-alist-cache))

;;;###autoload
(defun emms-stream-animate-update-cache-async ()
  "Update cache asynchronously."
  (emms-stream-animate--url-retrieve
   emms-stream-animate--url-radio
   (lambda (status &rest _)
     (when (memq :error status)
       (error "Failed to update animate stream listt : %s" (cdr status)))
     (let ((html (emms-stream-animate--url-to-html nil nil (current-buffer))))
       (setq emms-stream-animate--stream-alist-cache
             (cl-loop for day in emms-stream-animate--days collect
                      (cons day (emms-stream-animate--html-to-stream-list day html)))))
     (message "Updated animate stream list cache"))))

(defun emms-stream-animate-ger-stream-list-dow (day &optional updatep)
  "Return streamlist of the DAY of the weekdays.
If UPDATEP is non-nil, cache is updated."
  (assoc-default day (emms-stream-animate--fetch-stream-alist updatep)))

(defun emms-stream-animate-add-bookmark-1 (&optional updatep &rest days)
  "Helper function for `emms-stream-animate-add-bookmark', etc."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (emms-stream-animate--fetch-stream-alist updatep)
  (let* ((days (or days emms-stream-animate--days))
         (line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (dow days)
     (dolist (stream (emms-stream-animate-ger-stream-list-dow dow))
       (setq emms-stream-list (emms-stream-insert-at index stream
                                                     emms-stream-list))
       (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-animate-get-stream-list ()
  "Return new streamlist from cache."
  (cl-loop
   with ls = nil
   for day in emms-stream-animate--days do
   (dolist (stream (assoc-default day emms-stream-animate--stream-alist-cache))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-animate-add-bookmark (&optional updatep dow)
  "Create animate bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
DOW is a number of 0-6 or -1.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-animate-update-cache-async)
   (unless (integerp dow)
     (let ((msg (concat "[0] All  [1] Mon  [2] Tue  [3] Wed  [4] Thu\n"
                        "         [5] Fri  [6] Irr\n"
                        "[-1] Update stream list cache asynchronously\n\n"
                        "Input a number of 0-6 or -1: ")))
       (while (not (and (integerp (setq dow (read-number msg)))
                        (<= -1 dow) (<= dow 6))))))
   (cond ((= dow -1) (emms-stream-animate-update-cache-async))
         ((zerop dow) (emms-stream-animate-add-bookmark-1 updatep))
         (t (emms-stream-animate-add-bookmark-1
             updatep (nth (1- dow) emms-stream-animate--days))))))

;; For media player

;;;###autoload
(defun emms-stream-animate-stream-url-to-m3u8 (stream-url)
  "Return m3u8 url from STREAM-URL."
  (let ((html (emms-stream-animate--url-to-html
               (replace-regexp-in-string "\\`animate://" "" stream-url))))
    (car (emms-stream-animate--xml-collect-node
          'video html
          :test (lambda (node) (xml-get-attribute-or-nil node 'src ))
          :getter (lambda (node) (xml-get-attribute node 'src ))))))

(define-obsolete-function-alias 'emms-stream-animate-stream-url-to-rtmpdump-form
  'emms-stream-animate-stream-url-to-m3u8
  "20160430")

(define-obsolete-function-alias 'emms-stream-animate-stream-url-to-asx-ref
  'emms-stream-animate-stream-url-to-m3u8
  "20160329: animate.tv has ended support for WMP.")

(provide 'emms-streams-animate)
;;; emms-streams-animate.el ends here
