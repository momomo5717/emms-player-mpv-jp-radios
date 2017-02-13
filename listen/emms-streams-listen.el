;;; emms-streams-listen.el --- emms stream list for ListenRadio -*- lexical-binding: t -*-

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

;; This provides emms stream list for ListenRadio.

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'json)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defun emms-stream-listen--fetch-json-obj (url &optional buf)
  "Return a json object from URL.
If BUF is no-nil, it is used."
  (let* ((buf (or buf (url-retrieve-synchronously url)))
         ;; Decoding string
         (html
          (with-current-buffer buf
            (goto-char (point-min))
            (while (and (not (eobp)) (not (eolp))) (forward-line 1))
            (unless (eobp) (forward-line 1))
            (libxml-parse-html-region (point) (point-max))))
         (body (car (xml-get-children html 'body)))
         (p (car (xml-get-children body 'p)))
         (json-str (car (xml-node-children p))))
    (prog1 (with-temp-buffer
             (insert json-str)
             (goto-char (point-min))
             (json-read))
      (when (buffer-live-p buf)
            (kill-buffer buf)))))

(defvar emms-stream-listen--category-alist
  '((10002 . "音楽ジャンル")
    (10008 . "バラエティ")
    (10005 . "全国のラジオ局")))

(defvar emms-stream-listen--area-alist
  '((1 . "北海道")
    (2 . "東北")
    (3 . "関東")
    (4 . "東海")
    (5 . "北信越")
    (6 . "近畿")
    (7 . "中国・四国")
    (8 . "九州・沖縄")))

(defvar emms-stream-listen--stream-alist-cache nil
  "Cache for stream alist.")

(defvar emms-stream-listen--base-url-categorychannel
  "http://listenradio.jp/service/categorychannel.aspx")

(defun emms-stream-listen--get-categoryid-url (id)
  "Return category ID url."
  (format "%s?categoryid=%s" emms-stream-listen--base-url-categorychannel id))

(defun emms-stream-listen--fetch-category-streams (id &optional buf)
  "Return stream list or alist for ID.
If BUF non-nil, it is used."
  (cl-loop
   with area-alist = emms-stream-listen--area-alist
   with streams = (if (eq id 10005)
                      (cl-loop for i from 1 to 8 collect
                               (list (cdr (assq i area-alist))))
                    nil)
   with url = (emms-stream-listen--get-categoryid-url id)
   with category-name = (cdr (assq id emms-stream-listen--category-alist))
   for category across (cdr (assq 'Channel
                                  (emms-stream-listen--fetch-json-obj url buf)))
   for name   = (cdr (assq 'ChannelName category))
   for m3u8   = (cdr (assq 'ChannelHls  category))
   for chId   = (cdr (assq 'ChannelId category))
   for area   = (cdr (assq (cdr (assq 'AreaId category))
                           emms-stream-listen--area-alist))
   for stream = (list (concat name (if area (concat " : " area) "")
                              " : " category-name)
                      (format "listen://%s" chId) 1 'streamlist)
   when (string-match-p "[.]m3u8$" m3u8) do
   (if (eq id 10005) (nconc (assoc area streams) (list stream))
     (push stream streams))
   finally return
   (if (not (eq id 10005)) (nreverse streams)
     (cl-loop for (area . area-streams) in streams
              collect (cons (car (rassoc area area-alist)) area-streams)))))

(defun emms-stream-listen--fetch-stream-alist (&optional updatep)
  "Return stream-alist.
If UPDATEP is non-nil, cache is updated."
  (if (or updatep (null emms-stream-listen--stream-alist-cache))
      (setq emms-stream-listen--stream-alist-cache
            (cl-loop for (id . _) in emms-stream-listen--category-alist
                     collect (cons id (emms-stream-listen--fetch-category-streams id))))
    emms-stream-listen--stream-alist-cache))

(defun emms-stream-listen--update-cache-async-1 (id)
  "Update ID stream-list cache."
  (let ((url-request-extra-headers
         '(("Connection" . "close"))))
   (url-retrieve
    (emms-stream-listen--get-categoryid-url id)
    (lambda (status &rest _)
      (when (memq :error status)
        (error "Failed to get listen stream list of %s : %s" id (cdr status)))
      (setcdr (assq id emms-stream-listen--stream-alist-cache)
              (emms-stream-listen--fetch-category-streams id (current-buffer)))
      (when (cl-loop for ls in emms-stream-listen--stream-alist-cache
                     always (cdr ls))
        (message "Updated listen stream list cache"))))))

;;;###autoload
(defun emms-stream-listen-update-cache-async ()
  "Update cache asynchronously."
  (setq emms-stream-listen--stream-alist-cache
        (cl-loop for (id . _) in emms-stream-listen--category-alist
                 collect (list id)))
  (cl-loop for (id . _) in emms-stream-listen--category-alist
           do (emms-stream-listen--update-cache-async-1 id)))

(defun emms-stream-listen--get-stream-list (id &optional area)
  "Return stream list of ID.
If ID is 10005, AREA is required."
  (let ((stream-list (cdr (assq id emms-stream-listen--stream-alist-cache))))
    (if (eq id 10005) (cdr (assq area stream-list)) stream-list)))

(defun emms-stream-listen--add-bookmark-1 (ids &optional updatep)
  "Helper function for `emms-stream-listen-add-bookmark'.
IDS = (id ...)
id  = number or (number ...)

If UPDATEP is no-nil, cache is updated."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (emms-stream-listen--fetch-stream-alist updatep)
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (cl-loop
     with areas = nil
     for id in ids
     ;; For id = '(10005 area-id ...) or 10005
     if (or (listp id) (eq id 10005)) do
     (setq areas (if (listp id) (cdr id)
                   (mapcar #'car emms-stream-listen--area-alist)))
     (setq id (if (listp id) (car id) id))
     (dolist (area areas)
       (dolist (stream (emms-stream-listen--get-stream-list id area))
         (setq emms-stream-list
               (emms-stream-insert-at index stream emms-stream-list))
         (cl-incf index)))
     else do
     (dolist (stream (emms-stream-listen--get-stream-list id))
       (setq emms-stream-list
             (emms-stream-insert-at index stream emms-stream-list))
       (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-listen-get-stream-list ()
  "Return new stream-list."
  (cl-loop
   with ls = nil
   for id in (mapcar #'car emms-stream-listen--category-alist) do
   (if (eq id 10005)
       (cl-loop
        for area in (mapcar #'car emms-stream-listen--area-alist) do
        (dolist (stream (emms-stream-listen--get-stream-list id area))
          (push stream ls)))
     (dolist (stream (emms-stream-listen--get-stream-list id))
       (push stream ls)))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-listen-add-bookmark (&optional updatep category area)
  "Create listen bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
CATEGORY is a number of 0-3.
AREA is a number of 0-8.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-listen-update-cache-async)
    (unless (integerp category)
      (let ((msg (concat "[0] All  [1] 音楽ジャンル  [2] バラエティ  [3] 全国のラジオ局\n"
                         "[-1] Update stream list cache asynchronously\n\n"
                         "Input a number of 0-3 or -1: ")))
        (while (not (and (integerp (setq category (read-number msg)))
                         (and (<= -1 category) (<= category 3)))))))
    (when (and (eq category 3) (null area))
      (let ((msg (concat "[0] All  [1] 北海道  [2] 東北  [3] 関東        [4] 東海\n"
                         "         [5] 北信越  [6] 近畿  [7] 中国・四国  [8] 九州・沖縄\n\n"
                         "Input a number of 0-8: ")))
        (while (not (and (integerp (setq area (read-number msg)))
                         (<= 0 area) (<= area 8))))))
    (let ((id-ls (mapcar #'car emms-stream-listen--category-alist)))
      (cond
       ((= category -1) (emms-stream-listen-update-cache-async))
       ((zerop category) (emms-stream-listen--add-bookmark-1 id-ls updatep))
       (area (if (zerop area)
                 (emms-stream-listen--add-bookmark-1 (list (nth (1- category) id-ls)) updatep)
               (emms-stream-listen--add-bookmark-1 `((,(nth (1- category) id-ls) ,area)) updatep)))
       (t (emms-stream-listen--add-bookmark-1 (list (nth (1- category) id-ls)) updatep))))))

;; For media player

;;;###autoload
(defun emms-stream-listen-stream-url-to-m3u8 (stream-url)
  "Return m3u8 from STREAM-URL."
  (format "http://mtist.as.smartstream.ne.jp/%s/livestream/playlist.m3u8"
          (replace-regexp-in-string "\\`listen://" "" stream-url)))

(provide 'emms-streams-listen)
;;; emms-streams-listen.el ends here
