;;; emms-streams-hibiki.el --- emms stream list for HiBiKi Radio Station  -*- lexical-binding: t -*-
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

;; This provides emms stream list for 響 - HiBiKi Radio Station -.

;; (require 'emms-streams-hibiki)

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

(defvar emms-stream-hibiki--stream-alist-cache
  (cl-loop for i from 1 to 6 collect (list i))
  "Cache for stream alist.")

(defvar emms-stream-hibiki--base-url-programs
  "https://vcms-api.hibiki-radio.jp/api/v1//programs")

(defvar emms-stream-hibiki--dow-alist
  '((1 . "mon") (2 . "tue") (3 . "wed") (4 . "thu") (5 . "fri") (6 . "satsun")))

(defun emms-stream-hibiki--get-programs-url (dow)
  "Return programs url of DOW."
  (format "%s?day_of_week=%s"
          emms-stream-hibiki--base-url-programs
          (cdr (assq dow emms-stream-hibiki--dow-alist))))

(defvar emms-stream-hibiki--url-request-extra-headers
  '(("x-requested-with" . "XMLHttpRequest")
    ("Connection" . "close")))

(defun emms-stream-hibiki--url-retrieve-synchronously
    (url &optional silent inhibit-cookies)
  "`url-retrieve-synchronously' for hibiki."
  (let ((url-request-extra-headers
         emms-stream-hibiki--url-request-extra-headers))
    (url-retrieve-synchronously url silent inhibit-cookies)))

(defun emms-stream-hibiki--url-retrieve
    (url callback &optional cbargs silent inhibit-cookies)
  "`url-retrieve' for hibiki."
  (let ((url-request-extra-headers
         emms-stream-hibiki--url-request-extra-headers))
    (url-retrieve url callback cbargs silent inhibit-cookies)))

(defun emms-stream-hibiki--url-to-json (url &optional buf)
  "Return a json object from URL.
If BUF is no-nil, it is used."
  (let ((buf (or buf (emms-stream-hibiki--url-retrieve-synchronously url))))
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
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(defun emms-stream-hibiki--json-program-to-streamlist (program)
  "Retrun streamlist from json PROGRAM."
  (let* ((name (cdr (assq 'name program)))
         (latest_episode_name (cdr (assq 'latest_episode_name program)) )
         (cast (cdr (assq 'cast program)))
         (access_id (cdr (assq 'access_id program)))
         (episode_updated_at (cdr (assq 'episode_updated_at program))))
    (list (format "%s %s %s: %s"
                  name latest_episode_name
                  (if (equal cast "") ""
                    (format ": %s " cast))
                  (if (stringp episode_updated_at)
                      (car (split-string episode_updated_at))
                    ""))
          (format "hibiki://%s" access_id)
          1 'streamlist)))

(defun emms-stream-hibiki--dow-to-stream-list (dow &optional buf)
  "Return json object for programs of DOW.
DOW is a number of 1-6.
If BUF is non-nil, it is used."
  (let* ((url (emms-stream-hibiki--get-programs-url dow))
         (programs (emms-stream-hibiki--url-to-json url buf)))
    (cl-loop for program across programs
             collect (emms-stream-hibiki--json-program-to-streamlist program))))

(defun emms-stream-hibiki--fetch-dow-stream-list (dow &optional updatep)
  "Retrun stream-list of DOW.
If UPDATEP is non-nil, cache of DOW is updated."
  (unless (and (< 0 dow) (< dow 7)) (error "DOW needs an integer of 1-6"))
  (let (dow-stream-list)
    (if (or updatep
            (null (setq dow-stream-list
                        (cdr (assq dow emms-stream-hibiki--stream-alist-cache)))))
        (progn
          (unless emms-stream-hibiki--stream-alist-cache
            (setq emms-stream-hibiki--stream-alist-cache
                  (cl-loop for i from 1 to 6 collect (list i))))
          (setcdr
           (assq dow emms-stream-hibiki--stream-alist-cache)
           (emms-stream-hibiki--dow-to-stream-list dow)))
      dow-stream-list)))

(defun emms-stream-hibiki--update-cache-async-1 (dow)
  "Update DOW stream-list cache."
  (if (and (< 0 dow) (< dow 7))
      (emms-stream-hibiki--url-retrieve
       (emms-stream-hibiki--get-programs-url dow)
       (lambda (status &rest _)
         (when (memq :error status)
           (error "Failed to get hibiki stream list of %s : %s" dow (cdr status)))
         (unless emms-stream-hibiki--stream-alist-cache
           (setq emms-stream-hibiki--stream-alist-cache
                 (cl-loop for i from 1 to 6 collect (list i))))
         (setcdr (assq dow emms-stream-hibiki--stream-alist-cache)
                 (emms-stream-hibiki--dow-to-stream-list nil (current-buffer)))
         (emms-stream-hibiki--update-cache-async-1 (+ dow 6))))
    (when (cl-loop for ls in emms-stream-hibiki--stream-alist-cache
                   always (cdr ls))
      (message "Updated hibiki stream list cache"))))

;;;###autoload
(defun emms-stream-hibiki-update-cache-async ()
  "Update cache asynchronously."
  (setq emms-stream-hibiki--stream-alist-cache
        (cl-loop for i from 1 to 6 collect (list i)))
  (cl-loop for i from 1 to 6 do
           (emms-stream-hibiki--update-cache-async-1 i)))

(defun emms-stream-hibiki--add-bookmark-dows (nums &optional updatep)
  "Helper function for `emms-stream-hibiki-add-bookmark', etc.
Add stream list of NUMS.
If UPDATEP is non-nil, cache is updated."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (n nums)
      (dolist (stream (emms-stream-hibiki--fetch-dow-stream-list n updatep))
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-hibiki-get-stream-list ()
  "Return new stream-list from cache."
  (cl-loop
   with ls = nil
   for dow from 1 to 6 do
   (dolist (stream (cdr (assq dow emms-stream-hibiki--stream-alist-cache)))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-hibiki-add-bookmark (&optional updatep dow)
  "Create hibiki bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
DOW is a number of 0-6 or -1.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-hibiki-update-cache-async)
   (unless (integerp dow)
     (let ((msg (concat
                 "[0] All  [1] Mon  [2] Tue  [3] Wed  [4] Thu\n"
                 "         [5] Fri  [6] Sat/San\n"
                 "[-1] Update stream list cache asynchronously\n\n"
                 "Input a number of 0-6 or -1: ")))
       (while (not (and (integerp (setq dow (read-number msg)))
                        (<= -1 dow) (<= dow 6))))))
   (cond
    ((= dow -1) (emms-stream-hibiki-update-cache-async))
    ((zerop dow) (emms-stream-hibiki--add-bookmark-dows '(1 2 3 4 5 6) updatep))
    (t (emms-stream-hibiki--add-bookmark-dows (list dow) updatep)))))

;; For media player

(defvar emms-stream-hibiki--base-url-access-programs
  "https://vcms-api.hibiki-radio.jp/api/v1/programs/")

(defun emms-stream-hibiki--get-access-programs-url (access_id)
  "Return programs url of ACCESS_ID."
  (format "%s%s"
          emms-stream-hibiki--base-url-access-programs access_id))

(defvar emms-stream-hibiki--base-url-play_check
  "https://vcms-api.hibiki-radio.jp/api/v1/videos/play_check")

(defun emms-stream-hibiki--get-play_check-url (video_id)
  "Return play_check url of VIDEO_ID."
  (format "%s?video_id=%s"
          emms-stream-hibiki--base-url-play_check video_id))

(defun emms-stream-hibiki--access_id-to-video_id (access_id)
  "Return video id of ACCESS_ID ."
  (let* ((programs (emms-stream-hibiki--url-to-json
                    (emms-stream-hibiki--get-access-programs-url access_id)))
         (id (cdr (assq 'id (assq 'video (assq 'episode programs))))))
    (unless id
      (error "Failed to get video id from %S" access_id))
    id))

(defun emms-stream-hibiki--url-to-playlist_url (url)
  "Return playlist_url from URL."
  (let* ((play_check (emms-stream-hibiki--url-to-json url))
         (playlist_url (cdr (assq 'playlist_url play_check))))
    (unless playlist_url
      (error "Failed to get playlist_url from %S" url))
    playlist_url))

;;;###autoload
(defun emms-stream-hibiki-stream-url-to-m3u8 (stream-url)
  "Return m3u8 link from STREAM-URL."
  (emms-stream-hibiki--url-to-playlist_url
   (emms-stream-hibiki--get-play_check-url
    (emms-stream-hibiki--access_id-to-video_id
     (replace-regexp-in-string "\\`hibiki://" "" stream-url)))))

(provide 'emms-streams-hibiki)
;;; emms-streams-hibiki.el ends here
