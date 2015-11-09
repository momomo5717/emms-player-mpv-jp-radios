;;; emms-player-mpv-hibiki.el --- An emms simple mpv player for HiBiKi Radio Station -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-hibiki.

;; (require 'emms-player-mpv-hibiki)
;; (add-to-list 'emms-player-list 'emms-player-mpv-hibiki)

;;; Code:
(require 'cl-lib)
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'xml)
(require 'url)
(require 'later-do)

(define-emms-simple-player-mpv mpv-hibiki '(streamlist)
  "\\`hibiki://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-hibiki "." t
 'emms-player-mpv-hibiki--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-hibiki 'get-media-title
                 'emms-player-mpv-hibiki--get-media-title)

(defvar emms-player-mpv-hibiki--base-url-programs
  "https://vcms-api.hibiki-radio.jp/api/v1/programs/")

(defun emms-player-mpv-hibiki--get-programs-url (access_id)
  "Return programs url of ACCESS_ID."
  (format "%s%s"
          emms-player-mpv-hibiki--base-url-programs access_id))

(defvar emms-player-mpv-hibiki--base-url-play_check
  "https://vcms-api.hibiki-radio.jp/api/v1/videos/play_check")

(defun emms-player-mpv-hibiki--get-play_check-url (video_id)
  "Return play_check url of VIDEO_ID."
  (format "%s?video_id=%s"
          emms-player-mpv-hibiki--base-url-play_check video_id))

(defun emms-player-mpv-hibiki--url-to-json (url &optional buf)
  "Return a json object from URL.
If BUF is no-nil, it is used."
  (let ((buf (or buf (url-retrieve-synchronously url nil))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let ((str (buffer-substring (point) (point-max))))
             (prog1 (with-temp-buffer
                      (insert str)
                      (decode-coding-region (point-min) (point-max) 'utf-8)
                      (goto-char (point-min))
                      (json-read))
               (kill-buffer))))))

(defun emms-player-mpv-hibiki--access_id-to-video_id (access_id)
  "Return video id of ACCESS_ID ."
  (let* ((programs (emms-player-mpv-hibiki--url-to-json
                    (emms-player-mpv-hibiki--get-programs-url access_id)))
         (id (cdr (assq 'id (assq 'video (assq 'episode programs))))))
    (unless id
      (error "Failed to get video id form %S" access_id))
    id))

(defun emms-player-mpv-hibiki--url-to-playlist_url (url)
  "Return playlist_url from URL."
  (let* ((play_check (emms-player-mpv-hibiki--url-to-json url))
         (playlist_url (cdr (assq 'playlist_url play_check))))
    (unless playlist_url
      (error "Failed to get playlist_url from %S" url))
    playlist_url))

(defun emms-player-mpv-hibiki--loading-message ()
  "Loading message."
  (message "Loading 響 ... "))

(defun emms-player-mpv-hibiki--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let ((playlist_url (emms-player-mpv-hibiki--url-to-playlist_url
                       (emms-player-mpv-hibiki--get-play_check-url
                        (emms-player-mpv-hibiki--access_id-to-video_id
                         (replace-regexp-in-string "\\`hibiki://" "" track-name))))))
    (later-do 'emms-player-mpv-hibiki--loading-message)
    playlist_url))

(defun emms-player-mpv-hibiki--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-hibiki)
;;; emms-player-mpv-hibiki.el ends here
