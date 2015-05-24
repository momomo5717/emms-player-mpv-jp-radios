;;; emms-player-mpv-listen.el --- An emms simple mpv player for ListhenRadio -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-listen.

;; (require 'emms-player-mpv-listen)
;; (add-to-list 'emms-player-list 'emms-player-mpv-listen)


;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)

(define-emms-simple-player-mpv mpv-listen '(streamlist)
  "\\`listen://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-listen "." t
 'emms-player-mpv-listen--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-listen 'get-media-title
                 'emms-player-mpv-listen--get-media-title)

(defun emms-player-mpv-listen--loading-message ()
  "Loading message."
  (message "Loading ListhenRadio ... "))

(defun emms-player-mpv-listen--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let ((url (format "http://mtist.as.smartstream.ne.jp/%s/livestream/playlist.m3u8"
                     (replace-regexp-in-string "\\`listen://" "" track-name))))
    (later-do 'emms-player-mpv-listen--loading-message)
    url))

(defun emms-player-mpv-listen--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-listen)
;;; emms-player-mpv-listen.el ends here
