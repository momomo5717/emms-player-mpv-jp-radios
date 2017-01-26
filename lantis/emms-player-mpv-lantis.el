;;; emms-player-mpv-lantis.el --- An emms simple mpv player for Lantis -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-lantis.

;; (require 'emms-player-mpv-lantis)
;; (add-to-list 'emms-player-list 'emms-player-mpv-lantis)


;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-lantis)

(define-emms-simple-player-mpv mpv-lantis '(streamlist)
  "\\`lantis://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-lantis "." t
 'emms-player-mpv-lantis--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-lantis 'get-media-title
                 'emms-player-mpv-lantis--get-media-title)

(defun emms-player-mpv-lantis--loading-message ()
  "Loading message."
  (message "Loading Lantis ... "))

(defun emms-player-mpv-lantis--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (later-do 'emms-player-mpv-lantis--loading-message)
  (emms-stream-lantis-stream-url-to-m3u8 track-name))

(defun emms-player-mpv-lantis--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (car (split-string
            (emms-stream-name (emms-track-get track 'metadata)) " : "))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-lantis)
;;; emms-player-mpv-lantis.el ends here
