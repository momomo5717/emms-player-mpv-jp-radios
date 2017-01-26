;;; emms-player-mpv-famitsu.el --- An emms simple mpv player for ファミ通.com -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-famitsu.

;; (require 'emms-player-mpv-famitsu)
;; (add-to-list 'emms-player-list 'emms-player-mpv-famitsu)


;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-famitsu)

(define-emms-simple-player-mpv mpv-famitsu '(streamlist)
  "\\`famitsu://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-famitsu "." t
 'emms-player-mpv-famitsu--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-famitsu 'get-media-title
                 'emms-player-mpv-famitsu--get-media-title)

(defun emms-player-mpv-famitsu--loading-message ()
  "Loading message."
  (message "Loading ファミ通.com ... "))

(defun emms-player-mpv-famitsu--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (later-do 'emms-player-mpv-famitsu--loading-message)
  (emms-stream-famitsu-stream-url-to-mp3 track-name))

(defun emms-player-mpv-famitsu--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (car (split-string
            (emms-stream-name (emms-track-get track 'metadata)) " : "))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-famitsu)
;;; emms-player-mpv-famitsu.el ends here
