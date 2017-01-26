;;; emms-player-mpv-onsen.el --- An emms simple mpv player for 音泉 -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-onsen.

;; (require 'emms-player-mpv-onsen)
;; (add-to-list 'emms-player-list 'emms-player-mpv-onsen)


;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-onsen)

(define-emms-simple-player-mpv mpv-onsen '(streamlist)
  "\\`onsen://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-onsen "." t
 'emms-player-mpv-onsen--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-onsen 'get-media-title
                 'emms-player-mpv-onsen--get-media-title)

(defun emms-player-mpv-onsen--loading-message ()
  "Loading message."
  (message "Loading 音泉 ... "))

(defun emms-player-mpv-onsen--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let ((url (emms-stream-onsen-stream-url-to-moviePath track-name)))
    (later-do 'emms-player-mpv-onsen--loading-message)
    url))

(defun emms-player-mpv-onsen--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (car (split-string
            (emms-stream-name (emms-track-get track 'metadata)) " : "))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-onsen)
;;; emms-player-mpv-onsen.el ends here
