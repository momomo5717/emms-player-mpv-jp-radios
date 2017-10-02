;;; emms-player-mpv-simul.el --- An emms simple mpv player for SimulRadio -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-simul.

;; (require 'emms-player-mpv-simul)
;; (add-to-list 'emms-player-list 'emms-player-mpv-simul)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-simul)

(define-emms-simple-player-mpv mpv-simul '(streamlist)
  "\\`s\\(imul\\|aimaru\\)://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no" "--no-ytdl")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-simul "." t
 'emms-player-mpv-simul--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-simul 'get-media-title
                 'emms-player-mpv-simul--get-media-title)

(defun emms-player-mpv-simul--loading-message ()
  "Loading message."
  (message "Loading SimulRadio ... "))

(defun emms-player-mpv-simul--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let ((url (if (string-match-p "[.]asx$" track-name)
                 (emms-stream-simul-stream-url-to-asx-ref track-name)
               (emms-stream-simul-stream-url-to-specific-form track-name))))
    (later-do 'emms-player-mpv-simul--loading-message)
    url))

(defun emms-player-mpv-simul--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-simul)
;;; emms-player-mpv-simul.el ends here
