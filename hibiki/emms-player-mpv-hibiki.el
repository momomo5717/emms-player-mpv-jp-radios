;;; emms-player-mpv-hibiki.el --- An emms simple mpv player for HiBiKi Radio Station -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-hibiki.

;; (require 'emms-player-mpv-hibiki)
;; (add-to-list 'emms-player-list 'emms-player-mpv-hibiki)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-hibiki)

(define-emms-simple-player-mpv mpv-hibiki '(streamlist)
  "\\`hibiki://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-hibiki "." t
 'emms-player-mpv-hibiki--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-hibiki 'get-media-title
                 'emms-player-mpv-hibiki--get-media-title)

(defun emms-player-mpv-hibiki--loading-message ()
  "Loading message."
  (message "Loading 響 ... "))

(defun emms-player-mpv-hibiki--track-name-to-input-form (track-name)
  "Return m3u8 link from TRACK-NAME."
  (let ((m3u8 (emms-stream-hibiki-stream-url-to-m3u8 track-name)))
    (later-do 'emms-player-mpv-hibiki--loading-message)
    (concat "ffmpeg://" m3u8)))

(defun emms-player-mpv-hibiki--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-hibiki)
;;; emms-player-mpv-hibiki.el ends here
