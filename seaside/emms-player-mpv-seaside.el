;;; emms-player-mpv-seaside.el --- An emms simple mpv player for Sea Side Communications -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-seaside.

;; (require 'emms-player-mpv-seaside)
;; (add-to-list 'emms-player-list 'emms-player-mpv-seaside)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-seaside)

(define-emms-simple-player-mpv mpv-seaside '(streamlist)
  "\\`seaside://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no" "--ytdl")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-seaside "." t
 'emms-player-mpv-seaside--track-name-to-wma-input-form)

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-seaside
 emms-stream-seaside-nico-stream-regex t
 'emms-player-mpv-seaside--track-name-to-nico-input-form)

(emms-player-set 'emms-player-mpv-seaside 'get-media-title
                 'emms-player-mpv-seaside--get-media-title)

(defun emms-player-mpv-seaside--loading-message ()
  "Loading message."
  (message "Loading Sea Side Communications ... "))

(defun emms-player-mpv-seaside--track-name-to-wma-input-form (track-name)
  "Return wma from TRACK-NAME."
  (let ((wma (emms-stream-seaside-wax-to-wma
              (emms-stream-seaside-stream-url-to-wax track-name))))
    (later-do 'emms-player-mpv-seaside--loading-message)
    wma))

(defun emms-player-mpv-seaside--track-name-to-nico-input-form (track-name)
  "Return nico url from TRACK-NAME."
  (unless (executable-find "youtube-dl")
    (error "Can not play %s : youtube-dl not found" track-name))
  (let ((nico-url (emms-stream-seaside-stream-url-to-nico-url track-name)))
    (later-do 'emms-player-mpv-seaside--loading-message)
    nico-url))

(defun emms-player-mpv-seaside--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))


(define-obsolete-function-alias 'emms-player-mpv-seaside--xml-collect-node
  'emms-stream-seaside--xml-collect-node "20151128")
(define-obsolete-function-alias 'emms-player-mpv-seaside--url-to-html
  'emms-stream-seaside--url-to-html "20151128")
(define-obsolete-face-alias 'emms-player-mpv-seaside--wax-to-wma
  'emms-stream-seaside--html-to-wax "20151128")
(define-obsolete-function-alias 'emms-player-mpv-seaside--html-to-nico
  'emms-stream-seaside--html-to-nico "20151128")

(provide 'emms-player-mpv-seaside)
;;; emms-player-mpv-seaside.el ends here
