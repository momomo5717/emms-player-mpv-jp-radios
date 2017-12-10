;;; emms-player-mpv-seaside.el --- An emms simple mpv player for Sea Side Communications -*- lexical-binding: t -*-

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
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no" "--no-ytdl")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-seaside "\\`seaside://" t
 'emms-player-mpv-seaside--track-name-to-nico-input-form)

(emms-player-set 'emms-player-mpv-seaside 'get-media-title
                 'emms-player-mpv-seaside--get-media-title)

(emms-player-set 'emms-player-mpv-seaside 'mpv-start-process-function
                 'emms-player-mpv-seaside--start-process)

(defun emms-player-mpv-seaside--loading-message ()
  "Loading message."
  (message "Loading Sea Side Communications ... "))

(defun emms-player-mpv-seaside--track-name-to-nico-input-form (track-name)
  "Return nicovideo url from TRACK-NAME."
  (let ((video-url (emms-stream-seaside-stream-url-to-nicovideo-url track-name)))
    (unless emms-stream-nico-use-old-api
      (emms-stream-nico-session-hb-start-timer))
    (later-do 'emms-player-mpv-seaside--loading-message)
    video-url))

(add-hook 'emms-player-finished-hook 'emms-stream-nico-session-hb-cancel-timer)
(add-hook 'emms-player-stopped-hook 'emms-stream-nico-session-hb-cancel-timer)

(defun emms-player-mpv-seaside--start-process (cmdname params input-form _track)
  "Function for mpv-start-process-function."
  (let ((cookies-params
         (when (and emms-stream-nico-use-old-api
                    (string-match-p "nicovideo" input-form))
           (list "--cookies" (format "--cookies-file=%s"
                                     emms-stream-nico-old--cookies-file)))))
    (apply #'start-process
           emms-player-simple-process-name
           nil
           cmdname
           `(,@cookies-params ,@params ,input-form))))

(defun emms-player-mpv-seaside--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-seaside)
;;; emms-player-mpv-seaside.el ends here
