;;; emms-player-mpv-anitama.el --- An emms simple mpv player for アニたまどっとコム -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-anitama.

;; (require 'emms-player-mpv-anitama)
;; (add-to-list 'emms-player-list 'emms-player-mpv-anitama)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-anitama)

(define-emms-simple-player-mpv mpv-anitama '(streamlist)
  "\\`anitama://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no" "--no-ytdl")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-anitama "\\`anitama://" t
 'emms-player-mpv-anitama--track-name-to-nicovideo-form)

(emms-player-set 'emms-player-mpv-anitama 'get-media-title
                 'emms-player-mpv-anitama--get-media-title)

(emms-player-set 'emms-player-mpv-anitama 'mpv-start-process-function
                 'emms-player-mpv-anitama--start-process)

(defun emms-player-mpv-anitama--loading-message ()
  "Loading message."
  (message "Loading アニたまどっとコム ... "))

(defun emms-player-mpv-anitama--track-name-to-nicovideo-form (track-name)
  "Return nicovideo url from TRACK-NAME."
  (let ((nicovideo-url (emms-stream-anitama-stream-url-to-nicovideo-url track-name)))
    (unless emms-stream-nico-use-old-api
      (emms-stream-nico-session-hb-start-timer))
    (later-do 'emms-player-mpv-anitama--loading-message)
    nicovideo-url))

(add-hook 'emms-player-finished-hook 'emms-stream-nico-session-hb-cancel-timer)
(add-hook 'emms-player-stopped-hook 'emms-stream-nico-session-hb-cancel-timer)

(defun emms-player-mpv-anitama--start-process (cmdname params input-form _track)
  "Function for mpv-start-process-function."
  (let ((cookies-params
         (when emms-stream-nico-use-old-api
           (list "--cookies" (format "--cookies-file=%s"
                                     emms-stream-nico-old--cookies-file)))))
    (apply #'start-process
           emms-player-simple-process-name
           nil
           cmdname
           `(,@cookies-params ,@params ,input-form))))

(defun emms-player-mpv-anitama--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-anitama)
;;; emms-player-mpv-anitama.el ends here
