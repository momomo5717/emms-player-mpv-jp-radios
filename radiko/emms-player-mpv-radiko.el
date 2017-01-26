;;; emms-player-mpv-radiko.el --- An emms simple mpv player for radiko -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-radiko.

;; (require 'emms-player-mpv-radio)
;; (add-to-list 'emms-player-list 'emms-player-mpv-radiko)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-radiko)

(define-emms-simple-player-mpv mpv-radiko '(streamlist)
  "\\`radiko://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-radiko "\\`radiko://" t
 'emms-player-mpv-radiko--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-radiko 'get-media-title
                 'emms-player-mpv-radiko--get-media-title)

;; mpv cannot seek while playing Radiko.
(emms-player-set emms-player-mpv-radiko 'seek nil)
(emms-player-set emms-player-mpv-radiko 'seek-to nil)

(defun emms-player-mpv-radiko--loading-message ()
  "Loading message."
  (message "Loading Radiko ... It takes a few seconds."))

(defun emms-player-mpv-radiko--track-name-to-input-form (track-name)
  "Retrun \"rtmpe://...\" ffplay format from TRACK-NAME."
  (emms-stream-radiko--wget-playerfile)
  (emms-stream-radiko--write-keydata)
  (let* ((rtmpe (emms-stream-radiko-stream-url-to-rtmpe track-name))
         (auth1 (emms-stream-radiko--access-auth1-fms))
         (authtoken (emms-stream-radiko--get-authtoken auth1)))
    (emms-stream-radiko--access-auth2-fms auth1)
    (later-do 'emms-player-mpv-radiko--loading-message)
    (format "%s swfUrl=%s swfVfy=1 conn=S:  conn=S:  conn=S:  conn=S:%s live=1"
            rtmpe emms-stream-radiko--playerurl authtoken)))

(defun emms-player-mpv-radiko--get-media-title (track)
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-radiko)
;;; emms-player-mpv-radiko.el ends here
