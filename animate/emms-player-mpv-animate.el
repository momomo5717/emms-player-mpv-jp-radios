;;; emms-player-mpv-animate.el --- An emms simple mpv player for animate.tv -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-animate.

;; (require 'emms-player-mpv-animate)
;; (add-to-list 'emms-player-list 'emms-player-mpv-animate)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'later-do)
(require 'emms-streams-animate)

(define-emms-simple-player-mpv mpv-animate '(streamlist)
  "\\`animate://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no"
  "--cache=150000" "--force-seekable=yes")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-animate "." t
 'emms-player-mpv-animate--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-animate 'get-media-title
                 'emms-player-mpv-animate--get-media-title)

(emms-player-set 'emms-player-mpv-animate 'mpv-start-process-function
                 'emms-player-mpv-animate--start-process)

(defun emms-player-mpv-animate--loading-message ()
  "Loading message."
  (message "Loading animate.tv ... "))

(defun emms-player-mpv-animate--track-name-to-input-form (track-name)
  "Return rtmpdump form from TRACK-NAME."
  (let ((rtmpdump-form (emms-stream-animate-stream-url-to-rtmpdump-form track-name)))
    (later-do 'emms-player-mpv-animate--loading-message)
    rtmpdump-form))

(defun emms-player-mpv-animate--shell-command-format (params rtmpdump-form)
  "Return shell command for mpv."
  (concat rtmpdump-form " | mpv "
          (mapconcat #'shell-quote-argument `(,@params "-") " ")))

(defun emms-player-mpv-animate--start-process (_cmdname params rtmpdump-form _track)
  "Function for mpv-start-process-function."
  (start-process-shell-command
   emms-player-simple-process-name
   nil
   (emms-player-mpv-animate--shell-command-format params rtmpdump-form)))

(defun emms-player-mpv-animate--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(define-obsolete-function-alias 'emms-player-mpv-animate--xml-collect-node
  'emms-stream-animate--xml-collect-node "20151128")
(define-obsolete-function-alias 'emms-player-mpv-animate--url-to-html
  'emms-stream-animate--url-to-html "20151128")
(define-obsolete-function-alias 'emms-player-mpv-animate--fetch-wmp
  'emms-stream-animate--fetch-wmp "20151128")
(define-obsolete-function-alias 'emms-player-mpv-animate--asx-to-href
  'emms-stream-animate--asx-to-href "20151128")

(provide 'emms-player-mpv-animate)
;;; emms-player-mpv-animate.el ends here
