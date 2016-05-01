;;; emms-player-mpv-anitama.el --- An emms simple mpv player for アニたまどっとコム -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 momomo5717

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
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no"
  "--cache=150000" "--force-seekable=yes")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-anitama "\\`anitama://" t
 'emms-player-mpv-anitama--track-name-to-wget-form)

(emms-player-set 'emms-player-mpv-anitama 'get-media-title
                 'emms-player-mpv-anitama--get-media-title)

(emms-player-set 'emms-player-mpv-anitama 'mpv-start-process-function
                 'emms-player-mpv-anitama--start-process)

(defun emms-player-mpv-anitama--loading-message ()
  "Loading message."
  (message "Loading アニたまどっとコム ... "))

(defun emms-player-mpv-anitama--track-name-to-wget-form (track-name)
  "Return wget-form from TRACK-NAME."
  (let ((wget-form (emms-stream-anitama-stream-url-to-wget-form track-name)))
    (later-do 'emms-player-mpv-anitama--loading-message)
    wget-form))

(defun emms-player-mpv-anitama--shell-command-format (params wget-form)
  "Shell command  for PARAMS, NODEID."
  (emms-stream-anitama--write-unless-cookies)
  (concat wget-form " | mpv "
          (mapconcat #'shell-quote-argument `(,@params "-") " ")))

(defun emms-player-mpv-anitama--start-process (_cmdname params wget-form _track)
  "Function for mpv-start-process-function."
  (start-process-shell-command
   emms-player-simple-process-name
   nil
   (emms-player-mpv-anitama--shell-command-format params wget-form)))

(defun emms-player-mpv-anitama--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))


(define-obsolete-variable-alias 'emms-player-mpv-anitama--cookie-file
  'emms-stream-anitama--cookie-file "20151128")
(define-obsolete-function-alias 'emms-player-mpv-anitama--access-weeeef
  'emms-stream-anitama--access-weeeef "20151128")
(define-obsolete-function-alias 'emms-player-mpv-anitama--write-cookies
  'emms-stream-anitama--write-cookies "20151128")
(define-obsolete-function-alias 'emms-player-mpv-anitama--have-cookies-p
  'emms-stream-anitama--have-cookies-p "20151128")
(define-obsolete-function-alias 'emms-player-mpv-anitama--write-unless-cookies
  'emms-stream-anitama--write-unless-cookies "20151128")
(define-obsolete-function-alias 'emms-player-mpv-anitama--fetch-BookXmlGet-nodeId
  'emms-stream-anitama--fetch-BookXmlGet-nodeId "20151128")
(define-obsolete-function-alias 'emms-player-mpv-anitama--track-name-to-nodeId
  'identity "20151128")

(provide 'emms-player-mpv-anitama)
;;; emms-player-mpv-anitama.el ends here
