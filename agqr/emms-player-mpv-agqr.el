;;; emms-player-mpv-agqr.el --- a emms simple mpv player for 超!A&G+ -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-agqr.

;; (require 'emms-player-mpv-agqr)
;; (add-to-list 'emms-player-list 'emms-player-mpv-agqr)

;;; Code:
(require 'emms-player-simple-mpv)

(define-emms-simple-player-mpv mpv-agqr '(streamlist)
  "\\`agqr://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-agqr "." t
 'emms-player-mpv-agqr--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-agqr 'get-media-title
                 'emms-player-mpv-agqr--get-media-title)

(defun emms-player-mpv-agqr--track-name-to-input-form (track-name)
  "Retrun \"rtmp://fms-base2.mitene.ad.jp/agqr/aandg22 live=1\" from TRACK-NAME."
  (format "\"%s live=1\"" (replace-regexp-in-string "\\`agqr" "rtmp" track-name)))

(defun emms-player-mpv-agqr--get-media-title (track)
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track-name))))

(provide 'emms-player-mpv-agqr)
;;; emms-player-mpv-agqr.el ends here
