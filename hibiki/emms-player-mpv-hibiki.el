;;; emms-player-mpv-hibiki.el --- a emms simple mpv player for HiBiKi Radio Station -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-hibiki.

;; (require 'emms-player-mpv-hibiki)
;; (add-to-list 'emms-player-list 'emms-player-mpv-hibiki)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'xml)
(require 'url)

(define-emms-simple-player-mpv mpv-hibiki '(streamlist)
  "\\`hibiki://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-hibiki "." t
 'emms-player-mpv-hibiki--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-hibiki 'get-media-title
                 'emms-player-mpv-hibiki--get-media-title)

(defun emms-player-mpv-hibiki--data-xml-to-url (data)
  "Return url from DATA XML."
  (let* ((protocol (cl-third (assq 'protocol data)))
         (domain (cl-third (assq 'domain data)))
         (dir (cl-third (assq 'dir data)))
         (flv (cl-third (assq 'flv (assq 'channel data))))
         (mp4 (when (string-match  "\[.\]mp4" flv 4)
                (substring flv 4 (match-end 0)))))
    (unless mp4 (error "Failed to parce DATA XML"))
    (format "%s://%s/%s/%s" protocol domain dir mp4)))

(defun emms-player-mpv-hibiki--loading-message ()
  "Loading message."
  (message "Loding 響 ... "))

(defun emms-player-mpv-hibiki--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let ((buf (url-retrieve-synchronously
              (format "http://image.hibiki-radio.jp/uploads/data/channel/%s.xml"
                      (replace-regexp-in-string "hibiki://" "" track-name)))))
    (later-do 'emms-player-mpv-hibiki--loading-message)
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eolp))) (forward-line 1))
          (unless (eobp) (forward-line 1))
          (let* ((html (libxml-parse-html-region (point) (point-max)))
                 (body (car (xml-get-children html 'body)))
                 (data (car (xml-get-children body 'data))))
            (emms-player-mpv-hibiki--data-xml-to-url data)))
      (kill-buffer buf))))

(defun emms-player-mpv-hibiki--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track-name))))

(provide 'emms-player-mpv-hibiki)
;;; emms-player-mpv-hibiki.el ends here
