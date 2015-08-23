;;; emms-streams-radiko.el --- emms stream list for Radiko -*- lexical-binding: t -*-

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

;; This provides emms stream list for Radiko.

;; (require 'emms-streams-radiko)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'emms-player-mpv-radiko)

(defvar emms-stream-radiko-streamlist-cache nil)

(defun emms-stream-radiko--fetch-streamlist (area-id)
  "Retrun AREA-ID radiko stream list.
string -> streamlist
\(emms-radiko-wget-radiko-stream-list \"JP13\"\) => streamlist"
  (let ((buf (url-retrieve-synchronously
              (format "http://radiko.jp/v2/station/list/%s.xml" area-id))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (cl-loop for station
               in (xml-get-children (libxml-parse-xml-region
                                     (point) (point-max))
                                    'station)
               for id = (car (xml-node-children (car (xml-get-children station 'id))))
               for name = (car (xml-node-children (car (xml-get-children station 'name))))
               collect (list name (format "radiko://%s" id) 1 'streamlist)
               finally do (kill-buffer buf)))))

(defun emms-stream-radiko-fetch-current-area-streamlist (&optional updatep)
  "Return streamlist of the current area."
  (if (and (not updatep) (consp emms-stream-radiko-streamlist-cache))
      emms-stream-radiko-streamlist-cache
    (setq emms-stream-radiko-streamlist-cache
     (emms-stream-radiko--fetch-streamlist
        (emms-player-mpv-radiko--get-area-id
         (emms-player-mpv-radiko--access-auth2-fms
          (emms-player-mpv-radiko--access-auth1-fms)))))))

;;;###autoload
(defun emms-stream-radiko-add-bookmark (&optional updatep)
  "Create radiko bookmarks, and insert it at point position.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (let* ((streamlist (emms-stream-radiko-fetch-current-area-streamlist updatep))
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-radiko)
;;; emms-streams-radiko.el ends here
