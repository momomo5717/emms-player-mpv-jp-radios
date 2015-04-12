;;; emms-streams-agqr.el --- emms stream list for 超!A&G+ -*- lexical-binding: t -*-

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

;; (require 'emms-stream-agqr)


;;; Code:
(require 'emms-streams)
(require 'emms-player-mpv-agqr)

(defvar emms-stream-agqr-streamlist
  '(("超!A&G+" "agqr://fms-base2.mitene.ad.jp/agqr/aandg22" 1 streamlist))
  "超!A&G+ stream list.
Replace agqr:// with rtmp:// when use it.")

;;;###autoload
(defun emms-stream-agqr-add-bookmark ()
  "Create agqr bookmark, and insert it at point position.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive)
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (let* ((streamlist emms-stream-agqr-streamlist)
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-agqr)
;;; emms-streams-agqr.el ends here
