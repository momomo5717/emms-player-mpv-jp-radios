;;; emms-streams-agqr.el --- emms stream list for 超!A&G+ -*- lexical-binding: t -*-

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

;; This provides emms stream list for Radiko.

;; (require 'emms-streams-agqr)


;;; Code:
(require 'cl-lib)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-agqr-stream-list
  '(("超!A&G+" "agqr://rtmp://fms-base2.mitene.ad.jp/agqr/aandg2" 1 streamlist))
  "超!A&G+ stream list.")

;;;###autoload
(defun emms-stream-agqr-get-stream-list ()
  "Return new streamlist."
  (cl-copy-list emms-stream-agqr-stream-list))

;;;###autoload
(defun emms-stream-agqr-add-bookmark ()
  "Create agqr bookmark, and insert it at point position.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive)
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((streamlist emms-stream-agqr-stream-list)
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;; For media player

;;;###autoload
(defun emms-stream-agqr-stream-url-to-rtmp (stream-url)
  "Return rtmp of STREAM-URL for meida player."
  (if (string-match "\\`agqr://fms-base2" stream-url)
      (replace-regexp-in-string "\\`agqr" "rtmp" stream-url)
    (replace-regexp-in-string "\\`agqr://" "" stream-url)))

(provide 'emms-streams-agqr)
;;; emms-streams-agqr.el ends here
