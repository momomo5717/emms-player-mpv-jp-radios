;;; emms-streams-radiru.el --- emms stream list for らじる★らじる -*- lexical-binding: t -*-

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

;; This provides emms stream list for らじる★らじる.

;; (require 'emms-streams-radiru)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)

(defvar emms-stream-radiru-streamlist-sendai
  '(("NHK第1 仙台"
     "radiru://rtmpe://netradio-hkr1-flash.nhk.jp/live/NetRadio_HKR1_flash@108442"
     1 streamlist)
    ("NHK第2"
     "radiru://rtmpe://netradio-r2-flash.nhk.jp/live/NetRadio_R2_flash@63342"
     1 streamlist)
    ("NHK-FM 仙台"
     "radiru://rtmpe://netradio-hkfm-flash.nhk.jp/live/NetRadio_HKFM_flash@108237"
     1 streamlist))
  "らじる★らじる 仙台 stream list.
Replace radiru:// with mms:// when use it.")

(defvar emms-stream-radiru-streamlist-tokyo
  '(("NHK第1 東京"
     "radiru://rtmpe://netradio-r1-flash.nhk.jp/live/NetRadio_R1_flash@63346"
     1 streamlist)
    ("NHK第2"
     "radiru://rtmpe://netradio-r2-flash.nhk.jp/live/NetRadio_R2_flash@63342"
     1 streamlist)
    ("NHK-FM 東京"
     "radiru://rtmpe://netradio-fm-flash.nhk.jp/live/NetRadio_FM_flash@63343"
     1 streamlist))
  "らじる★らじる 東京 stream list.
Replace radiru:// with mms:// when use it.")

(defvar emms-stream-radiru-streamlist-nagoya
  '(("NHK第1 名古屋"
     "radiru://rtmpe://netradio-ckr1-flash.nhk.jp/live/NetRadio_CKR1_flash@108234"
     1 streamlist)
    ("NHK第2"
     "radiru://rtmpe://netradio-r2-flash.nhk.jp/live/NetRadio_R2_flash@63342"
     1 streamlist)
    ("NHK-FM 名古屋"
     "radiru://rtmpe://netradio-ckfm-flash.nhk.jp/live/NetRadio_CKFM_flash@108235"
     1 streamlist))
  "らじる★らじる 名古屋 stream list.
Replace radiru:// with mms:// when use it.")

(defvar emms-stream-radiru-streamlist-osaka
  '(("NHK第1 大阪"
     "radiru://rtmpe://netradio-bkr1-flash.nhk.jp/live/NetRadio_BKR1_flash@108232"
     1 streamlist)
    ("NHK第2"
     "radiru://rtmpe://netradio-r2-flash.nhk.jp/live/NetRadio_R2_flash@63342"
     1 streamlist)
    ("NHK-FM 大阪"
     "radiru://rtmpe://netradio-bkfm-flash.nhk.jp/live/NetRadio_BKFM_flash@108233"
     1 streamlist))
  "らじる★らじる 大阪 stream list.
Replace radiru:// with mms:// when use it.")

(defun emms-stream-rajiru-get-streamlist (area)
  "Return streamlist of the AREA."
  (cl-case area
    (sendai emms-stream-radiru-streamlist-sendai)
    (tokyo  emms-stream-radiru-streamlist-tokyo)
    (nagoya emms-stream-radiru-streamlist-nagoya)
    (osaka  emms-stream-radiru-streamlist-osaka)
    (t nil)))

;;;###autoload
(defun emms-stream-radiru-add-bookmark (area)
  "Create radiru bookmarks of the AREA, and insert it at point position.

If save ,run `emms-stream-save-bookmarks-file' after."
  (interactive "SInput area (sendai, tokyo, nagoya, osaka) : ")
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((streamlist (emms-stream-rajiru-get-streamlist area))
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-radiru)
;;; emms-streams-radiru.el ends here
