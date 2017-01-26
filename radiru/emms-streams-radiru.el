;;; emms-streams-radiru.el --- emms stream list for らじる★らじる -*- lexical-binding: t -*-

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

;; This provides emms stream list for らじる★らじる.

;; (require 'emms-streams-radiru)

;;; Code:
(require 'cl-lib)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-radiru-streamlist-daini
  '("NHK第2"
     "radiru://rtmpe://netradio-r2-flash.nhk.jp/live/NetRadio_R2_flash@63342"
     1 streamlist))

(defvar emms-stream-radiru-stream-list-sendai
  `(("NHK第1 仙台"
     "radiru://rtmpe://netradio-hkr1-flash.nhk.jp/live/NetRadio_HKR1_flash@108442"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 仙台"
     "radiru://rtmpe://netradio-hkfm-flash.nhk.jp/live/NetRadio_HKFM_flash@108237"
     1 streamlist))
  "らじる★らじる 仙台 stream list.")

(defvar emms-stream-radiru-stream-list-tokyo
  `(("NHK第1 東京"
     "radiru://rtmpe://netradio-r1-flash.nhk.jp/live/NetRadio_R1_flash@63346"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 東京"
     "radiru://rtmpe://netradio-fm-flash.nhk.jp/live/NetRadio_FM_flash@63343"
     1 streamlist))
  "らじる★らじる 東京 stream list.")

(defvar emms-stream-radiru-stream-list-nagoya
  `(("NHK第1 名古屋"
     "radiru://rtmpe://netradio-ckr1-flash.nhk.jp/live/NetRadio_CKR1_flash@108234"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 名古屋"
     "radiru://rtmpe://netradio-ckfm-flash.nhk.jp/live/NetRadio_CKFM_flash@108235"
     1 streamlist))
  "らじる★らじる 名古屋 stream list.")

(defvar emms-stream-radiru-stream-list-osaka
  `(("NHK第1 大阪"
     "radiru://rtmpe://netradio-bkr1-flash.nhk.jp/live/NetRadio_BKR1_flash@108232"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 大阪"
     "radiru://rtmpe://netradio-bkfm-flash.nhk.jp/live/NetRadio_BKFM_flash@108233"
     1 streamlist))
  "らじる★らじる 大阪 stream list.")

(defun emms-stream-radiru-get-area-stream-list (area)
  "Return streamlist of the AREA."
  (cl-case area
    (sendai emms-stream-radiru-stream-list-sendai)
    (tokyo  emms-stream-radiru-stream-list-tokyo)
    (nagoya emms-stream-radiru-stream-list-nagoya)
    (osaka  emms-stream-radiru-stream-list-osaka)
    (t nil)))

;;;###autoload
(defun emms-stream-radiru-get-stream-list ()
  "Return new stream-list."
  (cons emms-stream-radiru-streamlist-daini
        (cl-loop
         with ls = nil
         for area-stream-list in (list emms-stream-radiru-stream-list-sendai
                                       emms-stream-radiru-stream-list-tokyo
                                       emms-stream-radiru-stream-list-nagoya
                                       emms-stream-radiru-stream-list-osaka)
         do (dolist (stream area-stream-list)
              (unless (equal (car stream) "NHK第2")
                (push stream ls)))
         finally return (nreverse ls))))

;;;###autoload
(defun emms-stream-radiru-add-bookmark (area)
  "Create radiru bookmarks of the AREA, and insert it at point position.

If save ,run `emms-stream-save-bookmarks-file' after."
  (interactive "SInput area (sendai, tokyo, nagoya, osaka) : ")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((stream-list (emms-stream-radiru-get-area-stream-list area))
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream stream-list)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;; For media player

(defvar emms-stream-radiru-url-swf
  "http://www3.nhk.or.jp/netradio/files/swf/rtmpe.swf")

;;;###autoload
(defun emms-stream-radiru-stream-url-to-rtmpe (stream-url)
  "Return rtmpe from STREAM-URL."
  (replace-regexp-in-string "\\`radiru://" "" stream-url))

(provide 'emms-streams-radiru)
;;; emms-streams-radiru.el ends here
