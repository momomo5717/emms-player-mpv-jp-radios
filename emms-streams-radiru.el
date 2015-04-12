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

;; (require 'emms-stream-radiru)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)

(defvar emms-stream-radiru-streamlist-sendai
  '(("NHK第1 仙台"
     "radiru://a1125.l22063959124.c220639.g.lm.akamaistream.net/D/1125/220639/v0001/reflector:59124"
     1 streamlist)
    ("NHK第2"
     "radiru://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 仙台"
     "radiru://a18.l22064052017.c220640.g.lm.akamaistream.net/D/18/220640/v0001/reflector:52017"
     1 streamlist))
  "らじる★らじる 仙台 stream list.
Replace radiru:// with mms:// when use it.")

(defvar emms-stream-radiru-streamlist-tokyo
  '(("NHK第1 東京"
     "radiru://a33.l12993146032.c129931.g.lm.akamaistream.net/D/33/129931/v0001/reflector:46032"
     1 streamlist)
    ("NHK第2"
     "radiru://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 東京"
     "radiru://a52.l12993346051.c129933.g.lm.akamaistream.net/D/52/129933/v0001/reflector:46051"
     1 streamlist))
  "らじる★らじる 東京 stream list.
Replace radiru:// with mms:// when use it.")

(defvar emms-stream-radiru-streamlist-nagoya
  '(("NHK第1 名古屋"
     "radiru://a220.l22063752219.c220637.g.lm.akamaistream.net/D/220/220637/v0001/reflector:52219"
     1 streamlist)
    ("NHK第2"
     "radiru://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 名古屋"
     "radiru://a1741.l22063855740.c220638.g.lm.akamaistream.net/D/1741/220638/v0001/reflector:55740"
     1 streamlist))
  "らじる★らじる 名古屋 stream list.
Replace radiru:// with mms:// when use it.")

(defvar emms-stream-radiru-streamlist-osaka
  '(("NHK第1 大阪"
     "radiru://a1532.l22063553531.c220635.g.lm.akamaistream.net/D/1532/220635/v0001/reflector:53531"
     1 streamlist)
    ("NHK第2"
     "radiru://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 大阪"
     "radiru://a884.l22063650883.c220636.g.lm.akamaistream.net/D/884/220636/v0001/reflector:50883"
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
  (set-buffer (get-buffer-create emms-stream-buffer-name))
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
