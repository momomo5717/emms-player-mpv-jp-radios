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
    "radiru://https://nhkradioakr2-i.akamaihd.net/hls/live/511929/1-r2/1-r2-01.m3u8"
    1 streamlist))


(defvar emms-stream-radiru-stream-list-sapporo
  `(("NHK第1 札幌"
     "radiru://https://nhkradioikr1-i.akamaihd.net/hls/live/512098/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 札幌"
     "radiru://https://nhkradioikfm-i.akamaihd.net/hls/live/512100/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 札幌 stream list.")

(defvar emms-stream-radiru-stream-list-sendai
  `(("NHK第1 仙台"
     "radiru://https://nhkradiohkr1-i.akamaihd.net/hls/live/512075/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 仙台"
     "radiru://https://nhkradiohkfm-i.akamaihd.net/hls/live/512076/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 仙台 stream list.")

(defvar emms-stream-radiru-stream-list-tokyo
  `(("NHK第1 東京"
     "radiru://https://nhkradioakr1-i.akamaihd.net/hls/live/511633/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 東京"
     "radiru://https://nhkradioakfm-i.akamaihd.net/hls/live/512290/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 東京 stream list.")

(defvar emms-stream-radiru-stream-list-nagoya
  `(("NHK第1 名古屋"
     "radiru://https://nhkradiockr1-i.akamaihd.net/hls/live/512072/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 名古屋"
     "radiru://https://nhkradiockfm-i.akamaihd.net/hls/live/512074/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 名古屋 stream list.")

(defvar emms-stream-radiru-stream-list-osaka
  `(("NHK第1 大阪"
     "radiru://https://nhkradiobkr1-i.akamaihd.net/hls/live/512291/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 大阪"
     "radiru://https://nhkradiobkfm-i.akamaihd.net/hls/live/512070/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 大阪 stream list.")

(defvar emms-stream-radiru-stream-list-hiroshima
  `(("NHK第1 広島"
     "radiru://https://nhkradiofkr1-i.akamaihd.net/hls/live/512086/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 広島"
     "radiru://https://nhkradiofkfm-i.akamaihd.net/hls/live/512087/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 広島 stream list.")

(defvar emms-stream-radiru-stream-list-matsuyama
  `(("NHK第1 松山"
     "radiru://https://nhkradiozkr1-i.akamaihd.net/hls/live/512103/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 松山"
     "radiru://https://nhkradiozkfm-i.akamaihd.net/hls/live/512106/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 松山 stream list.")

(defvar emms-stream-radiru-stream-list-fukuoka
  `(("NHK第1 福岡"
     "radiru://https://nhkradiolkr1-i.akamaihd.net/hls/live/512088/1-r1/1-r1-01.m3u8"
     1 streamlist)
    ,emms-stream-radiru-streamlist-daini
    ("NHK-FM 福岡"
     "radiru://https://nhkradiolkfm-i.akamaihd.net/hls/live/512097/1-fm/1-fm-01.m3u8"
     1 streamlist))
  "らじる★らじる 福岡 stream list.")

(defvar emms-stream-radiru-stream-list-names
  '(emms-stream-radiru-stream-list-sapporo
    emms-stream-radiru-stream-list-sendai
    emms-stream-radiru-stream-list-tokyo
    emms-stream-radiru-stream-list-nagoya
    emms-stream-radiru-stream-list-osaka
    emms-stream-radiru-stream-list-hiroshima
    emms-stream-radiru-stream-list-matsuyama
    emms-stream-radiru-stream-list-fukuoka))

(defun emms-stream-radiru-get-area-stream-list (area)
  "Return stream-list of the AREA.

AREA is a number of 1-8 or the symobl:
 1: sapporo 2: sendai 3: tokyo 4: nagoya
 5: osaka 6: hiroshima 7: matsuyama 8: fukuoka"
  (cl-case area
    ((1 sapporo) emms-stream-radiru-stream-list-sapporo)
    ((2 sendai) emms-stream-radiru-stream-list-sendai)
    ((3 tokyo) emms-stream-radiru-stream-list-tokyo)
    ((4 nagoya) emms-stream-radiru-stream-list-nagoya)
    ((5 osaka) emms-stream-radiru-stream-list-osaka)
    ((6 hiroshima) emms-stream-radiru-stream-list-hiroshima)
    ((7 matsuyama) emms-stream-radiru-stream-list-matsuyama)
    ((8 fukuoka) emms-stream-radiru-stream-list-fukuoka)
    (t nil)))

;;;###autoload
(defun emms-stream-radiru-get-stream-list ()
  "Return new stream-list."
  (cons emms-stream-radiru-streamlist-daini
        (cl-loop
         with ls = nil
         for area-stream-list-name in emms-stream-radiru-stream-list-names
         do (dolist (stream (symbol-value area-stream-list-name))
              (unless (equal (car stream) "NHK第2")
                (push stream ls)))
         finally return (nreverse ls))))

(defvar emms-stream-radiru--add-bookmark-msg
  (concat "[0] All  [1] 札幌(sapporo)   [2] 仙台(sendai)  [3] 東京(tokyo)\n"
          "         [4] 名古屋(nagoya)  [5] 大阪(osaka)   [6] 広島(hiroshima)\n"
          "         [7] 松山(matsuyama) [8] 福岡(fukuoka)\n"
          "Input a number of 0-8: "))

(defun emms-stream-radiru--add-bookmark-1 (stream-list)
  "Helper functions for `emms-stream-radiru-add-bookmark'"
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (stream stream-list)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-radiru-add-bookmark (&optional area)
  "Create simul bookmark, and insert it at point position.
AREA is a number of 0-8.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (unless (integerp area)
    (let ((msg emms-stream-radiru--add-bookmark-msg))
      (while (not (and (integerp (setq area (read-number msg)))
                       (<= 0 area) (<= area 8))))))
  (emms-stream-radiru--add-bookmark-1
   (if (zerop area)
       (emms-stream-radiru-get-stream-list)
     (emms-stream-radiru-get-area-stream-list area))))

;; For media player

;;;###autoload
(defun emms-stream-radiru-stream-url-to-m3u8 (stream-url)
  "Return rtmpe from STREAM-URL."
  (replace-regexp-in-string "\\`radiru://" "" stream-url))

(define-obsolete-function-alias 'emms-stream-radiru-stream-url-to-rtmpe
  'emms-stream-radiru-stream-url-to-m3u8
  "20171002")

(provide 'emms-streams-radiru)
;;; emms-streams-radiru.el ends here
