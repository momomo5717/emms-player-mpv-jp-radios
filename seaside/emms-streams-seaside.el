;;; emms-streams-seaside.el --- emms stream list for Sea Side Communications -*- lexical-binding: t -*-
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

;; This provides emms stream list for Sea Side Communications.

;; (require 'emms-streams-seaside)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)

(defvar emms-stream-seaside-stream-list
  '(("あどりぶ : 月曜日更新"
     "seaside://http://seaside-c.jp/program/adlib/"
     1 streamlist)
    ("ステキ情報バラエティ 発信！もいとろ君 : 月曜日更新"
     "seaside://http://seaside-c.jp/program/moitorokun/"
     1 streamlist)
    ("BELOVED MEMORIES : 月曜日更新"
     "seaside://http://seaside-c.jp/program/belovedmemories/"
     1 streamlist)
    ("ごぶごぶちゃん☆～中村繪里子・田村睦心 : 水曜日更新(最終回)"
     "seaside://http://seaside-c.jp/program/gobu/"
     1 streamlist)
    ("内田さんと浅倉さん : 水曜日更新"
     "seaside://http://seaside-c.jp/program/uchidaasakura/"
     1 streamlist)
    ("ありがた系迷惑プレゼンショー　はるか・ちなみの「りめいく！」 : 水曜日更新"
     "seaside://http://seaside-c.jp/program/remake/"
     1 streamlist)
    ("西明日香のデリケートゾーン！ : 水曜日更新"
     "seaside://http://seaside-c.jp/program/delicatezone/"
     1 streamlist)
    ("春佳・彩花のSSちゃんねる : 水曜日更新"
     "seaside://http://seaside-c.jp/program/ssc/"
     1 streamlist)
    ("井上麻里奈・下田麻美のIT革命！ : 木曜日更新"
     "seaside://http://it.seaside-c.jp/"
     1 streamlist)
    ("洲崎西 : 木曜日更新"
     "seaside://http://seaside-c.jp/program/suzakinishi/"
     1 streamlist)
    ("中村繪里子ら・ら☆ら♪　なかむランド～Love・Laugh☆Live♪～ : 木曜日更新"
     "seaside://http://nakamuland.net/"
     1 streamlist)
    ("EMERGENCY the RADIO : 土曜日更新"
     "seaside://http://seaside-c.jp/program/emergency/"
     1 streamlist)))

(defun emms-stream-seaside-get-stream-list ()
  "Return new stream-list."
  (cl-copy-list emms-stream-seaside-stream-list))

;;;###autoload
(defun emms-stream-seaside-add-bookmark ()
  "Create seaside bookmark, and insert it at point position.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive)
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((stream-list emms-stream-seaside-stream-list)
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream stream-list)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-seaside)
;;; emms-streams-seaside.el ends here
