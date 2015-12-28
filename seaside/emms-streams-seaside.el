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
(require 'cl-lib)
(require 'xml)
(require 'url)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

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
    ("かおりとあさみのグリラジ!! : 火曜日更新"
     "seaside://http://ch.nicovideo.jp/grimoire-gakuen"
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
  (unless (featurep 'emms-streams) (require 'emms-streams))
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

;; For media player

(cl-defun emms-stream-seaside--xml-collect-node
    (name xml-ls &key (test #'identity) (getter #'identity))
  "Collect nodes of NAME from XML-LS.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
Object returned by GETTER is collected."
  (cl-labels ((collect-name-node (xml-ls ls)
               (cond
                ((atom xml-ls) ls)
                ((consp (car xml-ls))
                 (collect-name-node (car xml-ls)
                                    (collect-name-node (cdr xml-ls) ls)))
                ((and (eq (car xml-ls) name)
                      (funcall test xml-ls))
                 (cons (funcall getter xml-ls) ls))
                ((or (null (car xml-ls))
                     (not (symbolp (car xml-ls))))
                 (collect-name-node (cdr xml-ls) ls))
                ((symbolp (car xml-ls))
                 (collect-name-node (xml-node-children xml-ls) ls ))
                (t ls))))
    (collect-name-node xml-ls nil)))

(defun emms-stream-seaside--url-to-html (url)
  "Return html list fron URL."
  (let ((buf (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eolp))) (forward-line 1))
          (unless (eobp) (forward-line 1))
          (libxml-parse-html-region (point) (point-max)))
      (kill-buffer buf))))

(defun emms-stream-seaside--html-to-wax (html track-url)
  "Return WAX from HTML with TRACK-URL."
  (let ((wax
         (car (emms-stream-seaside--xml-collect-node
               'a html
               :test
               (lambda (node) (let ((href (xml-get-attribute-or-nil node 'href)))
                            (and href
                                 (string-match-p "[.]wax$" href))))
               :getter
               (lambda (node)
                 (let ((href (xml-get-attribute-or-nil node 'href)))
                   (if (string-match-p "\\`http://" href)
                       href
                     (concat track-url href))))))))
    (if wax wax (error "Not found WAX"))))

(defun emms-stream-seaside--html-to-nico (html)
  "Return nico url from HTML with TRACK-URL."
  (let ((nico-url
         (car (emms-stream-seaside--xml-collect-node
               'a html
               :test
               (lambda (node) (let ((href (xml-get-attribute-or-nil node 'href)))
                            (and href
                                 (string-match-p "\\`http://www.nicovideo.jp/watch/"
                                                 href))))
               :getter
               (lambda (node) (xml-get-attribute-or-nil node 'href))))))
    (if nico-url nico-url (error "Not found nico url"))))

(defvar emms-stream-seaside-nico-stream-regex
  (concat "\\`"
   (regexp-opt '("seaside://http://seaside-c.jp/program/emergency/"
                 "seaside://http://seaside-c.jp/program/delicatezone/"
                 "seaside://http://ch.nicovideo.jp/grimoire-gakuen"))))

(defun emms-stream-seaside-nico-stream-url-p (stream-url)
  "Return t, if STREAM-URL needs nico url."
  (string-match emms-stream-seaside-nico-stream-regex stream-url))

(defun emms-stream-seaside--stream-url-to-url (stream-url)
  "Replace seaside:// with of STREAM-URL empty string."
  (replace-regexp-in-string "\\`seaside://" "" stream-url))

(defun emms-stream-seaside-stream-url-to-wax (stream-url)
  "Return wax from STREAM-URL."
  (let ((url (emms-stream-seaside--stream-url-to-url stream-url)))
   (emms-stream-seaside--html-to-wax
    (emms-stream-seaside--url-to-html url) url)))

(defun emms-stream-seaside-wax-to-wma (wax)
  "Return wax fron WAX."
  (let ((buf (url-retrieve-synchronously wax)))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eolp))) (forward-line 1))
          (unless (eobp) (forward-line 1))
          (let ((wma (car
                      (emms-stream-seaside--xml-collect-node
                       'ref (libxml-parse-html-region (point) (point-max))
                       :getter (lambda (node) (xml-get-attribute-or-nil node 'href)) ))))
            (if wma wma (error "Not found WMA"))))
      (kill-buffer buf))))

(defun emms-stream-seaside-stream-url-to-nico-url (stream-url)
  "Return nico url from STREAM-URL."
  (emms-stream-seaside--html-to-nico
   (emms-stream-seaside--url-to-html
    (emms-stream-seaside--stream-url-to-url stream-url))))

(provide 'emms-streams-seaside)
;;; emms-streams-seaside.el ends here
