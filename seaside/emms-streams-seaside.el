;;; emms-streams-seaside.el --- emms stream list for Sea Side Communications -*- lexical-binding: t -*-
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

;; This provides emms stream list for Sea Side Communications.

;; (require 'emms-streams-seaside)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'url-cookie)
(require 'emms-streams-jp-radios-util)
(require 'emms-streams-nico-util)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-seaside-stream-list-cache nil)

(defvar emms-stream-seaside--url "http://ch.nicovideo.jp/seaside-channel")

(defvar emms-stream-seaside--stream-list
  (cl-loop
   for (title url) in
   '(("RADIOアニメロミックス 内山夕実と吉田有里のゆゆらじ : 内山夕実、吉田有里 : 毎月第2・第4金曜日22：00更新"
      "http://ch.nicovideo.jp/ch312/video")
     ("巽悠衣子の「下も向いて歩こう＼(^o^)／」 : 巽悠衣子 : 隔週金曜日23：30以降（生放送日翌週更新）"
      "http://ch.nicovideo.jp/tatsumiyuiko")
     ("ゆずかアプセット ニコ生広報室  : 小澤亜李, 冨岡美沙子 : 月1回更新"
      "http://ch.nicovideo.jp/upset"))
   collect (list title (concat "seaside://" url) 1 'streamlist))
  "This will be appended to `emms-stream-seaside-stream-list-cache'.")

(defvar emms-stream-seaside--cast-alist
  (cl-loop
   for (title . cast) in
   '(("井上麻里奈・下田麻美のIT革命！" . "井上麻里奈、下田麻美")
     ("西明日香のデリケートゾーン！" . "西明日香")
     ("田村睦心×瀬戸麻沙美の獅子奮迅！体育会系ラジオ！" . "田村睦心、瀬戸麻沙美")
     ("洲崎西" . "洲崎綾、西明日香")
     ("春佳・彩花のSSちゃんねる" . "照井春佳、諏訪彩花")
     ("西田望見・奥野香耶のず～ぱらだいす" . "西田望見、奥野香耶")
     ("安済知佳と朝井彩加のふたりはシンパシー！" . "安済知佳、朝井彩加")
     ("なんでもヒーロー！ゆっけとまーぼー" . "小林裕介、古川慎")
     ("内田さんと浅倉さん" . "内田彩、浅倉杏美")
     ("BELOVED MEMORIES" . "田丸篤志、内田雄馬")
     ("フレッシュたかまつ" . "高田憂希、松田颯水")
     ("あどりぶ" . "巽悠衣子、大橋彩香")
     ("EMERGENCY the RADIO" . "NEW YOUNG（小野坂昌也）CRAZY YU（小林ゆう）DANCING YUKARI（後藤友香里）"))
   collect (cons (regexp-quote title) cast)))

(defun emms-stream-seaside--get-cast (title)
  (cl-loop for (re . cast) in emms-stream-seaside--cast-alist
           thereis (and (string-match-p re title) cast)))

(defconst emms-stream-seaside--title-date-regex
  (rx (and "（"
           (group-n 1 (= 4 (any digit))) "."
           (group-n 2 (= 2 (any digit))) "."
           (group-n 3 (= 2 (any digit)))
           "）")))

(defun emms-stream-seaside--from_video-to-streamlist (from_video)
  "Return streamlist from a node whcih have FROM_VIDEO of class value."
  (car (emms-stream-jpr-get-node
        from_video :ignore
        :test
        (lambda (xml) (equal (xml-get-attribute-or-nil xml 'class) "g-video-link"))
        :getter
        (lambda (xml)
          (let* ((title (car (split-string (xml-get-attribute xml 'title) "[\n\r]")))
                 (url (xml-get-attribute xml 'href))
                 (cast (emms-stream-seaside--get-cast title))
                 (date (when (string-match emms-stream-seaside--title-date-regex title)
                         (format " : %s/%s/%s"
                                 (match-string 1 title)
                                 (match-string 2 title)
                                 (match-string 3 title)))))
            (list (concat title (when cast (format " : %s" cast)) date)
                  (concat "seaside://" url) 1 'streamlist))))))

(defun emms-stream-seaside--html-to-raw-stream-list (html)
  "Extract stream list from HTML."
  (emms-stream-jpr-get-node
   html :ignore
   :test
   (lambda (xml) (string-match "from_video" (xml-get-attribute xml 'class)))
   :getter #'emms-stream-seaside--from_video-to-streamlist))

(defun emms-stream-seaside--usable-stream-list-1 (str1 str2)
  "Predicate function for `emms-stream-seaside--usable-stream-list'."
  (let* ((s-re "[ 　]")
         (n-re "第\\([[:digit:]]+\\)回")
         (s1 (car (split-string str1 s-re t)))
         (s2 (car (split-string str2 s-re t))))
    (if (equal s1 s2)
        (let ((n1 (when (string-match n-re str1)
                    (string-to-number (match-string 1 str1))))
              (n2 (when (string-match n-re str2)
                    (string-to-number (match-string 1 str2)))))
          (if (and n1 n2) (> n1 n2) (not (string< str1 str2))))
      (string< str1 str2))))

(defun emms-stream-seaside--usable-stream-list (streams)
  "Return usable stream list from STREAMS.
Destructively remove `equal' duplicates STREAMS."
  (cl-sort (cl-delete-if
            (lambda (str) (string-match-p "月額会員" str))
            (cl-delete-duplicates streams :key #'cl-second :test #'equal)
            :key #'cl-first)
           #'emms-stream-seaside--usable-stream-list-1
           :key #'cl-first))

(defun emms-stream-seaside--html-to-stream-list (html)
  "Return stream list from HTML."
  (nconc (emms-stream-seaside--usable-stream-list
          (emms-stream-seaside--html-to-raw-stream-list html))
         (cl-copy-list emms-stream-seaside--stream-list)))

(defun emms-stream-seaside-fetch-stream-list (&optional updatep)
  "Retrun seaside stream list.
If UPDATEP is no-nil, cache is updated."
  (if (and (not updatep) (consp emms-stream-seaside-stream-list-cache))
      emms-stream-seaside-stream-list-cache
    (setq emms-stream-seaside-stream-list-cache
          (emms-stream-seaside--html-to-stream-list
           (emms-stream-jpr-url-to-html emms-stream-seaside--url)))))

;;;###autoload
(defun emms-stream-seaside-update-cache-async ()
  "Update cache asynchronously."
  (url-queue-retrieve
   emms-stream-seaside--url
   (lambda (status &rest _)
     (if (memq :error status)
         (message "Failed to get seaside stream list : %s" (cdr status))
       (setq emms-stream-seaside-stream-list-cache
             (emms-stream-seaside--html-to-stream-list
              (emms-stream-jpr-url-to-html (current-buffer))))
       (message "Updated seaside stream list cache")))))

;;;###autoload
(defun emms-stream-seaside-get-stream-list ()
  "Return new stream-list."
  (cl-copy-list emms-stream-seaside-stream-list-cache))

;;;###autoload
(defun emms-stream-seaside-add-bookmark (&optional updatep)
  "Create seaside bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1)
      (emms-stream-seaside-update-cache-async)
    (let ((buf (get-buffer emms-stream-buffer-name)))
      (unless (buffer-live-p buf)
        (error "%s is not a live buffer" emms-stream-buffer-name))
      (set-buffer buf))
    (let* ((stream-list (emms-stream-seaside-fetch-stream-list updatep))
           (line       (emms-line-number-at-pos (point)))
           (index      (+ (/ line 2) 1)))
      (dolist (stream stream-list)
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index))
      (emms-stream-redisplay)
      (goto-char (point-min))
      (forward-line (1- line)))))

;; For media player

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

(defun emms-stream-seaside--html-to-nico-watch (html &optional n)
  "Return nico watch url of N th in HTML.
If N is nil, it will be set to 0."
  (setq n (or n 0))
  (let ((nico-url
         (nth n
              (or (emms-stream-jpr-get-node
                   html 'iframe
                   :test
                   (lambda (node)
                     (let ((src (xml-get-attribute-or-nil node 'src)))
                       (and src
                            (string-match "\\`http://ext\\.nicovideo\\.jp/thumb/\\([[:alnum:]]+\\)\\'"
                                          src))))
                   :getter
                   (lambda (node) (concat "http://www.nicovideo.jp/watch/"
                                      (match-string 1 (xml-get-attribute node 'src)))))
                  (emms-stream-jpr-get-node
                   html 'a
                   :test
                   (lambda (node)
                     (let ((href (xml-get-attribute-or-nil node 'href)))
                       (and href
                            (string-match-p "\\`http://www\\.nicovideo\\.jp/watch/"
                                            href))))
                   :getter
                   (lambda (node) (xml-get-attribute-or-nil node 'href)))))))
    (if nico-url nico-url (error "Not found nico url"))))

(defun emms-stream-seaside--stream-url-to-url (stream-url)
  "Replace seaside:// of STREAM-URL with empty string."
  (replace-regexp-in-string "\\`seaside://" "" stream-url))

;;;###autoload
(defun emms-stream-seaside-stream-url-to-nico-url (stream-url)
  "Return nico url from STREAM-URL."
  (if (string-match "http://www\\.nicovideo\\.jp/watch/[[:alnum:]]+" stream-url)
      (match-string 0 stream-url)
    (emms-stream-seaside--html-to-nico-watch
     (emms-stream-seaside--url-to-html
      (emms-stream-seaside--stream-url-to-url stream-url)))))

;;;###autoload
(defun emms-stream-seaside-stream-url-to-nicovideo-url (stream-url)
  "Return nicovideo url for STREAM-URL."
  (funcall (if emms-stream-nico-use-old-api
               #'emms-stream-nico-old-nico-url-to-nicovideo-url
             #'emms-stream-nico-url-to-nicovideo)
           (emms-stream-seaside-stream-url-to-nico-url stream-url)))

(define-obsolete-variable-alias 'emms-stream-seaside--nico-domain
  'emms-stream-nico-old--domain
  "20171112")

(define-obsolete-variable-alias 'emms-stream-seaside--nico-cookies-file
  'emms-stream-nico-old--cookies-file
  "20171112")

(define-obsolete-function-alias 'emms-stream-seaside--write-nico-cookies
  'emms-stream-nico-old--write-nico-cookies
  "20171112")

(define-obsolete-variable-alias 'emms-stream-seaside--nico-url-user-agent
  'emms-stream-nico-url-user-agent
  "20171112")

(define-obsolete-function-alias 'emms-stream-seaside--nico-url-http-user-agent-string
  'emms-stream-nico-url-http-user-agent-string
  "20171112")

(make-obsolete 'emms-stream-seaside--nico-url-retrieve-synchronously
               'emms-stream-nico-with-user-agent
               "20171112")

(define-obsolete-function-alias 'emms-stream-seaside--search-thumbKey
  'emms-stream-nico-old--search-thumbKey
  "20171112")

(define-obsolete-function-alias 'emms-stream-seaside--fetch-nico-thumbPlayKey
  'emms-stream-nico-old--fetch-nico-thumbPlayKey
  "20171112")

(define-obsolete-function-alias 'emms-stream-seaside--search-nicovideo-url
  'emms-stream-nico-old--search-nicovideo-url
  "20171112")

(define-obsolete-function-alias 'emms-stream-seaside--fetch-nicovideo-url
  'emms-stream-nico-old--fetch-nicovideo-url
  "20171112")

(provide 'emms-streams-seaside)
;;; emms-streams-seaside.el ends here
