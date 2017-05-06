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

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-seaside-stream-list
  (cl-loop
   for (title url) in
   '(("井上麻里奈・下田麻美のIT革命！ : 井上麻里奈、下田麻美 : 毎週火曜日20：00更新"
      "http://it.seaside-c.jp/")
     ("西明日香のデリケートゾーン！ : 西明日香 : 毎週火曜日20：30更新"
      "http://seaside-c.jp/program/delicatezone/")
     ("田村睦心×瀬戸麻沙美の獅子奮迅！体育会系ラジオ！ : 田村睦心、瀬戸麻沙美 : 毎週火曜日21：30更新"
      "http://seaside-c.jp/program/t-radio/")
     ("洲崎西 : 洲崎綾、西明日香 : 毎週水曜日20：30更新"
      "http://seaside-c.jp/program/suzakinishi/")
     ("春佳・彩花のSSちゃんねる : 照井春佳、諏訪彩花 : 毎週水曜日21：30更新"
      "http://seaside-c.jp/program/ssc/")
     ("西田望見・奥野香耶のず～ぱらだいす : 西田望見、奥野香耶 : 毎週水曜日21：00更新"
      "http://seaside-station.net/program/zoo/")
     ("安済知佳と朝井彩加のふたりはシンパシー！ : 安済知佳、朝井彩加 : 毎週木曜日22：00以降（生放送終了後）"
      "http://seaside-station.net/program/futapathy/")
     ("なんでもヒーロー！ゆっけとまーぼー : 小林裕介、古川慎 : 毎週金曜日21：00更新"
      "http://seaside-station.net/program/ytom/")
     ("内田さんと浅倉さん : 内田彩、浅倉杏美 : 毎週月曜日20：00更新"
      "http://seaside-c.jp/program/uchidaasakura/")
     ("巽悠衣子の「下も向いて歩こう＼(^o^)／」 : 巽悠衣子 : 隔週金曜日23：30以降（生放送日翌週更新）"
      "http://ch.nicovideo.jp/tatsumiyuiko")
     ("RADIOアニメロミックス 内山夕実と吉田有里のゆゆらじ : 内山夕実、吉田有里 : 毎月第2・第4金曜日22：00更新"
      "http://ch.nicovideo.jp/ch312/video")
     ("BELOVED MEMORIES : 田丸篤志、内田雄馬 : 毎週月曜日20：00更新"
      "http://seaside-c.jp/program/belovedmemories/")
     ("フレッシュたかまつ : 高田憂希、松田颯水 : 毎週月曜日21：30更新"
      "http://seaside-station.net/program/fresh/")
     ("あどりぶ : 巽悠衣子、大橋彩香 : 毎週月曜日20：30更新"
      "http://seaside-c.jp/program/adlib/")
     ("EMERGENCY the RADIO : NEW YOUNG（小野坂昌也）CRAZY YU（小林ゆう）DANCING YUKARI（後藤友香里） : 毎週月曜日21：30更新"
      "http://seaside-c.jp/program/emergency/"))
   collect (list title (concat "seaside://" url) 1 'streamlist)))

;;;###autoload
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
  (cl-labels
      ((collect-name-node (xml-ls &optional ls)
        (cond ((not (consp xml-ls)) ls)
              ((and (eq name (car xml-ls)) (funcall test xml-ls))
               (cons (funcall getter xml-ls) ls))
              (t (dolist (e xml-ls ls)
                   (when (and (car-safe e) (symbolp (car e)))
                     (setq ls (collect-name-node e ls))))))))
    (nreverse (collect-name-node xml-ls))))

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
  "Return nico url from HTML."
  (let ((nico-url
         (car (or (emms-stream-seaside--xml-collect-node
                   'iframe html
                   :test
                   (lambda (node) (let ((src (xml-get-attribute-or-nil node 'src)))
                                (and src
                                     (string-match "\\`http://ext\\.nicovideo\\.jp/thumb/\\([[:alnum:]]+\\)\\'"
                                                   src))))
                   :getter
                   (lambda (node) (concat "http://www.nicovideo.jp/watch/"
                                      (match-string 1 (xml-get-attribute node 'src)))))
                  (emms-stream-seaside--xml-collect-node
                   'a html
                   :test
                   (lambda (node) (let ((href (xml-get-attribute-or-nil node 'href)))
                                (and href
                                     (string-match-p "\\`http://www\\.nicovideo\\.jp/watch/"
                                                     href))))
                   :getter
                   (lambda (node) (xml-get-attribute-or-nil node 'href)))))))
    (if nico-url nico-url (error "Not found nico url"))))

(defvar emms-stream-seaside-nico-stream-regex
  (rx bos "seaside://"
      (or "http://seaside-c.jp/program/uchidaasakura/"
          "http://seaside-c.jp/program/delicatezone/"
          "http://seaside-c.jp/program/remake/"
          "http://it.seaside-c.jp/"
          "http://seaside-c.jp/program/suzakinishi/"
          "http://seaside-c.jp/program/ssc/"
          "http://seaside-c.jp/program/t-radio/"
          "http://seaside-station.net/program/zoo/"
          "http://seaside-station.net/program/futapathy/"
          "http://seaside-station.net/program/ytom/"
          "http://seaside-c.jp/program/belovedmemories/"
          "http://seaside-c.jp/program/adlib/"
          "http://seaside-c.jp/program/emergency/"
          "http://ch.nicovideo.jp/ch312/video"
          "http://ch.nicovideo.jp/grimoire-gakuen"
          "http://ch.nicovideo.jp/tatsumiyuiko"
          "http://seaside-station.net/program/fresh/")))

(defun emms-stream-seaside-nico-stream-url-p (stream-url)
  "Return t, if STREAM-URL needs nico url."
  (string-match emms-stream-seaside-nico-stream-regex stream-url))

(defun emms-stream-seaside--stream-url-to-url (stream-url)
  "Replace seaside:// of STREAM-URL with empty string."
  (replace-regexp-in-string "\\`seaside://" "" stream-url))

;;;###autoload
(defun emms-stream-seaside-stream-url-to-wax (stream-url)
  "Return wax from STREAM-URL."
  (let ((url (emms-stream-seaside--stream-url-to-url stream-url)))
   (emms-stream-seaside--html-to-wax
    (emms-stream-seaside--url-to-html url) url)))

;;;###autoload
(defun emms-stream-seaside-wax-to-wma (wax)
  "Return wma from WAX."
  (let ((buf (url-retrieve-synchronously wax)))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eolp))) (forward-line 1))
          (unless (eobp) (forward-line 1))
          (let ((wma (car
                      (emms-stream-seaside--xml-collect-node
                       'ref (libxml-parse-html-region (point) (point-max))
                       :getter (lambda (node) (xml-get-attribute-or-nil node 'href))))))
            (if wma wma (error "Not found WMA"))))
      (kill-buffer buf))))

;;;###autoload
(defun emms-stream-seaside-stream-url-to-nico-url (stream-url)
  "Return nico url from STREAM-URL."
  (if (string-match "http://www\\.nicovideo\\.jp/watch/[[:alnum:]]+" stream-url)
      (match-string 0 stream-url)
    (emms-stream-seaside--html-to-nico
     (emms-stream-seaside--url-to-html
      (emms-stream-seaside--stream-url-to-url stream-url)))))

(defvar emms-stream-seaside--nico-domain ".nicovideo.jp")

(defvar emms-stream-seaside--nico-cookies-file
  (expand-file-name (make-temp-name "nicovideo_cookies") temporary-file-directory))

(defun emms-stream-seaside--write-nico-cookies ()
  "Write cookies to `emms-stream-seaside--nico-cookies-file'."
  (let* ((domain emms-stream-seaside--nico-domain)
         (cookies (assoc-default domain url-cookie-storage))
         (file emms-stream-seaside--nico-cookies-file))
    (with-temp-buffer
      (dolist (cookie cookies)
        (insert (mapconcat
                 #'identity
                 (list domain "TRUE" (url-cookie-localpart cookie)
                       (if (url-cookie-secure cookie) "TRUE" "FALSE")
                       (number-to-string
                        (floor (float-time (date-to-time (url-cookie-expires cookie)))))
                       (url-cookie-name cookie) (url-cookie-value cookie) "\n")
                 "\t")))
      (write-region (point-min) (point-max) file nil 'nomessage))))

(defvar emms-stream-seaside--nico-url-user-agent
  (format "User-Agent: %s\n"
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36")
  "User-Agent for nicovideo.
This will be ussed for `emms-stream-seaside--nico-url-http-user-agent-string'.")

(defun emms-stream-seaside--nico-url-http-user-agent-string ()
  "`url-http-user-agent-string' for nicovideo."
  emms-stream-seaside--nico-url-user-agent)

(defun emms-stream-seaside--nico-url-retrieve-synchronously
    (url &optional silent inhibit-cookies)
  "`url-retrieve-synchronously' with `emms-stream-seaside--nico-url-http-user-agent-string'."
  ;; Memo: Emacs 24.x doesn't have `url-user-agent'.
  (cl-letf (((symbol-function 'url-http-user-agent-string)
             #'emms-stream-seaside--nico-url-http-user-agent-string))
    (apply #'url-retrieve-synchronously url silent inhibit-cookies)))

(defun emms-stream-seaside--search-thumbKey (buf)
  "Return thumbPlayKey on BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (re-search-forward "'thumbPlayKey':\\s-*'\\([^']*\\)'" nil t)
        (match-string 1)
      (error "Failed to search thumbPlayKey"))))

(defun emms-stream-seaside--fetch-nico-thumbPlayKey (nico-url)
  "Return thumbPlayKey from NICO-URL."
  (let* ((thumb_watch-url (url-expand-file-name
                           (url-file-nondirectory nico-url)
                           "http://ext.nicovideo.jp/thumb_watch/"))
         (url-request-extra-headers `(("Referer" .  ,nico-url)))
         (buf (emms-stream-seaside--nico-url-retrieve-synchronously thumb_watch-url)))
    (prog1 (emms-stream-seaside--search-thumbKey buf)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun emms-stream-seaside--search-nicovideo-url (buf)
  "Return nicovideo url on BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (re-search-forward "thread_id" nil t)
        (car (assoc-default "url"
                            (url-parse-query-string
                             (buffer-substring (line-beginning-position)
                                               (line-end-position)))))
      (error "Failed to search nicovideo url"))))

(defun emms-stream-seaside--fetch-nicovideo-url (nico-url)
  "Return nicovideo url from NICO-URL."
  (let* ((thumb_watch-url
          (format "http://ext.nicovideo.jp/thumb_watch?%s"
                  (url-build-query-string
                   `(("k" ,(emms-stream-seaside--fetch-nico-thumbPlayKey nico-url))
                     ("v" ,(url-file-nondirectory nico-url))))))
         (buf (emms-stream-seaside--nico-url-retrieve-synchronously thumb_watch-url)))
    (prog1 (emms-stream-seaside--search-nicovideo-url buf)
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;###autoload
(defun emms-stream-seaside-stream-url-to-nicovideo-url (stream-url)
  "Return curl format for STREAM-URL.
Update `emms-stream-seaside--nico-cookies-file'."
  (let ((video-url (emms-stream-seaside--fetch-nicovideo-url
                    (emms-stream-seaside-stream-url-to-nico-url stream-url))))
    (emms-stream-seaside--write-nico-cookies)
    video-url))

(provide 'emms-streams-seaside)
;;; emms-streams-seaside.el ends here
