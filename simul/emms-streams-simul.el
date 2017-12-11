;;; emms-streams-simul.el --- emms stream list for SimulRadio -*- lexical-binding: t -*-

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

;; This provides emms stream list for SimulRadio.

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)

;; Suppress warning messages.
(defvar url-http-end-of-headers)
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-simul--url-top "http://www.simulradio.info/")

(defvar emms-stream-simul-hokkaido nil
  "Stream list of hokkaido area.")

(defvar emms-stream-simul-touhoku nil
  "Stream list of touhoku area.")

(defvar emms-stream-simul-shinetsu nil
  "Stream list of shinetsu area.")

(defvar emms-stream-simul-kantou nil
  "Stream list of kantou area.")

(defvar emms-stream-simul-toukai nil
  "Stream list of toukai area.")

(defvar emms-stream-simul-hokuriku nil
  "Stream list of hokuriku area.")

(defvar emms-stream-simul-kinki nil
  "Stream list of kinki area.")

(defvar emms-stream-simul-chugoku nil
  "Stream list of chugoku area.")

(defvar emms-stream-simul-shikoku nil
  "Stream list of shikoku area.")

(defvar emms-stream-simul-kyusyu nil
  "Stream list of kyusyu area.")

(defvar emms-stream-simul-okinawa nil
  "Stream list of okinawa area.")

(defvar emms-stream-simul-streams-name
  '(emms-stream-simul-hokkaido
    emms-stream-simul-touhoku
    emms-stream-simul-shinetsu
    emms-stream-simul-kantou
    emms-stream-simul-toukai
    emms-stream-simul-hokuriku
    emms-stream-simul-kinki
    emms-stream-simul-chugoku
    emms-stream-simul-shikoku
    emms-stream-simul-kyusyu
    emms-stream-simul-okinawa)
  "Symbol list of SimulRadio streams name.")

(defvar emms-stream-simul--areas
  '("北海道" "東北" "信越" "関東" "東海" "北陸" "近畿" "中国" "四国" "九州" "沖縄"))

(cl-defun emms-stream-simul--xml-collect-node
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

(defun emms-stream-simul--parse-html-buf (buf)
  "Return html list from BUF."
  (with-current-buffer buf
    (goto-char url-http-end-of-headers)
    (libxml-parse-html-region (point) (point-max))))

(defun emms-stream-simul--collect-radiosbox (html)
  "Collect radiosbox from HTML."
  (emms-stream-simul--xml-collect-node
   'div html
   :test (lambda (xml) (equal (xml-get-attribute-or-nil xml 'class) "radiosbox"))))

(defun emms-stream-simul--collect-radiobox (html)
  "Collect radiosbox from HTML."
  (emms-stream-simul--xml-collect-node
   'div html
   :test (lambda (xml) (equal (xml-get-attribute-or-nil xml 'class) "radiobox"))))

(defun emms-stream-simul--radiobox-to-streamlist (radiobox &optional area)
  "Return a streamlist from RADIOBOX and AREA."
  (cl-labels
      ((get-url (a-ls)
         (cl-loop for a in a-ls
                  for onclick = (xml-get-attribute-or-nil a 'onclick)
                  when (and onclick (string-match "('\\(.*\\)')" onclick))
                  return (match-string 1 onclick)))
       (get-name (strong)
         (let ((a (car (xml-get-children strong 'a))))
           (car (xml-node-children (if a a strong)))))
       (get-city (p-ls)
         (let* ((p (cl-find-if (lambda (p) (xml-get-children p 'strong)) p-ls))
                (str (car (last p))))
           (when (stringp str) (car (split-string str))))))
    (let* ((strong (car (emms-stream-simul--xml-collect-node 'strong radiobox)))
           (p-ls (emms-stream-simul--xml-collect-node 'p radiobox))
           (a-ls (emms-stream-simul--xml-collect-node 'a radiobox)))
      (list (format "%s : %s%s" (get-name strong) (get-city p-ls)
                    (if area (format " : %s" area) ""))
            (format "simul://%s" (get-url a-ls))
            1 'streamlist))))

(defun emms-stream-simul--set-streams-name (html)
  "Set `emms-stream-simul-streams-name' from HTML."
  (cl-loop
   for radiosbox in (emms-stream-simul--collect-radiosbox html)
   for area in emms-stream-simul--areas
   for streams-name in emms-stream-simul-streams-name
   when radiosbox do
   (set streams-name
        (cl-loop
         for radiobox in (emms-stream-simul--collect-radiobox radiosbox)
         for streamlist = (emms-stream-simul--radiobox-to-streamlist radiobox area)
         for stream-url = (cl-second streamlist)
         unless (and (stringp stream-url)
                     (or (string-match "simul://nil" stream-url)
                         (string-match "http://listenradio\\.jp/Home/ProgramSchedule"
                                       stream-url)))
         collect streamlist))))

(defun emms-stream-simul--maybe-update-cache (&optional updatep)
  "Update `emms-stream-simul-streams-name' synchronously if one of those is nil.
If UPDATEP is non-nil, cache is updated."
  (when (or updatep
            (cl-loop for name in emms-stream-simul-streams-name
                     thereis (null (symbol-value name))))
    (let ((buf (url-retrieve-synchronously emms-stream-simul--url-top)))
      (emms-stream-simul--set-streams-name
       (emms-stream-simul--parse-html-buf buf))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;###autoload
(defun emms-stream-simul-update-cache-async ()
  "Update cache asynchronously."
  (url-queue-retrieve
   emms-stream-simul--url-top
   (lambda (status &rest _)
     (if (plist-get status :error)
         (message "Failed to update simul stream lists : %s" (cdr status))
       (emms-stream-simul--set-streams-name
        (emms-stream-simul--parse-html-buf (current-buffer)))
       (message "Updated simul stream lists cache")))))

(defun emms-stream-simul--add-bookmark-1 (simul-streams-name &optional updatep)
  "Helper functions for `emms-stream-simul-add-bookmark'.
SIMUL-STREAMS-NAME is a list of symbols the values of which will be inserted.
If UPDATEP is non-nil, cache will be updated."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (emms-stream-simul--maybe-update-cache updatep)
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (streams-name simul-streams-name)
      (dolist (stream (symbol-value streams-name))
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-simul-get-stream-list ()
  "Return new stream-list."
  (cl-loop
   with ls = nil
   for streams-name in emms-stream-simul-streams-name do
   (dolist (stream (symbol-value streams-name))
     (push stream ls))
   finally return (nreverse ls)))

(defvar emms-stream-simul--add-bookmark-msg
  (concat "[0] All  [1] 北海道(Hokkaido)  [2] 東北(Touhoku)  [3] 信州(Shinetsu)\n"
          "         [4] 関東(Kantou)      [5] 東海(Toukai)   [6] 北陸(Hokuriku)\n"
          "         [7] 近畿(kinki)       [8] 中国(Chugoku)  [9] 四国(Shikoku)\n"
          "        [10] 九州(kyusyu)     [11] 沖縄(Okinawa)\n"
          "        [-1] Update stream lists cache asynchronously\n\n"
          "Input a number of 0-11 or -1: "))

;;;###autoload
(defun emms-stream-simul-add-bookmark (&optional updatep location)
  "Create simul bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
LOCATION is a number of 0-11.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-simul-update-cache-async)
    (unless (integerp location)
      (let ((msg emms-stream-simul--add-bookmark-msg))
        (while (not (and (integerp (setq location (read-number msg)))
                         (<= -1 location) (<= location 11))))))
    (cond
     ((eq updatep -1) (emms-stream-simul-update-cache-async))
     ((zerop location)
      (emms-stream-simul--add-bookmark-1 emms-stream-simul-streams-name updatep))
     (t (emms-stream-simul--add-bookmark-1
         (list (nth (1- location) emms-stream-simul-streams-name)) updatep)))))

;; For media player

(defun emms-stream-simul--asx-to-href (asx)
  "Return href from ASX."
  (let* ((buf (url-retrieve-synchronously asx))
         (html (emms-stream-simul--parse-html-buf buf)))
    (car (last (emms-stream-simul--xml-collect-node
                'ref html
                :test
                (lambda (node) (xml-get-attribute-or-nil node 'href))
                :getter
                (lambda (node) (xml-get-attribute node 'href)))))))

;;;###autoload
(defun emms-stream-simul-stream-url-to-url (stream-url)
  "Replace simul:\\ of STREAM-URL with empty string."
  (replace-regexp-in-string "\\`s\\(imul\\|aimaru\\)://" "" stream-url))

;;;###autoload
(defun emms-stream-simul-stream-url-to-asx-ref (stream-url)
  "Return ref of asx from STREAM-URL."
  (emms-stream-simul--asx-to-href
   (emms-stream-simul-stream-url-to-url stream-url)))

(defvar emms-stream-simul--specific-url-alist
  `(("http://live.776.fm/radiotxt.html" .
     "http://153.126.158.50/live1/radiotxt.m3u8")
    ("http://www.fmpalulun.co.jp/movie" .
     "rtmp://round4x.lve.jp:80/live/_definst_/ZStYp14JYyxU")
    ("http://www.web-services.jp/harbor779/" .
     "http://211.11.77.51:8000/")
    ("http://www.media-gather.jp/_mg_standard/deliverer2.php?p=IaxEXCgTuKI%3D" .
     ,(format "%s %s %s"
              "rtmp://host02.media-gather.jp/_media__mg00009kj5c_live/_definst_/1359071597_3874"
              "live=1"
              "swfUrl=http://www.media-gather.jp/_mg_standard/players/flash/11/flvplayer.f10_normal.swf"))))

;;;###autoload
(defun emms-stream-simul-stream-url-to-specific-form (stream-url)
  "Return a specific form to play STREAM-URL."
  (let* ((url (emms-stream-simul-stream-url-to-url stream-url))
         (form (assoc-default url emms-stream-simul--specific-url-alist)))
    (if form form url)))

(provide 'emms-streams-simul)
;;; emms-streams-simul.el ends here
