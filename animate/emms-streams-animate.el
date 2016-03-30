;;; emms-streams-animate.el --- emms stream list for animate.tv -*- lexical-binding: t -*-

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

;; This provides emms stream list for animate.tv.

;; (require 'emms-streams-animate)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'json)

;; Suppress warning messages
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-animate--url "http://www.animate.tv/radio/")

(defvar emms-stream-animate--stream-alist-cache nil)

(defvar emms-stream-animate--days
  '("mon" "tue" "wed" "thu" "fri" "irr")
  "Days of the weekdays.")

(cl-defun emms-stream-animate--xml-collect-node
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

(defun emms-stream-animate--url-to-html (url &optional xmlp buf)
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (kill-buffer buf)))))

(defun emms-stream-animate--url-to-json (url &optional buf)
  "Return a json object from URL.
If BUF is no-nil, it is used."
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let ((p (point))
            (p-max (point-max)))
        (prog1 (with-temp-buffer
                 (insert-buffer-substring-no-properties buf p p-max)
                 (decode-coding-region (point-min) (point-max) 'utf-8)
                 (goto-char (point-min))
                 (json-read))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(defun emms-stream-animate--box-to-stream (box)
  "BOX to streamlist."
  (cl-labels ((get-span-content
               (content box)
               (car (emms-stream-animate--xml-collect-node
                     'span box
                     :test
                     (lambda (node) (equal (xml-get-attribute node 'class)
                                             content))
                     :getter
                     (lambda (node)
                       (cond
                        ((cl-find-if #'stringp (xml-node-children node)))
                        (t "")))))))
    (let ((title (get-span-content "title" box))
          (main  (get-span-content "main" box))
          (date  (car (split-string (get-span-content "date" box))))
          (href  (car (emms-stream-animate--xml-collect-node
                       'a box :test (lambda (node) (xml-get-attribute-or-nil node 'href))
                       :getter (lambda (node) (xml-get-attribute node 'href))))))
      (when (string< "2015" date)
       (list (format "%s : %s : %s" title (mapconcat #'identity (split-string main) " ") date)
             (concat "animate://" (url-expand-file-name href emms-stream-animate--url))
             1 'streamlist)))))

(defun emms-stream-animate--html-to-stream-list (day html)
  "Retrun stream list of DAY from HTML."
  (let ((boxes
         (car (emms-stream-animate--xml-collect-node
               'div html
               :test
               (lambda (node) (and (equal (xml-get-attribute node 'class)
                                      "list")
                               (equal (xml-get-attribute node 'id)
                                      day)))
               :getter
               (lambda (node) (emms-stream-animate--xml-collect-node
                           'div node
                           :test
                           (lambda (node) (equal (xml-get-attribute node 'class)
                                             "box"))))))))
    (cl-loop for box in boxes
             for stream = (emms-stream-animate--box-to-stream box)
             when stream collect stream)))

(defun emms-stream-animate--fetch-stream-alist (&optional updatep)
  "Fetch stream alist.
If UPDATEP is non-nil, cache is updated."
  (if (or updatep (null emms-stream-animate--stream-alist-cache))
      (let ((html (emms-stream-animate--url-to-html emms-stream-animate--url)))
        (setq emms-stream-animate--stream-alist-cache
              (cl-loop for day in emms-stream-animate--days collect
                       (cons day (emms-stream-animate--html-to-stream-list day html)))))
    emms-stream-animate--stream-alist-cache))

;;;###autoload
(defun emms-stream-animate-update-cache-async ()
  "Update cache asynchronously."
  (url-retrieve
   emms-stream-animate--url
   (lambda (status &rest _)
     (when (memq :error status)
       (error "Failed to update animate stream listt : %s" (cdr status)))
     (let ((html (emms-stream-animate--url-to-html nil nil (current-buffer))))
       (setq emms-stream-animate--stream-alist-cache
             (cl-loop for day in emms-stream-animate--days collect
                      (cons day (emms-stream-animate--html-to-stream-list day html)))))
     (message "Updated animate stream list cache"))))

(defun emms-stream-animate-ger-stream-list-dow (day &optional updatep)
  "Return streamlist of the DAY of the weekdays.
If UPDATEP is non-nil, cache is updated."
  (assoc-default day (emms-stream-animate--fetch-stream-alist updatep)))

(defun emms-stream-animate-add-bookmark-1 (&optional updatep &rest days)
  "Helper function for `emms-stream-animate-add-bookmark', etc."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (emms-stream-animate--fetch-stream-alist updatep)
  (let* ((days (or days emms-stream-animate--days))
         (line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (dow days)
     (dolist (stream (emms-stream-animate-ger-stream-list-dow dow))
       (setq emms-stream-list (emms-stream-insert-at index stream
                                                     emms-stream-list))
       (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-animate-get-stream-list ()
  "Return new streamlist from cache."
  (cl-loop
   with ls = nil
   for day in emms-stream-animate--days do
   (dolist (stream (assoc-default day emms-stream-animate--stream-alist-cache))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-animate-add-bookmark (&optional updatep dow)
  "Create animate bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
DOW is a number of 0-6 or -1.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1) (emms-stream-animate-update-cache-async)
   (unless (integerp dow)
     (let ((msg (concat "[0] All  [1] Mon  [2] Tue  [3] Wed  [4] Thu\n"
                        "         [5] Fri  [6] Irr\n"
                        "[-1] Update stream list cache asynchronously\n\n"
                        "Input a number of 0-6 or -1: ")))
       (while (not (and (integerp (setq dow (read-number msg)))
                        (<= -1 dow) (<= dow 6))))))
   (cond ((= dow -1) (emms-stream-animate-update-cache-async))
         ((zerop dow) (emms-stream-animate-add-bookmark-1 updatep))
         (t (emms-stream-animate-add-bookmark-1
             updatep (nth (1- dow) emms-stream-animate--days))))))

;; For media player
(defvar emms-stream-animate--video_info-url
  "https://www2.uliza.jp/api/get_player_video_info.aspx")

(defvar emms-stream-animate--swf-file
  (expand-file-name "animate_player.swf" temporary-file-directory))

(defvar emms-stream-animate--swf-url nil)

(defvar emms-stream-animate--swf-hash nil)

(defvar emms-stream-animate--swf-size nil)

(defun emms-stream-animate--$wf-file ()
  "Return $wf file path."
  (let ((file emms-stream-animate--swf-file))
   (expand-file-name (concat (file-name-base file) ".$wf")
                     (file-name-directory file))))

(defun emms-stream-animate--fetch-player-link (url)
  "Return flash link at URL."
  (url-expand-file-name
   (car (emms-stream-animate--xml-collect-node
         'a (emms-stream-animate--url-to-html url)
         :test (lambda (node) (let ((link (xml-get-attribute node 'href)))
                            (string-match "#player\\'" link)))
         :getter (lambda (node) (xml-get-attribute node 'href))))
   "http://www.animate.tv"))

(defun emms-stream-animate--get-FlashVars (&optional buf)
  "Return FlashVars.
If BUF is nil, the current buffer will be used."
  (with-current-buffer (or buf (current-buffer))
    (goto-char (point-min))
    (if (re-search-forward "^\\s-+\"FlashVars\",\\s-+\"\\([^\"]+\\)\"\\s-+\+?" nil t)
        (mapcar (lambda (str) (let ((ls (split-string str "=")))
                            (cons (intern (car ls)) (cadr ls))))
                (split-string (match-string-no-properties 1) "&"))
      (error "Failed to get FlashVars"))))

(defun emms-stream-animate--get-swf-url (&optional buf)
  "Return swf url.
If BUF is nil, the current buffer will be used."
  (with-current-buffer (or buf (current-buffer))
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*\"name\"\\s-*,\\s-*\"\\([^\"]+\\)\"\\s-*," nil t)
        (let ((str (match-string-no-properties 1)))
          (concat str (if (string-match "\.swf\\'" str) "" ".swf")))
      emms-stream-animate--swf-url)))

(defun emms-stream-animate--wget-swf ()
  "Wget `emms-stream-animate--swf-file'."
  (unless (zerop (call-process "wget" nil nil nil
                               "-O" emms-stream-animate--swf-file
                               emms-stream-animate--swf-url))
    (error "wget %s" emms-stream-animate--swf-url)))

(defun emms-stream-animate--flasm-x-swf ()
  "Decompress `emms-stream-animate--swf-file'."
  (if (file-exists-p emms-stream-animate--swf-file)
      (unless (zerop (call-process "flasm" nil nil nil
                                   "-x" emms-stream-animate--swf-file))
        (unless (file-exists-p (emms-stream-animate--$wf-file))
          (error "flasm -x %s" emms-stream-animate--swf-file)))
    (error "Not found: %s" emms-stream-animate--swf-file)))

(defun emms-stream-animate--get-swf-hash ()
  "Return hash from `emms-stream-animate--swf-file'."
  (with-temp-buffer
    (if (not (zerop (call-process "openssl" nil t nil
                                  "sha" "-sha256" "-hmac"
                                  "Genuine Adobe Flash Player 001"
                                  emms-stream-animate--swf-file)))
        (error "openssl %s" emms-stream-animate--swf-file)
      (goto-char (point-min))
      (if (re-search-forward "^.*=\\s-*\\(\\w+\\)$" nil t)
          (match-string-no-properties 1)
        (error "Failed to get swf hash")))))

(defun emms-stream-animate--get-swf-size (&optional fname)
  "Return size string of `emms-stream-animate--swf-file'."
  (number-to-string
   (nth 7 (file-attributes (or fname emms-stream-animate--swf-file)))))

(defun emms-stream-animate--get-video_info-url (flashvars)
  "Return get_player_video_info url of FLASHVARS for getting a json object."
  (concat emms-stream-animate--video_info-url
          "?vid=" (url-hexify-string (cdr (assq 'vid flashvars)))
          "&eid=" (url-hexify-string (cdr (assq 'eid flashvars)))
          "&pid=" (url-hexify-string (cdr (assq 'pid flashvars)))
          "&rnd=" (url-hexify-string (number-to-string (random 10000)))))

(defun emms-stream-animate--set-swf-vars (url)
  "Set variables for swf via URL."
  (setq emms-stream-animate--swf-url url)
  (emms-stream-animate--wget-swf)
  (emms-stream-animate--flasm-x-swf)
  (setq emms-stream-animate--swf-hash
        (emms-stream-animate--get-swf-hash)
        emms-stream-animate--swf-size
        (emms-stream-animate--get-swf-size)))

(defun emms-stream-animate--reset-swf-vars-p (swf-url)
  "Return t if swf vars need to reset."
  (let ((backup (emms-stream-animate--$wf-file)))
    (not (and emms-stream-animate--swf-url
              (equal emms-stream-animate--swf-url swf-url)
              (file-exists-p emms-stream-animate--swf-file)
              (file-exists-p backup)
              (> (string-to-number (emms-stream-animate--get-swf-size))
                 (string-to-number (emms-stream-animate--get-swf-size backup)))
              emms-stream-animate--swf-hash
              emms-stream-animate--swf-size))))

(defun emms-stream-animate--json-to-rtmpdump-form (json-obj flashvars swf-url)
  "Return rtmpdump format via JSON-OBJ, FLASHVARS and SWF-URL."
  (cl-labels ((assq-v (key als) (cdr (assq key als))))
    (let* ((net_conn (assq-v 'NET_CONN json-obj))
           (key (assq-v 'KEY json-obj))
           (playlist (car (cl-loop for e across (assq-v 'PLAYLIST json-obj)
                                   for name = (assq-v 'NAME e)
                                   when name collect e)))
           (playpath (assq-v 'NAME playlist))
           (stime (floor (string-to-number (assq-v 'STIME json-obj))))
           (len (floor (string-to-number (assq-v 'LEN playlist))))
           (etime (number-to-string (floor (+ stime len ))))
           (stime (number-to-string stime))
           (did (assq-v 'did flashvars))
           (pid (assq-v 'pid flashvars))
           (eid (assq-v 'eid flashvars))
           (vid (assq-v 'vid flashvars))
           (hash (md5 (concat did pid eid key vid stime etime))))
      (when (emms-stream-animate--reset-swf-vars-p swf-url)
        (emms-stream-animate--set-swf-vars swf-url))
      (concat "rtmpdump -r " net_conn playpath
              " --swfhash " emms-stream-animate--swf-hash
              " --swfsize " emms-stream-animate--swf-size
              " -C O:1"
              " -C NS:stm:" stime
              " -C NS:pid:" pid
              " -C NS:hash:" hash
              " -C NS:etm:" etime
              " -C NS:eid:" eid
              " -C NS:did:" did
              " -C NS:vid:" vid
              " -C O:0"
              " --quiet"))))

;;;###autoload
(defun emms-stream-animate-stream-url-to-rtmpdump-form (stream-url)
  "Return rtmpdump form of STREAM-URL."
  (let* ((stream-url (replace-regexp-in-string "\\`animate://" "" stream-url))
         (player-link (emms-stream-animate--fetch-player-link stream-url))
         (buf (url-retrieve-synchronously player-link))
         json-obj flashvars swf-url)
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (setq flashvars (emms-stream-animate--get-FlashVars)
            swf-url (emms-stream-animate--get-swf-url)))
    (when (buffer-live-p buf) (kill-buffer buf))
    (setq json-obj (emms-stream-animate--url-to-json
                    (emms-stream-animate--get-video_info-url flashvars)))
    (emms-stream-animate--json-to-rtmpdump-form json-obj flashvars swf-url)))


(define-obsolete-function-alias 'emms-stream-animate-stream-url-to-asx-ref
  'emms-stream-animate-stream-url-to-rtmpdump-form
  "20160329: animate.tv has ended support for WMP.")

(provide 'emms-streams-animate)
;;; emms-streams-animate.el ends here
