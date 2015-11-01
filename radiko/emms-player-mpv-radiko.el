;;; emms-player-mpv-radiko.el --- An emms simple mpv player for radiko -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-radiko.

;; (require 'emms-player-mpv-radio)
;; (add-to-list 'emms-player-list 'emms-player-mpv-radiko)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'xml)
(require 'later-do)

(define-emms-simple-player-mpv mpv-radiko '(streamlist)
  "\\`radiko://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-radiko "\\`radiko://" t
 'emms-player-mpv-radiko--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-radiko 'get-media-title
                 'emms-player-mpv-radiko--get-media-title)

;; mpv cannot seek while playing Radiko.
(emms-player-set emms-player-mpv-radiko 'seek nil)
(emms-player-set emms-player-mpv-radiko 'seek-to nil)

(defvar emms-player-mpv-radiko--playerurl
  "http://radiko.jp/player/swf/player_4.1.0.00.swf")

(defvar emms-player-mpv-radiko--playerfile
  (expand-file-name "radiko_player.swf" temporary-file-directory))

(defvar emms-player-mpv-radiko--keyfile
  (expand-file-name "radiko_authkey.png" temporary-file-directory))

(defun emms-player-mpv-radiko--wget-playerfile ()
  "Wget player.swf to `emms-player-mpv-radiko--playerfile'."
  (unless (file-exists-p emms-player-mpv-radiko--playerfile)
    (unless (zerop (call-process-shell-command
                    (format "wget -q -O %s %s"
                            emms-player-mpv-radiko--playerfile
                            emms-player-mpv-radiko--playerurl)))
      (error "Failed to wget the player.swf"))))

(defun emms-player-mpv-radiko--write-keydata ()
  "Write keydata from emms-radiko-keyfile."
  (unless (file-exists-p emms-player-mpv-radiko--keyfile)
    (unless (zerop (call-process-shell-command
                    (format "swfextract -b 14 %s -o %s"
                            emms-player-mpv-radiko--playerfile
                            emms-player-mpv-radiko--keyfile)))
      (error "Failed to write the keydata"))))

(defvar emms-player-mpv-radiko--auth-fms-base-headers
  '(("pragma" . "no-cache")
    ("X-Radiko-App" . "pc_1")
    ("X-Radiko-App-Version" . "2.0.1")
    ("X-Radiko-User" . "test-stream")
    ("X-Radiko-Device" . "pc"))
  "Base headers for auth fms.")

(defun emms-player-mpv-radiko--access-auth1-fms ()
  "Return auth1_fms."
  (let* ((url-request-method "POST")
         (url-request-data "\\r\\n")
         (url-request-extra-headers
          emms-player-mpv-radiko--auth-fms-base-headers)
         (buf (url-retrieve-synchronously
               "https://radiko.jp/v2/api/auth1_fms" t)))
    (with-current-buffer buf
      (prog1 (buffer-substring-no-properties
              (point-min) (point-max))
        (kill-buffer buf)))))

(defun emms-player-mpv-radiko--get-auth1-value (key auth1)
  "Return value of KEY from AUTH1."
  (if (string-match (format "%s: " key) auth1)
      (let ((pos (match-end 0)))
        (if (string-match "\n\\|\r\n" auth1 pos)
            (substring auth1 pos (match-beginning 0))
          (error "Failed to get %s value" key)))
    (error "Failed to get %s value" key)))

(defun emms-player-mpv-radiko--get-authtoken (auth1)
  "Return X-Radiko-Authtoken value from  AUTH1."
  (emms-player-mpv-radiko--get-auth1-value "X-Radiko-AuthToken" auth1))

(defun emms-player-mpv-radiko--get-offset (auth1)
  "Return x-radiko-keyoffset value from  AUTH1."
  (emms-player-mpv-radiko--get-auth1-value "X-Radiko-KeyOffset" auth1))

(defun emms-player-mpv-radiko--get-length (auth1)
  "Return x-radiko-keylength value from  AUTH1."
  (emms-player-mpv-radiko--get-auth1-value "X-Radiko-KeyLength" auth1))

(defun emms-player-mpv-radiko--get-partialkey (keyfile auth1)
  "Return partialkey from KEYFILE, AUTH1."
  (car (split-string
        (shell-command-to-string
         (format "dd if=%s bs=1 skip=%s count=%s 2> /dev/null | base64"
                 keyfile
                 (emms-player-mpv-radiko--get-offset auth1)
                 (emms-player-mpv-radiko--get-length auth1)))
        "\n")))

(defun emms-player-mpv-radiko--access-auth2-fms (auth1)
  "Return auth2_fms from AUTH1."
  (let* ((url-request-method "POST")
         (url-request-data "\\r\\n")
         (url-request-extra-headers
          `(("X-Radiko-Authtoken" .
             ,(emms-player-mpv-radiko--get-authtoken auth1))
            ("X-Radiko-Partialkey" .
             ,(emms-player-mpv-radiko--get-partialkey
               emms-player-mpv-radiko--keyfile auth1))
            ,@emms-player-mpv-radiko--auth-fms-base-headers))
         (buf (url-retrieve-synchronously
               "https://radiko.jp/v2/api/auth2_fms" t)))
    (with-current-buffer buf
      (prog1 (buffer-substring-no-properties
              (point-min) (point-max))
        (kill-buffer buf)))))

(defun emms-player-mpv-radiko--get-area-id (auth2)
  "Retrun area-id from AUTH2."
  (if (string-match "^JP[0-9]+[,]" auth2)
      (substring auth2 (match-beginning 0) (1- (match-end 0)))
    (error "Failed to get area-id")))

(defun emms-player-mpv-radiko--wget-stream-url (channel)
  "Return stream-url from CHANNEL.
\"channel\"
=> \"rtmpe://f-radiko.smartstream.ne.jp/channel/_definst_/simul-stream.stream\""
  (let ((buf (url-retrieve-synchronously
              (format "http://radiko.jp/v2/station/stream/%s.xml" channel) t))
        (stream-url nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let* ((stream-xml (libxml-parse-xml-region (point) (point-max)))
             (item (car (xml-get-children stream-xml 'item))))
        (setq stream-url (car (xml-node-children item))))
      (kill-buffer buf))
    (if (stringp stream-url) stream-url
      (error "Failed to parce stream/%s.xml" channel))))

(defun emms-player-mpv-radiko--loading-message ()
  "Loading message."
  (message "Loading Radiko ... It takes a few seconds."))

(defun emms-player-mpv-radiko--track-name-to-input-form (track-name)
  "Retrun \"rtmpe://...\" ffplay format from TRACK-NAME."
  (emms-player-mpv-radiko--wget-playerfile)
  (emms-player-mpv-radiko--write-keydata)
  (let* ((channel (replace-regexp-in-string "\\`radiko://" "" track-name))
         (rtmpe-url (emms-player-mpv-radiko--wget-stream-url channel))
         (auth1 (emms-player-mpv-radiko--access-auth1-fms))
         (authtoken (emms-player-mpv-radiko--get-authtoken auth1)))
    (emms-player-mpv-radiko--access-auth2-fms auth1)
    (later-do 'emms-player-mpv-radiko--loading-message)
    (format "%s swfUrl=%s swfVfy=1 conn=S:  conn=S:  conn=S:  conn=S:%s live=1"
            rtmpe-url emms-player-mpv-radiko--playerurl authtoken)))

(defun emms-player-mpv-radiko--get-media-title (track)
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-radiko)
;;; emms-player-mpv-radiko.el ends here
