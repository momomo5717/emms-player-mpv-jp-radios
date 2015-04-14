;;; emms-player-mpv-radiko.el --- a emms simple mpv player for radiko -*- lexical-binding: t -*-

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

(define-emms-simple-player-mpv mpv-radiko '(streamlist)
  "\\`radiko://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-radiko "\\`radiko://" t
 'emms-player-mpv-radiko--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-radiko 'get-media-title
                 'emms-player-mpv-radiko--get-media-title)

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

(defun emms-player-mpv-radiko--access-auth1-fms ()
  "Return auth1_fms."
  (shell-command-to-string
   "wget -q \\
     --header=\"pragma: no-cache\" \\
     --header=\"X-Radiko-App: pc_1\" \\
     --header=\"X-Radiko-App-Version: 2.0.1\" \\
     --header=\"X-Radiko-User: test-stream\" \\
     --header=\"X-Radiko-Device: pc\" \\
     --post-data='\\r\\n' \\
     --no-check-certificate \\
     --save-headers \\
     -O - \\
     https://radiko.jp/v2/api/auth1_fms"))

(defun emms-player-mpv-radiko--get-auth1-value (key auth1)
  "Return value of KEY from AUTH1."
  (if (string-match (format "%s: " key) auth1)
      (let ((pos (match-end 0)))
        (if (string-match "\r\n" auth1 pos)
            (substring auth1 pos (match-beginning 0))))
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
  (shell-command-to-string
   (format
    "wget -q \\
        --header=\"pragma: no-cache\" \\
        --header=\"X-Radiko-App: pc_1\" \\
        --header=\"X-Radiko-App-Version: 2.0.1\" \\
        --header=\"X-Radiko-User: test-stream\" \\
        --header=\"X-Radiko-Device: pc\" \\
        --header=\"X-Radiko-Authtoken: %s\" \\
        --header=\"X-Radiko-Partialkey: %s\" \\
        --post-data='\\r\\n' \\
        --no-check-certificate \\
        -O - \\
        https://radiko.jp/v2/api/auth2_fms"
    (emms-player-mpv-radiko--get-authtoken auth1)
    (emms-player-mpv-radiko--get-partialkey emms-player-mpv-radiko--keyfile auth1))))

(defun emms-player-mpv-radiko--get-area-id (auth2)
  "Retrun area-id from AUTH2."
  (if (string-match "^.+[0-9][,]" auth2)
      (substring auth2 (match-beginning 0) (1- (match-end 0)))
    (error "Failed to get area-id")))

(defun emms-player-mpv-radiko--wget-stream-url (channel)
  "Return stream-url from CHANNEL.
\"channel\"
=> \"rtmpe://f-radiko.smartstream.ne.jp/channel/_definst_/simul-stream.stream\""
  (with-temp-buffer
    (unless (zerop
             (call-process
              "wget"  nil t nil "-q" "-O" "-"
              (format "http://radiko.jp/v2/station/stream/%s.xml" channel)))
      (error "Failed to wget station/stream/%s.xml" channel))
    (let* ((stream-xml (libxml-parse-xml-region (point-min) (point-max)))
           (item (car (xml-get-children stream-xml 'item)))
           (stream-url (car (xml-node-children item))))
      (if (stringp stream-url) stream-url
        (error "Failed to parce stream url")))))

(defun emms-player-mpv-radiko--track-name-to-input-form (track-name)
  "Retrun \"rtmpe://...\" ffplay format from TRACK-NAME."
  (emms-player-mpv-radiko--wget-playerfile)
  (emms-player-mpv-radiko--write-keydata)
  (let* ((channel (replace-regexp-in-string "\\`radiko://" "" track-name))
         (rtmpe-url (emms-player-mpv-radiko--wget-stream-url channel))
         (auth1 (emms-player-mpv-radiko--access-auth1-fms))
         (authtoken (emms-player-mpv-radiko--get-authtoken auth1)))
    (emms-player-mpv-radiko--access-auth2-fms auth1)
    (format "%s swfUrl=%s swfVfy=1 conn=S:  conn=S:  conn=S:  conn=S:%s live=1"
            rtmpe-url emms-player-mpv-radiko--playerurl authtoken)))

(defun emms-player-mpv-radiko--get-media-title (track)
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track-name))))

(provide 'emms-player-mpv-radiko)
;;; emms-player-mpv-radiko.el ends here
