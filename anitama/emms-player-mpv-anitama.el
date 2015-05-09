;;; emms-player-mpv-anitama.el --- An emms simple mpv player for アニたまどっとコム -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-anitama.

;; (require 'emms-player-mpv-anitama)
;; (add-to-list 'emms-player-list 'emms-player-mpv-anitama)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'xml)
(require 'url)
(require 'url-cookie)
(require 'later-do)

(defvar emms-player-mpv-anitama--cookie-file
  (expand-file-name "weeeef_cookies" temporary-file-directory))

(define-emms-simple-player-mpv mpv-anitama '(streamlist)
  "\\`anitama://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-anitama "\\`anitama://" t
 'emms-player-mpv-anitama--track-name-to-nodeId)

(emms-player-set 'emms-player-mpv-anitama 'get-media-title
                 'emms-player-mpv-anitama--get-media-title)

;; mpv cannot seek while playing アニたまどっとコム
;; because `emms-player-mpv-anitama' plays back a stream via wget.
(emms-player-set emms-player-mpv-anitama 'seek nil)
(emms-player-set emms-player-mpv-anitama 'seek-to nil)

(emms-player-set 'emms-player-mpv-anitama 'start
                 'emms-player-mpv-anitama-start-wget-pipe-mpv)

(defun emms-player-mpv-anitama--access-weeeef ()
  "Access www.weeeef.com to get cookes."
  (let ((buf (url-retrieve-synchronously
              "http://www.weeeef.com/weeeefww1/Transition?command=top&group=G0000049")))
    (kill-buffer buf)))

(defun emms-player-mpv-anitama--write-cookies ()
  "Write cookies to `emms-player-mpv-anitama--cookie-file'."
  (let* ((weeeef-cookie (cl-find "www.weeeef.com" url-cookie-storage
                                 :key #'car :test #'equal))
         (domain (car weeeef-cookie))
         (file emms-player-mpv-anitama--cookie-file))
    (unless weeeef-cookie (error "Not found cookies of www.weeeef.com"))
    (with-temp-buffer
      (dolist (cookie (cdr weeeef-cookie))
        (insert
         (mapconcat
          #'identity
          (list domain "FALSE" (url-cookie-localpart cookie) "FALSE" "0"
                (url-cookie-name  cookie) (url-cookie-value  cookie))
          "	") ;; mapconcat with TAB
         "\n"))
      (write-region (point-min) (point-max) file nil 'nomessage))))

(defun emms-player-mpv-anitama--have-cookies-p ()
  "Return non-nil, if `url-cookie-storage' has cookies."
  (> (length (cdr (cl-find "www.weeeef.com" url-cookie-storage
                           :key #'car :test #'equal)))
     1))

(defun emms-player-mpv-anitama--write-unless-cookies (&optional forcep)
  "Access and write if `url-cookie-storage' doesn't have cookies.
Access and write if `emms-player-mpv-anitama--cookie-file' doesn't exist.
If FORCEP is non-nil, force to access and write."
  (unless (and (emms-player-mpv-anitama--have-cookies-p)
               (file-executable-p emms-player-mpv-anitama--cookie-file)
               (not forcep))
    (emms-player-mpv-anitama--access-weeeef)
    (unless (emms-player-mpv-anitama--have-cookies-p)
      (error "Failed to get cookies of www.weeeef.com"))
    (emms-player-mpv-anitama--write-cookies)))

(defun emms-player-mpv-anitama--fetch-BookXmlGet-nodeId (id)
  "Return nodeId from ID."
  (emms-player-mpv-anitama--write-unless-cookies)
  (let*
      ((BookXmlGet-xml
        (with-temp-buffer
          (unless
              (zerop
               (call-process
                "wget"  nil t nil "-q" "-O" "-"
                (format "--load-cookies=%s" emms-player-mpv-anitama--cookie-file)
                (format "http://www.weeeef.com/weeeefww1/BookXmlGet?BookId=%s" id)))
            (error "Failed to fetch http://www.weeeef.com/weeeefww1/BookXmlGet"))
          (libxml-parse-xml-region (point-min) (point-max))))
       (Node (cl-loop
              with Node = (car (xml-get-children BookXmlGet-xml 'Node))
              with nextNode = (car (xml-get-children Node 'Node))
              while nextNode do
              (setq Node nextNode)
              (setq nextNode (car (xml-get-children Node 'Node)))
              finally return Node))
       (nodeId (car (xml-node-children (car (xml-get-children Node 'Id))))))
    nodeId))

(defun emms-player-mpv-anitama--loading-message ()
  "Loading message."
  (message "Loading アニたまどっとコム ... "))

(defun emms-player-mpv-anitama--track-name-to-nodeId (track-name)
  "Return nodeId from TRACK-NAME."
  (let* ((id (replace-regexp-in-string "\\`anitama://" "" track-name))
         (nodeId (emms-player-mpv-anitama--fetch-BookXmlGet-nodeId id)))
    (unless nodeId (error "Failed to fetch nodeId"))
    (later-do 'emms-player-mpv-anitama--loading-message)
    nodeId))

(defun emms-player-mpv-anitama--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name (emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(defun emms-player-mpv-anitama--shell-command-format
    (nodeId input-socket media-title params)
  "Start process to play NODEID with INPUT-SOCKET.
MEDIA-TITLE, PARAMS are mpv parameters."
  (emms-player-mpv-anitama--write-unless-cookies)
  (concat
   "wget "
   (mapconcat
    #'shell-quote-argument
    (list "-O" "-" "-q"
          (format "--load-cookie=%s" emms-player-mpv-anitama--cookie-file)
          (format "--post-data=nodeId=%s&type=S" nodeId)
          "http://www.weeeef.com/weeeefww1/OriginalGet")
    " ")
   " | mpv "
   (mapconcat
    #'shell-quote-argument
    `(,input-socket ,media-title ,@params "-")
    " ")))

(defun emms-player-mpv-anitama-start-wget-pipe-mpv (track)
  "Start mpv from TRACK via wget."
  (emms-player-simple-mpv--tq-close)
  (let* ((player emms-player-mpv-anitama)
         (params emms-player-mpv-anitama-parameters)
         (input-socket
          (format "--input-unix-socket=%s" (emms-player-simple-mpv--socket)))
         (nodeId
          (emms-player-simple-mpv--track-to-input-form
           track (emms-player-get player 'mpv-track-name-converters)))
         (get-media-title (emms-player-get player 'get-media-title))
         (media-title
          (if get-media-title
              (format "--media-title=%s"
                      (funcall get-media-title track))
            ""))
         (process
          (start-process-shell-command
           emms-player-simple-process-name
           nil
           (emms-player-mpv-anitama--shell-command-format
            nodeId input-socket media-title params))))
    (set-process-sentinel process 'emms-player-simple-sentinel)
    (emms-player-started player)
    (setq emms-player-paused-p t)
    (run-hooks 'emms-player-paused-hook)
    (while (and (eq (process-status process) 'run)
                (not (file-exists-p emms-player-simple-mpv--socket)))
      (sit-for 0.05))
    (condition-case err
        (setq emms-player-simple-mpv--tq (emms-player-simple-mpv--tq-create))
      (error (message "%s" (error-message-string err))
             (when emms-player-simple-mpv-use-start-tq-error-message-p
               (later-do 'emms-player-simple-mpv--start-tq-error-message
                         params nodeId))))
    (when (tq-process emms-player-simple-mpv--tq)
      (set-process-filter (tq-process emms-player-simple-mpv--tq)
                          'emms-player-simple-mpv--socket-filter)
      (when emms-player-simple-mpv-use-volume-change-function-p
        (emms-player-simple-mpv--set-volume-change-function)))))

(provide 'emms-player-mpv-anitama)
;;; emms-player-mpv-anitama.el ends here
