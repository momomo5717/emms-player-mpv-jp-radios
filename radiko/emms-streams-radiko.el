;;; emms-streams-radiko.el --- emms stream list for Radiko -*- lexical-binding: t -*-

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

;; This provides emms stream list for Radiko.

;; (require 'emms-streams-radiko)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'emms-player-mpv-radiko)

(defvar emms-stream-radiko-stream-list-cache nil)
(defun emms-stream-radiko--fetch-stream-list (area-id &optional buf)
  "Retrun AREA-ID radiko stream list.
string -> stream-list
\(emms-radiko-wget-radiko-stream-list \"JP13\"\) => stream-list"
  (let ((buf (or buf (url-retrieve-synchronously
                      (format "http://radiko.jp/v2/station/list/%s.xml" area-id)))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (cl-loop for station
               in (xml-get-children (libxml-parse-xml-region
                                     (point) (point-max))
                                    'station)
               for id = (car (xml-node-children (car (xml-get-children station 'id))))
               for name = (car (xml-node-children (car (xml-get-children station 'name))))
               collect (list name (format "radiko://%s" id) 1 'streamlist)
               finally do (kill-buffer buf)))))

(defun emms-stream-radiko-fetch-current-area-stream-list (&optional updatep)
  "Return stream-list of the current area."
  (if (and (not updatep) (consp emms-stream-radiko-stream-list-cache))
      emms-stream-radiko-stream-list-cache
    (setq emms-stream-radiko-stream-list-cache
          (progn
            (emms-player-mpv-radiko--wget-playerfile)
            (emms-player-mpv-radiko--write-keydata)
            (emms-stream-radiko--fetch-stream-list
             (emms-player-mpv-radiko--get-area-id
              (emms-player-mpv-radiko--access-auth2-fms
               (emms-player-mpv-radiko--access-auth1-fms))))))))

(defun emms-stream-radiko--wget-playerfile-async (&optional cont)
  "Run CONT with no arguments after doing wget playerfile."
  (if (file-exists-p emms-player-mpv-radiko--playerfile)
      (when (functionp cont) (funcall cont))
    (cl-labels
        ((wget-playerfile-async-filter (proc _)
          (let ((ps (process-status proc)))
            (when (eq ps 'exit)
              (unless (file-exists-p emms-player-mpv-radiko--playerfile)
                (error "Failed to get %s" emms-player-mpv-radiko--playerfile))
              (when (functionp cont) (funcall cont))))))
      (set-process-sentinel
       (start-process "radiko-wget-playerfile-async"
                      nil
                      "wget" "-q" "-O"
                      emms-player-mpv-radiko--playerfile
                      emms-player-mpv-radiko--playerurl)
       #'wget-playerfile-async-filter))))

(defun emms-stream-radiko--write-keydata-async (&optional cont)
  "Run CONT after writing keydata."
  (if (file-exists-p  emms-player-mpv-radiko--keyfile)
      (when (functionp cont) (funcall cont))
    (cl-labels
        ((write-keydate-async-filter (proc _)
          (let ((ps (process-status proc)))
            (when (eq ps 'exit)
              (unless (file-exists-p emms-player-mpv-radiko--keyfile)
                (error "Failed to write %s" emms-player-mpv-radiko--keyfile))
              (when (functionp cont) (funcall cont))))))
     (set-process-sentinel
      (start-process "radiko-write-keydata-async"
                     nil
                     "swfextract" "-b" "14"
                     emms-player-mpv-radiko--playerfile
                     "-o"
                     emms-player-mpv-radiko--keyfile)
      #'write-keydate-async-filter))))

(defun emms-stream-radiko--access-auth1-fms-async (&optional cont)
  "Send auth1 to CONT."
  (let ((url-request-method "POST")
        (url-request-data "\\r\\n")
        (url-request-extra-headers
          emms-player-mpv-radiko--auth-fms-base-headers)
        (auth1 ""))
    (url-retrieve
     "https://radiko.jp/v2/api/auth1_fms"
     (lambda (status &rest _)
       (when (memq :error status)
         (error "Failed to get auth1_fms : %s" (cdr status)))
       (setq auth1
             (buffer-substring-no-properties
              (point-min) (point-max)))
       (kill-buffer)
       (when (functionp cont) (funcall cont auth1))))))

(defun emms-stream-radiko--access-auth2-fms-async (auth1 cont)
  "Send auth2 of AUTH1 to CONT."
  (let ((url-request-method "POST")
        (url-request-data "\\r\\n")
        (url-request-extra-headers
         `(("X-Radiko-Authtoken" .
            ,(emms-player-mpv-radiko--get-authtoken auth1))
           ("X-Radiko-Partialkey" .
            ,(emms-player-mpv-radiko--get-partialkey
              emms-player-mpv-radiko--keyfile auth1))
           ,@emms-player-mpv-radiko--auth-fms-base-headers))
        (auth2 ""))
    (url-retrieve
     "https://radiko.jp/v2/api/auth2_fms"
     (lambda (status &rest _)
       (when (memq :error status)
         (error "Failed to get auth1_fms : %s" (cdr status)))
       (setq auth2 (buffer-substring-no-properties
                    (point-min) (point-max)))
                     (kill-buffer)
              (funcall cont auth2)))))

(defun emms-stream-radiko--fetch-stream-list-async-1 (area-id)
  "Helper function for `emms-stream-radiko--fetch-stream-list-async'."
  (url-retrieve
   (format "http://radiko.jp/v2/station/list/%s.xml" area-id)
   (lambda (status &rest _)
     (when (memq :error status)
       (error "Failed to get radiko station list : %s" (cdr status)))
     (setq emms-stream-radiko-stream-list-cache
           (emms-stream-radiko--fetch-stream-list nil (current-buffer)))
     (unless emms-stream-radiko-stream-list-cache
       (error "Failed to get radiko stream list"))
     (message "Updated radiko stream list cache"))))

(defun emms-stream-radiko--fetch-stream-list-async ()
  "Update cache asynchronously."
  (cl-labels
      ((fetch-stream-list-async ()
        (emms-stream-radiko--access-auth1-fms-async
         (lambda (auth1)
           (emms-stream-radiko--access-auth2-fms-async
            auth1 (lambda (auth2)
                    (emms-stream-radiko--fetch-stream-list-async-1
                     (emms-player-mpv-radiko--get-area-id auth2))))))))
    (emms-stream-radiko--wget-playerfile-async
     (lambda ()
       (emms-stream-radiko--write-keydata-async
        #'fetch-stream-list-async)))))

(defun emms-stream-radiko-get-stream-list ()
  "Return new stream-list from cache."
  (cl-copy-list emms-stream-radiko-stream-list-cache))

;;;###autoload
(defun emms-stream-radiko-add-bookmark (&optional updatep)
  "Create radiko bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (if (eq updatep -1) (emms-stream-radiko--fetch-stream-list-async)
    (let ((buf (get-buffer emms-stream-buffer-name)))
      (unless (buffer-live-p buf)
        (error "%s is not a live buffer" emms-stream-buffer-name))
      (set-buffer buf))
    (let* ((stream-list (emms-stream-radiko-fetch-current-area-stream-list updatep))
           (line       (emms-line-number-at-pos (point)))
           (index      (+ (/ line 2) 1)))
      (dolist (stream stream-list)
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index))
      (emms-stream-redisplay)
      (goto-char (point-min))
      (forward-line (1- line)))))

(provide 'emms-streams-radiko)
;;; emms-streams-radiko.el ends here
