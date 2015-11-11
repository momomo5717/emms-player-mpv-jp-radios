;;; emms-streams-anitama.el --- emms stream list for アニたまどっとコム -*- lexical-binding: t -*-

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

;; This provides emms stream list for アニたまどっとコム インターネットラジオ.

;; (require 'emms-streams-anitama)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'emms-player-mpv-anitama)

(defvar emms-stream-anitama-stream-list-cache nil)

(defun emms-stream-anitama--Book-to-KikanTo-date (Book)
  "Return date from BOOK."
  (let* ((node Book)
         (date (dolist (name '(NodeData Node KikanFrom)
                             (car (xml-node-children node)))
                 (setq node (car (xml-get-children node name))))))
    (if (and (stringp date) (eq (length date) 8))
        (concat (cl-loop for c across date for i from 0
                         collect c
                         when (or (eq i 3) (eq i 5)) collect ?/))
      date)))

(defun emms-stream-anitaam--bookservlet-xml-to-stream-list (bookservlet-xml)
  "Retrun stream list from BOOKSERVLET-XML."
  (let ((Books (xml-get-children bookservlet-xml 'Book)))
    (cl-loop for Book in Books
             for id    = (xml-get-attribute Book 'id)
             for label = (xml-get-attribute Book 'label)
             for date  = (emms-stream-anitama--Book-to-KikanTo-date Book)
             collect (list (format "%s : %s" label date)
                           (format "anitama://%s" id) 1 'streamlist))))

(defun emms-stream-anitama-fetch-stream-list (&optional updatep)
  "Retrun anitama stream list.
If UPDATEP is no-nil, cache is updated."
  (if (and (not updatep) (consp emms-stream-anitama-stream-list-cache))
      emms-stream-anitama-stream-list-cache
    (emms-player-mpv-anitama--write-unless-cookies t)
    (setq emms-stream-anitama-stream-list-cache
     (with-temp-buffer
       (unless (zerop (call-process
                       "wget"  nil t nil "-q" "-O" "-"
                       (format "--load-cookies=%s"
                               emms-player-mpv-anitama--cookie-file)
                       "http://www.weeeef.com/weeeefww1/BookServlet"))
         (error "Failed to fetch http://www.weeeef.com/weeeefww1/BookServlet"))
       (emms-stream-anitaam--bookservlet-xml-to-stream-list
        (libxml-parse-xml-region (point-min) (point-max)))))))

(defun emms-stream-anitama--write-weeeef-async (&optional cont)
  "Access and write www.weeeef.com cookies asynchronously.
If CONT is no-nil, it is run with no arguments."
  (url-retrieve
   "http://www.weeeef.com/weeeefww1/Transition?command=top&group=G0000049"
   (lambda (status &rest _)
     (when (memq :error status)
       (error "Failed to write www.weeeef.com cookies : %s" (cdr status)))
     (kill-buffer)
     (emms-player-mpv-anitama--write-cookies)
     (unless (emms-player-mpv-anitama--have-cookies-p)
       (error "Failed to get cookies of www.weeeef.com"))
     (when (functionp cont) (funcall cont)))))

(defun emms-stream-anitama-update-cache-async ()
  "Update cache asynchronously."
  (cl-labels
      ((stream-list-async-filter (proc _)
        (let ((buf (process-buffer proc))
              (ps (process-status proc)))
          (when (eq ps 'signal)
            (error "Failed to fetch animate stream list"))
          (when (eq ps 'exit)
            (with-current-buffer buf
              (goto-char (point-max))
              (search-backward ">" (point-min) t)
              (goto-char (match-end 0))
              (setq emms-stream-anitama-stream-list-cache
                    (emms-stream-anitaam--bookservlet-xml-to-stream-list
                     (libxml-parse-xml-region (point-min) (point))))
              (unless emms-stream-anitama-stream-list-cache
                (error "Failed to read anitama xml"))
              (kill-buffer)
              (message "Updated anitama stream list cache"))))))
    (emms-stream-anitama--write-weeeef-async
     (lambda () (set-process-sentinel
             (start-process "animate-fetch-stream-list-async"
                            (make-temp-name "*animate-fetch-stream-list-async*")
                            "wget" "-q" "-O" "-"
                            (format "--load-cookies=%s"
                                    emms-player-mpv-anitama--cookie-file)
                            "http://www.weeeef.com/weeeefww1/BookServlet")
             #'stream-list-async-filter)))))

(defun emms-stream-anitama-get-stream-list ()
  "Return new stream-list from cache."
  (cl-copy-list emms-stream-anitama-stream-list-cache))

;;;###autoload
(defun emms-stream-anitama-add-bookmark (&optional updatep)
  "Create anitama bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (if (eq updatep -1)
      (emms-stream-anitama-update-cache-async)
   (let ((buf (get-buffer emms-stream-buffer-name)))
     (unless (buffer-live-p buf)
       (error "%s is not a live buffer" emms-stream-buffer-name))
     (set-buffer buf))
   (let* ((stream-list (emms-stream-anitama-fetch-stream-list updatep))
          (line       (emms-line-number-at-pos (point)))
          (index      (+ (/ line 2) 1)))
     (dolist (stream stream-list)
       (setq emms-stream-list (emms-stream-insert-at index stream
                                                     emms-stream-list))
       (cl-incf index))
     (emms-stream-redisplay)
     (goto-char (point-min))
     (forward-line (1- line)))))

(provide 'emms-streams-anitama)
;;; emms-streams-anitama.el ends here
