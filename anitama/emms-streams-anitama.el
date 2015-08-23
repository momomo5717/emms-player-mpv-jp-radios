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

(defvar emms-stream-anitama-streamlist-cache nil)

(defun emms-stream-anitaam--bookservlet-xml-to-streamlist (bookservlet-xml)
  "Retrun stream list from BOOKSERVLET-XML."
  (let ((Books (xml-get-children bookservlet-xml 'Book)))
    (cl-loop for Book in Books
             for id    = (xml-get-attribute Book 'id)
             for label = (xml-get-attribute Book 'label)
             collect (list label (format "anitama://%s" id) 1 'streamlist))))

(defun emms-stream-anitama-fetch-streamlist (&optional updatep)
  "Retrun anitama stream list.
If UPDATEP is no-nil, cache is updated."
  (if (and (not updatep) (consp emms-stream-anitama-streamlist-cache))
      emms-stream-anitama-streamlist-cache
    (emms-player-mpv-anitama--write-unless-cookies t)
    (setq emms-stream-anitama-streamlist-cache
     (with-temp-buffer
       (unless (zerop (call-process
                       "wget"  nil t nil "-q" "-O" "-"
                       (format "--load-cookies=%s"
                               emms-player-mpv-anitama--cookie-file)
                       "http://www.weeeef.com/weeeefww1/BookServlet"))
         (error "Failed to fetch http://www.weeeef.com/weeeefww1/BookServlet"))
       (emms-stream-anitaam--bookservlet-xml-to-streamlist
        (libxml-parse-xml-region (point-min) (point-max)))))))

;;;###autoload
(defun emms-stream-anitama-add-bookmark (&optional updatep)
  "Create anitama bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
  
If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (let* ((streamlist (emms-stream-anitama-fetch-streamlist updatep))
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-anitama)
;;; emms-streams-anitama.el ends here
