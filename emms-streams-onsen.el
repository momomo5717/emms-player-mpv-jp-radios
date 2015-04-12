;;; emms-streams-onsen.el --- emms stream list for 音泉 -*- lexical-binding: t -*-

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

;; This provides emms stream list for 音泉.

;; (require 'emms-stream-onsen)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'emms-player-mpv-radiko)
(require 'xml)
(require 'url)

(defvar emms-stream-onsen-streamlist-cache nil)

(defun emms-stream-onsen--li-to-stream (li)
  "Retrun stream from LI."
  (let* ((id (xml-get-attribute li 'id))
         (week (xml-get-attribute li 'data-week))
         (update (xml-get-attribute li 'data-update))
         (h4 (car (xml-get-children li 'h4)))
         (title
          (car (xml-node-children
                (car (xml-get-children h4 'span)))))
         (p (cl-find "navigator listItem" (xml-get-children li 'p)
                             :key (lambda (p) (xml-get-attribute p 'class))
                             :test #'equal))
         (navigator (car (xml-node-children (car (xml-get-children p 'span))))))
    (list (format "%s : %s : update %s(%s)" title navigator update week)
          (format "onsen://%s" id) 1 'streamlist)))

(defun emms-stream-onsen--onsen-top-html-to-streamlist (html)
  "Return onsen stream list from HTML."
  (let* ((body (car (xml-get-children html 'body)))
         (div-moveListWrap
          (cl-loop with div = body
                   for i from 0 to 10 do
                   (setq div (car (xml-get-children div 'div)))
                   when (equal (xml-get-attribute-or-nil div 'class)
                               "movieListWrap")
                   return div
                   when (= i 10) do (error "Failed to parse onsen top html")))
         (section-movieList
          (cl-find "movieList" (xml-get-children div-moveListWrap 'section)
                   :key (lambda (section) (xml-get-attribute section 'id))
                   :test #'equal))
         (div-listWrap
          (cl-find "listWrap" (xml-get-children section-movieList 'div)
                      :key #'(lambda (div) (xml-get-attribute-or-nil div 'class))
                      :test #'equal))
         (li-ls (xml-get-children
                 (car (xml-get-children div-listWrap 'ul)) 'li)))
    (mapcar #'emms-stream-onsen--li-to-stream li-ls)))

(defun emms-stream-onsen-fetch-streamlist (&optional updatep)
  "Return onsen stream list.
If UPDATEP is no-nil, cache is updated."
  (if  (and (null updatep)
            (consp emms-stream-onsen-streamlist-cache))
      emms-stream-onsen-streamlist-cache
    (setq emms-stream-onsen-streamlist-cache
          (let ((buf (url-retrieve-synchronously "http://www.onsen.ag")))
            (with-current-buffer buf
              (goto-char (point-min))
              (while (and (not (eobp)) (not (eolp))) (forward-line 1))
              (unless (eobp) (forward-line 1))
             (prog1
                 (emms-stream-onsen--onsen-top-html-to-streamlist
                  (libxml-parse-html-region (point-min) (point-max)))
               (kill-buffer buf)))))))

;;;###autoload
(defun emms-stream-onsen-add-bookmark (&optional updatep)
  "Create agqr bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (let* ((streamlist (emms-stream-onsen-fetch-streamlist updatep))
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-onsen)
;;; emms-streams-onsen.el ends here
