;;; emms-streams-hibiki.el --- emms stream list for HiBiKi Radio Station  -*- lexical-binding: t -*-
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

;; This provides emms stream list for 響 - HiBiKi Radio Station -.

;; (require 'emms-stream-hibiki)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)

(defvar emms-stream-hibiki-streamlist-cache nil)

(defun emms-stream-hibiki--div-hbkPrograms-to-streamlist (div-hbkPrograms)
  "Return stream from DIV-HBKPROGRAMS."
  (let* ((a (car (xml-get-children div-hbkPrograms 'a)))
         (AttachVideo
          (split-string
           (replace-regexp-in-string "AttachVideo\\|\(\\|'\\|\)" ""
                                     (xml-get-attribute a 'onclick)) ","))
         (name-comment (mapcar (lambda (div) (car (xml-node-children div)))
                               (xml-get-children a 'div))))
    (list (format "%s : %s" (cl-first name-comment) (cl-second name-comment))
          (format "hibiki://%s/%s" (cl-first AttachVideo) (cl-second AttachVideo))
          1 'streamlist)))

(defun emms-stream-hibiki--fetch-streamlist-dow (n)
  "Retrun streamlist of N\(DOW\)."
  (unless (< 0 n 7) (error "Number of DOW should be from 1 to 6"))
  (let ((buf (url-retrieve-synchronously
              (format "http://hibiki-radio.jp/get_program/%d" n))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let* ((body (car (xml-get-children
                         (libxml-parse-html-region (point) (point-max)) 'body)))
             (hbkTabsMain (format "hbkTabsMain%d" n))
             (div-hbkTabsMain
              (cl-find hbkTabsMain
                       (xml-get-children (car (xml-get-children body 'div)) 'div)
                       :key (lambda (div) (xml-get-attribute div 'id))
                       :test #'equal))
             (div-hbkPrograms
              (cl-delete-if-not
               (lambda (div) (equal (xml-get-attribute div 'class) "hbkProgram"))
               (xml-get-children div-hbkTabsMain 'div))))
        (prog1
            (mapcar #'emms-stream-hibiki--div-hbkPrograms-to-streamlist div-hbkPrograms)
          (kill-buffer buf))))))

(defun emms-stream-hibiki-fetch-streamlist (&optional updatep)
  "Return hibiki stream list.
If UPDATEP is no-nil, cache is updated."
  (if (and (null updatep) (consp emms-stream-hibiki-streamlist-cache))
      emms-stream-hibiki-streamlist-cache
    (setq emms-stream-hibiki-streamlist-cache
          (cl-loop for i from 1 below 7
                   nconc (emms-stream-hibiki--fetch-streamlist-dow i)))))

;;;###autoload
(defun emms-stream-hibiki-add-bookmark (&optional updatep)
  "Create agqr bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (let* ((streamlist (emms-stream-hibiki-fetch-streamlist updatep))
         (line       (emms-line-number-at-pos (point)))
         (index      (+ (/ line 2) 1)))
    (dolist (stream streamlist)
      (setq emms-stream-list (emms-stream-insert-at index stream
                                                    emms-stream-list))
      (cl-incf index))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'emms-streams-hibiki)
;;; emms-streams-hibiki.el ends here
