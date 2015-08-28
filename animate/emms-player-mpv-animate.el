;;; emms-player-mpv-animate.el --- An emms simple mpv player for animate.tv -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-animate.

;; (require 'emms-player-mpv-animate)
;; (add-to-list 'emms-player-list 'emms-player-mpv-animate)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'xml)
(require 'url)
(require 'later-do)

(define-emms-simple-player-mpv mpv-animate '(streamlist)
  "\\`animate://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-animate "." t
 'emms-player-mpv-animate--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-animate 'get-media-title
                 'emms-player-mpv-animate--get-media-title)

(cl-defun emms-player-mpv-animate--xml-collect-node
    (name xml-ls &key (test #'identity) (getter #'identity))
  "Collect nodes of NAME from XML-LS.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
Object returned by GETTER is collected."
  (cl-labels ((collect-name-node (xml-ls ls)
                (cond
                 ((atom xml-ls) ls)
                 ((consp (car xml-ls))
                  (collect-name-node
                   (car xml-ls)
                   (collect-name-node (cdr xml-ls) ls)))
                 ((and (eq (car xml-ls) name)
                       (funcall test xml-ls))
                  (cons (funcall getter xml-ls) ls))
                 ((or (null (car xml-ls))
                      (not (symbolp (car xml-ls))))
                  (collect-name-node (cdr xml-ls) ls))
                 ((symbolp (car xml-ls))
                  (collect-name-node (xml-node-children xml-ls) ls ))
                 (t ls))))
    (collect-name-node xml-ls nil)))

(defun emms-player-mpv-animate--url-to-html (url)
  (let* ((buf (url-retrieve-synchronously url)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (libxml-parse-html-region (point) (point-max))
        (kill-buffer buf)))))

(defun emms-player-mpv-animate--fetch-wmp (url)
  "Fetch wmp link from URL."
  (let* ((wmp (car (emms-player-mpv-animate--xml-collect-node
                    'li (emms-player-mpv-animate--url-to-html url)
                    :test
                    (lambda (node) (equal (xml-get-attribute-or-nil node 'class) "wmp clearfix"))
                    :getter
                    (lambda (node) (let ((a (car (xml-get-children node 'a))))
                                 (xml-get-attribute-or-nil a 'href)))))))
    (unless wmp (error "Failed to get wmp"))
    (url-expand-file-name wmp url)))

(defun emms-player-mpv-animate--asx-to-href (asx)
  "Return href from ASX."
  (car (emms-player-mpv-animate--xml-collect-node
        'ref (emms-player-mpv-animate--url-to-html asx)
        :test
        (lambda (node) (xml-get-attribute-or-nil node 'href))
        :getter
        (lambda (node) (xml-get-attribute node 'href)))))

(defun emms-player-mpv-animate--loading-message ()
  "Loading message."
  (message "Loading animate.tv ... "))

(defun emms-player-mpv-animate--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let* ((stream-url (replace-regexp-in-string "\\`animate://" "" track-name))
         (url (emms-player-mpv-animate--asx-to-href
               (emms-player-mpv-animate--fetch-wmp stream-url))))
    (later-do 'emms-player-mpv-animate--loading-message)
    url))

(defun emms-player-mpv-animate--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-animate)
;;; emms-player-mpv-animate.el ends here
