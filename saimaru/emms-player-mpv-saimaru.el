;;; emms-player-mpv-saimaru.el --- An emms simple mpv player for SaimaruRadio -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-saimaru.

;; (require 'emms-player-mpv-saimaru)
;; (add-to-list 'emms-player-list 'emms-player-mpv-saimaru)

;;; Code:
(require 'cl-lib)
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'xml)
(require 'url)
(require 'later-do)

(define-emms-simple-player-mpv mpv-saimaru '(streamlist)
  "\\`saimaru://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-saimaru "." t
 'emms-player-mpv-saimaru--track-name-to-input-form)

(emms-player-set 'emms-player-mpv-saimaru 'get-media-title
                 'emms-player-mpv-saimaru--get-media-title)

(cl-defun emms-player-mpv-saimaru--xml-collect-node
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

(defun emms-player-mpv-saimaru--asx-to-href (asx)
  "Return href from ASX."
  (let* ((buf (url-retrieve-synchronously asx))
         (html (with-current-buffer buf
                 (goto-char (point-min))
                 (while (and (not (eobp)) (not (eolp))) (forward-line 1))
                 (unless (eobp) (forward-line 1))
                 (prog1 (libxml-parse-html-region (point) (point-max))
                   (kill-buffer buf)))))
    (car (last (emms-player-mpv-saimaru--xml-collect-node
                'ref html
                :test
                (lambda (node) (xml-get-attribute-or-nil node 'href))
                :getter
                (lambda (node) (xml-get-attribute node 'href)))))))

(defun emms-player-mpv-saimaru--loading-message ()
  "Loading message."
  (message "Loding SaimaruRadio ... "))

(defun emms-player-mpv-saimaru--track-name-to-input-form (track-name)
  "Return url from TRACK-NAME."
  (let* ((stream-url (replace-regexp-in-string "\\`saimaru://" "" track-name))
         (url (if (string-match-p "[.]asx$" stream-url)
                  (emms-player-mpv-saimaru--asx-to-href stream-url)
                stream-url)))
    (later-do 'emms-player-mpv-saimaru--loading-message)
    url))

(defun emms-player-mpv-saimaru--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-saimaru)
;;; emms-player-mpv-saimaru.el ends here
