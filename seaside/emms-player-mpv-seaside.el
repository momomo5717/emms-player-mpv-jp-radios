;;; emms-player-mpv-seaside.el --- An emms simple mpv player for Sea Side Communications -*- lexical-binding: t -*-

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

;; This provides emms-player-mpv-seaside.

;; (require 'emms-player-mpv-seaside)
;; (add-to-list 'emms-player-list 'emms-player-mpv-seaside)

;;; Code:
(require 'cl-lib)
(require 'emms-player-simple-mpv)
(require 'emms-streams)
(require 'xml)
(require 'url)
(require 'later-do)

(define-emms-simple-player-mpv mpv-seaside '(streamlist)
  "\\`seaside://"
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no" "--ytdl")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-seaside "." t
 'emms-player-mpv-seaside--track-name-to-wma-input-form)

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv-seaside
 "\\`seaside://http://seaside-c.jp/program/emergency/" t
 'emms-player-mpv-seaside--track-name-to-nico-input-form)

(emms-player-set 'emms-player-mpv-seaside 'get-media-title
                 'emms-player-mpv-seaside--get-media-title)

(cl-defun emms-player-mpv-seaside--xml-collect-node
    (name xml-ls &key (test #'identity) (getter #'identity))
  "Collect nodes of NAME from XML-LS.
TEST and GETTER takes a node of NAME as an argument.
TEST is a predicate function.
Object returned by GETTER is collected."
  (cl-labels ((collect-name-node (xml-ls ls)
                (cond
                 ((atom xml-ls) ls)
                 ((consp (car xml-ls))
                  (collect-name-node (car xml-ls)
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

(defun emms-player-mpv-seaside--loading-message ()
  "Loading message."
  (message "Loding Sea Side Communications ... "))

(defun emms-player-mpv-seaside--url-to-body (url)
  "Return body html list fron URL."
  (let ((buf (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eolp))) (forward-line 1))
          (unless (eobp) (forward-line 1))
          (let* ((html (libxml-parse-html-region (point) (point-max)))
                 (body (car (xml-get-children html 'body))))
            body))
      (kill-buffer buf))))

(defun emms-player-mpv-seaside--body-html-to-wax (body-html track-url)
  "Return WAX from BODY-HTML with TRACK-URL."
  (let ((wax
         (car (emms-player-mpv-seaside--xml-collect-node
               'a body-html
               :test
               (lambda (node) (let ((href (xml-get-attribute-or-nil node 'href)))
                            (and href
                                 (string-match-p "[.]wax$" href))))
               :getter
               (lambda (node)
                 (let ((href (xml-get-attribute-or-nil node 'href)))
                   (if (string-match-p "\\`http://" href)
                       href
                     (concat track-url href))))))))
    (if wax wax (error "Not found WAX"))))

(defun emms-player-mpv-seaside--body-html-to-nico (body-html)
  "Return nico url from BODY-HTML with TRACK-URL."
  (let ((nico-url
         (car (emms-player-mpv-seaside--xml-collect-node
               'a body-html
               :test
               (lambda (node) (let ((href (xml-get-attribute-or-nil node 'href)))
                            (and href
                                 (string-match-p "\\`http://www.nicovideo.jp/watch/"
                                                 href))))
               :getter
               (lambda (node) (xml-get-attribute-or-nil node 'href))))))
    (if nico-url nico-url (error "Not found nico url"))))

(defun emms-player-mpv-seaside--wax-to-wma (wax)
  "Return WMA fron WAX."
  (let ((buf (url-retrieve-synchronously wax)))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eolp))) (forward-line 1))
          (unless (eobp) (forward-line 1))
          (let ((wma (car
                      (emms-player-mpv-seaside--xml-collect-node
                       'ref (libxml-parse-html-region (point) (point-max))
                       :getter (lambda (node) (xml-get-attribute-or-nil node 'href)) ))))
            (if wma wma (error "Not found WMA"))))
      (kill-buffer buf))))

(defun emms-player-mpv-seaside--track-name-to-wma-input-form (track-name)
  "Return url from TRACK-NAME."
  (let* ((track-url
          (replace-regexp-in-string "\\`seaside://" "" track-name))
         (wax (emms-player-mpv-seaside--body-html-to-wax
               (emms-player-mpv-seaside--url-to-body track-url)
               track-url))
         (wma (emms-player-mpv-seaside--wax-to-wma wax)))
    (later-do 'emms-player-mpv-seaside--loading-message)
    wma))

(defun emms-player-mpv-seaside--track-name-to-nico-input-form (track-name)
  "Return url from TRACK-NAME."
  (unless (executable-find "youtube-dl")
      (error "Can not play %s" track-name))
  (let* ((track-url
          (replace-regexp-in-string "\\`seaside://" "" track-name))
         (nico-url
          (emms-player-mpv-seaside--body-html-to-nico
           (emms-player-mpv-seaside--url-to-body track-url))))
    (later-do 'emms-player-mpv-seaside--loading-message)
    nico-url))

(defun emms-player-mpv-seaside--get-media-title (track)
  "Return media title from TRACK."
  (if (eq (emms-track-type track) 'streamlist)
      (emms-stream-name(emms-track-get track 'metadata))
    (file-name-nondirectory (emms-track-name track))))

(provide 'emms-player-mpv-seaside)
;;; emms-player-mpv-seaside.el ends here
