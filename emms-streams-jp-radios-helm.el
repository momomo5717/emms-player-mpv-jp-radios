;;; emms-streams-jp-radios-helm.el --- emms-streams-jp-radios for helm -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2015 momomo5717

;; URL: https://github.com/momomo5717/

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

;; This package provides a helm command for stream lists
;; which are defined or installed by each emms streams package.
;;
;; If `helm' v1.7.9 or later is installed,
;; `emms-streams-jp-radios-helm' is available.
;;
;; Setup:
;;
;; (require 'emms-streams-jp-radios-helm)
;;
;; Usage:
;;
;; Some stream lists need to update cache.
;; M-x `emms-streams-jp-radios-helm' <f3> or TAB (Actions)
;; provides updating cache asynchronously.
;;
;; If you need a helm command for specific stream lists,
;; `emms-stream-jp-radios-helm-source' can be used as bellow.
;;
;; e.g. 音泉
;; (defvar emms-stream-onsen-helm-source-list
;;   (helm-make-source "EMMS Streams 音泉" 'emms-stream-jp-radios-helm-source
;;     :init nil
;;     :candidates (lambda () (cl-loop for stream in (emms-stream-onsen-get-stream-list)
;;                                 collect (cons (car stream) stream)))))
;; (defun emms-stream-onsen-helm ()
;;   "`helm' for emms streams onsen."
;;   (interactive)
;;   (helm :sources 'emms-stream-onsen-helm-source-list
;;         :buffer "*Helm Emms Streams 音泉"))
;;
;; Note: This package is not compiled to be installed from MELPA.

;;; Code:
(require 'emms-player-mpv-jp-radios)
(require 'emms-streams-jp-radios)
(require 'emms-streams)
(require 'cl-lib)
(require 'helm)

(defgroup emms-streams-jp-radios-helm nil
  "Helm for jp radio stations"
  :group 'emms
  :prefix "emms-stream-jp-radios-")

(defcustom emms-stream-jp-radios-hlem-use-emms-stream-list-p nil
  "If non-nil, `emms-stream-list' is added to candidates."
  :group 'emms-streams-jp-radios-helm
  :type 'boolean)

(defcustom emms-stream-jp-radios-helm-collect-function-alist
  (cl-loop for station in emms-player-mpv-jp-radios-list
           collect (cons station
                         (intern (format "emms-stream-%s-get-stream-list"
                                         station))))
  "Each function returns new stream list."
  :group 'emms-streams-jp-radios-helm
  :type '(alist :key-type string  :value-type function))

(defun emms-stream-jp-radios--helm-collect-candidates ()
  "Combine stream-list which are defined or cache."
  (let ((candidates nil))
    (when emms-stream-jp-radios-hlem-use-emms-stream-list-p
      (dolist (can (cl-copy-list emms-stream-list))
        (setq candidates (cons (cons (format "emms: %s"  (car can))
                                     can)
                               candidates))))
    (cl-loop for (station . fn) in emms-stream-jp-radios-helm-collect-function-alist
             for stream-list = (if (fboundp fn) (funcall fn) '()) do
             (cl-loop
              for can in stream-list do
              (setq candidates (cons (cons (format "%s: %s" station (car can))
                                           can)
                                     candidates))))
    (nreverse candidates)))
(byte-compile 'emms-stream-jp-radios--helm-collect-candidates)

(defclass emms-stream-jp-radios-helm-source (helm-source-sync)
  ((init :initform (lambda ()
                     (when (and emms-stream-jp-radios-hlem-use-emms-stream-list-p
                                (not (buffer-live-p (get-buffer emms-stream-buffer-name))))
                       (emms-stream-init))))
   (candidates :initform #'emms-stream-jp-radios--helm-collect-candidates)
   (action :initform
           '(("Play (C-u Add to Playlist)" .
              (lambda (stream)
                (let* ((url (cl-second stream))
                       (current-prefix-arg
                        (or helm-current-prefix-arg
                            current-prefix-arg)))
                  (setq emms-stream-last-stream stream)
                  (funcall #'emms-play-streamlist url))))
             ("Add Stream(s) to Playlist and play (C-u clear current)" .
              (lambda (_)
                (with-current-emms-playlist
                  (when helm-current-prefix-arg
                    (emms-playlist-current-clear))
                  (emms-playlist-new)
                  (let* ((current-prefix-arg nil)
                         (streams (helm-marked-candidates))
                         (len 0))
                    (dolist (stream streams)
                      (setq emms-stream-last-stream stream)
                      (funcall #'emms-add-streamlist (cl-second stream))
                      (cl-incf len))
                    (unless emms-player-playing-p
                      (if helm-current-prefix-arg
                          (emms-playlist-first)
                        (goto-char (point-max))
                        (cl-loop repeat len do (emms-playlist-previous))
                        (unless (emms-playlist-track-at (point))
                          (emms-playlist-first)))
                      (emms-playlist-mode-play-smart))))))
             ("Update each stream list cache asynchronously." .
              (lambda (_) (emms-stream-jp-radios-update-cache-async)))))
   (migemo :initform t)
   (candidate-number-limit :initform 9999)))

(defvar emms-stream-jp-radios-helm-source-list
  (helm-make-source "EMMS Streams JP Radio Stations"
      'emms-stream-jp-radios-helm-source))

(defun emms-streams-jp-radios-helm ()
  "`helm' for emms streams jp radio stations."
  (interactive)
  (helm :sources 'emms-stream-jp-radios-helm-source-list
        :buffer "*Helm Emms Streams JP Radio Stations*"))
(byte-compile 'emms-streams-jp-radios-helm)

(provide 'emms-streams-jp-radios-helm)
;;; emms-streams-jp-radios-helm.el ends here
