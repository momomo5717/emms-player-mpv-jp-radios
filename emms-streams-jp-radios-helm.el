;;; emms-streams-jp-radios-helm.el --- emms-streams-jp-radios for helm -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2015-2017 momomo5717

;; URL: https://github.com/momomo5717/emms-player-mpv-jp-radios

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

;; This file provides a helm command for stream lists
;; which are defined or installed by each emms streams package.
;;
;; If `helm' v1.7.9 or later is installed,
;; `emms-streams-jp-radios-helm' is available.
;;
;; Setup:
;;
;; (autoload 'emms-streams-jp-radios-helm
;;   "emms-streams-jp-radios-helm" nil t)
;;
;; ;; e.g.
;; (defalias 'helm-jp-radios 'emms-streams-jp-radios-helm)
;; (custom-set-variables
;;  '(emms-stream-jp-radios-helm-use-emms-stream-list-p t))
;;
;; Usage:
;;
;;  | key            | Action                                                      |
;;  |----------------+-------------------------------------------------------------|
;;  | Enter/<f1>     | Default action: Play the current stream                     |
;;  | C-u Enter/<f1> | Add the current stream                                      |
;;  | C-j            | Default persistent action: Play the current stream          |
;;  | C-u C-j        | Persistent action: Add the current stream                   |
;;  | <f2>           | Add streams(s) and Play if `emms-player-playing-p' is nil   |
;;  | C-u <f2>       | Clear `emms-playlist-buffer', Add stream(s) and Play        |
;;  |                | if `emms-player-playing-p' is nil                           |
;;  | <f3>           | Update streams asynchronously                               |
;;
;;
;; If you need a helm command for specific stream lists,
;; `emms-stream-jp-radios-helm-source' can be used as bellow.
;;
;; e.g. 音泉
;; (defvar emms-stream-onsen-helm-source-list
;;   (helm-make-source "EMMS Streams 音泉" 'emms-stream-jp-radios-helm-source
;;     :init nil
;;     :candidates (lambda () (emms-stream-jp-radios-helm-combine
;;                         '(emms-stream-onsen-get-stream-list)))))
;; (defun emms-stream-onsen-helm ()
;;   "`helm' for emms streams onsen."
;;   (interactive)
;;   (helm :sources 'emms-stream-onsen-helm-source-list
;;         :buffer "*Helm EMMS Streams 音泉*"))
;;
;; Note: This file is not compiled to be installed from MELPA.

;;; Code:
(require 'emms-player-mpv-jp-radios)
(require 'emms-streams-jp-radios)
(require 'emms-streams)
(require 'cl-lib)
(require 'helm)

(defgroup emms-streams-jp-radios-helm nil
  "Helm for jp radio stations"
  :group 'emms
  :prefix "emms-stream-jp-radios-helm-")

(defcustom emms-stream-jp-radios-helm-use-emms-stream-list-p nil
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
  :type '(alist :key-type string  :value-type symbol))

(defun emms-stream-jp-radios-helm-combine (get-fn-ls)
  "Combine stream-lists from GET-FN-LS.
GET-FN-LS is function list or \(station . function\) alist."
  (let ((candidates nil))
    (cl-loop for fn in get-fn-ls
             for stream-list = (if (consp fn)
                                   (if (fboundp (cdr fn)) (funcall (cdr fn)) '())
                                 (if (fboundp fn) (funcall fn) '()))
             do
             (if (consp fn)
                 (dolist (can stream-list)
                   (setq candidates (cons (cons (format "%s: %s" (car fn) (car can))
                                                can)
                                          candidates)))
               (dolist (can stream-list)
                 (setq candidates (cons (cons (car can) can) candidates)))))
    (nreverse candidates)))
(byte-compile 'emms-stream-jp-radios-helm-combine)

(defun emms-stream-jp-radios-helm-action-play (stream)
  "Helm action for `emms-stream-jp-radios-helm-action-list'."
  (let ((current-prefix-arg
         (or helm-current-prefix-arg
             current-prefix-arg)))
    (setq emms-stream-last-stream stream)
    (funcall (intern (format "emms-play-%s" (emms-stream-type stream)))
             (emms-stream-url stream))))
(byte-compile 'emms-stream-jp-radios-helm-action-play)

(defun emms-stream-jp-radios-helm-action-add-streams (_)
  "Helm action for `emms-stream-jp-radios-helm-action-list'."
  (with-current-emms-playlist
    (when helm-current-prefix-arg
      (emms-playlist-current-clear))
    (emms-playlist-new)
    (let ((current-prefix-arg nil)
          (streams (helm-marked-candidates))
          (len 0))
      (dolist (stream streams)
        (setq emms-stream-last-stream stream)
        (funcall (intern (format "emms-add-%s" (emms-stream-type stream)))
                 (emms-stream-url stream))
        (cl-incf len))
      (unless emms-player-playing-p
        (if helm-current-prefix-arg
            (emms-playlist-first)
          (goto-char (point-max))
          (cl-loop repeat len do (emms-playlist-previous))
          (unless (emms-playlist-track-at (point))
            (emms-playlist-first)))
        (emms-playlist-mode-play-smart)))))
(byte-compile 'emms-stream-jp-radios-helm-action-add-streams)

(defvar emms-stream-jp-radios-helm-action-list
  '(("Play (C-u Add to Playlist)" .
     emms-stream-jp-radios-helm-action-play)
    ("Add Stream(s) to Playlist and play (C-u clear current)" .
     emms-stream-jp-radios-helm-action-add-streams)
    ("Update each stream list cache asynchronously." .
     (lambda (_) (emms-stream-jp-radios-update-cache-async))))
  "Action list for `emms-stream-jp-radios-helm-source'.")

(defclass emms-stream-jp-radios-helm-source (helm-source-sync)
  ((init :initform
         (lambda ()
           (when (and emms-stream-jp-radios-helm-use-emms-stream-list-p
                      (not (buffer-live-p (get-buffer emms-stream-buffer-name))))
             (emms-stream-init))))
   (candidates :initform
               (lambda ()
                 (if emms-stream-jp-radios-helm-use-emms-stream-list-p
                     (nconc
                      (cl-loop for can in emms-stream-list
                               collect (cons (format "emms: %s" (car can)) can))
                      (emms-stream-jp-radios-helm-combine
                       emms-stream-jp-radios-helm-collect-function-alist))
                   (emms-stream-jp-radios-helm-combine
                    emms-stream-jp-radios-helm-collect-function-alist))))
   (action :initform emms-stream-jp-radios-helm-action-list)
   (migemo :initform t)
   (candidate-number-limit :initform 9999)))

(defvar emms-stream-jp-radios-helm-source-list
  (helm-make-source "EMMS Streams JP Radio Stations"
      'emms-stream-jp-radios-helm-source))

(defun emms-streams-jp-radios-helm ()
  "`helm' for emms streams jp radio stations."
  (interactive)
  (helm :sources 'emms-stream-jp-radios-helm-source-list
        :buffer "*Helm EMMS Streams JP Radios*"))
(byte-compile 'emms-streams-jp-radios-helm)

(provide 'emms-streams-jp-radios-helm)
;;; emms-streams-jp-radios-helm.el ends here
