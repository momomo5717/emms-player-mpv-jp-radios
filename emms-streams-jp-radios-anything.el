;;; emms-streams-jp-radios-anything.el --- emms-streams-jp-radios for anything -*- lexical-binding: t; no-byte-compile: t -*-

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

;; This file provides a anything command for stream lists
;; which are defined or installed by each emms streams package.
;;
;; If `anything' is installed,
;; `emms-streams-jp-radios-anything' is available.
;;
;; Setup:
;;
;; (autoload 'emms-streams-jp-radios-anything
;;   "emms-streams-jp-radios-anything" nil t)
;;
;; ;; e.g.
;; (defalias 'anything-jp-radios 'emms-streams-jp-radios-anything)
;; (custom-set-variables
;;  '(emms-stream-jp-radios-anything-use-emms-stream-list-p t))
;;
;; Usage:
;;
;; * Basic action
;;
;;   | key       | Action                                              |
;;   |-----------+-----------------------------------------------------|
;;   | Enter     | Default action: Play the current stream             |
;;   | C-u Enter | Add the current stream                              |
;;   | C-z       | Default persistent action: Play the current stream  |
;;   | C-u C-z   | Persistent action: Add the current stream           |
;;
;; * Other actions
;;
;;   + Action (with prefix)
;;     + Play(Add) the current stream
;;     + (Clear `emms-playlist-buffer', ) Add streams and Play if `emms-player-playing-p' is nil
;;     + Update streams asynchronously
;;
;;
;; If you need an `anything' command for specific stream lists,
;; `emms-stream-onsen-anything' can be defined as bellow.
;;
;; e.g. 音泉
;; (defvar emms-stream-onsen-anything-c-source
;;   `((name . "EMMS Streams 音泉")
;;    (candidates . (lambda ()
;;                    (emms-stream-jp-radios-anything-combine
;;                     '(emms-stream-onsen-get-stream-list))))
;;    (action . ,emms-stream-jp-radios-anything-action-list)
;;    (migemo)
;;    (candidate-number-limit . 9999)))
;;
;; (defun emms-stream-onsen-anything ()
;;   "`anything' for emms streams onsen."
;;   (interactive)
;;   (anything :sources '(emms-stream-onsen-anything-c-source)
;;             :buffer "*Anything EMMS Streams 音泉*"))
;;
;; Note: This file is not compiled to be installed from MELPA.
;;
;; An error might occure when some candidates are marked and `anything-select-action' is used.
;; The following workaround would work.
;;
;; (defun anything-revive-visible-mark ()
;;   "Restore marked candidates when anything update display."
;;   (with-current-buffer anything-buffer
;;     (dolist (o anything-visible-mark-overlays)
;;       (goto-char (point-min))
;;       (while (and (search-forward (overlay-get o 'string) nil t)
;;                   (anything-current-source-name= (overlay-get o 'source)))
;;         ;; Calculate real value of candidate.
;;         ;; It can be nil if candidate have only a display value.
;;         (let ((real (get-text-property (point-at-bol 0) 'anything-realvalue)))
;;           (if real
;;               ;; Check if real value of current candidate is the same
;;               ;; that the one stored in overlay.
;;               (and (equal (overlay-get o 'real) real) ; * workaround using `equal'
;;                    (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0))))
;;               (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0)))))))))
;;

;;; Code:
(require 'emms-player-mpv-jp-radios)
(require 'emms-streams-jp-radios)
(require 'emms-streams)
(require 'cl-lib)
(require 'anything)

(defgroup emms-streams-jp-radios-anything nil
  "Anything for jp radio stations"
  :group 'emms
  :prefix "emms-stream-jp-radios-anything-")

(defcustom emms-stream-jp-radios-anything-use-emms-stream-list-p nil
  "If non-nil, `emms-stream-list' is added to candidates."
  :group 'emms-streams-jp-radios-anything
  :type 'boolean)

(defcustom emms-stream-jp-radios-anything-collect-function-alist
  (cl-loop for station in emms-player-mpv-jp-radios-list
           collect (cons station
                         (intern (format "emms-stream-%s-get-stream-list"
                                         station))))
  "Each function returns new stream list."
  :group 'emms-streams-jp-radios-anything
  :type '(alist :key-type string  :value-type symbol))

(defun emms-stream-jp-radios-anything-combine (get-fn-ls)
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
(byte-compile 'emms-stream-jp-radios-anything-combine)

(defun emms-stream-jp-radios-anything-action-play (stream)
  "Anything action for `emms-stream-jp-radios-anything-action-list'."
  (let ((current-prefix-arg
         (or anything-current-prefix-arg
             current-prefix-arg)))
    (setq emms-stream-last-stream stream)
    (funcall (intern (format "emms-play-%s" (emms-stream-type stream)))
             (emms-stream-url stream))))
(byte-compile 'emms-stream-jp-radios-anything-action-play)

(defun emms-stream-jp-radios-anything-action-add-streams (_)
  "Anything action for `emms-stream-jp-radios-anything-action-list'."
  (with-current-emms-playlist
    (when anything-current-prefix-arg
      (emms-playlist-current-clear))
    (emms-playlist-new)
    (let ((current-prefix-arg nil)
          (streams (anything-marked-candidates))
          (len 0))
      (dolist (stream streams)
        (setq emms-stream-last-stream stream)
        (funcall (intern (format "emms-add-%s" (emms-stream-type stream)))
                 (emms-stream-url stream))
        (cl-incf len))
      (unless emms-player-playing-p
        (if anything-current-prefix-arg
            (emms-playlist-first)
          (goto-char (point-max))
          (cl-loop repeat len do (emms-playlist-previous))
          (unless (emms-playlist-track-at (point))
            (emms-playlist-first)))
        (emms-playlist-mode-play-smart)))))
(byte-compile 'emms-stream-jp-radios-anything-action-add-streams)

(defvar emms-stream-jp-radios-anything-action-list
  '(("Play (C-u Add to Playlist)" .
     emms-stream-jp-radios-anything-action-play)
    ("Add Stream(s) to Playlist and play (C-u clear current)" .
     emms-stream-jp-radios-anything-action-add-streams)
    ("Update each stream list cache asynchronously." .
     (lambda (_) (emms-stream-jp-radios-update-cache-async))))
  "Action list for `emms-stream-jp-radios-anything-source'.")

(defvar emms-stream-jp-radios-anything-c-source
  `((name . "EMMS Streams JP Radio Stations")
    (init .
          (lambda ()
            (when (and emms-stream-jp-radios-anything-use-emms-stream-list-p
                       (not (buffer-live-p (get-buffer emms-stream-buffer-name))))
              (emms-stream-init))))
   (candidates .
               (lambda ()
                 (if emms-stream-jp-radios-anything-use-emms-stream-list-p
                     (nconc
                      (cl-loop for can in emms-stream-list
                               collect (cons (format "emms: %s" (car can)) can))
                      (emms-stream-jp-radios-anything-combine
                       emms-stream-jp-radios-anything-collect-function-alist))
                   (emms-stream-jp-radios-anything-combine
                    emms-stream-jp-radios-anything-collect-function-alist))))
   (action . ,emms-stream-jp-radios-anything-action-list)
   (migemo)
   (candidate-number-limit . 9999)))

(defun emms-streams-jp-radios-anything ()
  "`anything' for emms streams jp radio stations."
  (interactive)
  (anything :sources '(emms-stream-jp-radios-anything-c-source)
            :buffer "*Anything EMMS Streams JP Radios*"))
(byte-compile 'emms-streams-jp-radios-anything)

(provide 'emms-streams-jp-radios-anything)
;;; emms-streams-jp-radios-anything.el ends here
