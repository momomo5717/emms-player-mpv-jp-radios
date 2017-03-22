;;; emms-streams-jp-radios-counsel.el --- Select stream(s) using ivy -*- lexical-binding: t; no-byte-compile: t  -*-

;; Copyright (C) 2016-2017 momomo5717

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

;; This file provides a counsel command for stream lists.
;; If `ivy' 0.8.0 or later is installed,
;; `emms-streams-jp-radios-counsel' is available.

;; Setup:
;;
;; (autoload 'emms-streams-jp-radios-counsel
;;   "emms-streams-jp-radios-counsel" nil t)
;;
;; ;; e.g.
;; (defalias 'counsel-jp-radios 'emms-streams-jp-radios-counsel)
;; (push '(emms-streams-jp-radios-counsel . ivy--regex-ignore-order)
;;       ivy-re-builders-alist)
;; (custom-set-variables
;;  '(emms-stream-jp-radios-counsel-use-emms-stream-list-p t)
;;  '(emms-stream-jp-radios-counsel-ivy-height 21))
;;
;; Usage:
;;
;; * Default emms-streams-jp-radios-counsel-map
;;
;;   | Key     | Action                         |
;;   |---------+--------------------------------|
;;   | C-SPC   | (Un)mark the current candidate |
;;   | M-a     | Mark visible candidates        |
;;   | C-u M-a | Unmark visible candidates      |
;;   | M-U     | Unmark all candidates          |
;;
;; * Actions
;;
;;   | Key | Action                                               |
;;   |-----+------------------------------------------------------|
;;   |  o  | Default action: Play the current stream              |
;;   |  a  | Add stream(s) and Play                               |
;;   |  A  | Add stream(s)                                        |
;;   |  c  | Clear `emms-playlist-buffer', Add stream(s) and Play |
;;   |  C  | Clear `emms-playlist-buffer' and Add stream(s)       |
;;   |  u  | Update streams asynchronously                        |
;;
;;   Key "a" and "c" depend on `emms-stream-jp-radios-counsel-always-play-p'.
;;
;;   If `emms-stream-jp-radios-counsel-always-play-p' is non-nil,
;;   a new stream will be started regardless of `emms-player-playing-p'.
;;
;; * For a specific radio station
;;
;; ;; e.g. 音泉 (onsen)
;; (defun counsel-onsen ()
;;   "Select onsen stream."
;;   (interactive)
;;   (let ((emms-stream-jp-radios-counsel-use-emms-stream-list-p nil)
;;         (emms-stream-jp-radios-counsel-collect-function-alist
;;          '(("" . emms-stream-onsen-get-stream-list))))
;;     (emms-streams-jp-radios-counsel)))
;;
;;
;; Note: This file is not compiled to be installed from MELPA.

;;; Code:
(require 'emms-player-mpv-jp-radios)
(require 'emms-streams-jp-radios)
(require 'emms-playlist-mode)
(require 'emms-streams)
(require 'cl-lib)
(require 'ivy)

(defgroup emms-streams-jp-radios-counsel nil
  "Counsel for jp radio stations"
  :group 'emms
  :prefix "emms-stream-jp-radios-counsel-")

(defcustom emms-stream-jp-radios-counsel-ivy-height ivy-height
  "`ivy-height' for `emms-streams-jp-radios-counsel'."
  :type 'integer)

(defcustom emms-stream-jp-radios-counsel-use-emms-stream-list-p nil
  "If non-nil, `emms-stream-list' is added to candidates."
  :type 'boolean)

(defcustom emms-stream-jp-radios-counsel-collect-function-alist
  (cl-loop for station in emms-player-mpv-jp-radios-list
           collect (cons station
                         (intern (format "emms-stream-%s-get-stream-list"
                                         station))))
  "Each function returns new stream list."
  :type '(alist :key-type string  :value-type function))

(defcustom emms-stream-jp-radios-counsel-always-play-p nil
  "If non-nil, an action will always start a new stream."
  :type 'boolean)

(defface emms-stream-jp-radios-counsel-highlight
  '((t (:inherit highlight)))
  "Face for marked streams.")

(defvar emms-stream-jp-radios-counsel--marked-cands nil
  "Current marked candidates.")

(defvar emms-stream-jp-radios-counsel--old-collect-function-alist nil
  "Store `emms-stream-jp-radios-counsel-collect-function-alist' for `ivy-resum'.")

(defvar emms-stream-jp-radios-counsel--old-use-emms-stream-list-p nil
  "Store `emms-stream-jp-radios-counsel-use-emms-stream-list-p' for `ivy-resume'.")

(defun emms-stream-jp-radios-counsel--add-text-property (prop value cand
                                                              &optional beg end)
  "Add text property PROP VALUE to CAND.
If BEG/END is non-nil, it will be used."
  (add-text-properties (or beg 0) (or end (length cand)) (list prop value) cand)
  cand)
(byte-compile 'emms-stream-jp-radios-counsel--add-text-property)

(defun emms-stream-jp-radios-counsel--get-text-property (prop cand)
  "Get text property PROP of CAND."
  (or (get-text-property 0 prop cand)
      (get-text-property (next-single-property-change 0 prop cand (length cand))
                         prop cand)))
(byte-compile 'emms-stream-jp-radios-counsel--get-text-property)

(defun emms-stream-jp-radios-counsel--add-tp-id (id cand &optional beg end)
  "Add text property ID to CAND.
If BEG/END is non-nil, it will be used."
  (emms-stream-jp-radios-counsel--add-text-property :jp-radios-stream-id id cand beg end))
(byte-compile 'emms-stream-jp-radios-counsel--add-tp-id)

(defun emms-stream-jp-radios-counsel--get-tp-id (cand)
  "Get id of CAND."
  (emms-stream-jp-radios-counsel--get-text-property :jp-radios-stream-id cand))
(byte-compile 'emms-stream-jp-radios-counsel--get-tp-id)

(defun emms-stream-jp-radios-counsel--add-tp-stream (stream cand &optional beg end)
  "Add text property STREAM to CAND.
If BEG/END is non-nil, it will be used."
  (emms-stream-jp-radios-counsel--add-text-property :jp-radios-stream stream cand beg end))
(byte-compile 'emms-stream-jp-radios-counsel--add-tp-stream)

(defun emms-stream-jp-radios-counsel--get-tp-stream (cand)
  "Get stream of CAND."
  (emms-stream-jp-radios-counsel--get-text-property :jp-radios-stream cand))
(byte-compile 'emms-stream-jp-radios-counsel--get-tp-stream)

(defun emms-stream-jp-radios-counsel--add-face-highlight (cand)
  "Add highlight face to CAND.
If BEG/END is non-nil, it will be used."
  (add-face-text-property 0 (length cand)
                          'emms-stream-jp-radios-counsel-highlight nil cand)
  cand)
(byte-compile 'emms-stream-jp-radios-counsel--add-face-highlight)

(defun emms-stream-jp-radios-counsel--remove-face (cand)
  "Remove face of CAND."
  (remove-text-properties 0 (length cand) '(face nil) cand)
  cand)
(byte-compile 'emms-stream-jp-radios-counsel--remove-face)

(defun emms-stream-jp-radios-counsel--make-id-hash (cands)
  "Make a hash table for id of CANDS."
  (let ((h (make-hash-table :test #'eql)))
    (dolist (cand cands h)
      (puthash (emms-stream-jp-radios-counsel--get-tp-id cand) t h))))
(byte-compile 'emms-stream-jp-radios-counsel--make-id-hash)

(defun emms-stream-jp-radios-counsel-list (&rest _)
  "Make a list of stream names from `emms-stream-jp-radios-counsel-collect-function-alist'."
  (when (and emms-stream-jp-radios-counsel-use-emms-stream-list-p
             (not (buffer-live-p (get-buffer emms-stream-buffer-name))))
    (emms-stream-init))
  (cl-loop
   with id = -1
   with cands = nil
   with marked-cands-h = (when emms-stream-jp-radios-counsel--marked-cands
                           (emms-stream-jp-radios-counsel--make-id-hash
                            emms-stream-jp-radios-counsel--marked-cands))
   for (name . fn) in (if emms-stream-jp-radios-counsel-use-emms-stream-list-p
                          (cons (cons "emms" (lambda () emms-stream-list))
                                emms-stream-jp-radios-counsel-collect-function-alist)
                        emms-stream-jp-radios-counsel-collect-function-alist)
   when (functionp fn) do
   (cl-loop for stream in (funcall fn)
            for cand = (format "%s%s%s" name (if (string= name "") "" ": ")
                               (emms-stream-name stream)) do
            (emms-stream-jp-radios-counsel--add-tp-stream stream cand 0 1)
            (emms-stream-jp-radios-counsel--add-tp-id (cl-incf id) cand 0 1)
            (when (and marked-cands-h
                       (gethash id marked-cands-h))
              (emms-stream-jp-radios-counsel--add-face-highlight cand))
            (push cand cands))
   finally return (nreverse cands)))
(byte-compile 'emms-stream-jp-radios-counsel-list)

(defun emms-stream-jp-radios-counsel--toggle-mark ()
  "Toggle marked cnadidate."
  (interactive)
  (let* ((get-tp-id #'emms-stream-jp-radios-counsel--get-tp-id)
         (id (funcall get-tp-id (if (fboundp 'ivy-state-current)
                                    (ivy-state-current ivy-last)
                                  (bound-and-true-p ivy--current))))
         (cand (cl-find id ivy--all-candidates :key get-tp-id)))
    (emms-stream-jp-radios-counsel--remove-face cand)
    (if (cl-member id emms-stream-jp-radios-counsel--marked-cands :key get-tp-id)
        (setq emms-stream-jp-radios-counsel--marked-cands
              (cl-delete id emms-stream-jp-radios-counsel--marked-cands
                         :key get-tp-id))
      (push (emms-stream-jp-radios-counsel--add-face-highlight cand)
            emms-stream-jp-radios-counsel--marked-cands))
    (ivy-next-line)))
(byte-compile 'emms-stream-jp-radios-counsel--toggle-mark)

(defun emms-stream-jp-radios-counsel--mark-visible-cands (&optional unmarkp)
  "Mark visible candidates.
If UNMARKP \(prefix arg) is non-nil, unmark visible candidates."
  (interactive "P")
  (cl-loop with get-tp-id = #'emms-stream-jp-radios-counsel--get-tp-id
           with marked-cands-h = (emms-stream-jp-radios-counsel--make-id-hash
                                  emms-stream-jp-radios-counsel--marked-cands)
           with old-cands-h = (emms-stream-jp-radios-counsel--make-id-hash ivy--old-cands)
           for cand in ivy--all-candidates
           for id = (funcall get-tp-id cand) do
           (if (not unmarkp)
               (when (and (gethash id old-cands-h)
                          (not (gethash id marked-cands-h)))
                 (emms-stream-jp-radios-counsel--add-face-highlight cand)
                 (push cand emms-stream-jp-radios-counsel--marked-cands)
                 (puthash id t marked-cands-h))
             (when (gethash id old-cands-h)
               (emms-stream-jp-radios-counsel--remove-face cand)))
           finally do
           (when unmarkp
             (setq emms-stream-jp-radios-counsel--marked-cands
                   (cl-delete-if (lambda (cand)
                                   (gethash (funcall get-tp-id cand)
                                            old-cands-h))
                                 emms-stream-jp-radios-counsel--marked-cands)))))
(byte-compile 'emms-stream-jp-radios-counsel--mark-visible-cands)

(defun emms-stream-jp-radios-counsel--unmark-all ()
  "Unmark all marked candidates."
  (interactive)
  (let ((ivy--old-cands emms-stream-jp-radios-counsel--marked-cands))
    (emms-stream-jp-radios-counsel--mark-visible-cands t)))
(byte-compile 'emms-stream-jp-radios-counsel--unmark-all)

(defun emms-stream-jp-radios-action-add-and-play (cand &optional non-play-p clearp)
  "Add marked CAND\(s) and Play.
If NON-PLAY-P is non-nil, not play a candidate.
If CLEARP is non-nil, add CAND\(s) after `emms-playlist-current-clear'."
  (if emms-stream-jp-radios-counsel--marked-cands
      (setq emms-stream-jp-radios-counsel--marked-cands
            (nreverse emms-stream-jp-radios-counsel--marked-cands))
    (push cand emms-stream-jp-radios-counsel--marked-cands))
  (with-current-emms-playlist
    (when clearp (emms-playlist-current-clear))
    (let ((len 0))
      (cl-loop for cand in emms-stream-jp-radios-counsel--marked-cands
               for stream = (emms-stream-jp-radios-counsel--get-tp-stream cand) do
               (setq emms-stream-last-stream stream)
               (funcall (intern (format "emms-add-%s" (emms-stream-type stream)))
                        (emms-stream-url stream))
               (cl-incf len))
      (goto-char (point-max))
      (cl-loop repeat len do (emms-playlist-previous))
      (unless (emms-playlist-track-at (point))
        (emms-playlist-first))
      (unless non-play-p
        (when (or (not emms-player-playing-p)
                  emms-stream-jp-radios-counsel-always-play-p)
          (emms-playlist-mode-play-smart)))))
  (emms-stream-jp-radios-counsel--unmark-all))
(byte-compile 'emms-stream-jp-radios-action-add-and-play)

(defun emms-stream-jp-radios-action-add (cand)
  "Add CAND\(s)."
  (emms-stream-jp-radios-action-add-and-play cand t))
(byte-compile 'emms-stream-jp-radios-action-add)

(defun emms-stream-jp-radios-action-clear-add-and-play (cand)
  "Clear, Add CAND\(s) and Play."
  (emms-stream-jp-radios-action-add-and-play cand nil t))
(byte-compile 'emms-stream-jp-radios-action-clear-add-and-play)

(defun emms-stream-jp-radios-action-clear-and-add (cand)
  "Clear, Add CAND\(s) and Play."
  (emms-stream-jp-radios-action-add-and-play cand t t))
(byte-compile 'emms-stream-jp-radios-action-clear-and-add)

(defun emms-stream-jp-radios-counsel-action (cand)
  "Default action for `emms-streams-jp-radios-counsel'.
CAND will be played."
  (let* ((stream (emms-stream-jp-radios-counsel--get-tp-stream cand))
         (type (emms-stream-type stream))
         (play-fn (intern (format "emms-play-%s" type))))
    (setq emms-stream-last-stream stream)
    (funcall play-fn (emms-stream-url stream))))
(byte-compile 'emms-stream-jp-radios-counsel-action)

(defun emms-stream-jp-radios-action-update-cache-async (_)
  "Update each stream list cache asynchronously."
  (run-with-idle-timer 0.01 nil #'emms-stream-jp-radios-update-cache-async)
  (setq emms-stream-jp-radios-counsel--marked-cands nil)
  (when (eq this-command 'ivy-dispatching-call) (minibuffer-keyboard-quit)))
(byte-compile 'emms-stream-jp-radios-action-update-cache-async)

(defun emms-stream-jp-radios-counsel--prompt ()
  "Update prompt for displaying marked number."
  (let ((prompt (ivy-add-prompt-count
                 (format "%s" (ivy-state-prompt ivy-last)))))
    (if emms-stream-jp-radios-counsel--marked-cands
        (concat prompt
                (format "M%s: " (length emms-stream-jp-radios-counsel--marked-cands)))
      prompt)))
(byte-compile 'emms-stream-jp-radios-counsel--prompt)

(defvar emms-streams-jp-radios-counsel-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-SPC") 'emms-stream-jp-radios-counsel--toggle-mark)
    (define-key map (kbd "M-a") 'emms-stream-jp-radios-counsel--mark-visible-cands)
    (define-key map (kbd "M-U") 'emms-stream-jp-radios-counsel--unmark-all)
    map))

(defvar emms-streams-jp-radios--counsel-history nil)

(defun emms-streams-jp-radios-counsel--collection (&rest _)
  "Collection function for `emms-streams-jp-radios-counsel'."
  (let ((emms-stream-jp-radios-counsel-collect-function-alist
         emms-stream-jp-radios-counsel--old-collect-function-alist)
        (emms-stream-jp-radios-counsel-use-emms-stream-list-p
         emms-stream-jp-radios-counsel--old-use-emms-stream-list-p))
    (emms-stream-jp-radios-counsel-list)))
(byte-compile 'emms-streams-jp-radios-counsel--collection)

(defun emms-streams-jp-radios-counsel ()
  "Select stream\(s) and Play."
  (interactive)
  (setq emms-stream-jp-radios-counsel--marked-cands nil)
  (setq emms-stream-jp-radios-counsel--old-collect-function-alist
        emms-stream-jp-radios-counsel-collect-function-alist)
  (setq emms-stream-jp-radios-counsel--old-use-emms-stream-list-p
        emms-stream-jp-radios-counsel-use-emms-stream-list-p)
  (ivy-set-prompt 'emms-streams-jp-radios-counsel
                  'emms-stream-jp-radios-counsel--prompt)
  (let ((ivy-height emms-stream-jp-radios-counsel-ivy-height))
    (ivy-read "stream: " 'emms-streams-jp-radios-counsel--collection
              :action 'emms-stream-jp-radios-counsel-action
              :caller 'emms-streams-jp-radios-counsel
              :keymap emms-streams-jp-radios-counsel-map
              :history 'emms-streams-jp-radios--counsel-history)))
(byte-compile 'emms-streams-jp-radios-counsel)

(ivy-set-actions
 'emms-streams-jp-radios-counsel
 '(("a" emms-stream-jp-radios-action-add-and-play "add stream\(s) and play")
   ("A" emms-stream-jp-radios-action-add "add stream(s)")
   ("c" emms-stream-jp-radios-action-clear-add-and-play "clear, add stream\(s) and play")
   ("C" emms-stream-jp-radios-action-clear-and-add "clear and add stream\(s)")
   ("u" emms-stream-jp-radios-action-update-cache-async "update streams asynchronously")))

(provide 'emms-streams-jp-radios-counsel)
;;; emms-streams-jp-radios-counsel.el ends here
