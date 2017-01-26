;;; emms-streams-jp-radios.el --- emms-streams for jp radio stations -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 momomo5717

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

;; This packages provides utility functions and emms-streams for jp radio stations.

;; Setup:
;;
;; (require 'emms-streams-jp-radios)
;;
;; ;; `emms-stream-jp-radios-track-description' can be used for
;; ;; `emms-track-description-function'.
;; (custom-set-variables
;;  '(emms-track-description-function
;;    'emms-stream-jp-radios-track-description))
;;
;; `emms-stream-jp-radios-playlist-current' and
;; `emms-stream-jp-radios-mode-line-icon-function'
;;  can be used for `emms-mode-line-mode-line-function'.
;;
;; `emms-stream-jp-radios-current-title-function' can be used for
;; `emms-mode-line-cycle-current-title-function'.
;;
;; Usage:
;;
;; M-x `emms-stream-jp-radios-update-cache-async' can update cache asynchronously.
;;
;; M-x `emms-streams-jp-radios' switches to a new buffer.
;; or
;; M-x `emms-stream-jp-radios-popup' displays a pop-up window.
;;

;;; Code:
(require 'emms-player-mpv-jp-radios)
(require 'emms)
(require 'emms-streams)
(require 'cl-lib)

;;;###autoload
(defun emms-stream-jp-radios-update-cache-async ()
    "Update each stream list or alist asynchronously."
    (interactive)
    (cl-loop for station in emms-player-mpv-jp-radios-list
             for fn = (intern (format "emms-stream-%s-update-cache-async" station))
             when (fboundp fn) do (funcall fn)))

;;;###autoload
(defun emms-stream-jp-radios-track-description (track)
  "Return TRACK description for `emms-track-description-function'.
Reference : `emms-track-simple-description'."
  (let ((type (emms-track-type track)))
      (cond ((eq 'file type)
             (file-name-nondirectory (emms-track-name track)))
            ((eq 'url type)
             (emms-format-url-track-name (emms-track-name track)))
            ((eq 'playlist type)
             (concat (symbol-name type)
                     ": "
                     (file-name-nondirectory (emms-track-name track))))
            ((eq 'streamlist type)
             (concat (symbol-name type)
                     ": "
                     (let ((stream-name (emms-stream-name
                                         (emms-track-get track 'metadata))))
                       (if stream-name stream-name (emms-track-name track)))))
            (t (concat (symbol-name type)
                       ": " (emms-track-name track))))))

;;;###autoload
(defun emms-stream-jp-radios-current-title-function ()
  "This can be used for `emms-mode-line-cycle-current-title-function'."
  (let* ((track (emms-playlist-current-selected-track))
         (description (emms-track-description track)))
    (cl-case (emms-track-type track)
      ((streamlist)
       (let ((stream-name (emms-stream-name
                           (emms-track-get track 'metadata))))
         (if stream-name stream-name description)))
      ((url) (emms-format-url-track-name (emms-track-name track)))
      ((playlist) (file-name-nondirectory (emms-track-name track)))
      (t description))))

(defvar emms-mode-line-format)
;;;###autoload
(defun emms-stream-jp-radios-playlist-current ()
  "This can be used for `emms-mode-line-mode-line-function'."
  (format emms-mode-line-format (emms-stream-jp-radios-current-title-function)))

(defvar emms-mode-line-icon-before-format)
(defvar emms-mode-line-icon-image-cache)
;;;###autoload
(defun emms-stream-jp-radios-mode-line-icon-function ()
  "This can be used for `emms-mode-line-mode-line-function'.
This need \(require 'emms-mode-line-icon\) before using it."
  (concat " "
          emms-mode-line-icon-before-format
          (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-stream-jp-radios-playlist-current)))


;; emms-stream-jp-radios-mode

(defgroup emms-streams-jp-radios nil
  "EMMS Streams JP Radio Stations"
  :group 'emms
  :prefix "emms-stream-jp-radios-"
  :prefix "emms-streams-jp-radios-")

(defcustom emms-stream-jp-radios-get-stream-list-alist
  (cl-loop for station in emms-player-mpv-jp-radios-list
           collect (cons station
                         (intern (format "emms-stream-%s-get-stream-list"
                                         station))))
  "Each function returns new stream list."
  :group 'emms-stream-jp-radios
  :type '(alist :key-type string :value-type function))

(defcustom emms-stream-jp-radios-hook nil
  "Hook run after entering emms-stream-jp-radios-mode."
  :group 'emms-stream-jp-radios
  :type 'hook)

(defcustom emms-stream-jp-radios-popup-height 0
  "Height for pop-up window.
If it is more than 0 or equal to 1, it is used."
  :group 'emms-stream-jp-radios
  :type 'number)

(defcustom emms-stream-jp-radios-popup-ratio 0.3
  "Ratio to frame height for calculating pop-up window height."
  :group 'emms-stream-jp-radios
  :type 'number)

(defvar emms-stream-jp-radios-buffer-name "*EMMS JP Radios*"
  "Buffer name for emms-streams-jp-radios.")

(defvar emms-stream-jp-radios-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emms-stream-mode-map)
    ;; Set `emms-stream-save-bookmarks-file' to nil
    (dolist (com '(emms-stream-add-bookmark
                   emms-stream-delete-bookmark
                   emms-stream-edit-bookmark
                   emms-stream-save-bookmarks-file
                   emms-stream-play))
      (dolist (key (where-is-internal com emms-stream-mode-map))
        (define-key map key nil)))
    (define-key map [return] 'emms-stream-jp-radios-play)
    (define-key map [S-return] 'emms-stream-jp-radios-add)
    (dolist (key (where-is-internal 'emms-stream-quit emms-stream-mode-map))
      (define-key map key 'emms-stream-jp-radios-quit))
    map))

(defvar emms-stream-jp-radios--key-tab nil)
(defvar emms-stream-jp-radios--poped-up-win nil)

;;;###autoload
(defun emms-streams-jp-radios ()
  "Switch to a new buffer for `emms-stream-jp-radios-mode'."
  (interactive)
  (kill-buffer (get-buffer-create emms-stream-jp-radios-buffer-name))
  (setq emms-stream-jp-radios--poped-up-win nil)
  (set-buffer (get-buffer-create emms-stream-jp-radios-buffer-name))
  (erase-buffer)
  (when (or (null emms-playlist-buffer)
            (not (buffer-live-p emms-playlist-buffer)))
    (emms-playlist-set-playlist-buffer (emms-playlist-new)))
  (emms-stream-jp-radios-mode)
  (when (and (eq (lookup-key emms-stream-jp-radios-mode-map [tab])
                 'emms-stream-jp-radios-quit)
         (not (eq emms-stream-jp-radios--key-tab 'emms-stream-jp-radios-quit)))
    (define-key emms-stream-jp-radios-mode-map [tab]
      emms-stream-jp-radios--key-tab))
  (switch-to-buffer emms-stream-jp-radios-buffer-name))

(defun emms-stream-jp-radios-mode ()
  "Switch to `emms-stream-jp-radios-mode'."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'emms-stream-jp-radios-mode)
  (setq mode-name "EMMS JP Radios")
  (use-local-map emms-stream-jp-radios-mode-map)
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'auto-hscroll-mode) t)
  (set (make-local-variable 'kill-whole-line) t)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (set (make-local-variable 'font-lock-fontify-buffer-function)
       'jit-lock-refontify)
  (goto-char (point-min))
  (emms-stream-jp-radios-display)
  (set (make-local-variable 'buffer-read-only) t)
  (set-buffer-modified-p nil)
  (run-hooks 'emms-stream-jp-radios-hook)
  (message "EMMS Streams JP Radios Menu"))

;;;###autoload
(defun emms-stream-jp-radios-display ()
  "Insert stream lists of each station."
  (when (eq major-mode 'emms-stream-jp-radios-mode)
   (let ((stream-list nil))
     (cl-loop
      for (_ . get-fn) in (reverse emms-stream-jp-radios-get-stream-list-alist)
      when (fboundp get-fn) do
      (setq stream-list (nconc (funcall get-fn) stream-list)))
     (mapc #'emms-stream-display-line stream-list)
     (goto-char (point-min)))))

;;;###autoload
(defun emms-stream-jp-radios-popup (&optional popup-height)
  "Display `emms-stream-jp-radios-buffer-name' in a pop-up window.
If POPUP-HEIGHT is non-nil, it is used for the height."
  (interactive "P")
  (let* ((buf-live-p (buffer-live-p (get-buffer emms-stream-jp-radios-buffer-name)))
         (buf (get-buffer-create emms-stream-jp-radios-buffer-name))
         (win (display-buffer
               buf
               `((display-buffer-reuse-window
                  display-buffer-at-bottom
                  display-buffer-pop-up-window)
                 ,`(window-height .
                    ,(cond ((integerp popup-height) popup-height)
                           ((>= (floor emms-stream-jp-radios-popup-height) 1))
                           (t (floor (* (frame-height)
                                       emms-stream-jp-radios-popup-ratio)))))))))
    (set-buffer buf)
    (unless buf-live-p
      (erase-buffer)
      (when (or (null emms-playlist-buffer)
                (not (buffer-live-p emms-playlist-buffer)))
        (emms-playlist-set-playlist-buffer (emms-playlist-new)))
      (emms-stream-jp-radios-mode))
    (unless (eq (window-frame win) (selected-frame))
      (select-frame-set-input-focus (window-frame win)))
    (select-window win)
    (setq emms-stream-jp-radios--poped-up-win win)
    (unless (eq (lookup-key emms-stream-jp-radios-mode-map [tab])
                'emms-stream-jp-radios-quit)
      (setq emms-stream-jp-radios--key-tab
       (lookup-key emms-stream-jp-radios-mode-map [tab])))
    (define-key emms-stream-jp-radios-mode-map
      [tab] 'emms-stream-jp-radios-quit)))

;;;###autoload
(defun emms-stream-jp-radios-quit ()
  "Bury `emms-stream-jp-radios-buffer-name' buffer."
  (interactive)
  (let* ((buf (get-buffer emms-stream-jp-radios-buffer-name))
         (buf-live-p (buffer-live-p buf)))
    (when buf-live-p
      (quit-windows-on buf)
      (unless emms-stream-jp-radios--poped-up-win
        (when (buffer-live-p emms-playlist-buffer)
          (switch-to-buffer emms-playlist-buffer)))
      (setq emms-stream-jp-radios--poped-up-win nil))))

(defun emms-stream-jp-radios--play-1 (play-or-add)
  "Helper function `emms-stream-jp-radios-play'."
  (when (eq major-mode 'emms-stream-jp-radios-mode)
    (let* ((line  (or (get-text-property (point) 'emms-stream)
                      (progn
                        (goto-char (or (previous-single-property-change
                                        (point) 'emms-stream)
                                       (point-min)))
                        (goto-char (or (previous-single-property-change
                                        (point) 'emms-stream)
                                       (point-min)))
                        (get-text-property (point) 'emms-stream))
                      (error "No stream found at point")))
           (url   (emms-stream-url  line))
           (type  (emms-stream-type line)))
      (setq emms-stream-last-stream line)
      (funcall (intern (format "emms-%s-%s" play-or-add type)) url))))

;;;###autoload
(defun emms-stream-jp-radios-play ()
  "Play stream at the point."
  (interactive)
  (emms-stream-jp-radios--play-1 "play"))

;;;###autoload
(defun emms-stream-jp-radios-add ()
  "Add stream at the point to playlist."
  (interactive)
  (emms-stream-jp-radios--play-1 "add"))

(provide 'emms-streams-jp-radios)
;;; emms-streams-jp-radios.el ends here
