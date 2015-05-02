;;; emms-player-mpv-jp-radios.el --- EMMS players and stream lists of Japan radio stations -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: emms, mpv, radio
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (EMMS "4.0") (emms-player-simple-mpv "0.1.2"))
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

;; This provides EMMS players and stream lists of Japan radio stations.

;; All programs are available only in Japan.

;;; Code:
(require 'cl-lib)
(require 'emms-player-simple-mpv)

(defvar emms-player-mpv-jp-radios-list
  '("radiko"
    "radiru"
    "agqr"
    "onsen"
    "hibiki")
  "List of radio servece names.")

(defun emms-player-mpv-jp-radios--add-service-1 (name)
  "Helper function for emms-player-mpv-jp-radios-add-serviceNAME."
  (let ((player  (intern (concat "emms-player-mpv-" name)))
        (streams (intern (concat "emms-streams-"    name))))
    (let ((jp-radios-dir
           (cl-find "/emms-player-mpv-jp-radios/?$" load-path
                    :test #'string-match-p)))
      (unless jp-radios-dir
        (error "Not added to load-path : emms-player-mpv-jp-radios"))
      (unless (file-exists-p jp-radios-dir)
        (error "Not found : %s" jp-radios-dir))
      (add-to-list 'load-path
                   (expand-file-name name jp-radios-dir))
      (require player)
      (require streams)
      (add-to-list 'emms-player-list player))))

;;;###autoload
(defun emms-player-mpv-jp-radios-add-radiko ()
  "Add `emms-mpv-radiko' to `emms-player-list'."
  (emms-player-mpv-jp-radios--add-service-1 "radiko"))
;;;###autoload
(defun emms-player-mpv-jp-radios-add-radiru ()
  "Add `emms-mpv-radiru' to `emms-player-list'."
  (emms-player-mpv-jp-radios--add-service-1 "radiru"))
;;;###autoload
(defun emms-player-mpv-jp-radios-add-agqr ()
  "Add `emms-mpv-agqr' to `emms-player-list'."
  (emms-player-mpv-jp-radios--add-service-1 "agqr"))
;;;###autoload
(defun emms-player-mpv-jp-radios-add-onsen ()
  "Add `emms-mpv-onsen' to `emms-player-list'."
  (emms-player-mpv-jp-radios--add-service-1 "onsen"))
;;;###autoload
(defun emms-player-mpv-jp-radios-add-hibiki ()
  "Add `emms-mpv-hibiki' to `emms-player-list'."
  (emms-player-mpv-jp-radios--add-service-1 "hibiki"))

;;;###autoload
(defun emms-player-mpv-jp-radios-add-all ()
  "Add all EMMS jp radio players to `emms-player-list'."
  (dolist (name emms-player-mpv-jp-radios-list)
    (funcall (intern (concat "emms-player-mpv-jp-radios-add-" name)))))

(provide 'emms-player-mpv-jp-radios)
;;; emms-player-mpv-jp-radios.el ends here
