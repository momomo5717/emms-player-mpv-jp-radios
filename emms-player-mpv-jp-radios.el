;;; emms-player-mpv-jp-radios.el --- EMMS players and stream lists of Japan radio stations -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: emms, mpv, radio
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (emms-player-simple-mpv "0.1.2"))
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

(defconst emms-player-mpv-jp-radios-version "0.1.0")

(eval-and-compile
 (defvar emms-player-mpv-jp-radios-list
   '("radiko"
     "radiru"
     "agqr"
     "onsen"
     "hibiki")
   "Alist of radio serveces' name."))

(defmacro emms-player-mpv-jp-radios--define-add-fns ()
  "Define emms-player-mpv-jp-radios-add-serviceName."
  `(progn
     ,@(cl-loop
        for name in emms-player-mpv-jp-radios-list
        for player =  (intern (concat "emms-player-mpv-" name))
        for streams = (intern (concat "emms-streams-"    name))
        collect
        `(defun ,(intern (concat "emms-player-mpv-jp-radios-add-" name)) ()
           ,(format "Add `%s' to `emms-player-list'." player)
           (let ((jp-radios-dir
                  (cl-find "/emms-player-mpv-jp-radios/?$" load-path
                           :test #'string-match-p)))
             (unless jp-radios-dir
               (error "Not added to load-path : emms-player-mpv-jp-radios"))
             (unless (file-exists-p jp-radios-dir)
               (error "Not found : %s" jp-radios-dir))
             (add-to-list 'load-path
                          (expand-file-name ,name jp-radios-dir))
             (require ',player)
             (require ',streams)
             (add-to-list 'emms-player-list ',player))))))

(emms-player-mpv-jp-radios--define-add-fns)

(defun emms-player-mpv-jp-radios-add-all ()
  "Add all EMMS jp radio players to `emms-player-list'."
  (cl-loop for name in emms-player-mpv-jp-radios-list do
           (funcall (intern (concat "emms-player-mpv-jp-radios-add-" name)))))

(provide 'emms-player-mpv-jp-radios)
;;; emms-player-mpv-jp-radios.el ends here
