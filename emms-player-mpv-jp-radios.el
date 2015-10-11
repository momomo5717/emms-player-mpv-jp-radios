;;; emms-player-mpv-jp-radios.el --- EMMS players and stream lists of Japan radio stations -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: emms, mpv, radio
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (emms "4.0") (emms-player-simple-mpv "0.1.7"))
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
;;
;; * Available only in Japan due to access restriction
;;     Radiko, らじる★らじる, 超！A&G+, 音泉, 響, アニたまどっとコム
;;
;; * Available anywhere
;;     SaimaruRadio, ListenRadio, Sea Side Communications

;; Other Requirements:
;;
;; + mpv v0.7 or later
;;   + ffmpeg ( the build with –enable-librtmp (for Radiko, らじる★らじる) )
;; + wget (for Radiko, アニたまどっとコム)
;; + swftools (for Radiko)

;; Setup:
;;
;; (add-to-list 'load-path "/path/to/emms-player-mpv-jp-radios")
;; (require 'emms-player-mpv-jp-radios)
;;
;; ;; Adding all emms jp radio players
;; (emms-player-mpv-jp-radios-add-all)
;;
;; ;; Adding separetely
;; ;; e.g. Only for Radiko and らじる★らじる
;; (emms-player-mpv-jp-radios-add "radiko" "radiru")

;; Usage:
;;
;; M-x emms-streams
;; ;; serviceName is radiko, radiru, etc.
;; M-x emms-stream-serviceName-add-bookmark

;;; Code:
(require 'cl-lib)
(require 'emms-player-simple-mpv)

(defvar emms-player-mpv-jp-radios-list
  '("radiko"
    "radiru"
    "agqr"
    "onsen"
    "hibiki"
    "anitama"
    "saimaru"
    "listen"
    "seaside")
  "List of radio servece names.")

(defvar emms-player-mpv-jp-radios--dir
  (file-name-directory (or load-file-name (buffer-file-name))))

;;;###autoload
(defun emms-player-mpv-jp-radios-add (&rest names)
  "Add emms simple players for NAMES to `emms-player-list'."
  (dolist (name names)
    (let ((player   (intern (concat "emms-player-mpv-" name)))
          (streams  (intern (concat "emms-streams-"    name)))
          (dir-name (expand-file-name name emms-player-mpv-jp-radios--dir)))
      (add-to-list 'load-path dir-name)
      (require player)
      (require streams)
      (add-to-list 'emms-player-list player))))

;;;###autoload
(defun emms-player-mpv-jp-radios-add-all ()
  "Add all EMMS jp radio players to `emms-player-list'."
  (apply #'emms-player-mpv-jp-radios-add emms-player-mpv-jp-radios-list))

(provide 'emms-player-mpv-jp-radios)
;;; emms-player-mpv-jp-radios.el ends here
