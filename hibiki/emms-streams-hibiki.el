;;; emms-streams-hibiki.el --- emms stream list for HiBiKi Radio Station  -*- lexical-binding: t -*-
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

;; This provides emms stream list for 響 - HiBiKi Radio Station -.

;; (require 'emms-streams-hibiki)

;;; Code:
(require 'emms-streams)
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'json)

(defvar emms-stream-hibiki--stream-alist-cache
  (cl-loop for i from 1 to 6 collect (list i))
  "Cache for stream alist.")

(defvar emms-stream-hibiki--base-url-programs
  "https://vcms-api.hibiki-radio.jp/api/v1//programs")

(defvar emms-stream-hibiki--dow-alist
  '((1 . "mon") (2 . "tue") (3 . "wed") (4 . "thu") (5 . "fri") (6 . "satsun")))

(defun emms-stream-hibiki--get-programs-url (dow)
  "Return programs url of DOW."
  (format "%s?day_of_week=%s"
          emms-stream-hibiki--base-url-programs dow))

(defun emms-stream-hibiki--url-to-json (url &optional buf)
  "Return a json object from URL.
If BUF is no-nil, it is used."
  (let ((buf (or buf (url-retrieve-synchronously url nil))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (let ((p (point))
            (p-max (point-max)))
        (prog1 (with-temp-buffer
                 (insert-buffer-substring-no-properties buf p p-max)
                 (decode-coding-region (point-min) (point-max) 'utf-8)
                 (goto-char (point-min))
                 (json-read))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(defun emms-stream-hibiki--json-program-to-streamlist (program)
  "Retrun streamlist from json PROGRAM."
  (let* ((name (cdr (assq 'name program)))
         (latest_episode_name (cdr (assq 'latest_episode_name program)) )
         (cast (cdr (assq 'cast program)))
         (access_id (cdr (assq 'access_id program)))
         (episode_updated_at (cdr (assq 'episode_updated_at program))))
    (list (format "%s %s %s: %s"
                  name latest_episode_name
                  (if (equal cast "") ""
                    (format ": %s " cast))
                  (if (stringp episode_updated_at)
                      (car (split-string episode_updated_at))
                    ""))
          (format "hibiki://%s" access_id)
          1 'streamlist)))

(defun emms-stream-hibiki--dow-to-stream-list (dow &optional buf)
  "Return json object for programs of DOW.
DOW is a number of 1-6.
If BUF is non-nil, it is used."
  (let* ((url (emms-stream-hibiki--get-programs-url dow))
         (programs (emms-stream-hibiki--url-to-json url buf)))
    (cl-loop for program across programs
             collect (emms-stream-hibiki--json-program-to-streamlist program))))

(defun emms-stream-hibiki--fetch-dow-stream-list (dow &optional updatep)
  "Retrun stream-list of DOW.
If UPDATEP is non-nil, cache of DOW is updated."
  (unless (and (< 0 dow) (< dow 7)) (error "DOW needs an integer of 1-6"))
  (let (dow-stream-list)
    (if (or updatep
            (null (setq dow-stream-list
                        (cdr (assq dow emms-stream-hibiki--stream-alist-cache)))))
        (progn
          (unless emms-stream-hibiki--stream-alist-cache
            (setq emms-stream-hibiki--stream-alist-cache
                  (cl-loop for i from 1 to 6 collect (list i))))
          (setcdr
           (assq dow emms-stream-hibiki--stream-alist-cache)
           (emms-stream-hibiki--dow-to-stream-list dow)))
      dow-stream-list)))

(defun emms-stream-hibiki--update-cache-async-1 (dow)
  "Update DOW stream-list cache."
  (if (and (< 0 dow) (< dow 7))
      (url-retrieve
       (emms-stream-hibiki--get-programs-url dow)
       (lambda (status &rest _)
         (when (memq :error status)
           (error "Failed to get hibiki stream list of %s : %s" dow (cdr status)))
         (unless emms-stream-hibiki--stream-alist-cache
           (setq emms-stream-hibiki--stream-alist-cache
                 (cl-loop for i from 1 to 6 collect (list i))))
         (setcdr (assq dow emms-stream-hibiki--stream-alist-cache)
                 (emms-stream-hibiki--dow-to-stream-list nil (current-buffer)))
         (emms-stream-hibiki--update-cache-async-1 (+ dow 6))))
    (when (cl-loop for ls in emms-stream-hibiki--stream-alist-cache
                   always (cdr ls))
      (message "Updated hibiki stream list cache"))))

(defun emms-stream-hibiki-update-cache-async ()
  "Update cache asynchronously."
  (setq emms-stream-hibiki--stream-alist-cache
        (cl-loop for i from 1 to 6 collect (list i)))
  (cl-loop for i from 1 to 6 do
           (emms-stream-hibiki--update-cache-async-1 i)))

(defun emms-stream-hibiki--add-bookmark-dows (nums &optional updatep)
  "Helper function for `emms-stream-hibiki-add-bookmark', etc.
Add stream list of NUMS.
If UPDATEP is non-nil, cache is updated."
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (unless (buffer-live-p buf)
      (error "%s is not a live buffer" emms-stream-buffer-name))
    (set-buffer buf))
  (let* ((line  (emms-line-number-at-pos (point)))
         (index (+ (/ line 2) 1)))
    (dolist (n nums)
      (dolist (stream (emms-stream-hibiki--fetch-dow-stream-list n updatep))
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun emms-stream-hibiki-get-stream-list ()
  "Return new stream-list from cache."
  (cl-loop
   with ls = nil
   for dow from 1 to 6 do
   (dolist (stream (cdr (assq dow emms-stream-hibiki--stream-alist-cache)))
     (push stream ls))
   finally return (nreverse ls)))

;;;###autoload
(defun emms-stream-hibiki-add-bookmark (&optional updatep dow)
  "Create hibiki bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
DOW is a number of 0-6 or -1.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (if (eq updatep -1) (emms-stream-hibiki-update-cache-async)
   (unless (integerp dow)
     (let ((msg (concat
                 "[0] All  [1] Mon  [2] Tue  [3] Wed  [4] Thu\n"
                 "         [5] Fri  [6] Sat/San\n"
                 "[-1] Update stream list cache asynchronously\n\n"
                 "Input a number of 0-6 or -1: ")))
       (while (not (and (integerp (setq dow (read-number msg)))
                        (<= -1 dow) (<= dow 6))))))
   (cond
    ((= dow -1) (emms-stream-hibiki-update-cache-async))
    ((zerop dow) (emms-stream-hibiki--add-bookmark-dows '(1 2 3 4 5 6) updatep))
    (t (emms-stream-hibiki--add-bookmark-dows (list dow) updatep)))))

(provide 'emms-streams-hibiki)
;;; emms-streams-hibiki.el ends here
