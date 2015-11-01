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

(defvar emms-stream-hibiki--stream-alist-cache
  (cl-loop for i from 1 to 6 collect (list i))
  "Cache for stream alist.")

(cl-defun emms-stream-hibiki--xml-collect-node
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

(defun emms-stream-hibiki--url-to-html (url &optional xmlp buf)
  (let ((buf (or buf (url-retrieve-synchronously url))))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eolp))) (forward-line 1))
      (unless (eobp) (forward-line 1))
      (unwind-protect (funcall (if xmlp #'libxml-parse-xml-region
                                 #'libxml-parse-html-region) (point) (point-max))
        (kill-buffer buf)))))

(defun emms-stream-hibiki--fetch-description (program-id)
  "Return description of PROGRAM-ID."
  (emms-stream-hibiki--url-to-html
   (format "http://image.hibiki-radio.jp/uploads/data/channel/%s/description.xml"
           program-id)
   'xml))

(defun emms-stream-hibiki--div-hbkProgram-to-streamlist (div-hbkProgram)
    "Return stream from DIV-HBKPROGRAMS."
    (let* ((a (car (xml-get-children div-hbkProgram 'a)))
           (AttachVideo
            (cdr (split-string (xml-get-attribute a 'onclick) "[()',]" t)))
           (program-id  (cl-first AttachVideo))
           (program-key (cl-second AttachVideo))
           (program-comment
            (car (emms-stream-hibiki--xml-collect-node
                  'div a
                  :test (lambda (node) (equal (xml-get-attribute-or-nil node 'class)
                                          "hbkProgramComment"))
                  :getter (lambda (node) (car (xml-node-children node))))))
           (data (emms-stream-hibiki--fetch-description program-id))
           (title
            (car (xml-node-children (car (xml-get-children data 'title)))))
           (cast
            (mapcar (lambda (node) (car (xml-node-children node)))
                    (xml-get-children (car (xml-get-children data 'cast)) 'name)))
           (cast-names (if cast (concat " : " (mapconcat #'identity cast ", ")) "")))
      (list (format "%s%s : %s" title cast-names program-comment)
            (format "hibiki://%s/%s" program-id program-key)
            1 'streamlist)))

(defun emms-stream-hibiki--html-to-stream-list (n html)
  "Return stream list of HTML."
  (let ((div-hbkPrograms
         (emms-stream-hibiki--xml-collect-node
          'div html
          :test
          (lambda (node) (equal (xml-get-attribute-or-nil node 'class)
                            "hbkProgram")))))
    (setcdr (assq n emms-stream-hibiki--stream-alist-cache)
            (mapcar #'emms-stream-hibiki--div-hbkProgram-to-streamlist
                    div-hbkPrograms))))

(defun emms-stream-hibiki--fetch-stream-list-dow (n &optional updatep)
  "Retrun streamlist of N\(DOW\).
If UPDATEP is non-nil, cache is updated."
  (unless (and (< 0 n) (< n 7)) (error "N needs an integer of 1-6"))
  (let (stream-list-dow)
    (if (or updatep
            (null (setq stream-list-dow
                        (cdr (assq n emms-stream-hibiki--stream-alist-cache)))))
        (progn
          (unless emms-stream-hibiki--stream-alist-cache
            (setq emms-stream-hibiki--stream-alist-cache
                  (cl-loop for i from 1 to 6 collect (list i))))
         (emms-stream-hibiki--html-to-stream-list n
          (emms-stream-hibiki--url-to-html
           (format "http://hibiki-radio.jp/get_program/%d" n))))
      stream-list-dow)))

(defun emms-stream-hibiki--update-cache-async-1 (n)
  "Helper function for `emms-stream-hibiki-update-cache-async' for N."
  (if (and (< 0 n) (< n 7))
      (url-retrieve
       (format "http://hibiki-radio.jp/get_program/%d" n)
       (lambda (status &rest _)
         (when (memq :error status)
           (error "Failed to get hibiki stream list of %s : %s" n (cdr status)))
         (let* ((html (emms-stream-hibiki--url-to-html
                       nil nil (current-buffer)))
                (div-hbkPrograms
                 (emms-stream-hibiki--xml-collect-node
                  'div html
                  :test
                  (lambda (node) (equal (xml-get-attribute-or-nil node 'class)
                                    "hbkProgram")))))
           (emms-stream-hibiki--div-hbkPrograms-to-stream-list-async n div-hbkPrograms)))
       nil (if (eq n 1) nil t))
    (when (cl-loop for ls in emms-stream-hibiki--stream-alist-cache
                   always (cdr ls))
      (message "Updated hibiki stream list cache"))))

(defun emms-stream-hibiki--div-hbkPrograms-to-stream-list-async (n div-hbkPrograms)
  "Update Nth cache of DIV-HBKPROGRAMS asynchronously."
  (cl-labels
      ((update-nth-list (div-hbkPrograms streams)
        (if div-hbkPrograms
            (let* ((div-hbkProgram (car div-hbkPrograms))
                   (a (car (xml-get-children div-hbkProgram 'a)))
                   (AttachVideo
                    (cdr (split-string (xml-get-attribute a 'onclick) "[()',]" t)))
                   (program-id  (cl-first AttachVideo))
                   (program-key (cl-second AttachVideo))
                   (program-comment
                    (car (emms-stream-hibiki--xml-collect-node
                          'div a
                          :test (lambda (node) (equal (xml-get-attribute-or-nil node 'class)
                                                  "hbkProgramComment"))
                          :getter (lambda (node) (car (xml-node-children node)))))))
              (url-retrieve
               (format "http://image.hibiki-radio.jp/uploads/data/channel/%s/description.xml"
                       program-id)
               (lambda (status &rest _)
                 (when (memq :error status)
                   (error "Failed to get hibiki %s description : %s"
                          program-id (cdr status)))
                 (let* ((data (emms-stream-hibiki--url-to-html nil 'xml
                                                               (current-buffer)))
                        (title
                         (car (xml-node-children (car (xml-get-children data 'title)))))
                        (cast
                         (mapcar (lambda (node) (car (xml-node-children node)))
                                 (xml-get-children (car (xml-get-children data 'cast)) 'name)))
                        (cast-names (if cast (concat " : " (mapconcat #'identity cast ", ")) "")))
                   (update-nth-list
                    (cdr div-hbkPrograms)
                    (cons (list (format "%s%s : %s" title cast-names program-comment)
                                (format "hibiki://%s/%s" program-id program-key)
                                1 'streamlist)
                          streams))))
               nil t))
          (setcdr (assq n emms-stream-hibiki--stream-alist-cache)
                  (nreverse streams))
          (emms-stream-hibiki--update-cache-async-1 (+ n 6)))))
    (update-nth-list div-hbkPrograms nil)))

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
      (dolist (stream (emms-stream-hibiki--fetch-stream-list-dow n updatep))
        (setq emms-stream-list (emms-stream-insert-at index stream
                                                      emms-stream-list))
        (cl-incf index)))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun emms-stream-hibiki-add-bookmark (&optional updatep dow)
  "Create hibiki bookmark, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.
DOW is a number of 0-6 or -1.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (if (eq updatep -1)(emms-stream-hibiki-update-cache-async)
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
