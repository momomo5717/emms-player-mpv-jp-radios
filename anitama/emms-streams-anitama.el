;;; emms-streams-anitama.el --- emms stream list for アニたまどっとコム -*- lexical-binding: t -*-

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

;; This provides emms stream list for アニたまどっとコム インターネットラジオ.

;; (require 'emms-streams-anitama)

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'url-cookie)

;; Suppress warning messages.
(defvar emms-stream-buffer-name)
(defvar emms-stream-list)
(declare-function emms-stream-redisplay "emms-streams")
(declare-function emms-line-number-at-pos "emms-streams")
(declare-function emms-stream-insert-at "emms-streams")

(defvar emms-stream-anitama--cookie-file
  (expand-file-name "weeeef_cookies" temporary-file-directory))

(defvar emms-stream-anitama--url-top
  "http://www.weeeef.com/weeeefww1/Transition?command=top&group=G0000049")

(defun emms-stream-anitama--access-weeeef ()
  "Access www.weeeef.com to get cookies."
  (let ((buf (url-retrieve-synchronously emms-stream-anitama--url-top)))
    (kill-buffer buf)))

(defun emms-stream-anitama--write-cookies ()
  "Write cookies to `emms-stream-anitama--cookie-file'."
  (let* ((weeeef-cookie (cl-find "www.weeeef.com" url-cookie-storage
                                 :key #'car :test #'equal))
         (domain (car weeeef-cookie))
         (file emms-stream-anitama--cookie-file))
    (unless weeeef-cookie (error "Not found cookies of www.weeeef.com"))
    (with-temp-buffer
      (dolist (cookie (cdr weeeef-cookie))
        (insert
         (mapconcat
          #'identity
          (list domain "FALSE" (url-cookie-localpart cookie) "FALSE" "0"
                (url-cookie-name  cookie) (url-cookie-value  cookie))
          (string 9)) ;; mapconcat with TAB
         "\n"))
      (write-region (point-min) (point-max) file nil 'nomessage))))

(defun emms-stream-anitama--have-cookies-p ()
  "Return non-nil, if `url-cookie-storage' has cookies."
  (> (length (cdr (cl-find "www.weeeef.com" url-cookie-storage
                           :key #'car :test #'equal)))
     1))

(defun emms-stream-anitama--write-unless-cookies (&optional forcep)
  "Access and write if `url-cookie-storage' doesn't have cookies.
Access and write if `emms-stream-anitama--cookie-file' doesn't exist
or the time of last acces is more than 1800 sec.
If FORCEP is non-nil, force to access and write."
  (unless (and (emms-stream-anitama--have-cookies-p)
               (file-exists-p emms-stream-anitama--cookie-file)
               (< (- (float-time (current-time))
                     (float-time (nth 5 (file-attributes
                                         emms-stream-anitama--cookie-file))))
                  1800)
               (not forcep))
    (emms-stream-anitama--access-weeeef)
    (unless (emms-stream-anitama--have-cookies-p)
      (error "Failed to get cookies of www.weeeef.com"))
    (emms-stream-anitama--write-cookies)))

;; For stream-list

(defvar emms-stream-anitama-stream-list-cache nil)

(defvar emms-stream-anitama--url-BookServlet
  "http://www.weeeef.com/weeeefww1/BookServlet")

(defun emms-stream-anitama--Book-to-KikanFrom-date (Book)
  "Return date from BOOK."
  (let* ((node Book)
         (date (dolist (name '(NodeData Node KikanFrom)
                             (car (xml-node-children node)))
                 (setq node (car (xml-get-children node name))))))
    (if (and (stringp date) (eq (length date) 8))
        (concat (cl-loop for c across date for i from 0
                         collect c
                         when (or (eq i 3) (eq i 5)) collect ?/))
      date)))

(defun emms-stream-anitama--bookservlet-xml-to-stream-list (bookservlet-xml)
  "Retrun stream list from BOOKSERVLET-XML."
  (let ((Books (xml-get-children bookservlet-xml 'Book)))
    (cl-loop for Book in Books
             for id    = (xml-get-attribute Book 'id)
             for label = (xml-get-attribute Book 'label)
             for date  = (emms-stream-anitama--Book-to-KikanFrom-date Book)
             collect (list (format "%s : %s" label date)
                           (format "anitama://%s" id) 1 'streamlist))))

(defun emms-stream-anitama-fetch-stream-list (&optional updatep)
  "Retrun anitama stream list.
If UPDATEP is no-nil, cache is updated."
  (if (and (not updatep) (consp emms-stream-anitama-stream-list-cache))
      emms-stream-anitama-stream-list-cache
    (emms-stream-anitama--write-unless-cookies t)
    (setq emms-stream-anitama-stream-list-cache
     (with-temp-buffer
       (unless (zerop (call-process
                       "wget"  nil t nil "-q" "-O" "-"
                       (format "--load-cookies=%s"
                               emms-stream-anitama--cookie-file)
                       emms-stream-anitama--url-BookServlet))
         (error "Failed to fetch %s" emms-stream-anitama--url-BookServlet))
       (emms-stream-anitama--bookservlet-xml-to-stream-list
        (libxml-parse-xml-region (point-min) (point-max)))))))

(defun emms-stream-anitama--write-weeeef-async (&optional cont)
  "Access and write www.weeeef.com cookies asynchronously.
If CONT is no-nil, it is run with no arguments."
  (url-retrieve
   emms-stream-anitama--url-top
   (lambda (status &rest _)
     (when (memq :error status)
       (error "Failed to write www.weeeef.com cookies : %s" (cdr status)))
     (kill-buffer)
     (emms-stream-anitama--write-cookies)
     (unless (emms-stream-anitama--have-cookies-p)
       (error "Failed to get cookies of www.weeeef.com"))
     (when (functionp cont) (funcall cont)))))

(defun emms-stream-anitama-update-cache-async ()
  "Update cache asynchronously."
  (cl-labels
      ((stream-list-async-filter (proc _)
        (let ((buf (process-buffer proc))
              (ps (process-status proc)))
          (when (eq ps 'signal)
            (error "Failed to fetch anitama stream list"))
          (when (eq ps 'exit)
            (with-current-buffer buf
              (goto-char (point-max))
              (search-backward ">" (point-min) t)
              (goto-char (match-end 0))
              (setq emms-stream-anitama-stream-list-cache
                    (emms-stream-anitama--bookservlet-xml-to-stream-list
                     (libxml-parse-xml-region (point-min) (point))))
              (unless emms-stream-anitama-stream-list-cache
                (error "Failed to read anitama xml"))
              (kill-buffer)
              (message "Updated anitama stream list cache"))))))
    (emms-stream-anitama--write-weeeef-async
     (lambda () (set-process-sentinel
             (start-process "anitama-fetch-stream-list-async"
                            (make-temp-name "*anitama-fetch-stream-list-async*")
                            "wget" "-q" "-O" "-"
                            (format "--load-cookies=%s"
                                    emms-stream-anitama--cookie-file)
                            emms-stream-anitama--url-BookServlet)
             #'stream-list-async-filter)))))

(defun emms-stream-anitama-get-stream-list ()
  "Return new stream-list from cache."
  (cl-copy-list emms-stream-anitama-stream-list-cache))

;;;###autoload
(defun emms-stream-anitama-add-bookmark (&optional updatep)
  "Create anitama bookmarks, and insert it at point position.
If UPDATEP is no-nil, cache is updated.
If UPDATEP is -1, cache is updated asynchronously.

If save,run `emms-stream-save-bookmarks-file' after."
  (interactive "P")
  (unless (featurep 'emms-streams) (require 'emms-streams))
  (if (eq updatep -1)
      (emms-stream-anitama-update-cache-async)
   (let ((buf (get-buffer emms-stream-buffer-name)))
     (unless (buffer-live-p buf)
       (error "%s is not a live buffer" emms-stream-buffer-name))
     (set-buffer buf))
   (let* ((stream-list (emms-stream-anitama-fetch-stream-list updatep))
          (line       (emms-line-number-at-pos (point)))
          (index      (+ (/ line 2) 1)))
     (dolist (stream stream-list)
       (setq emms-stream-list (emms-stream-insert-at index stream
                                                     emms-stream-list))
       (cl-incf index))
     (emms-stream-redisplay)
     (goto-char (point-min))
     (forward-line (1- line)))))

;; For media player

(defvar emms-stream-anitama-url-BookXmlGet
  "http://www.weeeef.com/weeeefww1/BookXmlGet")

(defun emms-stream-anitama-get-BookXmlGet-id-url (id)
  "Add Book ID to BookXmlGet."
  (format "%s?BookId=%s"
          emms-stream-anitama-url-BookXmlGet id))

(defvar emms-stream-anitama-url-OriginalGet
  "http://www.weeeef.com/weeeefww1/OriginalGet")

(defun emms-stream-anitama--fetch-BookXmlGet-nodeId (id)
  "Return nodeId from ID."
  (emms-stream-anitama--write-unless-cookies)
  (let*
      ((BookXmlGet-xml
        (with-temp-buffer
          (unless
              (zerop
               (call-process
                "wget"  nil t nil "-q" "-O" "-"
                (format "--load-cookies=%s" emms-stream-anitama--cookie-file)
                (emms-stream-anitama-get-BookXmlGet-id-url id)))
            (error "Failed to fetch %s" emms-stream-anitama-url-BookXmlGet))
          (libxml-parse-xml-region (point-min) (point-max))))
       (Node (cl-loop
              with Node = (car (xml-get-children BookXmlGet-xml 'Node))
              with nextNode = (car (xml-get-children Node 'Node))
              while nextNode do
              (setq Node nextNode)
              (setq nextNode (car (xml-get-children Node 'Node)))
              finally return Node))
       (nodeId (car (xml-node-children (car (xml-get-children Node 'Id))))))
    nodeId))

(defun emms-stream-anitama-stream-url-to-wget-form (stream-url)
  "Return wget form from STREAM-URL."
  (let* ((id (replace-regexp-in-string "\\`anitama://" "" stream-url))
         (nodeId (emms-stream-anitama--fetch-BookXmlGet-nodeId id)))
    (unless nodeId (error "Failed to fetch nodeId"))
    (mapconcat
     #'shell-quote-argument
     (list "wget" "-O" "-" "-q"
           (format "--load-cookie=%s" emms-stream-anitama--cookie-file)
           (format "--post-data=nodeId=%s&type=S" nodeId)
           emms-stream-anitama-url-OriginalGet)
     " ")))

(provide 'emms-streams-anitama)
;;; emms-streams-anitama.el ends here
