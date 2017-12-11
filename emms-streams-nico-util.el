;;; emms-streams-nico-util.el --- Utility for nicovideo -*- lexical-binding: t -*-

;; Copyright (C) 2017 momomo5717

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

;; This package provides utility functions for nicovideo.

;; Setup:
;;
;; (require 'emms-streams-nico-util)

;;; Code:
(require 'emms-streams-jp-radios-util)

(defvar emms-stream-nico-use-old-api nil)

(defvar emms-stream-nico-url-user-agent
  (format "User-Agent: %s\r\n"
          "Mozilla/5.0")
  "User-Agent for nicovideo.
This will be ussed for `emms-stream-nico-url-http-user-agent-string'.")

;;;###autoload
(defun emms-stream-nico-url-http-user-agent-string ()
  "`url-http-user-agent-string' for nicovideo."
  emms-stream-nico-url-user-agent)

;;;###autoload
(defmacro emms-stream-nico-with-user-agent (&rest body)
  "Run BODY with `emms-stream-nico-url-http-user-agent-string'."
  (declare (indent 0) (debug (form body)))
  ;; Memo: Emacs 24.x doesn't have `url-user-agent'.
  `(cl-letf (((symbol-function 'url-http-user-agent-string)
              'emms-stream-nico-url-http-user-agent-string))
     ,@body))

(defvar emms-stream-nico--last-watchid nil)

;;;###autoload
(defun emms-stream-nico-url-to-watchid (url)
  "Return WatchId of URL."
  (if (string-match "nicovideo\\.jp/watch/\\([^?]*\\)" url)
      (match-string 1 url)
    (error "Failed to get WatchId")))

(defvar emms-stream-nico--embed-url
  "https://embed.nicovideo.jp")

(defun emms-stream-nico--embed-watch-url (watchid)
  "Return https://embed.nicovideo.jp/watch/WATCHID."
  (concat emms-stream-nico--embed-url "/watch/" watchid))

(defvar emms-stream-nico--data-props nil)
(defvar emms-stream-nico--data-style-map nil)
(defvar emms-stream-nico--data-keyframes-map nil)

(defun emms-stream-nico--fetch-data-props (watchid)
  "Return data-props json object fron WATCHID."
  (emms-stream-nico-with-user-agent
    (let* ((html (emms-stream-jpr-url-to-html
                  (emms-stream-nico--embed-watch-url watchid)))
           (div (car (emms-stream-jpr-get-node
                      html 'div
                      :test (lambda (node) (equal (xml-get-attribute node 'id)
                                              "ext-player")))))
           (data-props (xml-get-attribute-or-nil div 'data-props))
           (data-style-map (xml-get-attribute-or-nil div 'data-style-map))
           (data-keyframes-map (xml-get-attribute-or-nil div 'data-keyframes-map)))
      (setq emms-stream-nico--data-style-map (json-read-from-string data-style-map)
            emms-stream-nico--data-keyframes-map (json-read-from-string data-keyframes-map)
            emms-stream-nico--data-props (json-read-from-string data-props)))))

(defvar emms-stream-nico--play-res nil)

(defun emms-stream-nico--embed-play-url (watchid)
  "Return https://embed.nicovideo.jp/play/WATCHID."
  (let ((serviceUserId (cdr (assq 'serviceUserId emms-stream-nico--play-res))))
    (concat emms-stream-nico--embed-url "/play/" watchid "?parent="
             (if serviceUserId
                 (concat "&serviceUserId=" (url-hexify-string serviceUserId))
               ""))))

(defun emms-stream-nico--fetch-play (watchid)
  "Return a json as response from play/WATCHID."
  (emms-stream-nico-with-user-agent
    (let* ((data-props emms-stream-nico--data-props)
           (cookie
            (format "nicoid=%s" (cdr (assq 'nicosid data-props))))
           (url-request-extra-headers
            `(("Accept" . "*/*")
              ("Referer" . ,(emms-stream-nico--embed-watch-url watchid))
              ("Cookie" . ,cookie)))))
    (setq emms-stream-nico--play-res
          (emms-stream-jpr-url-to-json
           (emms-stream-nico--embed-play-url watchid)))))

(defun emms-stream-nico--guest_watch-query-list ()
  "Return guest_watch query list.
Using `emms-stream-nico--data-props' and `emms-stream-nico--play-res'."
  (cl-labels
      ((assqv (key als)
              (let ((v (cdr (assq key als))))
                (if (vectorp v)
                    (mapconcat (lambda (x) (format "%s" x)) v ",")
                  (format "%s" v)))))
    (let ((data-props emms-stream-nico--data-props)
          (play-res emms-stream-nico--play-res))
      `(("frontend_id" ,(assqv 'frontendId data-props))
        ("videos" ,(assqv 'videos play-res))
        ("audios" ,(assqv 'audios play-res))
        ("protocols" "http")
        ("skips" ,(assqv 'skips play-res))
        ("content_key_timeout" ,(assqv 'contentKeyTimeout play-res))
        ("signature" ,(assqv 'watchApiSignature play-res))
        ("action_track_id" ,(assqv 'actionTrackId data-props))
        ("increment_view_counter" "true")
        ("is_https" "false")
        ("ver" "1")
        ("service_user_id" ,(assqv 'serviceUserId play-res))))))

(defun emms-stream-nico--guest_watch-url (watchid)
  "Return guest_watch/WATCHID?query-parameters."
  ;; Memo: "http://www.nicovideo.jp/api/guest_watch/"
  (concat "https://watchapi.nicovideo.jp/api/guest_watch/" watchid "?"
          (url-build-query-string (emms-stream-nico--guest_watch-query-list))))

(defvar emms-stream-nico--guest_watch-res nil)

(defun emms-stream-nico--fetch-guest_watch (watchid)
  "Retrun a json as response from api/guest_watch/WATCHID."
  (emms-stream-nico-with-user-agent
    (let ((url-request-extra-headers
           `(("Origin" . ,emms-stream-nico--embed-url)
             ("Accept" . "*/*")
             ("Referer" . ,(emms-stream-nico--embed-watch-url watchid)))))
      (setq emms-stream-nico--guest_watch-res
            (emms-stream-jpr-url-to-json
             (emms-stream-nico--guest_watch-url watchid))))))

(defun emms-stream-nico--getthreadkey-url (watchid)
  "Return getthreadkey url with WATCHID."
  (concat "https://flapi.nicovideo.jp/api/getthreadkey?thread="
          watchid))

(defun emms-stream-nico--search-getthreadkey ()
  "Return threadkey."
  (let (beg end)
    (if (not (re-search-forward "threadkey=.*$" nil t))
        (error "Failed to search threadkey")
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (car (assoc-default
            "threadkey"
            (mapcar (lambda (str) (split-string str "="))
                    (split-string (buffer-substring beg end) "&")))))))

(defvar emms-stream-nico--threadkey nil)

(defun emms-stream-nico--fetch-getthreadkey (watchid)
  "Return threadkey from WATCHID."
  (emms-stream-nico-with-user-agent
    (let* ((url-request-extra-headers
            `(("Accept" . "*/*")
              ("Referer" . ,(emms-stream-nico--embed-watch-url watchid))
              ("Origin" . ,emms-stream-nico--embed-url)))
           (buf (url-retrieve-synchronously
                 (emms-stream-nico--getthreadkey-url watchid))))
      (with-current-buffer buf
        (goto-char (point-min))
        (setq emms-stream-nico--threadkey
              (emms-stream-nico--search-getthreadkey)))
      (when (buffer-live-p buf) (kill-buffer buf))
      emms-stream-nico--threadkey)))

(defvar emms-stream-nico--session-url nil
  "URL like <http://api.dmc.nico:2805/api/sessions>.")

(defvar emms-stream-nico--session-url-params nil)

(defvar emms-stream-nico--session-id-url nil)

(defun emms-stream-nico--fetch-session-option (watchid)
  "Fetch session optins with WATCHID."
  (emms-stream-nico-with-user-agent
    (let ((url-request-method "OPTIONS")
          (url-request-extra-headers
           `(("Accept" . "*/*")
             ("Access-Control-Request-Headers" . "content-type")
             ("Access-Control-Request-Method" . "POST")
             ("Origin" . ,emms-stream-nico--embed-url)
             ("Referer" . ,(emms-stream-nico--embed-watch-url watchid)))))
      (emms-stream-jpr-url-to-html
       (concat emms-stream-nico--session-url "?_format=json")))))

(defun emms-stream-nico--session-reqest-data ()
  "Return query list for session from `emms-stream-nico--guest_watch-res'."
  (cl-labels ((assqv (keys als) (emms-stream-jpr-assq-get keys als))
              (use-param-p (json-bool) (if (eq json-bool :json-false) "no" "yes")))
    (let* ((guest emms-stream-nico--guest_watch-res)
           (data (assqv 'data guest))
           (session_api (assqv 'session_api data))
           (url-params emms-stream-nico--session-url-params)
           (use_well_known_port (use-param-p (assqv 'is_well_known_port url-params)))
           (use_ssl (use-param-p (assqv 'is_ssl url-params))))
      `((session
         (recipe_id . ,(assqv 'recipe_id session_api))
         (content_id . "out1")
         (content_type . "movie")
         (content_src_id_sets
          . [((content_src_ids
               . [((src_id_to_mux
                    (video_src_ids . ,(assqv 'videos session_api))
                    (audio_src_ids . ,(assqv 'audios session_api))))]))])
         (timing_constraint . "unlimited")
         (keep_method
          (heartbeat (lifetime . ,(assqv 'heartbeat_lifetime session_api))))
         (protocol (name . "http")
                   (parameters (http_parameters
                                (parameters
                                 (http_output_download_parameters
                                  (use_well_known_port . ,use_well_known_port)
                                  (use_ssl . ,use_ssl))))))
         (content_uri . "")
         (session_operation_auth
          (session_operation_auth_by_signature
           (token . ,(assqv 'token session_api))
           (signature . ,(assqv 'signature session_api))))
         (content_auth
          (auth_type . ,(assqv '(auth_types http) session_api))
          (content_key_timeout . 0)
          (service_id . "nicovideo")
          (service_user_id . ,(assqv 'service_user_id session_api)))
         (client_info (player_id . ,(assqv 'player_id session_api)))
         (priority . ,(assqv 'priority session_api)))))))

(defvar emms-stream-nico--session-res nil)
(defvar emms-stream-nico--session-json-str nil)
(defvar emms-stream-nico--session-json-len nil)

(defun emms-stream-nico--fetch-session (watchid)
  "Fetch session optins with WATCHID."
  (emms-stream-nico-with-user-agent
    (let* ((url-request-method "POST")
           (url-request-data
            (json-encode (emms-stream-nico--session-reqest-data)))
           (url-request-extra-headers
            `(("Accept" . "application/json")
              ("Content-Type" . "application/json")
              ("Content-Length" . ,(number-to-string (length url-request-data)))
              ("Origin" . ,emms-stream-nico--embed-url)
              ("Referer" . ,(emms-stream-nico--embed-watch-url watchid)))))
      (setq emms-stream-nico--session-res
            (emms-stream-jpr-url-to-json
             (concat emms-stream-nico--session-url "?_format=json"))))))

;;;###autoload
(defun emms-stream-nico-watchid-to-content_uri (watchid)
  "Return content_uri of WATCHID."
  (setq emms-stream-nico--last-watchid watchid)
  (emms-stream-nico--fetch-data-props watchid)
  (emms-stream-nico--fetch-play watchid)
  (emms-stream-nico--fetch-guest_watch watchid)
  (setq emms-stream-nico--session-url
        (aref (emms-stream-jpr-assq-get
               '(data session_api api_urls) emms-stream-nico--guest_watch-res) 0))
  (setq emms-stream-nico--session-url-params
        (cl-loop for als across (emms-stream-jpr-assq-get
                                 '(data session_api urls) emms-stream-nico--guest_watch-res)
                 for url = (cdr (assq 'url als))
                 thereis (and (equal url emms-stream-nico--session-url) als)))
  ;; (emms-stream-nico--fetch-getthreadkey watchid)
  (emms-stream-nico--fetch-session-option watchid)
  (emms-stream-nico--fetch-session watchid)
  (setq emms-stream-nico--session-json-str
        (json-encode (cdr (assq 'data emms-stream-nico--session-res))))
  (setq emms-stream-nico--session-json-len
        (length emms-stream-nico--session-json-str))
  (setq emms-stream-nico--session-id-url
        (concat emms-stream-nico--session-url "/"
                (emms-stream-jpr-assq-get
                 '(data session id) emms-stream-nico--session-res)
                "?_format=json&_method=PUT"))
  (emms-stream-jpr-assq-get
   '(data session content_uri) emms-stream-nico--session-res))

(defvar emms-stream-nico--session-hb-timer nil)
(defvar emms-stream-nico--session-hb-timer-interval 110)

;;;###autoload
(defun emms-stream-nico-session-hb-cancel-timer ()
  "Cancel `emms-stream-nico--session-hb-timer'."
  (when (timerp emms-stream-nico--session-hb-timer)
    (cancel-timer emms-stream-nico--session-hb-timer)
    (setq emms-stream-nico--session-hb-timer nil)))

(defun emms-stream-nico--fetch-session-id-option ()
  "Send `emms-stream-nico--session-res' to session."
  (emms-stream-nico-with-user-agent
    (let ((url-request-method "OPTIONS")
          (url-request-extra-headers
           `(("Accept" . "*/*")
             ("Access-Control-Request-Headers" . "content-type")
             ("Access-Control-Request-Method" . "POST")
             ("Connection" . "keep-alive")
             ("Origin" . ,emms-stream-nico--embed-url)
             ("Referer" . ,(emms-stream-nico--embed-watch-url
                            emms-stream-nico--last-watchid)))))
      (url-retrieve
       emms-stream-nico--session-id-url
       (lambda (status &rest _)
         (if (not (memq :error status))
             (kill-buffer (current-buffer))
           (message "Failed to send OPTION to %s:%s"
                    emms-stream-nico--session-id-url status)
           (emms-stream-nico-session-hb-cancel-timer)))))))

(defun emms-stream-nico--send-session-id ()
  "Send `emms-stream-nico--session-json-str' to session."
  (emms-stream-nico-with-user-agent
    (let ((url-request-method "POST")
          (url-request-data emms-stream-nico--session-json-str)
          (url-request-extra-headers
           `(("Accept" . "application/json")
             ("Accept-Encoding" . "gzip, deflate")
             ("Access-Control-Request-Headers" . "content-type")
             ("Access-Control-Request-Method" . "POST")
             ("Content-Length" . ,(number-to-string
                                   emms-stream-nico--session-json-len))
             ("Content-Type" . "application/json")
             ("Origin" . ,emms-stream-nico--embed-url)
             ("Referer" . ,(emms-stream-nico--embed-watch-url
                            emms-stream-nico--last-watchid)))))
      (url-retrieve
       emms-stream-nico--session-id-url
       (lambda (status &rest _)
         (if (not (memq :error status))
             (kill-buffer (current-buffer))
           (message "Failed to send json data to %s:%s"
                    emms-stream-nico--session-id-url status)
           (emms-stream-nico-session-hb-cancel-timer)))
       nil t))))

(defun emms-stream-nico--session-get-lifetime ()
  "Return lifetime from `emms-stream-nico--session-res'.."
  (emms-stream-jpr-assq-get '(data session keep_method heartbeat lifetime)
                            emms-stream-nico--session-res))

(defun emms-stream-nico--session-hb-set-timer-interval ()
  "Set emms-stream-nico--session-hb-timer-interval using lifetime."
  (setq emms-stream-nico--session-hb-timer-interval
        (max (floor (- (* (emms-stream-nico--session-get-lifetime) 0.001) 10.0))
             10)))

;;;###autoload
(defun emms-stream-nico-session-hb-start-timer ()
  "Start session heartbeat.
Set `emms-stream-nico--session-hb-timer' to a timer."
  (emms-stream-nico-session-hb-cancel-timer)
  (emms-stream-nico--session-hb-set-timer-interval)
  (emms-stream-nico--fetch-session-id-option)
  (setq emms-stream-nico--session-hb-timer
        (run-with-timer 0.3
                        emms-stream-nico--session-hb-timer-interval
                        'emms-stream-nico--send-session-id)))
;;;###autoload
(defun emms-stream-nico-url-to-nicovideo (url)
  "Return a nicovideo content_uri from URL."
  (emms-stream-nico-watchid-to-content_uri
   (emms-stream-nico-url-to-watchid url)))

;; Old API

(defvar emms-stream-nico-old--domain ".nicovideo.jp")

(defvar emms-stream-nico-old--cookies-file
  (expand-file-name (make-temp-name "nicovideo_cookies") temporary-file-directory))

(defun emms-stream-nico-old--write-nico-cookies ()
  "Write cookies to `emms-stream-nico-old--cookies-file'."
  (emms-stream-jpr-write-cookies emms-stream-nico-old--domain
                                 emms-stream-nico-old--cookies-file))

(defun emms-stream-nico-old--search-thumbKey (buf)
  "Return thumbPlayKey on BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (re-search-forward "'thumbPlayKey':\\s-*'\\([^']*\\)'" nil t)
        (match-string 1)
      (error "Failed to search thumbPlayKey"))))

(defun emms-stream-nico-old--fetch-nico-thumbPlayKey (nico-url)
  "Return thumbPlayKey from NICO-URL."
  (emms-stream-nico-with-user-agent
    (let* ((thumb_watch-url (url-expand-file-name
                             (url-file-nondirectory nico-url)
                             "http://ext.nicovideo.jp/thumb_watch/"))
           (url-request-extra-headers `(("Referer" .  ,nico-url)))
           (buf (url-retrieve-synchronously thumb_watch-url)))
      (prog1 (emms-stream-nico-old--search-thumbKey buf)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun emms-stream-nico-old--search-nicovideo-url (buf)
  "Return nicovideo url on BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (re-search-forward "thread_id" nil t)
        (car (assoc-default "url"
                            (url-parse-query-string
                             (buffer-substring (line-beginning-position)
                                               (line-end-position)))))
      (error "Failed to search nicovideo url"))))

(defun emms-stream-nico-old--fetch-nicovideo-url (nico-url)
  "Return nicovideo url from NICO-URL."
  (emms-stream-nico-with-user-agent
    (let* ((thumb_watch-url
            (format "http://ext.nicovideo.jp/thumb_watch?%s"
                    (url-build-query-string
                     `(("k" ,(emms-stream-nico-old--fetch-nico-thumbPlayKey nico-url))
                       ("v" ,(emms-stream-nico-url-to-watchid nico-url))))))
           (buf (url-retrieve-synchronously thumb_watch-url)))
      (prog1 (emms-stream-nico-old--search-nicovideo-url buf)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun emms-stream-nico-old-nico-url-to-nicovideo-url (nico-url)
  "Return video url format for NICO-URL.
Update `emms-stream-nico-old--cookies-file'."
  (let ((video-url (emms-stream-nico-old--fetch-nicovideo-url nico-url)))
    (emms-stream-nico-old--write-nico-cookies)
    video-url))

(provide 'emms-streams-nico-util)
;;; emms-streams-nico-util.el ends here
