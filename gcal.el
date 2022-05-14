;;; gcal.el --- Google Calendar Interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Version: 0.9.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.3"))
;; URL: https://github.com/misohena/gcal

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

;;
;; (require 'gcal)
;; (setq gcal-client-id "xxxxxxxxx.apps.googleusercontent.com")
;; (setq gcal-client-secret "xxxx-XxxxXxxXXXxx") ;;API-KEY
;;
;; ;; list my calendars
;; (gcal-calendar-list-list) ;; Calendar List
;;
;; ;; list events
;; (gcal-events-list
;;  "example@gmail.com" ;;<- calendar-id
;;   `((timeMin . ,(gcal-datetime 2016 5 1))
;;     (timeMax . ,(gcal-datetime 2016 6 1))))
;;
;; ;; insert event
;; (gcal-events-insert
;;  "example@gmail.com"
;;  `((start . ,(gcal-gtime 2016 5 27))
;;    (end . ,(gcal-gtime 2016 5 28))
;;   (summary . "My Special Holiday")))
;;
;; ;; delete event
;; (gcal-events-delete "example@gmail.com" "xxx{event id}xxx")
;;

;;; Code:
(require 'url)
(require 'url-util)
(require 'json)
(require 'parse-time)

(defcustom gcal-client-id ""
  "client-id for Google Calendar API"
  :group 'gcal :type 'string)

(defcustom gcal-client-secret ""
  "client-secret for Google Calendar API"
  :group 'gcal :type 'string)

(defconst gcal-auth-url "https://accounts.google.com/o/oauth2/auth")
(defconst gcal-token-url "https://www.googleapis.com/oauth2/v3/token")
(defconst gcal-scope-url "https://www.googleapis.com/auth/calendar")

;; HTTP

(defun gcal-http (method url params headers data)
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers headers)
         (url-request-data data)
         (buffer (url-retrieve-synchronously (gcal-http-make-query-url url params))))
    (unwind-protect
        (gcal-parse-http-response buffer)
      (kill-buffer buffer))))

(defun gcal-parse-http-response (buffer)
  "Parse HTTP response in buffer."
  (with-current-buffer buffer
    ;; Response Line (ex: HTTP/1.1 200 OK)
    (goto-char (point-min))
    (if (looking-at "^HTTP/[^ ]+ \\([0-9]+\\) ?\\(.*\\)$")
        (let ((status (string-to-number (match-string 1)))
              (message (match-string 2))
              (headers)
              (body))
          (forward-line)
          ;; Header Lines
          (while (not (eolp))
            (if (looking-at "^\\([^:]+\\): \\(.*\\)$")
                (push (cons (match-string 1) (match-string 2)) headers))
            (forward-line))

          ;; Body
          (forward-line)
          (setq body (buffer-substring (point) (point-max)))

          ;; Result
          ;;(push (cons ":Body" body) headers)
          ;;(push (cons ":Status" status) headers)
          ;;(push (cons ":Message" message) headers)
          (list status message headers body)
          ))))

(defun gcal-http-get (url params)
  "Send GET request to url with params as query parameter."
  (gcal-http "GET" url params nil nil))

(defun gcal-http-post-www-form (url params)
  "Send POST request(with x-www-form-url-encoded parms) to url."
  (gcal-http "POST" url nil
             '(("Content-Type" . "application/x-www-form-urlencoded"))
             (gcal-http-make-query params)))

(defun gcal-http-post-json (url params json-obj &optional method)
  "Send POST request(with json) to url."
  (gcal-http (or method "POST") url params
             '(("Content-Type" . "application/json"))
             (encode-coding-string (json-encode json-obj) 'utf-8)))


(defun gcal-retrieve-json (method url params &optional headers data)
  "Send HTTP request and return parsed JSON object."
  (gcal-http-response-to-json (gcal-http method url params headers data)))

(defun gcal-retrieve-json-get (url params)
  "Send HTTP GET request and return parsed JSON object."
  (gcal-http-response-to-json (gcal-http-get url params)))

(defun gcal-retrieve-json-post-www-form (url params)
  "Send HTTP POST request(x-www-form-url-encoded) and return
parsed JSON object."
  (gcal-http-response-to-json (gcal-http-post-www-form url params)))

(defun gcal-retrieve-json-post-json (url params json-obj &optional method)
  "Send HTTP POST request(with encoded JSON string) and return
parsed JSON object."
  (gcal-http-response-to-json (gcal-http-post-json url params json-obj method)))


(defun gcal-http-response-to-json (response)
  "Convert HTTP response(return value of gcal-http,
gcal-parse-http-response) to parsed JSON object(by
json-read-from-string)."
  (let* ((status (nth 0 response))
         (body (nth 3 response)))
    ;;@todo check status
    (cond
     ((= status 204) nil) ;;empty result
     (t
      (json-read-from-string (decode-coding-string body 'utf-8))))))

(defun gcal-http-make-query (params)
  "Build query string. (ex: a=1&b=2&c=3)"
  (mapconcat
   (lambda (kv)
     (let* ((key (car kv))
            (v (cdr kv))
            (values (if (listp v) v (list v))))
       (mapconcat
        (lambda (value)
          (concat
           (url-hexify-string (format "%s" key))
           "="
           (url-hexify-string (format "%s" value))))
        values
        "&")
     ))
   params
   "&"))

(defun gcal-http-make-query-url (url params)
  "Build url with query string. (ex:http://example.com/?a=1&b=2&c=3)"
  (let* ((query (gcal-http-make-query params)))
    (if (> (length query) 0) (concat url "?" query) url)))



;;
;; OAuth
;; (この部分は一応Google Calendar以外でも使い回せるように作っています)
;;
;; Example: (setq token (gcal-oauth-get nil "https://accounts.google.com/o/oauth2/auth" "https://www.googleapis.com/oauth2/v3/token" "xxx.apps.googleusercontent.com" "secret_xxx" "https://www.googleapis.com/auth/calendar"))
;; Example: (gcal-oauth-token-access token)
;; Example: (gcal-oauth-token-expires token)
;; Example: (gcal-oauth-token-refresh token)
;; Example: (gcal-oauth-token-url token)
;; Example: (gcal-oauth-auth "https://accounts.google.com/o/oauth2/auth" "https://www.googleapis.com/oauth2/v3/token" "xxx.apps.googleusercontent.com" "secret_xxx" "https://www.googleapis.com/auth/calendar"))
;; Example: (gcal-oauth-refresh token "xxxx" "xxxx")

(cl-defstruct gcal-oauth-token access expires refresh url)

(defun gcal-oauth-get (token auth-url token-url client-id client-secret scope token-file)
  "アクセストークンを取得します。必要なら認証やリフレッシュを行います。"

  (if (or (null client-id) (string-empty-p client-id))
      (error "client-id is not specified"))
  (if (or (null client-secret) (string-empty-p client-secret))
      (error "client-secret is not specified"))

  ;; load from token-file
  (when (null token)
    (setq token (gcal-oauth-load-token token-file)))

  ;; refresh token
  (when (and token
             (gcal-oauth-token-expired-p token))
    (setq token (gcal-oauth-refresh token client-id client-secret token-url))
    (gcal-oauth-save-token token-file token)) ;; save token if not null

  ;; new token
  (when (null token)
    (setq token (gcal-oauth-auth auth-url token-url client-id client-secret scope))
    (gcal-oauth-save-token token-file token)) ;; save token if not null

  ;; failed
  (when (null token)
    (error "Failed to get access token"))

  ;; return token
  token)

(defun gcal-oauth-auth (auth-url token-url client-id client-secret scope)
  "OAuthによりアクセストークンを取得します。

gcal-oauth-tokenオブジェクトを返します。

失敗したらnilを返します。"
  (when-let ((response (gcal-oauth-get-access-token
                        auth-url token-url client-id client-secret scope)))
    (let* ((access-token (cdr (assq 'access_token response)))
           (expires-in (cdr (assq 'expires_in response)))
           (refresh-token (cdr (assq 'refresh_token response)))
           (expires-at
            (if expires-in
                (time-add (current-time) (seconds-to-time expires-in))
              nil)))
      (make-gcal-oauth-token
       :access access-token
       :expires expires-at
       :refresh refresh-token
       :url token-url))))

(defun gcal-oauth-refresh (token client-id client-secret &optional token-url)
  "gcal-oauth-tokenオブジェクトのアクセストークンをリフレッシュします。

アクセストークンと期限を更新したTOKENを返します。

リフレッシュに失敗したらnilを返します。"
  (when-let ((response (gcal-oauth-get-refreshed-token
                        (gcal-oauth-token-refresh token)
                        (or token-url (gcal-oauth-token-url token))
                        client-id
                        client-secret)))
    (let* ((access-token (cdr (assq 'access_token response)))
           (expires-in (cdr (assq 'expires_in response)))
           (expires-at
            (if expires-in
                (time-add (current-time) (seconds-to-time expires-in))
              nil)))
      (setf (gcal-oauth-token-access token) access-token)
      (setf (gcal-oauth-token-expires token) expires-at)
      token)))

(defun gcal-oauth-token-expired-p (token)
  "アクセストークンが期限切れになっているならtを返します。"
  (and
   token
   (gcal-oauth-token-expires token) ;;not null
   (time-less-p (gcal-oauth-token-expires token) (current-time))))

   ;; implementation details
(defun gcal-oauth-get-access-token (auth-url token-url client-id client-secret scope)
  "アクセストークンを取得します。JSONをリストへ変換したもので返します。"
  (let* ((auth-code-and-uri
          (gcal-oauth-get-authorization-code auth-url client-id scope))
         (code (car auth-code-and-uri))
         (redirect-uri (cdr auth-code-and-uri)))
    (gcal-oauth-check-access-token-response
     (gcal-retrieve-json-post-www-form
      token-url
      `(
        ("client_id" . ,client-id)
        ("client_secret" . ,client-secret)
        ("redirect_uri" . ,redirect-uri)
        ("grant_type" . "authorization_code")
        ("code" . ,code)))
     "get")))

(defvar gcal-oauth-use-oob-p nil
  "When t, use deprecated OAuth Out of Bound (OOB) Flow.")

(defun gcal-oauth-get-authorization-code (auth-url client-id scope)
  "ブラウザを開いてユーザに認証してもらい、認証コードを受け付けます。"
  (if gcal-oauth-use-oob-p
      (gcal-oauth-get-authorization-code-oob auth-url client-id scope)
    (gcal-oauth-get-authorization-code-loopback auth-url client-id scope)))

(defun gcal-oauth-get-authorization-code-oob (auth-url client-id scope)
  "ブラウザを開いてユーザに認証してもらい、認証コードを受け付けます。"
  (let ((redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
    (browse-url
     (gcal-http-make-query-url
      auth-url
      `(("client_id" . ,client-id)
        ("response_type" . "code")
        ("redirect_uri" . ,redirect-uri)
        ("scope" . ,scope))))
    (cons
     (read-string "Enter the code your browser displayed: ")
     redirect-uri)))

(defun gcal-oauth-get-refreshed-token (refresh-token token-url client-id client-secret)
  "リフレッシュされたアクセストークンを取得します。JSONをリストへ変換したもので返します。"
  (gcal-oauth-check-access-token-response
   (gcal-retrieve-json-post-www-form
    token-url
    `(
      ("client_id" . ,client-id)
      ("client_secret" . ,client-secret)
      ("grant_type" . "refresh_token")
      ("refresh_token" . ,refresh-token)))
   "refresh"))

(defun gcal-oauth-check-access-token-response (response operation)
  "アクセストークン取得(またはリフレッシュ)のRESPONSEをチェックします。

問題があればエラーメッセージを表示してnilを返します。問題が無けれ
ばRESPONSEをそのまま返します。"
  ;;(message "%s access token response = %s" operation response)

  (let ((err (cdr (assq 'error response)))
        (err-desc (cdr (assq 'error_description response)))
        (access-token (cdr (assq 'access_token response))))
    (cond
     ;; Error
     (err
      (message "Failed to %s access token (err=%s description=%s)"
               operation err err-desc)
      nil)

     ;; Not contains access_token
     ((null access-token)
      (message "Failed to %s access token (response=%s)"
               operation response)
      nil)

     ;; Succeeded
     (t response))))

(defun gcal-oauth-save-token (file token)
  (if (and file token)
      (with-temp-file file
        (pp token (current-buffer)))))

(defun gcal-oauth-load-token (file)
  (if (and file (file-exists-p file))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (read (buffer-string))))))

;; OAuth Local Server (For Loopback IP Address Flow)

(defun gcal-oauth-local-server-start ()
  (make-network-process
   :name "gcal-oauth-local-server"
   :server t
   :host 'local
   :service t
   :family 'ipv4
   :coding 'binary
   :filter 'gcal-oauth-local-server-filter
   :log 'gcal-oauth-local-server-log))

(defun gcal-oauth-local-server-stop (proc)
  (when (process-status proc)
    (delete-process proc)
    ;;(message "Stop local server.")
    ))

(defun gcal-oauth-local-server-log (server proc message)
  ;;(message "Log: %s" message)
  (gcal-oauth-local-server-connect server proc message))

(defun gcal-oauth-local-server-sentinel (proc message)
  ;;(message "Sentinel: %s" message)
  (unless (string-match-p "^open " message)
    (gcal-oauth-local-server-disconnect proc message)))

(defun gcal-oauth-local-server-connect (server proc _message)
  (unless (process-get proc :gcal-oauth-connect-p)
    (process-put proc :gcal-oauth-connect-p t)
    (process-put proc :gcal-oauth-request-buffer (generate-new-buffer " *gcal-oauth-request*"))
    (process-put proc :gcal-oauth-server-proc server)
    (set-process-sentinel proc #'gcal-oauth-local-server-sentinel)
    ;;(message "Connect")
    ))

(defun gcal-oauth-local-server-disconnect (proc _message)
  (when (process-put proc :gcal-oauth-connect-p t)
    (process-put proc :gcal-oauth-connect-p nil)
    (let ((buffer (process-get proc :gcal-oauth-request-buffer))
          (server (process-get proc :gcal-oauth-server-proc))
          (result (process-get proc :gcal-oauth-result)))
      (kill-buffer buffer)

      (when (and result
                 (null (process-get server :gcal-oauth-post-result-p)))
        (process-put server :gcal-oauth-post-result-p t)
        (gcal-oauth-local-server-post-result result)))

    ;;(message "Disconnect")
    ))

(defun gcal-oauth-local-server-post-result (result)
  (push (cons t (list 'gcal-oauth-local-server-quit result))
        unread-command-events))

(defun gcal-oauth-local-server-wait-for-result ()
  (let (result)
    (while (null result)
      (let ((event (read-event)))
        (when (and (listp event)
                   (eq (car event) 'gcal-oauth-local-server-quit))
          (setq result (cadr event)))))
    result))

(defun gcal-oauth-local-server-filter (proc string)
  ;;(message "Filter: %s (%schars)" (truncate-string-to-width string 20) (length string))
  (with-current-buffer (process-get proc :gcal-oauth-request-buffer)
    (goto-char (point-max))
    (save-excursion
      (insert string))
    (when (re-search-forward "\r\n\r\n" nil t)
      ;;@todo Wait for content
      (unless (process-get proc :gcal-oauth-finish-p)
        (process-put proc :gcal-oauth-finish-p t)
        (goto-char (point-min))
        (let* ((request (gcal-oauth-local-server-parse-request-before-content))
               (method (alist-get :method request))
               ;;(headers (alist-get :headers request))
               (path (alist-get :path request))
               (args (alist-get :query-args request)))
          (pcase path
            ("/"
             (pcase method
               ("GET"
                (process-put proc :gcal-oauth-result args)
                (gcal-oauth-local-server-send-response proc 200 "OK"))
               ;;@todo ("POST" ...)
               (_
                (gcal-oauth-local-server-send-response proc 405 "Method Not Allowed"))))
            (_
             (gcal-oauth-local-server-send-response proc 404 "Not Found"))))
        (process-send-eof proc)))))

(defun gcal-oauth-local-server-parse-request-before-content ()
  ;;(goto-char (point-min))
  (let* ((method-url (gcal-oauth-local-server-parse-start-line))
         (method (nth 0 method-url))
         (url (nth 1 method-url))
         (headers (gcal-oauth-local-server-parse-headers))
         (path-and-query (url-path-and-query (url-generic-parse-url url)))
         (path (car path-and-query))
         (query (cdr path-and-query))
         (args (when query (url-parse-query-string query))))
    (list
     (cons :method method)
     (cons :url url)
     (cons :path path)
     (cons :query query)
     (cons :query-args args)
     (cons :headers headers))))

(defun gcal-oauth-local-server-parse-start-line ()
  ;;(goto-char (point-min))
  (unless (looking-at "^\\([A-Z]+\\) *\\([^ ]+\\) *HTTP/[0-9]+\\.[0-9]+\r\n")
    (error "Invalid HTTP Request"))
  (goto-char (match-end 0))
  (let ((method (match-string 1))
        (url (match-string 2))
        ;;(http-version (match-string 3))
        )
    (list method url)))

(defun gcal-oauth-local-server-parse-headers ()
  (let (headers)
    (while (not (looking-at "\r\n"))
      (if (looking-at "^\\([^:]+\\): \\(.*\\)\r\n")
          (push (cons (match-string 1) (match-string 2)) headers))
      (forward-line))
    headers))

(defun gcal-oauth-local-server-send-response (proc code message)
  (process-send-string
   proc
   (format
    "HTTP/1.1 %s %s\r\nContent-Type: text/plain\r\nContent-Length: %s\r\n\r\n%s"
    code message
    (length message)
    message)))

(defun gcal-oauth-get-authorization-code-loopback (auth-url client-id scope)
  (let* ((proc (gcal-oauth-local-server-start))
         (host-port (process-contact proc))
         (port (cadr host-port))
         (redirect-uri (format "http://127.0.0.1:%s" port)))
    (unwind-protect
        (progn
          (browse-url
           (gcal-http-make-query-url
            auth-url
            `(("client_id" . ,client-id)
              ("response_type" . "code")
              ("redirect_uri" . ,redirect-uri)
              ("scope" . ,scope))))
          (message "Please approve the authority on the consent screen displayed in your browser.")
          (let* ((result (gcal-oauth-local-server-wait-for-result))
                 (err (cadr (assoc "error" result)))
                 (code (cadr (assoc "code" result))))
            (when err
              (message "Error: %s" err)
              (error "Error: %s" err))
            (unless code
              (message "No auth code")
              (error "No auth code"))
            (cons code redirect-uri)))

      (gcal-oauth-local-server-stop proc))))



;;
;; Google Calendar OAuth
;;
;; Example: (gcal-access-token)

(defcustom gcal-token-file
  (expand-file-name (concat user-emacs-directory ".gcal-token"))
  "access token file"
  :group 'gcal
  :type 'file)

(defvar gcal-access-token nil)

(defun gcal-access-token ()
  ;; get token
  (setq gcal-access-token (gcal-oauth-get gcal-access-token
                                          gcal-auth-url gcal-token-url
                                          gcal-client-id gcal-client-secret
                                          gcal-scope-url
                                          gcal-token-file))
  ;; return current token
  (gcal-oauth-token-access gcal-access-token))

(defun gcal-access-token-params ()
  `(
    ("access_token" . ,(gcal-access-token))))




;;
;; API URL Builder
;;

(defconst gcal-calendar-url "https://www.googleapis.com/calendar/v3")

(defun gcal-calendar-list-url (&optional calendar-id)
  (concat
   gcal-calendar-url
   "/users/me/calendarList"
   (if calendar-id (concat "/" calendar-id))))

(defun gcal-calendars-url (&optional calendar-id suffix)
  (concat
   gcal-calendar-url
   "/calendars"
   (if calendar-id (concat "/" calendar-id))
   (if suffix (concat "/" suffix))))

(defun gcal-events-url (calendar-id &optional suffix1 suffix2)
  (concat
   (gcal-calendars-url calendar-id "events")
   (if suffix1 (concat "/" suffix1))
   (if suffix2 (concat "/" suffix2))))


;;
;; API Wrapper
;;

  ;; CalendarList

(defun gcal-calendar-list-list ()
  "CalendarList: list"
  (gcal-retrieve-json-get
   (gcal-calendar-list-url)
   (gcal-access-token-params)))

  ;; Events

(defun gcal-events-list (calendar-id &optional params)
  "Events: list"
  (gcal-retrieve-json-get
   (gcal-events-url calendar-id)
   (append (gcal-access-token-params) params)))

(defun gcal-events-get (calendar-id event-id &optional params)
  "Events: get"
  (gcal-retrieve-json-get
   (gcal-events-url calendar-id event-id)
   (append (gcal-access-token-params) params)))

(defun gcal-events-quick-add (calendar-id text &optional params)
  "Events: quickAdd"
  (gcal-retrieve-json-post-json
   (gcal-events-url calendar-id "quickAdd")
   (append (gcal-access-token-params) params `(("text" . ,text))) nil))

(defun gcal-events-insert (calendar-id event-data &optional params)
  "Events: insert

Example:
(gcal-events-insert
 \"xxxxxxxxxxxxx@group.calendar.google.com\"
 `(
   (start (date \"2016-05-25\") )
   (end (date \"2016-05-26\"))
   (summary . \"First Test Event\")
   )
 )"
  (gcal-retrieve-json-post-json
   (gcal-events-url calendar-id)
   (append (gcal-access-token-params) params)
   event-data))

(defun gcal-events-patch (calendar-id event-id event-data &optional params)
  "Events: patch"
  (gcal-retrieve-json-post-json
   (gcal-events-url calendar-id event-id)
   (append (gcal-access-token-params) params)
   event-data
   "PATCH"))

(defun gcal-events-update (calendar-id event-id event-data &optional params)
  "Events: update"
  (gcal-retrieve-json-post-json
   (gcal-events-url calendar-id event-id)
   (append (gcal-access-token-params) params)
   event-data
   "PUT"))

(defun gcal-events-delete (calendar-id event-id &optional params)
  "Events: delete"
  (gcal-retrieve-json
   "DELETE"
   (gcal-events-url calendar-id event-id)
   (append (gcal-access-token-params) params) ))



;;
;; Time Utilities
;;
;; time = Emacs Internal Time
;;   (ex: (encode-time 0 0 0 31 4 2016) )
;; gtime = Google Calendar Time
;;   (ex: ('date . "2016-05-27") ('dateTime . "2016-05-27T12:34:00+09:00"))
;; datetime = RFC3339
;;   (ex: 2016-05-01T12:34:00+09:00)
;;

(defcustom gcal-time-zone-name-default nil
  "デフォルトのタイムゾーン名です。

IANA Time Zone Database nameで指定します(例:Asia/Tokyo)。

単発のイベントのみ使う場合は省略可能ですが、繰り返しイベントに時刻を含めたいときは必須です。"
  :group 'gcal :type '(choice (const nil) string))

(defun gcal-time-zone-name-default ()
  gcal-time-zone-name-default)

(defun gcal-time-zone-suffix ()
  (let ((tz (format-time-string "%z")))
    (concat (substring tz 0 3) ":" (substring tz 3))))

(defun gcal-time-format (time date-only)
  (if date-only (format-time-string "%Y-%m-%d" time)
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" time)
     (gcal-time-zone-suffix))))

(defun gcal-time-from-ymdhm (y m d hh mm)
  (encode-time 0 (or mm 0) (or hh 0) d m y))

(defun gcal-time-to-gtime (time date-only)
  (append
   (list
    (cons
     (if date-only 'date 'dateTime)
     (gcal-time-format time date-only))
    (cons
     (if date-only 'dateTime 'date)
     nil))
   (if-let ((name (gcal-time-zone-name-default)))
       (list (cons 'timeZone name)))))


(defun gcal-gtime (y m d &optional hh mm)
  (gcal-time-to-gtime (gcal-time-from-ymdhm y m d hh mm) (null hh)))


(defun gcal-datetime (y m d &optional hh mm)
  (gcal-time-format (gcal-time-from-ymdhm y m d hh mm) nil))

  ;; google => emacs

;;(gcal-time-parse "2014-12-13T10:00:00+09:00")
;;(gcal-time-parse "2015-03-06T15:42:32.354Z")
(defun gcal-time-parse (str)
  (parse-iso8601-time-string str))

(defun gcal-gtime-date-str (gtime)
  "ex: ((date . \"2016-05-28\")) => \"2016-05-28\" or nil"
  (cdr (assq 'date gtime)))

(defun gcal-gtime-date-time-str (gtime)
  "gcal-gtime-date-time-str

ex: ((dateTime . \"2009-10-25T11:00:54+09:00\"))
     => \"2009-10-25T11:00:54+09:00\" or nil"
  (cdr (assq 'dateTime gtime)))

(defun gcal-time-from-gtime (gtime)
  (let ((date (gcal-gtime-date-str gtime)))
    (if (stringp date)
        (let ((d (parse-time-string date)))
          (encode-time 0 0 0 (nth 3 d)(nth 4 d)(nth 5 d)))
      (let ((datetime (gcal-gtime-date-time-str gtime)))
        (if (stringp datetime)
            (gcal-time-parse datetime))))))






;;
;; Utilities
;;

(defun gcal-get-error-code (response-json)
  (if (listp response-json)
      (cdr (assq 'code (cdr (assq 'error response-json))))))

(defun gcal-succeeded-p (response-json)
  (null (gcal-get-error-code response-json)))

(defun gcal-failed-p (response-json)
  (not (null (gcal-get-error-code response-json))))



(provide 'gcal)
;;; gcal.el ends here
