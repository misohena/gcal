;;; gcal.el --- Google Calendar Interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

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
(require 'cl)
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
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers headers)
        (url-request-data data))
    (gcal-parse-http-response
     (url-retrieve-synchronously (gcal-http-make-query-url url params)))))

(defun gcal-parse-http-response (buffer)
  "Parse HTTP response in buffer."
  (with-current-buffer buffer
    ;; Response Line (ex: HTTP/1.1 200 OK)
    (beginning-of-buffer)
    (if (looking-at "^HTTP/[^ ]+ \\([0-9]+\\) ?\\(.*\\)$")
        (let ((status (string-to-number (match-string 1)))
              (message (match-string 2))
              (headers)
              (body))
          (next-line)
          ;; Header Lines
          (while (not (eolp))
            (if (looking-at "^\\([^:]+\\): \\(.*\\)$")
                (push (cons (match-string 1) (match-string 2)) headers))
            (next-line))

          ;; Body
          (next-line)
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

(defstruct gcal-oauth-token access expires refresh url)

(defun gcal-oauth-get (token auth-url token-url client-id client-secret scope token-file)
  "アクセストークンを取得します。必要なら認証やリフレッシュを行います。"

  (if (= (length gcal-client-id) 0) (error "gcal-client-id is empty."))
  (if (= (length gcal-client-secret) 0) (error "gcal-client-secret is empty."))

  ;; load from token-file
  (if (null token)
      (setq token (gcal-oauth-load-token token-file)))

  ;; refresh token
  (when (and token
             (time-less-p (gcal-oauth-token-expires token) (current-time)))
    (setq token (gcal-oauth-refresh token client-id client-secret token-url))
    (gcal-oauth-save-token token-file token))

  ;; new token
  (when (null token)
    (setq token (gcal-oauth-auth auth-url token-url client-id client-secret scope))
    (gcal-oauth-save-token token-file token))

  ;; return token
  token)

(defun gcal-oauth-auth (auth-url token-url client-id client-secret scope)
  "OAuthによりアクセストークンを取得します。gcal-oauth-token構造体を返します。"
  (let* ((result (gcal-oauth-get-access-token auth-url token-url client-id client-secret scope))
         (access-token (cdr (assq 'access_token result)))
         (expires-in (cdr (assq 'expires_in result)))
         (refresh-token (cdr (assq 'refresh_token result)))
         (expires (time-add (current-time) (seconds-to-time expires-in))))
    (make-gcal-oauth-token
     :access access-token
     :expires expires
     :refresh refresh-token
     :url token-url)))

(defun gcal-oauth-refresh (token client-id client-secret &optional token-url)
  "gcal-oauth-token構造体のアクセストークンをリフレッシュします。"
  (let* ((result (gcal-oauth-get-refresh-token
                  (gcal-oauth-token-refresh token)
                  (or token-url (gcal-oauth-token-url token))
                  client-id client-secret))
         (access-token (cdr (assq 'access_token result)))
         (expires-in (cdr (assq 'expires_in result)))
         (expires (time-add (current-time) (seconds-to-time expires-in))))
    (when (and access-token expires)
      (setf (gcal-oauth-token-access token) access-token)
      (setf (gcal-oauth-token-expires token) expires)))
  token)



   ;; implementation details
(defun gcal-oauth-get-access-token (auth-url token-url client-id client-secret scope)
  "アクセストークンを取得します。JSONをリストへ変換したもので返します。"
  (gcal-retrieve-json-post-www-form
   token-url
   `(
     ("client_id" . ,client-id)
     ("client_secret" . ,client-secret)
     ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
     ("grant_type" . "authorization_code")
     ("code" . ,(gcal-oauth-get-authorization-code auth-url client-id scope)))))

(defun gcal-oauth-get-authorization-code (auth-url client-id scope)
  "ブラウザを開いてユーザに認証してもらい、認証コードを受け付けます。"
  (browse-url
   (gcal-http-make-query-url
    auth-url
    `(("client_id" . ,client-id)
      ("response_type" . "code")
      ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
      ("scope" . ,scope))))
  (read-string "Enter the code your browser displayed: "))

(defun gcal-oauth-get-refresh-token (refresh-token token-url client-id client-secret)
  "リフレッシュされたアクセストークンを取得します。JSONをリストへ変換したもので返します。"
  (gcal-retrieve-json-post-www-form
   gcal-token-url
   `(
     ("client_id" . ,client-id)
     ("client_secret" . ,client-secret)
     ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
     ("grant_type" . "refresh_token")
     ("refresh_token" . ,refresh-token))))

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
    ("access_token" . ,(gcal-access-token))
    ("key" . ,gcal-client-secret)
    ("grant_type" . "authorization_code")
    ))




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
  (list
   (cons
    (if date-only 'date 'dateTime)
    (gcal-time-format time date-only))
   (cons
    (if date-only 'dateTime 'date)
    nil)))


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
  "ex: ((dateTime . \"2009-10-25T11:00:54+09:00\")) => \"2009-10-25T11:00:54+09:00\" or nil"
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
