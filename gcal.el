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



;;;; Utilities

(defvar gcal-log-enabled nil)

(defun gcal-log (format-string &rest args)
  (when gcal-log-enabled
    (apply 'message format-string args))
  nil)

;;;; Asynchronous Execution Infrastructure

(defvar gcal-async-callback nil)

(defun gcal-async-callback-capture ()
  (pcase gcal-async-callback
    ;; nil
    ('nil
     nil)
    ;; (sticky <fun-ref> <fun-callback>)
    (`(sticky ,fun-ref ,fun-callback)
     (when fun-ref
       (funcall fun-ref))
     fun-callback)
    ;; <fun-callback>
    ((and (pred functionp) fun-callback)
     (setq gcal-async-callback nil)
     fun-callback)
    ;; Unknown
    (_
     nil)))

(defun gcal-async-callback-call (callback result)
  (if callback
      (funcall callback result)
    result))


;;;;; gcal-async-let*

;; Example1:
;; (gcal-http-async ;; or -sync
;;  (gcal-async-let
;;      ((title
;;        (gcal-async-let*
;;            (;; Get first web page (can be async)
;;             (response1
;;              (gcal-http "GET" "https://example.com/"))
;;             ;; Get first link destination in the page (can be async)
;;             (response2
;;              (let ((body (gcal-http-response-body response1)))
;;                (when (string-match "href=\"\\([^\"]+\\)\"" body)
;;                  (gcal-http "GET" (match-string 1 body))))))
;;          ;; Get title (not async)
;;          (let ((body (gcal-http-response-body response2)))
;;            (when (string-match "<title>\\([^<]+\\)</title>" body)
;;              (match-string 1 body))))))
;;    (message "title=%s" title)))
;;
;; The good thing about this way of writing is that the code is
;; exactly the same both synchronously and asynchronously. Also, for
;; synchronous execution, replacing gcal-async-let with let will work.

;; Example2:
;; (gcal-http-async
;;  (gcal-async-let ((callst (gcal-calendar-list-list)))
;;    (pp callst (current-buffer))))


(defmacro gcal-async-let*--0 (_varlist &rest body)
  ;; There is no async expression.
  `(progn
     ,@body))

(defmacro gcal-async-let*--1-inner (varlist body current-buffer callback)
  (if (null varlist)
      ;; Body
      ;; () body
      `(with-current-buffer
           (if (buffer-live-p ,current-buffer)
               ,current-buffer (current-buffer))
         (gcal-async-callback-call
          ,callback
          (progn
            ;; @todo Should I also consider asynchronous processing within the body?
            ;; That way, `gcal-calendar-list-list' can be written as follows.
            ;;   (defun gcal-calendar-list-list ()
            ;;     (gcal-async-let* ((params (gcal-access-token-params)))
            ;;       (gcal-retrieve-json-get (gcal-calendar-list-url) params)))
            ,@body)))
    ;; 1 or more variables
    ;; ((var expr)...) body
    (let ((var (nth 0 (car varlist)))
          (expr (nth 1 (car varlist))))
      `(let* ((gcal-async-callback
               (lambda (,var)
                 (gcal-async-let*--1-inner ,(cdr varlist) ,body
                                           ,current-buffer ,callback)))
              (result
               ;; @todo Should I restore the current buffer here as well?
               ;;(with-current-buffer (if (buffer-live-p ,current-buffer) ,current-buffer (current-buffer)) ,expr )
               ,expr))
         (if gcal-async-callback
             (funcall (gcal-async-callback-capture) result)
           result)))))

(defmacro gcal-async-let*--1 (varlist &rest body)
  (let ((current-buffer (gensym "current-buffer-"))
        (callback (gensym "callback-")))
    `(let ((,current-buffer (current-buffer))
           (,callback (gcal-async-callback-capture)))
       (gcal-async-let*--1-inner ,varlist ,body ,current-buffer ,callback))))

(defmacro gcal-async-let* (varlist &rest body)
  "(gcal-async-let* ((var1 async-expr1) (var2 async-expr2) ...) body)

First async-expr1 is evaluated and once its value is determined
it is set to var1. After that, async-exprN and varN are evaluated
in the same way, and finally body is evaluated.

Only one asynchronous function can be called within async-expr,
and it must be called at the end of the expression. If async-expr
does not contain an async function, its last evaluated value is
set to var.

Returns the return value of the function that was first executed
asynchronously. If there is no asynchronous function, returns
the value of BODY.

If `gcal-async-callback' is set, it will be called after body is
 evaluated.

Same as `let*' if no asynchronous processing is performed."
  (declare (indent 1))
  (unless lexical-binding
    (error "gcal.el requires lexical binding."))
  (if (null varlist)
      `(gcal-async-let*--0 nil ,@body)
    `(gcal-async-let*--1 ,varlist ,@body)))


;;;;; gcal-async-let (Parallel Execution)

(defmacro gcal-async-let--2 (varlist &rest body)
  (let ((current-buffer (gensym "current-buffer-"))
        (callback (gensym "callback-"))
        (result-vars (mapcar (lambda (_) (gensym "result-")) varlist))
        (fun-expr-vars (mapcar (lambda (_) (gensym "fun-expr-")) varlist))
        (fun-initialized (gensym "fun-initialized-"))
        (num-uninitialized (gensym "num-uninitialized")))
    `(let* (;; (fun-expr1 (lambda () <expr2>))
            ;; (fun-expr2 (lambda () <expr2>))
            ;; (fun-exprN (lambda () <exprN>))
            ,@(cl-loop for (_var expr) in varlist
                       for fun-expr in fun-expr-vars
                       collect `(,fun-expr (lambda () ,expr)))
            ;; var1
            ;; var2
            ;; varN
            ,@(cl-loop for (var _expr) in varlist
                       collect var)
            (,current-buffer (current-buffer))
            (,callback (gcal-async-callback-capture))
            (,num-uninitialized ,(length varlist))
            (,fun-initialized (lambda ()
                                (when (= (cl-decf ,num-uninitialized) 0)
                                  (with-current-buffer
                                      (if (buffer-live-p ,current-buffer)
                                          ,current-buffer (current-buffer))
                                    (gcal-async-callback-call
                                     ,callback
                                     (progn
                                       ,@body)))))))
       ,@(cl-loop for (var _expr) in varlist
                  for fun-expr in fun-expr-vars
                  for result in result-vars
                  collect
                  `(let* ((gcal-async-callback (lambda (res)
                                                 (setq ,var res)
                                                 (funcall ,fun-initialized)))
                          (,result (funcall ,fun-expr)))
                     (if gcal-async-callback
                         (funcall (gcal-async-callback-capture) ,result)
                       ,result))))))

(defmacro gcal-async-let (varlist &rest body)
  "(gcal-async-let ((var1 async-expr1) (var2 async-expr2) ...) body)

All async-exprs are evaluated first. Once the value of async-expr
is determined, it will be set to the corresponding var. Once all
vars are set, the body is evaluated.

Only one asynchronous function can be called within async-expr,
and it must be called at the end of the expression. If async-expr
does not contain an async function, its last evaluated value is
set to var.

If `gcal-async-callback' is set, it will be called after body is
 evaluated.

Returns the return value of the last function executed
asynchronously. If there is no asynchronous function, returns
the value of BODY.

Same as `let' if no asynchronous processing is performed."
  (declare (indent 1))
  (unless lexical-binding
    (error "gcal.el requires lexical binding."))
  (pcase (length varlist)
    (0 `(gcal-async-let*--0 nil ,@body))
    (1 `(gcal-async-let*--1 ,varlist ,@body))
    (_ `(gcal-async-let--2 ,varlist ,@body))))

;;;;; gcal-async-wait-all

;; Example:
;; (gcal-async-wait-all
;;     (progn
;;       (gcal-events-insert "xxxxxxxxxxxxx@gmail.com"
;;                           `((start (date "2024-02-11") )
;;                             (end (date "2024-02-12"))
;;                             (summary . "Test Event1")))
;;       (gcal-events-insert "xxxxxxxxxxxxx@gmail.com"
;;                           `((start (date "2024-02-11") )
;;                             (end (date "2024-02-12"))
;;                             (summary . "Test Event2")))
;;       (gcal-events-insert "xxxxxxxxxxxxx@gmail.com"
;;                           `((start (date "2024-02-11") )
;;                             (end (date "2024-02-12"))
;;                             (summary . "Test Event3"))))
;;   (message "Finish"))

(defun gcal-async-wait-all--impl (fun-async-expr fun-body)
  (let* ((callback (gcal-async-callback-capture))
         (current-buffer (current-buffer))
         (count 0)
         (waiting nil)
         (fun-body-caller (lambda ()
                            (with-current-buffer
                                (if (buffer-live-p current-buffer)
                                    current-buffer (current-buffer))
                              (gcal-async-callback-call
                               callback
                               (funcall fun-body)))))
         (gcal-async-callback (list 'sticky
                                    (lambda ()
                                      (cl-incf count))
                                    (lambda (_return-value)
                                      (cl-decf count)
                                      (when (and waiting (= count 0))
                                        (funcall fun-body-caller)))))
         (result-async-expr (funcall fun-async-expr)))
    (if (= count 0)
        (funcall fun-body-caller)
      (setq waiting t)
      result-async-expr)))

(defmacro gcal-async-wait-all (multiple-async-expr &rest body)
  "(gcal-async-wait-all multiple-async-expr body)"
  (declare (indent 1))
  `(gcal-async-wait-all--impl
    (lambda () ,multiple-async-expr)
    (lambda () ,@body)))



;;;; Process HTTP Request

(defvar gcal-http-sync nil
  "`t' or `sync' means `gcal-http' must be synchronous.
`async' means `gcal-http' must be asynchronous.
`nil' means to use the default (specified in `gcal-http-impl').")

(defmacro gcal-http-sync (&rest body)
  "`gcal-http' in BODY will be executed synchronously."
  `(let ((gcal-http-sync t))
     ,@body))

(defmacro gcal-http-async (&rest body)
  "`gcal-http' in BODY will be executed asynchronously."
  `(let ((gcal-http-sync 'async))
     ,@body))

(defvar gcal-http-impl 'gcal-http-impl-url-retrieve-synchronously
  "A function that handles a HTTP request.

Typically, this function actually sends the HTTP request and
returns the result, but it can also send it asynchronously and
return only the information to wait for the result, or it can
return the request itself without sending it.")

(defun gcal-http-impl ()
  (pcase gcal-http-sync
    ('nil gcal-http-impl)
    ('async 'gcal-http-impl-url-retrieve)
    (_ 'gcal-http-impl-url-retrieve-synchronously)))

(defun gcal-http (method url &optional params headers data response-filters)
  "Process a HTTP Request."
  (funcall
   (gcal-http-impl)
   method url params headers data response-filters))

(defun gcal-http-impl-url-retrieve-synchronously
    (method url params headers data &optional response-filters)
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers headers)
         (url-request-data data)
         (buffer (url-retrieve-synchronously
                  (gcal-http-make-query-url url params)))
         (response (unwind-protect
                       (gcal-parse-http-response buffer)
                     (kill-buffer buffer)))
         (response (gcal-http-apply-response-filters response
                                                     response-filters)))
    response))

(defun gcal-http-impl-url-retrieve
    (method url params headers data &optional response-filters)
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers headers)
         (url-request-data data)
         (callback (gcal-async-callback-capture))
         ;; @todo I want to use `url-queue-retrieve', but it doesn't
         ;; support url-request-* variables!
         ;;(retrieve-fun 'url-queue-retrieve)
         (retrieve-fun 'url-retrieve)
         (value
          (funcall
           retrieve-fun
           (gcal-http-make-query-url url params) ;; URL
           (gcal-http-impl-url-retrieve--make-callback-fun
            response-filters callback
            retrieve-fun url params headers data) ;; CALLBACK
           nil ;; CBARGS
           nil ;; SILENT
           t ;; INHIBIT-COOKIES
           )))
    (list 'gcal-http-waiting retrieve-fun value)))

(defun gcal-http-impl-url-retrieve--make-callback-fun
    (response-filters
     callback
     retrieve-fun url params headers data)
  (lambda (status)
    (let* ((response
            (if-let ((err (plist-get status :error)))
                (progn
                  (message
                   "%s error %s url=%s params=%s headers=%s data=[[[%s]]] buffer=[[[%s]]]"
                   retrieve-fun (prin1-to-string err)
                   url params headers data
                   (buffer-string))
                  (pcase err
                    ;; (error http 404)
                    (`(error http ,_code)
                     (gcal-parse-http-response (current-buffer)))
                    (_
                     ;;@todo 500?
                     (gcal-http-response-data
                      500 nil
                      "{ \"error\": { \"code\": 500, \"message\": \"An unexpected error occurred on url-retrieve\" } }"))))
              (gcal-parse-http-response (current-buffer))))
           (response
            (gcal-http-apply-response-filters
             response response-filters)))
      (gcal-async-callback-call callback response))))


;; For response

(defun gcal-http-response-status (response) (nth 0 response))
(defun gcal-http-response-headers (response) (nth 2 response))
(defun gcal-http-response-body (response) (nth 3 response))

(defun gcal-http-response-data (status headers body)
  (list status
        nil ;; reason-phrase (should not be used)
        headers
        body))

(defun gcal-parse-http-response (buffer)
  "Parse HTTP response in buffer."
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; status-line (ex: HTTP/1.1 200 OK)
    (when (looking-at "^HTTP/[^ ]+ \\([0-9]+\\) ?\\(.*\\)$")
      (forward-line)
      (gcal-http-response-data
       ;; status-code (integer)
       (string-to-number (match-string 1))
       ;; header-field* (alist)
       (gcal-parse-http-headers) ;; goto beginning of message-body
       ;; message-body (string)
       (buffer-substring (point) (point-max))))))

(defun gcal-parse-http-headers ()
  "Parse HTTP header fields in the current buffer."
  (let (headers)
    (while (not (looking-at "\\(?:\r\n\\|\n\\)"))
      (when (looking-at "^\\([^:\r\n]+\\): \\([^\r\n]*\\)\\(?:\r\n\\|\n\\)")
        (push (cons (match-string 1) (match-string 2)) headers))
      (forward-line))
    (goto-char (match-end 0)) ;;move to after \r\n (at beginning of content)
    (nreverse headers)))

(defun gcal-http-response-to-json (response)
  "Convert HTTP response(return value of gcal-http,
gcal-parse-http-response) to parsed JSON object(by
json-read-from-string)."
  (let* ((status (gcal-http-response-status response))
         (body (gcal-http-response-body response)))
    ;;@todo check status more
    (cond
     ((equal status 204) nil) ;;empty result
     ((and (stringp body) (not (string-empty-p body)))
      (json-read-from-string (decode-coding-string body 'utf-8))))))

(defun gcal-http-apply-response-filters (response response-filters)
  (dolist (fun response-filters)
    (setq response (funcall fun response)))
  response)

;; For request

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
        "&")))
   params
   "&"))

(defun gcal-http-make-query-url (url params)
  "Build url with query string. (ex:http://example.com/?a=1&b=2&c=3)"
  (let ((query (gcal-http-make-query params)))
    (if (string-empty-p query) url (concat url "?" query))))

(defconst gcal-http-headers-post-form
  '(("Content-Type" . "application/x-www-form-urlencoded")))

(defconst gcal-http-headers-post-json
  '(("Content-Type" . "application/json")))

(defun gcal-http-make-json-string (json-obj)
  (encode-coding-string (json-encode json-obj) 'utf-8))

(defun gcal-http-post-form (url params)
  "Send POST request(with x-www-form-url-encoded parms) to url."
  (gcal-http "POST" url nil
             gcal-http-headers-post-form
             (gcal-http-make-query params)))

(defun gcal-http-post-json (url params json-obj &optional method)
  "Send POST request(with json) to url."
  (gcal-http (or method "POST") url params
             gcal-http-headers-post-json
             (gcal-http-make-json-string json-obj)))

(defun gcal-retrieve-json (method url params &optional headers data)
  "Send HTTP request and return parsed JSON object."
  (gcal-http method url params headers data '(gcal-http-response-to-json)))

(defun gcal-retrieve-json-get (url params)
  "Send HTTP GET request and return parsed JSON object."
  (gcal-http "GET" url params nil nil '(gcal-http-response-to-json)))

(defun gcal-retrieve-json-post-form (url params)
  "Send HTTP POST request(x-www-form-url-encoded) and return
parsed JSON object."
  (gcal-http "POST" url nil
             gcal-http-headers-post-form
             (gcal-http-make-query params)
             '(gcal-http-response-to-json)))

(defun gcal-retrieve-json-post-json (url params json-obj &optional method)
  "Send HTTP POST request(with encoded JSON string) and return
parsed JSON object."
  (gcal-http (or method "POST") url params
             gcal-http-headers-post-json
             (gcal-http-make-json-string json-obj)
             '(gcal-http-response-to-json)))



;;;; OAuth

;; (This part can be used other than Google Calendar)
;;
;; Example:
;; (defvar example-token nil)
;; (setq example-token
;;       (gcal-oauth-token-get
;;         example-token
;;         "~/.gcal-token"
;;         "https://accounts.google.com/o/oauth2/v2/auth"
;;         "https://oauth2.googleapis.com/token"
;;         "xxx.apps.googleusercontent.com"
;;         "secret_xxx"
;;         "https://www.googleapis.com/auth/calendar"))
;;
;; (gcal-oauth-token-access example-token) ;;Access Token
;; (gcal-oauth-token-expires example-token) ;;Expiration Time
;; (gcal-oauth-token-refresh example-token) ;;Refresh Token
;; (gcal-oauth-token-expired-p example-token)

;; Example:
;; (setq token
;;       (gcal-oauth-auth
;;         "https://accounts.google.com/o/oauth2/v2/auth"
;;         "https://oauth2.googleapis.com/token"
;;         "xxx.apps.googleusercontent.com"
;;         "secret_xxx"
;;         "https://www.googleapis.com/auth/calendar"))

;; Example:
;; (gcal-oauth-refresh
;;   token "xxxx" "xxxx" "https://oauth2.googleapis.com/token")

(cl-defstruct (gcal-oauth-token
               (:constructor gcal-oauth-token-make))
  access expires refresh url-unused)

(defun gcal-oauth-token-get (token
                             token-file
                             auth-url token-url client-id client-secret scope
                             &optional force-update)
  "Get an OAuth token.
If necessary, load from TOKEN-FILE, authenticate, and refresh.

FORCE-UPDATE specifies the TOKEN forced update method.
Can be one of the following:
- nil : Do not force updates.
- reauth : Discard TOKEN and re-authenticate.
- refresh : Refresh access token in TOKEN."

  (when (or (null client-id) (string-empty-p client-id))
    (error "client-id is not specified"))
  (when (or (null client-secret) (string-empty-p client-secret))
    (error "client-secret is not specified"))

  ;; Load from token-file
  (if (eq force-update 'reauth)
      (setq token nil)
    (unless token
      (setq token (gcal-oauth-load-token token-file))))

  (gcal-async-let*
      ((token
        (if (and token
                 (or (eq force-update 'refresh)
                     (gcal-oauth-token-expired-p token)))
            ;; Refresh token
            (gcal-oauth-refresh token client-id client-secret token-url
                                ;; Save when refreshed
                                token-file)
          token))
       (token
        (if token
            token
          ;; New token
          (gcal-oauth-auth auth-url token-url client-id client-secret scope
                           ;; Save when created
                           token-file))))
    ;; failed
    (unless token
      (error "Failed to get access token"))
    ;; return token
    token))

(defun gcal-oauth-auth (auth-url token-url client-id client-secret scope
                                 &optional token-file)
  "Get a new OAuth token.
Returns a `gcal-oauth-token' object or nil on failure.
Returns nil on failure."
  (gcal-async-let ((response (gcal-oauth-auth--retrieve
                              auth-url token-url
                              client-id client-secret scope)))
    (when response
      (let ((token (gcal-oauth-token-make
                    :access (alist-get 'access_token response)
                    :expires (gcal-oauth-response-expires-at response)
                    :refresh (alist-get 'refresh_token response))))
        (when token-file
          (gcal-oauth-save-token token-file token))
        token))))

(defun gcal-oauth-refresh (token client-id client-secret token-url
                                 &optional token-file)
  "Refresh `gcal-oauth-token' object TOKEN.
Returns TOKEN with updated access token and expiration date.
Returns nil if refresh fails."
  (gcal-async-let ((response (gcal-oauth-refresh--retrieve
                              (gcal-oauth-token-refresh token)
                              token-url
                              client-id
                              client-secret)))
    (when response
      (setf (gcal-oauth-token-access token)
            (alist-get 'access_token response))
      (setf (gcal-oauth-token-expires token)
            (gcal-oauth-response-expires-at response))
      (when token-file
        (gcal-oauth-save-token token-file token))
      token)))

;; Expiration Time

(defun gcal-oauth-response-expires-at (response)
  "Obtain the token expiration time from RESPONSE.

Assume that RESPONSE was obtained at (current-time)."
  (let* ((expires-in (alist-get 'expires_in response))
         (expires-at
          (if expires-in
              (time-add (current-time) (seconds-to-time expires-in))
            nil)))
    expires-at))

(defun gcal-oauth-token-expired-p (token)
  "Return non-nil if the access token held by TOKEN has expired."
  (and
   token
   (gcal-oauth-token-expires token) ;;not null
   (time-less-p (gcal-oauth-token-expires token) (current-time))))

;; Token File I/O

(defun gcal-oauth-save-token (file token)
  (when (and file token)
    (with-temp-file file
      (pp token (current-buffer)))))

(defun gcal-oauth-load-token (file)
  (when (and file (file-exists-p file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file)
        (read (buffer-string))))))

;; Retrieve Token

(defun gcal-oauth-auth--retrieve (auth-url
                                  token-url client-id client-secret scope)
  "Authenticate with OAuth and obtain an access token.
Returns parsed JSON."
  (gcal-oauth-retrieve-token
   token-url
   (let* ((auth-code-and-uri
           (gcal-oauth-get-authorization-code auth-url client-id scope))
          (code (car auth-code-and-uri))
          (redirect-uri (cdr auth-code-and-uri)))
     `(
       ("client_id" . ,client-id)
       ("client_secret" . ,client-secret)
       ("redirect_uri" . ,redirect-uri)
       ("grant_type" . "authorization_code")
       ("code" . ,code)))
   "get"))

(defun gcal-oauth-refresh--retrieve (refresh-token
                                     token-url client-id client-secret)
  "Refresh token.
Returns parsed JSON."
  (gcal-oauth-retrieve-token
   token-url
   `(
     ("client_id" . ,client-id)
     ("client_secret" . ,client-secret)
     ("grant_type" . "refresh_token")
     ("refresh_token" . ,refresh-token))
   "refresh"))

(defun gcal-oauth-retrieve-token (token-url params operation)
  (gcal-async-let ((response (gcal-retrieve-json-post-form token-url params)))
    (gcal-oauth-check-access-token-response
     response
     operation)))

(defun gcal-oauth-check-access-token-response (response operation)
  "Check the RESPONSE of access token acquisition (or refresh).

If there is a problem, display an error message and return
nil. If there is no problem, return RESPONSE as is."
  ;;(message "%s access token response = %s" operation response)

  (let ((err          (alist-get 'error response))
        (err-desc     (alist-get 'error_description response))
        (access-token (alist-get 'access_token response)))
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

;; Authorization Code

(defvar gcal-oauth-use-oob-p nil
  "When t, use deprecated OAuth Out of Bound (OOB) Flow.")

(defun gcal-oauth-get-authorization-code (auth-url client-id scope)
  "Open a browser, ask the user to consent, and receive authorization code."
  (if gcal-oauth-use-oob-p
      (gcal-oauth-get-authorization-code-oob auth-url client-id scope)
    (gcal-oauth-get-authorization-code-loopback auth-url client-id scope)))

(defun gcal-oauth-get-authorization-code-oob (auth-url client-id scope)
  "Open a browser, ask the user to consent, and receive authorization code."
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

      ;;(message "Result=%s" result)
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
  ;;(message "Filter: --\n%s\n--\n(%schars)"
  ;;         string ;;(truncate-string-to-width string 20)
  ;;         (length string))
  (with-current-buffer (process-get proc :gcal-oauth-request-buffer)
    (goto-char (point-max))
    (save-excursion
      (insert string))

    (let ((request (process-get proc :gcal-oauth-request)))

      ;; Reach the end of headers
      (when (and (null request)
                 (re-search-forward "\r\n\r\n" nil t));;Reach the end of headers
        ;; Parse & delete start line and headers and separator(blank line)
        (setq request (gcal-oauth-local-server-parse-request-before-content))
        (process-put proc :gcal-oauth-request request))

      ;; Reach the end of content
      (when (and request ;;Headers already parsed
                 (null (alist-get :content request));;No content yet
                 (>= (buffer-size)
                     (alist-get :content-length request)));;Reach the end of content
        ;; Push the content to the end of the request list
        (nconc request
               (list
                (cons
                 :content
                 (buffer-substring
                  (point-min)
                  (+ (point-min) (alist-get :content-length request))))))

        ;; Make response and post result
        (gcal-oauth-local-server-execute-request proc request)

        ;; Disconnect
        (process-send-eof proc)))))

(defun gcal-oauth-local-server-execute-request (proc request)
  (let ((content (alist-get :content request))
        (method (alist-get :method request))
        (headers (alist-get :headers request))
        (path (alist-get :path request)))
    (pcase path
      ("/"
       (pcase method
         ("GET"
          (process-put proc
                       :gcal-oauth-result
                       (alist-get :query-args request))
          (gcal-oauth-local-server-send-response proc 200 "OK"))
         ("POST"
          (cond
           ((string=
             (alist-get "Content-Type" headers "" nil #'equal)
             "application/x-www-form-urlencoded")
            (process-put proc
                         :gcal-oauth-result
                         (url-parse-query-string content))
            (gcal-oauth-local-server-send-response proc 200 "OK"))
           (t
            (gcal-oauth-local-server-send-response
             proc 415 "Unsupported Media Type"))))
         (_
          (gcal-oauth-local-server-send-response
           proc 405 "Method Not Allowed"))))
      (_
       (gcal-oauth-local-server-send-response proc 404 "Not Found")))))

(defun gcal-oauth-local-server-parse-request-before-content ()
  (goto-char (point-min))
  (let* ((method-url (gcal-oauth-local-server-parse-start-line))
         (method (nth 0 method-url))
         (url (nth 1 method-url))
         (headers (gcal-parse-http-headers))
         (path-and-query (url-path-and-query (url-generic-parse-url url)))
         (path (car path-and-query))
         (query (cdr path-and-query))
         (args (when query (url-parse-query-string query)))
         (content-length (string-to-number
                          (alist-get
                           "Content-Length" headers "0" nil #'equal))))
    ;; Delete string before content
    (delete-region (point-min) (point))

    (list
     (cons :method method)
     (cons :url url)
     (cons :path path)
     (cons :query query)
     (cons :query-args args)
     (cons :headers headers)
     (cons :content-length content-length))))

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

(defun gcal-oauth-local-server-send-response (proc code message)
  (process-send-string
   proc
   (format
    "HTTP/1.1 %s %s\r\nContent-Type: text/plain\r\nContent-Length: %s\r\n\r\n%s"
    code message
    (length message)
    message)))

(defun gcal-oauth-get-authorization-code-loopback (auth-url client-id scope)
  "Open a browser, ask the user to consent, and receive authorization code."
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



;;;; Google Calendar OAuth

;; Example: (gcal-access-token)

(defcustom gcal-token-file
  (expand-file-name (concat user-emacs-directory ".gcal-token"))
  "access token file"
  :group 'gcal
  :type 'file)

(defcustom gcal-client-id ""
  "client-id for Google Calendar API"
  :group 'gcal :type 'string)

(defcustom gcal-client-secret ""
  "client-secret for Google Calendar API"
  :group 'gcal :type 'string)

(defconst gcal-auth-url "https://accounts.google.com/o/oauth2/v2/auth")
(defconst gcal-token-url "https://oauth2.googleapis.com/token")
(defconst gcal-scope-url "https://www.googleapis.com/auth/calendar")

(defvar gcal-access-token nil)

(defun gcal-access-token (&optional force-update)
  "Return the default access token for the Google Calendar API.

OAuth token is recorded in the `gcal-access-token' variable and
the file pointed to by `gcal-token-file'. They will be updated as
necessary.

URLs and client information required for authentication are
stored in variables `gcal-auth-url', `gcal-token-url',
`gcal-scope-url', `gcal-client-id' and `gcal-client-secret'.

See `gcal-oauth-token-get' for FORCE-UPDATE."
  (gcal-async-let ((token (gcal-oauth-token-get
                           gcal-access-token
                           gcal-token-file
                           gcal-auth-url gcal-token-url
                           gcal-client-id gcal-client-secret
                           gcal-scope-url
                           force-update)))
    (setq gcal-access-token token)
    ;; Return access token
    (gcal-oauth-token-access gcal-access-token)))

(defun gcal-access-token-params (&optional additional-params)
  (gcal-async-let ((access-token (gcal-access-token)))
    `(("access_token" . ,access-token)
      ,@additional-params)))




;;;; API URL Builder

(defconst gcal-calendar-url "https://www.googleapis.com/calendar/v3")

(defun gcal-calendar-list-url (&optional calendar-id)
  (concat
   gcal-calendar-url
   "/users/me/calendarList"
   (and calendar-id (concat "/" calendar-id))))

(defun gcal-calendars-url (&optional calendar-id suffix)
  (concat
   gcal-calendar-url
   "/calendars"
   (and calendar-id (concat "/" calendar-id))
   (and suffix (concat "/" suffix))))

(defun gcal-events-url (calendar-id &optional suffix1 suffix2)
  (concat
   (gcal-calendars-url calendar-id "events")
   (and suffix1 (concat "/" suffix1))
   (and suffix2 (concat "/" suffix2))))



;;;; API Wrapper

  ;; API Error

(defun gcal-get-error-code (response-json)
  (when (listp response-json)
    (cdr (assq 'code (cdr (assq 'error response-json))))))

(defun gcal-succeeded-p (response-json)
  ;; NOTE: Events:delete returns an empty body response if successful.
  (null (gcal-get-error-code response-json)))

(defun gcal-failed-p (response-json)
  ;; NOTE: Events:delete returns an empty body response if successful.
  (not (null (gcal-get-error-code response-json))))

  ;; CalendarList

(defun gcal-calendar-list-list ()
  "CalendarList: list

URL `https://developers.google.com/calendar/api/v3/reference/calendarList/list'"
  (gcal-async-let* ((params (gcal-access-token-params))
                    (response (gcal-retrieve-json-get
                               (gcal-calendar-list-url)
                               params)))
    response))

  ;; Events

(defun gcal-events-list (calendar-id &optional params)
  "Events: list

URL `https://developers.google.com/calendar/api/v3/reference/events/list'"
  (gcal-async-let* ((params (gcal-access-token-params params))
                    (response (gcal-retrieve-json-get
                               (gcal-events-url calendar-id)
                               params)))
    response))

(defun gcal-events-get (calendar-id event-id &optional params)
  "Events: get

URL `https://developers.google.com/calendar/api/v3/reference/events/get'"
  (gcal-async-let* ((params (gcal-access-token-params params))
                    (response (gcal-retrieve-json-get
                               (gcal-events-url calendar-id event-id)
                               params)))
    response))

(defun gcal-events-quick-add (calendar-id text &optional params)
  "Events: quickAdd

URL `https://developers.google.com/calendar/api/v3/reference/events/quickAdd'"
  (gcal-async-let* ((params (gcal-access-token-params
                             (append params `(("text" . ,text)))))
                    (response (gcal-retrieve-json-post-json
                               (gcal-events-url calendar-id "quickAdd")
                               params
                               nil)))
    response))

(defun gcal-events-insert (calendar-id event-data &optional params)
  "Events: insert

URL `https://developers.google.com/calendar/api/v3/reference/events/insert'

Example:
(gcal-events-insert
 \"xxxxxxxxxxxxx@group.calendar.google.com\"
 `(
   (start (date \"2016-05-25\") )
   (end (date \"2016-05-26\"))
   (summary . \"First Test Event\")
   )
 )"
  (gcal-async-let* ((params (gcal-access-token-params params))
                    (response (gcal-retrieve-json-post-json
                               (gcal-events-url calendar-id)
                               params
                               event-data)))
    response))

(defun gcal-events-patch (calendar-id event-id event-data &optional params)
  "Events: patch

URL `https://developers.google.com/calendar/api/v3/reference/events/patch'"
  (gcal-async-let* ((params (gcal-access-token-params params))
                    (response (gcal-retrieve-json-post-json
                               (gcal-events-url calendar-id event-id)
                               params
                               event-data
                               "PATCH")))
    response))

(defun gcal-events-update (calendar-id event-id event-data &optional params)
  "Events: update

URL `https://developers.google.com/calendar/api/v3/reference/events/update'"
  (gcal-async-let* ((params (gcal-access-token-params params))
                    (response (gcal-retrieve-json-post-json
                               (gcal-events-url calendar-id event-id)
                               params
                               event-data
                               "PUT")))
    response))

(defun gcal-events-delete (calendar-id event-id &optional params)
  "Events: delete

URL `https://developers.google.com/calendar/api/v3/reference/events/delete'"
  (gcal-async-let* ((params (gcal-access-token-params params))
                    (response (gcal-retrieve-json
                               "DELETE"
                               (gcal-events-url calendar-id event-id)
                               params)))
    response))



;;;; Time Utilities

;;
;; time = Emacs Internal Time
;;   (ex: (encode-time 0 0 0 31 4 2016) )
;; gtime = Google Calendar Time
;;   (ex: ('date . "2016-05-27") ('dateTime . "2016-05-27T12:34:00+09:00"))
;; datetime = RFC3339
;;   (ex: 2016-05-01T12:34:00+09:00)
;;

(defcustom gcal-time-zone-name-default nil
  "Default time zone name.

Specify by IANA Time Zone Database name (for example, Asia/Tokyo).

This is optional if you only use one-time events, but it is
required if you want to include time in repeated events."
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
   (when-let ((name (gcal-time-zone-name-default)))
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



(provide 'gcal)
;;; gcal.el ends here
