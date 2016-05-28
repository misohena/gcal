;;; gcal-org.el --- Org mode to Google Calendar      -*- lexical-binding: t; -*-

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
;; (require 'gcal-org)
;;
;; (gcal-org-push-file "example@gmail.com" "~/my-schedule.org")
;;

;;; Code:


;;
;; parse org-mode document
;;

(require 'gcal)

(defun make-gcal-org-event (&rest args) args)
(defun gcal-org-event-id (oevent) (plist-get oevent :id))
(defun gcal-org-event-ord (oevent) (plist-get oevent :ord))
(defun gcal-org-event-summary (oevent) (plist-get oevent :summary))
(defun gcal-org-event-ts-prefix (oevent) (plist-get oevent :ts-prefix))
(defun gcal-org-event-ts-start (oevent) (plist-get oevent :ts-start))
(defun gcal-org-event-ts-end (oevent) (plist-get oevent :ts-end))
(defun gcal-org-event-location (oevent) (plist-get oevent :location))

(defun gcal-org-parse-file (file)
  "指定されたファイルからイベントを集めます。"
  (save-window-excursion
    (save-excursion
      (find-file file)
      (gcal-org-parse-buffer)
      )))

(defun gcal-org-parse-buffer ()
  "現在のバッファからイベントを集めます。

タイムスタンプ一つ毎に一つのイベントを作ります。Agendaのデフォル
ト(org-agenda-entry-types ?)がそうなっているからです。

タイムスタンプ毎にイベントを作るとIDが重複するため、同一エントリー
内でのタイムスタンプの序数を持たせています。
"
  (save-excursion
    (beginning-of-buffer)
    (let (entries events)
      ;; search timestamps
      (while (re-search-forward org-ts-regexp nil t)
        (goto-char (match-beginning 0))
        (let* ((ts-prefix  (if (looking-back "\\(SCHEDULED\\|DEADLINE\\): *")
                               (match-string-no-properties 1)))
               (id         (org-id-get-create)) ;; change (point)
               (location   (org-entry-get (point) "LOCATION"))
               (summary    (substring-no-properties (org-get-heading t t)))
               (ts         (cadr (org-element-timestamp-parser)))
               (ts-end-pos (plist-get ts :end))
               (ts-start   (list
                            (plist-get ts :year-start)
                            (plist-get ts :month-start)
                            (plist-get ts :day-start)
                            (plist-get ts :hour-start)
                            (plist-get ts :minute-start)))
               (ts-end     (list
                            (plist-get ts :year-end)
                            (plist-get ts :month-end)
                            (plist-get ts :day-end)
                            (plist-get ts :hour-end)
                            (plist-get ts :minute-end)))
               (same-entry-info  (assoc id entries))
               (same-entry-count (length (nth 1 same-entry-info)))
               (oevent      (make-gcal-org-event
                             :id id
                             :ord same-entry-count
                             :summary summary
                             :ts-prefix ts-prefix
                             :ts-start ts-start
                             :ts-end ts-end
                             :location location))
               )

          (when (null same-entry-info)  ;; New ID found
            (setq same-entry-info (list id nil))
            (push same-entry-info entries))

          (push oevent (nth 1 same-entry-info))
          (push oevent events)
          (goto-char ts-end-pos)))
      (nreverse events))))


;;
;; push
;;

(defun gcal-org-push-file (calendar-id file &optional cache-file)
  (if cache-file
      (gcal-org-push-file-specified-cache calendar-id file cache-file)
    (gcal-org-push-file-global-cache calendar-id file)))

    ;; use specified-cache

(defun gcal-org-push-file-specified-cache (calendar-id file cache-file)
  (let ((old-events (gcal-oevents-load cache-file))
        (new-events (gcal-org-parse-file file)))

    (gcal-oevents-save
     cache-file
     (gcal-org-push-oevents calendar-id new-events old-events))))


(defun gcal-oevents-save (file oevents)
  (with-temp-file file
    (pp oevents (current-buffer))))

(defun gcal-oevents-load (file)
  (if (file-exists-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (read (buffer-string))))))

    ;; use global-cache

(defun gcal-org-push-file-global-cache (calendar-id file)
  (let ((calfile-cache (gcal-org-pushed-events-cache calendar-id file)))

    (setf (nth 1 calfile-cache)
          (gcal-org-push-oevents calendar-id
                                 (gcal-org-parse-file file) ;;new events
                                 (nth 1 calfile-cache)))) ;;old events

  (gcal-org-pushed-events-save))

(defvar gcal-org-pushed-events nil)

(defcustom gcal-org-pushed-events-file
  (expand-file-name (concat user-emacs-directory ".gcal-org-pushed-events"))
  ""
  :group 'gcal-org
  :type 'file)

(defun gcal-org-pushed-events-save ()
  (with-temp-file gcal-org-pushed-events-file
    (pp gcal-org-pushed-events (current-buffer))))

(defun gcal-org-pushed-events-load ()
  (if (null gcal-org-pushed-events)
      (setq gcal-org-pushed-events
            (if (file-exists-p gcal-org-pushed-events-file)
                (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents gcal-org-pushed-events-file)
                    (read (buffer-string))))))))

(defun gcal-org-pushed-events-cache (calendar-id file)
  (gcal-org-pushed-events-load)

  (let* ((calfile-key (cons calendar-id (expand-file-name file)))
         (calfile-cache (assoc calfile-key gcal-org-pushed-events)))

    (when (null calfile-cache)
      (setq calfile-cache (list calfile-key nil)) ;;0:key 1:events
      (push calfile-cache gcal-org-pushed-events))

    calfile-cache))





;;
;; push org-mode events to Google Calendar
;;

(defun gcal-org-push-oevents (calendar-id new-events old-events)
  "Send delta between old-events and new-events to calendar(calendar-id).
old-events will be destroyed."
  (let ((result-events))

    (gcal-oevents-diff
     old-events
     new-events
     ;;(lambda (old-oe new-oe) (insert (format "mod %s\n" (gcal-org-event-summary new-oe))))
     ;;(lambda (new-oe) (insert (format "add %s\n" (gcal-org-event-summary new-oe))))
     ;;(lambda (old-oe) (insert (format "del %s\n" (gcal-org-event-summary old-oe))))
     ;;(lambda (old-oe) (insert (format "eq %s\n" (gcal-org-event-summary old-oe))))
     ;; Change
     (lambda (old-oe new-oe)
       (setq result-events (gcal-org-push-oevents--check
                            (gcal-oevent-patch calendar-id new-oe)
                            new-oe old-oe result-events
                            "update")))
     ;; Add
     (lambda (new-oe)
       (setq result-events (gcal-org-push-oevents--check
                            (gcal-org-push-oevents--insert calendar-id new-oe)
                            new-oe nil result-events
                            "insert")))
     ;; Del
     (lambda (old-oe)
       (setq result-events (gcal-org-push-oevents--check
                            (gcal-oevent-delete calendar-id old-oe)
                            nil old-oe result-events
                            "delete")))
     ;; Not Change
     (lambda (old-oe)
       (push old-oe result-events))
     )
    (nreverse result-events)))

(defun gcal-org-push-oevents--check (res succ-oe fail-oe result-events op)
  "Check response and add event to result-events list."
  (if (gcal-succeeded-p res)
      (if succ-oe (cons succ-oe result-events) result-events)
    (message "Failed to %s event '%s' err=%s"
             op (gcal-org-event-summary (or succ-oe fail-oe)) res)
    (if fail-oe (cons fail-oe result-events) result-events)))

(defun gcal-org-push-oevents--insert (calendar-id new-oe)
  (let* ((res (gcal-oevent-insert calendar-id new-oe))
         (err (gcal-get-error-code res)))
    ;; conflict (may be status=cancelled)
    (if (and (integerp err) (= err 409))
        (setq res (gcal-oevent-update calendar-id new-oe))
      res)))




;;
;; diff org-mode events
;;

(defun gcal-oevents-find-first-and-remove (cons-oevents id ord)
  "cons-oeventsのcdr以降からid,ordとマッチするイベントを探し、そ
の要素を削除し、その要素を返します。cons-oeventsの中身は変更され
ます。"
  (let ((curr cons-oevents)
        result)
    (while (cdr curr)
      (let ((oe (cadr curr)))
        (when (and oe
                   (equal (gcal-org-event-id oe) id)
                   (equal (gcal-org-event-ord oe) ord))
          (setcdr curr (cddr curr)) ;;remove element
          (setq curr nil) ;;break loop
          (setq result oe)))
      (setq curr (cdr curr)))
    result))

(defun gcal-oevents-diff (old-oevents new-oevents func-mod func-add func-del func-eq)
  "gcal-org-eventのリスト二つを比較し、変更、追加、削除されたイベ
ントについて関数を適用します。"
  (let ((cons-old-oevents (cons nil old-oevents)))
    (loop for new-oe in new-oevents
          do (let ((old-oe (gcal-oevents-find-first-and-remove
                            cons-old-oevents
                            (gcal-org-event-id new-oe)
                            (gcal-org-event-ord new-oe))))
               (cond
                ((null old-oe)
                 ;; new event
                 (funcall func-add new-oe)
                 )
                ((not (equal old-oe new-oe))
                 ;; modified event
                 (funcall func-mod old-oe new-oe)
                 )
                (t
                 ;; not modified event
                 (funcall func-eq new-oe))
                )))
    (setq old-oevents (cdr cons-old-oevents)))
  ;; deleted event
  (loop for old-oe in old-oevents
        do (funcall func-del old-oe)))




;;
;; convert oevent(Org-mode Event) to gevent(Google Calendar Event)
;;

(defun gcal-oevent-id-to-gevent-id (uuid)
  "oeventのID(UUID)をGoogle CalendarのイベントID表現へ変換します。
base32hexへ変換します。"
  ;; new method
  (if (gcal-uuid-p uuid)
      (downcase (gcal-uuid-to-base32hex uuid))
    uuid))

(defun gcal-oevent-gevent-id (oevent)
  "gcal-org-event構造体からGoogle CalendarのイベントIDを求めます。
同一エントリー内に複数のタイムスタンプがある場合に別々のIDを振り
ます。"
  (let ((gid (gcal-oevent-id-to-gevent-id (gcal-org-event-id oevent)))
        (ord (gcal-org-event-ord oevent)))
    (if (= ord 0)
        gid ;;0のときはそのまま。代表ID。Google Calendarから取り込んだイベントは必ずこれ。
      (format "%s%05d" gid ord))))

(defun gcal-oevent-to-gevent (oevent)
  "gcal-org-event構造体をGoogle Calendarのイベント表現へ変換します。"
  (let* ((summary   (gcal-org-event-summary oevent))
         (ts-prefix (gcal-org-event-ts-prefix oevent))
         (ts-start  (gcal-org-event-ts-start oevent))
         (ts-end    (gcal-org-event-ts-end oevent))
         (location  (gcal-org-event-location oevent))
         (ord  (gcal-org-event-ord oevent)))
    `((id . ,(gcal-oevent-gevent-id oevent))
      (status . "confirmed")
      (summary . ,(concat (if (string= ts-prefix "DEADLINE") "DL:") summary))
      (start . ,(gcal-ts-to-gtime ts-start))
      (end   . ,(gcal-ts-to-gtime (gcal-ts-end-exclusive ts-start ts-end)))
      (extendedProperties . ((private . ((gcalTsPrefix . ,ts-prefix)
                                         (gcalOrd . ,ord)))))
      ,@(if location `((location . ,location)))
      )))

(defun gcal-oevent-base32hex-uuid-p (id)
  ;; i5o7hja5ch1r14crqmp8g9mv6k
  (and (gcal-base32hex-p id) (= (length id) 26)))

(defun gcal-oevent-base32hex-uuid-with-ord-p (id)
  ;; i5o7hja5ch1r14crqmp8g9mv6k00001
  ;;;@todo check last 5 digits
  (and (gcal-base32hex-p id) (= (length id) (+ 26 5))))


;;
;; oevent API
;;

(defun gcal-oevent-insert (calendar-id oevent)
  (gcal-events-insert calendar-id (gcal-oevent-to-gevent oevent)))

(defun gcal-oevent-update (calendar-id oevent)
  (gcal-events-update calendar-id (gcal-oevent-gevent-id oevent) (gcal-oevent-to-gevent oevent)))

(defun gcal-oevent-patch (calendar-id oevent)
  (gcal-events-patch calendar-id (gcal-oevent-gevent-id oevent) (gcal-oevent-to-gevent oevent)))

(defun gcal-oevent-delete (calendar-id oevent)
  (gcal-events-delete calendar-id (gcal-oevent-gevent-id oevent)))


(defun gcal-oevents-insert (calendar-id oevents)
  (loop for oevent in oevents
        do (gcal-oevent-insert calendar-id oevent)))

(defun gcal-oevents-delete (calendar-id oevents)
  (loop for oevent in oevents
        do (gcal-oevent-delete calendar-id oevent)))





;;
;; pull oevents from Google Calendar
;;

(defun gcal-org-pull-oevents (calendar-id &optional params)
  "Download calendar events as list of oevent."
  (let ((gevents (gcal-events-list calendar-id params)))
    (if (gcal-failed-p gevents)
        (message "error %s" gevents)
      ;; succeeded
      (delq
       nil
       (mapcar #'gcal-oevent-from-gevent (cdr (assq 'items gevents)))))))

(defun gcal-oevent-from-gevent (gevent)
  "Google Calendar event to oevent(gcal-org-event object)"
  (let* ((gid (cdr (assq 'id gevent)))
         (id-ord (gcal-oevent-id-ord-from-gevent-id gid))
         (id (car id-ord))
         (ord (cdr id-ord))
         (status (cdr (assq 'status gevent)))
         (start (cdr (assq 'start gevent)))
         (end   (cdr (assq 'end gevent)))
         (ts-start (if start (gcal-ts-from-gtime start)))
         (ts-end   (if start (gcal-ts-from-gtime end)))
         (created (cdr (assq 'created gevent)))
         (updated (cdr (assq 'updated gevent)))
         (summary (cdr (assq 'summary gevent)))
         (location (cdr (assq 'location gevent)))
         (ex-props (cdr (assq 'private (cdr (assq 'extendedProperties gevent)))))
         (ts-prefix (cdr (assq 'gcalTsPrefix ex-props))) )

    (if (not (stringp id))
        (message "invalid event id found '%s'" id)
      (if (not (string= status "cancelled"))
          (make-gcal-org-event
           :id id
           :ord ord
           :summary (if (and (stringp ts-prefix)
                             (string= ts-prefix "DEADLINE")
                             (>= (length summary) 3)
                             (string= (substring summary 0 3) "DL:"))
                        (substring summary 3) summary) ;;strip "DL:"
           :ts-prefix ts-prefix
           :ts-start ts-start
           :ts-end (gcal-ts-end-inclusive ts-start ts-end)
           :location location
  )))))

(defun gcal-oevent-id-ord-from-gevent-id (id)
  "Convert Google Calendar's event id to oevent's :id and :ord."
  (cond
   ;; base32hex-uuid + ord
   ((gcal-oevent-base32hex-uuid-with-ord-p id)
    (cons
     (gcal-uuid-from-base32hex (substring id 0 26))
     (string-to-number (substring id 26 (+ 26 5)))))

   ;; base32hex-uuid
   ((gcal-oevent-base32hex-uuid-p id)
    (cons (gcal-uuid-from-base32hex id) 0))

   ;; unknown
   (t
    (cons id 0))))





;;
;; org-mode timestamp representation
;;
;; Examples:
;;   (gcal-ts-to-time '(2016 5 27 nil nil)) => (22343 3952)
;;   (gcal-ts-date-only '(2016 5 27 12 34)) => nil
;;   (gcal-ts-inc '(2016 5 27 12 34)) => (2016 5 27 12 35)
;;   (gcal-ts-to-gtime '(2016 5 27 12 34)) => ((dateTime . "2016-05-27T12:34:00+09:00") (date))
;;

(defun gcal-ts-to-time (ts)
  (apply 'gcal-time-from-ymdhm ts))

(defun gcal-ts-date-only (ts)
  (null (nth 3 ts)))

(defun gcal-ts-inc (ts)
  (let ((date-only (gcal-ts-date-only ts))
        (y (nth 0 ts))
        (m (nth 1 ts))
        (d (nth 2 ts))
        (hh (nth 3 ts))
        (mm (nth 4 ts)))
    (list y m (if date-only (1+ d) d) hh (if date-only mm (1+ mm)))))

(defun gcal-ts-end-exclusive (ts-start ts-end)
  "終了時間がその時間自身を含まないように補正します。"
  (if (or (equal (gcal-ts-to-time ts-start) (gcal-ts-to-time ts-end)) ;;<2016-05-26 Thu> => 27へ
          (gcal-ts-date-only ts-end)) ;;<2016-05-26 Thu>--<2016-05-27 Fri> => 28へ
      (gcal-ts-inc ts-end)
    ;; <2016-05-26 Thu 15:00-16:00> ;; => 16:00 (16:01にしない)
    ts-end))

(defun gcal-ts-to-gtime (ts)
  "タイムスタンプをGoogle Calendarの時間表現へ変換します。"
  (gcal-time-to-gtime (gcal-ts-to-time ts) (gcal-ts-date-only ts)))


(defun gcal-ts-from-gtime (gtime)
  "Google Calendarのイベント時間表現からタイムスタンプへ変換します。"
  (let* ((time (gcal-time-from-gtime gtime))
         (dect (if time (decode-time time))))
    (if dect
        (if (gcal-gtime-date-str gtime)
            ;; date-only
            (list (nth 5 dect) (nth 4 dect) (nth 3 dect) nil nil)
          ;; date and time
          (list (nth 5 dect) (nth 4 dect) (nth 3 dect) (nth 2 dect) (nth 1 dect))))))



(provide 'gcal-org)
;;; gcal-org.el ends here
