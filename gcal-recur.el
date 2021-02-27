;;; gcal-recur.el --- Expand recurrence rule       -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'calendar)


(defun gcal-recur-between (recurrence
                           ;; encoded-time
                           enct-dtstart
                           enct-time-min time-min-inclusive
                           enct-time-max time-max-inclusive
                           min-count
                           &optional time-zone)
  ;;@todo Support RDATE and EXDATE? Google Calendar's recurrence property does not support RDATE and EXDATE.
  (seq-uniq
   (sort
    (seq-mapcat
     (lambda (line)
       (let* ((name-param-value (gcal-recur-parse-content-line line))
              (name (car name-param-value))
              (value (caddr name-param-value)))
         (when (string= name "RRULE")
           (gcal-recur-rrule-between
            (gcal-recur-parse-value-properties value)
            enct-dtstart
            enct-time-min time-min-inclusive
            enct-time-max time-max-inclusive
            min-count
            time-zone))))
     recurrence)
    #'time-less-p)
   #'time-equal-p))

;; (mapcar
;;  (lambda (t) (format-time-string "%F(%a) %T" t))
;;  (gcal-recur-between
;;   ["RRULE:FREQ=DAILY;INTERVAL=10" "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=1MO;BYHOUR=12"]
;;   (encode-time '(0 0 0 1 2 2021 nil nil nil))
;;   (encode-time '(0 0 0 1 2 2021 nil nil nil)) t
;;   (encode-time '(0 0 0 1 2 2100 nil nil nil)) nil
;;   1))


;;
;; RRULE
;;

;; (mapcar #'decode-time
;;         (gcal-recur-rrule-between
;; ;;         "FREQ=YEARLY;INTERVAL=2;BYMONTH=1,2,3,4,5,6;BYWEEKNO=4,5;BYYEARDAY=1,28,31;BYMONTHDAY=28,29,30;BYDAY=TH"
;; ;;         "FREQ=MONTHLY;INTERVAL=2;BYMONTH=1,2,3,4,5,6;BYMONTHDAY=28,29,30;BYDAY=TH"
;;          ;;"FREQ=WEEKLY;INTERVAL=2;COUNT=3"
;;          ;;"FREQ=DAILY;INTERVAL=2"
;;          ;;"FREQ=WEEKLY;WKST=SU;UNTIL=20210321"
;;          ;;""
;;          "FREQ=YEARLY;BYMONTHDAY=-1,-2,-3;BYDAY=MO"
;;          (encode-time '(59 59 23 14 2 2021 nil nil nil))
;;          (encode-time '(0 0 0 10 2 2021 nil nil nil)) nil
;;          (encode-time '(0 0 0 1 1 2100 nil nil nil)) nil
;;          10))

(defun gcal-recur-rrule-between (rules
                                 enct-dtstart
                                 enct-time-min time-min-inclusive
                                 enct-time-max time-max-inclusive
                                 min-count
                                 &optional time-zone)
  (when (stringp rules)
    (setq rules (gcal-recur-parse-value-properties rules)))

  (let* ((freq (gcal-recur-rrule-freq rules))
         (interval (gcal-recur-rrule-interval rules))
         (wkst (gcal-recur-rrule-wkst rules))
         (bysetpos (gcal-recur-rrule-bysetpos rules))
         (count (gcal-recur-rrule-count rules))
         (until (gcal-recur-rrule-until rules time-zone)))

    ;; apply until to time-max
    (when (and until
               (time-less-p until enct-time-max)) ;;or (and (time-equal-p until time-max) time-max-inclusive)
      (setq enct-time-max until)
      (setq time-max-inclusive t))

    ;;
    (if (or
         ;; time-min > time-max
         (time-less-p enct-time-max enct-time-min)
         (and (time-equal-p enct-time-min enct-time-max)
              (not time-min-inclusive)
              (not time-max-inclusive))
         ;; time-max < dtstart
         (time-less-p enct-time-max enct-dtstart)
         (and (time-equal-p enct-time-max enct-dtstart)
              (not time-max-inclusive))
         ;; count < 1
         (and count
              (< count 1)))
        nil

      (let* (;; decoded time
             (dect-dtstart (decode-time enct-dtstart time-zone))
             (dect-time-min (decode-time enct-time-min time-zone))
             (dect-time-max (decode-time enct-time-max time-zone))
             ;; date number of based on FREQ
             (dtstart-num (gcal-recur-dect-to-freq-number dect-dtstart freq wkst))
             (time-min-num (gcal-recur-dect-to-freq-number dect-time-min freq wkst))
             (time-max-num (gcal-recur-dect-to-freq-number dect-time-max freq wkst))

             (period
              (if (and (null count)
                       (< dtstart-num time-min-num))
                  ;; Calculate from the middle period
                  (let ((num-in-interval (mod (- time-min-num dtstart-num) interval)))
                    (+ time-min-num
                       (if (> num-in-interval 0) (- interval num-in-interval) 0)))
                ;; Calculate from the first period if count= is specified or time-min <= dtstart
                dtstart-num))
             result-datetimes)

        ;; Push the first event(dtstart) if time-min <= dtstart
        (when (or (time-less-p enct-time-min enct-dtstart)
                  (and (time-equal-p enct-time-min enct-dtstart)
                       time-min-inclusive))
          (setq result-datetimes (list enct-dtstart))
          (setq enct-time-min enct-dtstart)
          (setq time-min-inclusive nil))
        ;; Count the first event
        ;; Must count even if time-min > dtstart
        (when count
          (setq count (1- count)))

        ;; events between time-min to time-max
        (while (and
                (<= period time-max-num)
                (or (null count) (> count 0))
                (< (length result-datetimes) min-count))

          (let ((datetimes-on-period
                 (thread-first
                     (gcal-recur-dates-on-period freq rules period dect-dtstart)
                   (gcal-recur-ymd-list-sort-and-unique )
                   (gcal-recur-limit-bysetpos  bysetpos)
                   (gcal-recur-complete-hms  rules dect-dtstart time-zone)
                   (gcal-recur-sorted-encoded-time-list-after  enct-dtstart nil) ;; nil means exclusive. dtstart already pushed
                   (gcal-recur-limit-by-count  count))))

            (when count
              (setq count (- count (length datetimes-on-period))))

            (setq result-datetimes
                  (nconc
                   result-datetimes
                   (gcal-recur-sorted-encoded-time-list-between
                    datetimes-on-period
                    enct-time-min time-min-inclusive
                    enct-time-max time-max-inclusive)))
            (setq period (+ period interval))))

        result-datetimes))))

(defun gcal-recur-dect-to-freq-number (dect freq wkst)
  (cond
   ((eq freq 'yearly) (decoded-time-year dect)) ;;year number = year
   ((eq freq 'monthly) (+ (* 12 (decoded-time-year dect)) (1- (decoded-time-month dect)))) ;;month number = 12*year+month-1
   ((eq freq 'weekly) (gcal-absdn-to-abs-week-number (gcal-absdn-from-decoded-time dect) wkst)) ;;week number = (absdn-wkst)/7
   ((eq freq 'daily) (gcal-absdn-from-decoded-time dect)) ;;day number = absdn
   (t 1)))

(defun gcal-recur-dates-on-period (freq rules period dect-dtstart)
  (cond
   ((eq freq 'yearly) (gcal-recur-dates-on-period-year rules period dect-dtstart))
   ((eq freq 'monthly) (gcal-recur-dates-on-period-month rules period dect-dtstart))
   ((eq freq 'weekly) (gcal-recur-dates-on-period-week rules period dect-dtstart))
   ((eq freq 'daily) (gcal-recur-dates-on-period-day rules period dect-dtstart))))

(defun gcal-recur-dates-on-period-year (rules year dect-dtstart)
  (let ((bymonth (gcal-recur-rrule-bymonth rules))
        (byweekno (gcal-recur-rrule-byweekno rules))
        (byyearday (gcal-recur-rrule-byyearday rules))
        (bymonthday (gcal-recur-rrule-bymonthday rules))
        (byday (gcal-recur-rrule-byday rules))
        (wkst (gcal-recur-rrule-wkst rules))
        (candidates (list (list year nil nil))))

    (if bymonth
        (setq candidates
              (gcal-recur-expand-bymonth year bymonth)))

    (if byweekno
        (setq candidates
              (gcal-recur-ymd-list-filter-matched
               candidates
               (gcal-recur-expand-byweekno year wkst byweekno))))

    (if byyearday
        (setq candidates
              (gcal-recur-ymd-list-filter-matched
               candidates
               (gcal-recur-expand-byyearday year byyearday))))

    (if bymonthday
        (setq candidates
              (gcal-recur-ymd-list-make-combination
               candidates
               (gcal-recur-expand-bymonthday year bymonthday))))

    (if byday
        (setq candidates
              (gcal-recur-ymd-list-make-combination
               candidates
               ;; within the year/months specified by bymonth
               (gcal-recur-expand-byday year bymonth byday))))

    (setq candidates
          (gcal-recur-ymd-list-complete
           candidates
           year
           (decoded-time-month dect-dtstart) ;;same month as dtstart
           (decoded-time-day dect-dtstart))) ;;same day as dtstart

    candidates))

(defun gcal-recur-dates-on-period-month (rules month-num dect-dtstart)
  (let* ((year (/ month-num 12))
         (month (1+ (% month-num 12)))
         (bymonth (gcal-recur-rrule-bymonth rules))
         ;;(byweekno (gcal-recur-rrule-byweekno rules))
         ;;(byyearday (gcal-recur-rrule-byyearday rules))
         (bymonthday (gcal-recur-rrule-bymonthday rules))
         (byday (gcal-recur-rrule-byday rules))
         ;;(wkst (gcal-recur-rrule-wkst rules))
         (candidates (list (list year month nil))))

    (if bymonth
        (setq candidates
              (gcal-recur-ymd-list-filter-matched
               candidates
               (gcal-recur-expand-bymonth year bymonth)))) ;;Limit

    (if bymonthday
        (setq candidates
              (gcal-recur-ymd-list-make-combination
               candidates
               (gcal-recur-expand-bymonthday year bymonthday))))

    (if byday
        (setq candidates
              (gcal-recur-ymd-list-make-combination
               candidates
               ;; within the month
               (gcal-recur-expand-byday year month byday))))

    (setq candidates
          (gcal-recur-ymd-list-complete
           candidates
           year
           month
           (decoded-time-day dect-dtstart))) ;;same day as dtstart

    candidates))

(defun gcal-recur-dates-on-period-week (rules abs-week-num dect-dtstart)
  (let* ((bymonth (gcal-recur-rrule-bymonth rules))
         ;;(byweekno (gcal-recur-rrule-byweekno rules))
         ;;(byyearday (gcal-recur-rrule-byyearday rules))
         ;;(bymonthday (gcal-recur-rrule-bymonthday rules))
         (byday (gcal-recur-rrule-byday rules))
         (wkst (gcal-recur-rrule-wkst rules))
         (dtstart-dow (gcal-absdn-day-of-week (gcal-absdn-from-decoded-time dect-dtstart)))
         (candidates (list (list nil nil nil))))

    (if bymonth
        (setq candidates
              (gcal-recur-expand-bymonth nil bymonth)))

    (setq candidates
          (gcal-recur-ymd-list-make-combination
           candidates
           ;; within the week
           (mapcar
            #'gcal-absdn-to-ymd
            (gcal-recur-expand-byday-absdn
             (gcal-absdn-from-abs-week-number abs-week-num wkst)
             (+ (gcal-absdn-from-abs-week-number abs-week-num wkst) 7)
             (or byday (list (cons nil dtstart-dow)))))))

    candidates))

(defun gcal-recur-dates-on-period-day (rules absdn _dect-dtstart)
  (let* ((ymd (gcal-absdn-to-ymd absdn))
         (year (car ymd))
         (month (cadr ymd))
         (day (caddr ymd))
         (bymonth (gcal-recur-rrule-bymonth rules))
         ;;(byweekno (gcal-recur-rrule-byweekno rules))
         ;;(byyearday (gcal-recur-rrule-byyearday rules))
         (bymonthday (gcal-recur-rrule-bymonthday rules))
         (byday (gcal-recur-rrule-byday rules))
         ;;(wkst (gcal-recur-rrule-wkst rules))
         (candidates (list (list year month day))))

    (if bymonth
        (setq candidates
              (gcal-recur-ymd-list-filter-matched
               candidates
               (gcal-recur-expand-bymonth year bymonth)))) ;;Limit

    (if bymonthday
        (setq candidates
              (gcal-recur-ymd-list-make-combination
               candidates
               (gcal-recur-expand-bymonthday year bymonthday))))

    (if byday
        (setq candidates
              (gcal-recur-ymd-list-make-combination
               candidates
               ;; within the day
               (mapcar
                #'gcal-absdn-to-ymd
                (gcal-recur-expand-byday-absdn absdn (1+ absdn) byday)))))

    (setq candidates
          (gcal-recur-ymd-list-complete
           candidates
           year
           month
           day))

    candidates))

(defun gcal-recur-complete-hms (ymd-list rules dect-dtstart time-zone)
  (let* ((byhour (gcal-recur-rrule-byhour rules))
         (byminute (gcal-recur-rrule-byminute rules))
         (bysecond (gcal-recur-rrule-bysecond rules))
         ;; Google Calendar does not support hourly, minutely, secondly frequencies.
         ;; Multiple values of byhour, byminute, bysecond properties are also not supported.
         (hour (if (= (length byhour) 1) (car byhour) (decoded-time-hour dect-dtstart)))
         (minute (if (= (length byminute) 1) (car byminute) (decoded-time-minute dect-dtstart)))
         (second (if (= (length bysecond) 1) (car bysecond) (decoded-time-second dect-dtstart))))
    (gcal-recur-ymd-list-to-encoded-time ymd-list hour minute second time-zone)))



;;
;; Expand BYxxxxx rules
;;

(defun gcal-recur-expand-bymonth (year bymonth)
  (mapcar (lambda (month) (list year month nil)) bymonth))

(defun gcal-recur-expand-byweekno (year wkst byweekno)
  (let ((absdn-weekno1 (gcal-absdn-first-week-of-year year wkst))
        (absdn-weekno1-next-year (gcal-absdn-first-week-of-year (1+ year) wkst)))

    (mapcan
     (lambda (weekno)
       (let ((absdn-week
              (cond
               ((>= weekno 1)
                (+ absdn-weekno1 (* (1- weekno) 7)))
               ((<= weekno -1)
                (+ absdn-weekno1-next-year (* weekno 7)))
               (t
                (error "Invalid weekno %s" weekno))))
             dates-in-week)
         (dotimes (i 7)
           (let ((ymd (gcal-absdn-to-ymd (+ absdn-week i))))
             (if (= (car ymd) year)
                 (push ymd dates-in-week))))
         (nreverse dates-in-week)))
     byweekno)))
;;TEST: (gcal-recur-expand-byweekno 2021 0 '(1 -1)) => ((2021 1 3) (2021 1 4) (2021 1 5) (2021 1 6) (2021 1 7) (2021 1 8) (2021 1 9) (2021 12 26) (2021 12 27) (2021 12 28) (2021 12 29) (2021 12 30) (2021 12 31))

(defun gcal-recur-expand-byyearday (year byyearday)
  (mapcar
   (lambda (d)
     (let ((tm (decoded-time-add
               (if (< d 0)
                   (make-decoded-time :year (1+ year) :month 1 :day 1)
                 (make-decoded-time :year year :month 1 :day 0))
               (make-decoded-time :day d))))
       (list
        (decoded-time-year tm)
        (decoded-time-month tm)
        (decoded-time-day tm))))
   byyearday))
;;TEST: (gcal-recur-expand-byyearday 2021 '(1 2 -1 -2)) => ((2021 1 1) (2021 1 2) (2021 12 31) (2021 12 30))

(defun gcal-recur-expand-bymonthday (year bymonthday)
  ;; Keep negative day because month may not be fixed.
  ;; FREQ=YEARLY;BYMONTHDAY=-1,-2,-3;BYDAY=MO
  ;; see: gcal-recur-complete-negative-day
  (mapcar (lambda (d) (list year nil d)) bymonthday))
;;TEST: (gcal-recur-expand-bymonthday 2021 '(1 2 3 -1 -2 -3))

(defun gcal-recur-expand-byday (year month-or-bymonth byday)
  ;; (From https://tools.ietf.org/html/rfc5545#section-3.3.10)
  ;; Each BYDAY value can also be preceded by a positive (+n) or
  ;; negative (-n) integer.  If present, this indicates the nth
  ;; occurrence of a specific day within the MONTHLY or YEARLY "RRULE".

  ;; For example, within a MONTHLY rule, +1MO (or simply 1MO)
  ;; represents the first Monday within the month, whereas -1MO
  ;; represents the last Monday of the month.  The numeric value in a
  ;; BYDAY rule part with the FREQ rule part set to YEARLY corresponds
  ;; to an offset within the month when the BYMONTH rule part is
  ;; present, and corresponds to an offset within the year when the
  ;; BYWEEKNO or BYMONTH rule parts are present.  If an integer
  ;;             ^^^^^^^ ?
  ;; modifier is not present, it means all days of this type within the
  ;; specified frequency.  For example, within a MONTHLY rule, MO
  ;; represents all Mondays within the month.  The BYDAY rule part MUST
  ;; NOT be specified with a numeric value when the FREQ rule part is
  ;; not set to MONTHLY or YEARLY.  Furthermore, the BYDAY rule part
  ;; MUST NOT be specified with a numeric value with the FREQ rule part
  ;; set to YEARLY when the BYWEEKNO rule part is specified.

  (mapcar
   #'gcal-absdn-to-ymd
   (cond
    ;; FREQ=MONTHLY
    ((integerp month-or-bymonth)
     (gcal-recur-expand-byday-absdn
      (gcal-absdn year month-or-bymonth 1)
      (gcal-absdn year (1+ month-or-bymonth) 1)
      byday))
    ;; FREQ=YEARLY;BYMONTH=
    ((consp month-or-bymonth)
     (mapcan
      (lambda (m)
        (gcal-recur-expand-byday-absdn
         (gcal-absdn year m 1)
         (gcal-absdn year (1+ m) 1)
         byday))
      month-or-bymonth))
    ;; FREQ=YEARLY
    (t
     (gcal-recur-expand-byday-absdn
      (gcal-absdn year 1 1)
      (gcal-absdn (1+ year) 1 1)
      byday)))))
;;TEST: (gcal-recur-expand-byday 2021 2 '((nil . 1) (nil . 2))) => ((2021 2 1) (2021 2 2) (2021 2 8) (2021 2 9) (2021 2 15) (2021 2 16) (2021 2 22) (2021 2 23))
;;TEST: (gcal-recur-expand-byday 2021 '(1 2) '((1 . 1) (-1 . 2))) => ((2021 1 4) (2021 1 26) (2021 2 1) (2021 2 23))
;;TEST: (gcal-recur-expand-byday 2021 nil '((1 . 1) (-1 . 2))) => ((2021 1 4) (2021 12 28))

(defun gcal-recur-expand-byday-absdn (absdn-lower absdn-upper byday)
  (seq-uniq
   (sort
    (mapcan
     (lambda (num-day)
       (let* ((num (car num-day))
              (day (cdr num-day))
              (first-day (+ absdn-lower (mod (- day absdn-lower) 7)))
              (last-day (- (1- absdn-upper) (mod (- (1- absdn-upper) day) 7)))
              result-days)

         (cond
          ;; All days within the period(between absdn-lower and absdn-upper)
          ((null num)
           (let ((d first-day))
             (while (< d absdn-upper)
               (push d result-days)
               (setq d (+ d 7)))))
          ;; First day
          ((>= num 1)
           (let ((d (+ first-day (* (1- num) 7))))
             (if (< d absdn-upper)
                 (push d result-days))))
          ;; Last day
          ((<= num -1)
           (let ((d (+ last-day (* (1+ num) 7))))
             (if (>= d absdn-lower)
                 (push d result-days))))
          ;; 0 is invalid
          (t (error "Invalid byday")))

         (nreverse result-days)))
     byday)
    #'<)))
;;TEST: (mapcar #'gcal-absdn-to-ymd (gcal-recur-expand-byday-absdn (gcal-absdn 2021 2 1) (gcal-absdn 2021 2 28) '((nil . 1) (1 . 2) (-1 . 3) (2 . 4) (-2 . 5)))) => ((2021 2 1) (2021 2 2) (2021 2 8) (2021 2 11) (2021 2 15) (2021 2 19) (2021 2 22) (2021 2 24))



;;
;; Limit
;;

(defun gcal-recur-limit-bysetpos (sorted-ymd-list bysetpos)
  (if (null bysetpos)
      sorted-ymd-list
    (let* ((len (length sorted-ymd-list))
           (indices
            (seq-uniq
             (sort (seq-filter
                    (lambda (pos) (and (>= pos 0) (< pos len)))
                    (mapcar
                     (lambda (pos) (cond
                                    ((<= pos -1) (+ len pos))
                                    ((>= pos 1) (1- pos))
                                    (t (error "Invalid bysetpos"))))
                     bysetpos))
                   #'<)))
           (index 0)
           result)
      (while (and sorted-ymd-list indices)
        (when (= (car indices) index)
          (push (car sorted-ymd-list) result)
          (setq indices (cdr indices)))
        (setq sorted-ymd-list (cdr sorted-ymd-list))
        (setq index (1+ index)))
      (nreverse result))))
;;TEST: (gcal-recur-limit-bysetpos '((2021 2 17) (2021 2 18) (2021 2 19)) '(1 -1 -2 -3 -4))

(defun gcal-recur-limit-by-count (list count)
  ;; (seq-take list count) is non-destructive
  (cond
   ((null count) list)
   ((<= count 0) nil)
   ((<= (length list) count) list)
   (t
    (setcdr (nthcdr (1- count) list) nil) ;;destructive
    list)))



;;
;; Year/Month/Day
;;

(defun gcal-recur-ymd-match-p (l r)
  (while (and l r (or (null (car l)) (null (car r)) (= (car l) (car r))))
    (setq l (cdr l))
    (setq r (cdr r)))
  (and (null l) (null r)))
;;TEST: (gcal-recur-ymd-match-p '(2021 8 nil) '(2021 8 14))

(defun gcal-recur-ymd-list-filter-matched (pattern-list target-list)
  (if pattern-list
      (seq-filter
       (lambda (target)
         (seq-contains-p pattern-list target #'gcal-recur-ymd-match-p))
       target-list)))
;;TEST: (gcal-recur-ymd-list-filter-matched '((2021 2 nil) (2021 4 nil)) '((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))) => ((2021 2 10) (2021 4 10))
;;TEST: (gcal-recur-ymd-list-filter-matched nil '((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))) => nil
;;TEST: (gcal-recur-ymd-list-filter-matched '((nil nil nil)) '((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))) => ((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))
;;TEST: (gcal-recur-ymd-list-filter-matched '((2021 3 nil) (2021 5 nil)) '((nil nil 13) (nil nil 18) (nil nil 21))) => ((nil nil 13) (nil nil 18) (nil nil 21))
;;TEST: (gcal-recur-ymd-list-filter-matched '((2021 3 12) (2021 3 13) (2021 3 14)) '((nil nil 13) (nil nil 14))) => ((nil nil 13) (nil nil 14))
;;TEST: (gcal-recur-ymd-list-filter-matched nil '((nil nil 13) (nil nil 14))) => nil
;;TEST: (gcal-recur-ymd-list-filter-matched '((nil nil nil)) '((nil nil 13) (nil nil 14))) => ((nil nil 13) (nil nil 14))

(defun gcal-recur-ymd-fulldate-p (ymd)
  (and (car ymd) (cadr ymd) (caddr ymd)))

(defun gcal-recur-ymd-list-fulldate-p (ymd-list)
  (not (seq-some (lambda (ymd) (not (gcal-recur-ymd-complete-p ymd))) ymd-list)))
;;TEST: (gcal-recur-ymd-list-fulldate-p '((2021 2 16) (2021 2 17) (2021 2 18))) => t
;;TEST: (gcal-recur-ymd-list-fulldate-p '((2021 2 nil) (2021 2 nil) (2021 2 nil))) => nil

(defun gcal-recur-ymd-complete (ymd year month day)
  "Fill in the undetermined part of the YMD to YEAR/MONTH/DAY."
  (gcal-recur-ymd-filter-invalid-date
   (if (gcal-recur-ymd-fulldate-p ymd)
       ymd
     (let ((year (or (car ymd) year))
           (month (or (cadr ymd) month))
           (day (or (caddr ymd) day)))
       (list
        year
        month
        (gcal-recur-complete-negative-day year month day))))))
;;TEST: (gcal-recur-ymd-complete '(nil nil nil) 2021 2 14) => (2021 2 14)
;;TEST: (gcal-recur-ymd-complete '(2022 10 nil) 2021 2 14) => (2022 10 14)
;;TEST: (gcal-recur-ymd-complete '(2022 10 20) 2021 2 14) => (2022 10 20)
;;TEST: (gcal-recur-ymd-complete '(2021 nil -28) 2021 2 14) => (2021 2 1)
;;TEST: (gcal-recur-ymd-complete '(2021 nil -28) 2021 2 14) => (2021 2 1)
;;TEST: (gcal-recur-ymd-complete '(2021 nil -31) 2021 2 14) => nil

(defun gcal-recur-ymd-list-complete (ymd-list year month day)
  (mapcar
   (lambda (ymd)
     (gcal-recur-ymd-complete ymd year month day))
   ymd-list))

(defun gcal-recur-complete-negative-day (year month day)
  (if (and year month day)
      (let ((days-in-month (date-days-in-month year month)))
        (if (<= day -1)
            (+ days-in-month day 1)
          day))
    day))
;;TEST: (gcal-recur-complete-negative-day 2021 2 -1) => 28

(defun gcal-recur-ymd-filter-invalid-date (ymd)
  (if (or (not (gcal-recur-ymd-fulldate-p ymd))
          ;; 1 <= day <= (date-days-in-month year month)
          (<= 1 (caddr ymd) (date-days-in-month (car ymd) (cadr ymd))))
      ymd
    nil))

(defun gcal-recur-ymd-mix (l-ymd r-ymd)
  (catch 'gcal-recur--break
    (let* ((year (gcal-recur-ymd-mix-num (car l-ymd) (car r-ymd)))
           (month (gcal-recur-ymd-mix-num (cadr l-ymd) (cadr r-ymd)))
           (day (gcal-recur-ymd-mix-num
                 (gcal-recur-complete-negative-day year month (caddr l-ymd))
                 (gcal-recur-complete-negative-day year month (caddr r-ymd)))))
      (gcal-recur-ymd-filter-invalid-date
       (list year month day)))))
;;TEST: (gcal-recur-ymd-mix '(2021 7 nil) '(nil nil 10)) => (2021 7 10)
;;TEST: (gcal-recur-ymd-mix '(2021 2 nil) '(nil nil 29)) => nil
;;TEST: (gcal-recur-ymd-mix '(2021 7 nil) '(nil 8 10)) => nil
;;TEST: (gcal-recur-ymd-mix '(2021 2 nil) '(nil 2 -28)) => (2021 2 1)
;;TEST: (gcal-recur-ymd-mix '(2021 2 1) '(nil 2 -28)) => (2021 2 1)
;;TEST: (gcal-recur-ymd-mix '(2021 nil -28) '(nil 2 nil)) => (2021 2 1)

(defun gcal-recur-ymd-mix-num (l r)
  (cond
   ((null l) r)
   ((null r) l)
   ((= l r) l)
   (t (throw 'gcal-recur--break nil))))

(defun gcal-recur-ymd-list-make-combination (l-list r-list)
  (let (result)
    (dolist (l l-list)
      (dolist (r r-list)
        (if-let ((ymd (gcal-recur-ymd-mix l r)))
            (push ymd result))))
    (nreverse result)))
;;TEST: (gcal-recur-ymd-list-make-combination '((2021 2 nil) (2021 4 nil)) '((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))) => ((2021 2 10) (2021 4 10))
;;TEST: (gcal-recur-ymd-list-make-combination nil '((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))) => nil
;;TEST: (gcal-recur-ymd-list-make-combination '((nil nil nil)) '((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))) => ((2021 2 10) (2021 3 10) (2021 4 10) (2021 5 10))
;;TEST: (gcal-recur-ymd-list-make-combination '((2021 3 nil) (2021 5 nil)) '((nil nil 13) (nil nil 18) (nil nil 21))) => ((2021 3 13) (2021 3 18) (2021 3 21) (2021 5 13) (2021 5 18) (2021 5 21))
;;TEST: (gcal-recur-ymd-list-make-combination '((2021 3 12) (2021 3 13) (2021 3 14)) '((nil nil 13) (nil nil 14))) => ((2021 3 13) (2021 3 14))
;;TEST: (gcal-recur-ymd-list-make-combination nil '((nil nil 13) (nil nil 14))) => nil
;;TEST: (gcal-recur-ymd-list-make-combination '((nil nil nil)) '((nil nil 13) (nil nil 14))) => ((nil nil 13) (nil nil 14))

(defun gcal-recur-ymd-less-p (l r)
  (or
   (< (car l) (car r))
   (and
    (= (car l) (car r))
    (or
     (< (cadr l) (cadr r))
     (and
      (= (cadr l) (cadr r))
      (< (caddr l) (caddr r)))))))

(defun gcal-recur-ymd-list-sort-and-unique (ymd-list)
  ;;@todo Remove same value with adjacent only
  (seq-uniq
   (sort ymd-list #'gcal-recur-ymd-less-p)))

(defun gcal-recur-split-list (lst predicate)
  (cond
   ((null lst)
    (cons nil nil))
   ((funcall predicate (car lst))
    (cons nil lst))
   (t
    (let ((p lst))
      (while (and (cdr p) (not (funcall predicate (cadr p))))
        (setq p (cdr p)))
      (prog1 (cons lst (cdr p))
        (setcdr p nil))))))

(defun gcal-recur-sorted-ymd-list-after (ymd-list-sorted ymd-lower lower-inclusive)
  (cdr
   (gcal-recur-split-list
    ymd-list-sorted
    (lambda (ymd)
      (if lower-inclusive
          (not (gcal-recur-ymd-less-p ymd ymd-lower))
        (gcal-recur-ymd-less-p ymd-lower ymd))))))

(defun gcal-recur-sorted-encoded-time-list-after (enct-list-sorted enct-lower lower-inclusive)
  (cdr
   (gcal-recur-split-list
    enct-list-sorted
    (lambda (enct)
      (if lower-inclusive
          (not (time-less-p enct enct-lower))
        (time-less-p enct-lower enct))))))

(defun gcal-recur-sorted-encoded-time-list-between (enct-list-sorted
                                                    enct-lower lower-inclusive
                                                    enct-upper upper-inclusive)
  (car
   (gcal-recur-split-list
    (cdr
     (gcal-recur-split-list
      enct-list-sorted
      (lambda (enct)
        (if lower-inclusive
            (not (time-less-p enct enct-lower))
          (time-less-p enct-lower enct)))))
    (lambda (enct)
      (if upper-inclusive
          (time-less-p enct-upper enct)
        (not (time-less-p enct enct-upper)))))))

(defun gcal-recur-ymd-to-encoded-time (ymd h m s time-zone)
  (encode-time (list s m h (caddr ymd) (cadr ymd) (car ymd) nil nil time-zone)))
;;TEST: (decode-time (gcal-recur-ymd-to-encoded-time '(2012 3 4) 5 6 7 nil)) => (7 6 5 4 3 2012 0 nil 32400)

(defun gcal-recur-ymd-list-to-encoded-time (ymd-list h m s time-zone)
  (mapcar
   (lambda (ymd)
     (gcal-recur-ymd-to-encoded-time ymd h m s time-zone))
   ymd-list))



;;
;; Absolute Day Number
;;

(defun gcal-absdn (year month day)
  ;;(time-to-days (encode-time (list 0 0 0 day month year nil nil nil)))
  (calendar-absolute-from-gregorian (list month day year)))

(defun gcal-absdn-day-of-week (absdn)
  (mod absdn 7))

(defun gcal-absdn-from-decoded-time (dect)
  (gcal-absdn
   (decoded-time-year dect)
   (decoded-time-month dect)
   (decoded-time-day dect)))

(defun gcal-absdn-to-decoded-time (absdn)
  ;; Is converting function in time-date.el?
  (let ((date (calendar-gregorian-from-absolute absdn)))
    (make-decoded-time :year (caddr date)
                       :month (car date)
                       :day (cadr date))))

(defun gcal-absdn-to-ymd (absdn)
  ;; Is converting function in time-date.el?
  (let ((date (calendar-gregorian-from-absolute absdn)))
    (list (caddr date)
          (car date)
          (cadr date))))

(defun gcal-absdn-first-week-of-year (year wkst)
  ;; A week is defined as a seven day period, starting on the day of
  ;; the week defined to be the week start (see WKST).  Week number
  ;; one of the calendar year is the first week that contains at least
  ;; four (4) days in that calendar year.
  ;; (from https://tools.ietf.org/html/rfc5545#section-3.3.10)
  (let* ((absdn-jan1 (gcal-absdn year 1 1))
         (weekdn-jan1 (gcal-absdn-day-of-week (- absdn-jan1 wkst))))
    (+
     (- absdn-jan1 weekdn-jan1)
     (if (>= weekdn-jan1 4) 7 0))))

(defun gcal-absdn-to-abs-week-number (absdn wkst)
  (/ (- absdn wkst) 7))

(defun gcal-absdn-from-abs-week-number (abs-week-number wkst)
  (+ (* abs-week-number 7) wkst))



;;
;; Parse String
;;

(defun gcal-recur-parse-content-line (line)
  ;; https://tools.ietf.org/html/rfc5545#section-3.1
  (when (stringp line)
    (let* ((nameparam-value (gcal-string-divide line ?:))
           (nameparam (car nameparam-value))
           (value (cdr nameparam-value))
           (name-param (gcal-string-divide nameparam ?\;))
           (name (car name-param))
           (param (cdr name-param)))
      (list
       name
       (gcal-recur-parse-value-properties param)
       value))))
;;TEST: (gcal-recur-parse-content-line "RDATE;VALUE=DATE:19970304,19970504") => ("RDATE" (("VALUE" . "DATE")) "19970304,19970504")

(defun gcal-recur-parse-value-properties (value)
  ;; https://tools.ietf.org/html/rfc5545#section-3.1.1
  (when (stringp value)
    (mapcar (lambda (v) (gcal-string-divide v ?=)) (split-string value ";"))))
;;TEST: (gcal-recur-parse-value-properties "FREQ=YEARLY;INTERVAL=2") => (("FREQ" . "YEARLY") ("INTERVAL" . "2"))

(defun gcal-recur-parse-number-list (str)
  (when (stringp str)
    (mapcar #'string-to-number (split-string str " *, *"))))
;; TEST (gcal-recur-parse-number-list "1,2,3") => (1 2 3)

(defun gcal-recur-parse-bywday-list (str)
  (when (stringp str)
    (save-match-data
      (mapcar
       (lambda (weekdaynum)
         (when (string-match "\\([-+]?[0-9]+\\)?\\([A-Z][A-Z]\\)" weekdaynum)
           (cons
            (if-let ((num (match-string 1 weekdaynum)))
                (string-to-number num))
            (gcal-recur-parse-weekday (match-string 2 weekdaynum)))))
       (split-string str " *, *")))))
;; TEST: (gcal-recur-parse-bywday-list "MO,1TU,-2WED") => ((nil . 1) (1 . 2) (-2 . 3))

(defun gcal-recur-parse-weekday (str)
  (seq-position '("SU" "MO" "TU" "WE" "TH" "FR" "SA") str))

(defun gcal-recur-rrule-freq (rules)
  (cdr
   (assoc
    (cdr (assoc "FREQ" rules))
    '(("YEARLY" . yearly)
      ("MONTHLY" . monthly)
      ("WEEKLY" . weekly)
      ("DAILY" . daily)
      ("HOURLY" . hourly)
      ("MINUTELY" . minutely)
      ("SECONDLY" . secondly)))))
(defun gcal-recur-rrule-interval (rules)
  (max 1 (string-to-number (or (cdr (assoc "INTERVAL" rules)) "1"))))
(defun gcal-recur-rrule-bymonth (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYMONTH" rules))))
(defun gcal-recur-rrule-byweekno (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYWEEKNO" rules))))
(defun gcal-recur-rrule-byyearday (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYYEARDAY" rules))))
(defun gcal-recur-rrule-bymonthday (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYMONTHDAY" rules))))
(defun gcal-recur-rrule-byday (rules)
  (gcal-recur-parse-bywday-list (cdr (assoc "BYDAY" rules))))
(defun gcal-recur-rrule-byhour (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYHOUR" rules))))
(defun gcal-recur-rrule-byminute (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYMINUTE" rules))))
(defun gcal-recur-rrule-bysecond (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYSECOND" rules))))
(defun gcal-recur-rrule-bysetpos (rules)
  (gcal-recur-parse-number-list (cdr (assoc "BYSETPOS" rules))))
(defun gcal-recur-rrule-wkst (rules)
  (gcal-recur-parse-weekday (or (cdr (assoc "WKST" rules)) "MO"))) ;;default=MO(1)
(defun gcal-recur-rrule-count (rules)
  (when-let ((str (cdr (assoc "COUNT" rules))))
    (string-to-number str)))
(defun gcal-recur-rrule-until (rules time-zone)
  (when-let ((str (cdr (assoc "UNTIL" rules))))
    (save-match-data
      (if (string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)\\'" str)
          ;;date only
          (encode-time
           (list
            59 59 23 ;;last second of the date in local time
            (string-to-number (match-string 3 str))
            (string-to-number (match-string 2 str))
            (string-to-number (match-string 1 str))
            nil nil time-zone))
        ;; date-time
        (parse-iso8601-time-string str)))))



;;
;; String
;;

(defun gcal-string-divide (string divider)
  (if-let ((pos (seq-position string divider)))
      (cons (substring string 0 pos)
            (substring string (1+ pos)))
    (cons string nil)))
;;TEST: (gcal-string-divide "abc" ?:) => ("abc")
;;TEST: (gcal-string-divide "abc:" ?:) => ("abc" . "")
;;TEST: (gcal-string-divide "abc:def" ?:) => ("abc" . "def")
;;TEST: (gcal-string-divide "abc:def:ghi" ?:) => ("abc" . "def:ghi")



;;
;; Sexp Diary Entry
;;

;; %%(gcal-recur-diary "2021-02-22" ["RRULE:FREQ=DAILY;BYDAY=MO,TU,WED"]) Test Entry
(defun gcal-recur-diary (dtstart recurrence &optional mark)
  (with-no-warnings (defvar date) (defvar entry))
  (let* (;; calendar date
         (m (calendar-extract-month date))
         (d (calendar-extract-day date))
         (y (calendar-extract-year date))
         ;; dtstart
         (dtstart-parsed (parse-time-string dtstart))
         (dtstart-date-only (or (null (decoded-time-second dtstart-parsed))
                                (null (decoded-time-minute dtstart-parsed))
                                (null (decoded-time-hour dtstart-parsed))))
         (dtstart-datetime (if dtstart-date-only
                               (nconc (list 0 0 0) (cdddr dtstart-parsed))
                             dtstart-parsed))
         ;; occurrences
         (event-times
          (gcal-recur-between
           recurrence
           (encode-time dtstart-datetime)
           (encode-time (list 0 0 0 d m y nil nil nil)) t
           (encode-time (list 0 0 0 (1+ d) m y nil nil nil)) nil
           1)))
    (message "entry=%s" entry)
    (if event-times
        (cons mark
              (if dtstart-date-only
                  entry
                (concat (format-time-string "%R " (car event-times)) entry))))))



(provide 'gcal-recur)
;;; gcal-recur.el ends here
