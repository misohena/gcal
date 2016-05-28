;;; gcal-id.el --- Google Calendar ID                -*- lexical-binding: t; -*-

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

;;; Code:

(defvar gcal-base32hex-table "0123456789ABCDEFGHIJKLMNOPQRSTUV")

(defun gcal-base32hex-p (string)
  (not
   (find-if-not (lambda (c) (position c gcal-base32hex-table)) (upcase string))))

(defun gcal-base32hex-decode (string)
  "RFC2938"
  (let* ((nums (mapcar
                (lambda (c) (position c gcal-base32hex-table))
                (upcase string)))
         (nums-len (length nums))
         (i 0)
         (last-byte 0)
         (result nil))
    (while (< i nums-len)
      (let* ((num (elt nums i))
             (dstbit (% (* i 5) 8)))

        (setq last-byte (logior last-byte (lsh num (- 3 dstbit))))

        (when (>= dstbit 3)
          (push last-byte result)
          (setq last-byte (logand (lsh num (- (+ 8 3) dstbit)) 255))))

      (setq i (1+ i)))

    (if (> (% nums-len 8) 0)
        (push last-byte result))

    (concat (nreverse result))))

(defun gcal-base32hex-encode (string)
  "RFC2938"
  (let* ((string-len (length string))
         (i 0)
         (last-byte 0)
         (result nil))
    (while (< i string-len)
      (let* ((char (elt string i))
             (dstbit (% (* i 8) 5))
             (left (logior (lsh char (- -3 dstbit)) last-byte))
             (center (logand (lsh char (- 2 dstbit)) 31))
             (right (logand (lsh char (- 7 dstbit)) 31)))
        (push left result)
        (if (< dstbit 2)
            (setq last-byte center)
          (setq last-byte right)
          (push center result)))
      (setq i (1+ i)))

    (if (> (% string-len 5) 0)
        (push last-byte result))

    (concat
     (mapcar
      (lambda (n) (elt gcal-base32hex-table n))
      (nreverse result)))))

(defun gcal-hexstr-to-bytes (str)
  (concat
   (loop for (a b) on (mapcar (lambda (c) (string-to-number (string c) 16)) str)
         by #'cddr
         collect (+ (lsh a 4) b))))

(defun gcal-hexstr-from-bytes (bytes)
  (mapconcat (lambda (c) (format "%02x" c)) bytes ""))

(defun gcal-uuid-to-bytes (uuid)
  ;;;@todo swap endian?
  (mapconcat
   (lambda (str) (gcal-hexstr-to-bytes str))
   (split-string uuid "-")
   ""))

(defun gcal-uuid-from-bytes (bytes)
  ;;;@todo swap endian?
  (let ((str (gcal-hexstr-from-bytes bytes)))
    (concat
     (substring str 0 8) "-"
     (substring str 8 12) "-"
     (substring str 12 16) "-"
     (substring str 16 20) "-"
     (substring str 20 32))))

(defun gcal-uuid-from-base32hex (base32hex)
  (gcal-uuid-from-bytes (gcal-base32hex-decode base32hex)))

(defun gcal-uuid-to-base32hex (uuid)
  (gcal-base32hex-encode (gcal-uuid-to-bytes uuid)))


(defun gcal-uuid-p (uuid)
  (and
   (stringp uuid)
   (let ((compos (split-string uuid "-")))
     (and (= (length compos) 5)
          (= (length (nth 0 compos)) 8)
          (= (length (nth 1 compos)) 4)
          (= (length (nth 2 compos)) 4)
          (= (length (nth 3 compos)) 4)
          (= (length (nth 4 compos)) 12)
          (null (find-if-not (lambda (c)
                               (or (and (>= c ?0) (<= c ?9))
                                   (and (>= c ?a) (<= c ?f))
                                   (and (>= c ?A) (<= c ?F))
                                   (= c ?-)))
                             uuid))))))

(provide 'gcal-id)
;;; gcal-id.el ends here
