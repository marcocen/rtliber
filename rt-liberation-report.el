;;; rt-liberation-report.el --- Free from RT

;; Copyright (C) 2015  Yoni Rabkin
;;
;; Authors: Yoni Rabkin <yrk@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; History:
;;
;; I wrote rt-report.py sometime in 2013 because people asked for some
;; information as to how many tickets were being resolved, and by
;; whom. When in came time up update rt-report.py I came to my senses
;; and decided to re-write it in Emacs Lisp as a part of
;; rt-liberation.


;;; Code:

(require 'rt-liberation-rest)

(defvar rt-liber-report-csv-header
  '("date" "tickets resolved")
  "Headers for comma separated value output.")

(defun rt-liber-report-get-interval (rt-queue start-date end-date)
  "Return tickets resolved between START-DATE and END-DATE.

The tickets must have their current status be Resolved in order
to be returned by this function. If no tickets match the query,
return `nil'."
  (when (or (not (stringp rt-queue))
	    (not (stringp start-date))
	    (not (stringp end-date)))
    (error "bad argument/s"))
  (rt-liber-rest-run-show-base-query
   (rt-liber-rest-run-ls-query
    (rt-liber-compile-query
     (and (queue    rt-queue)
	  (resolved start-date end-date)
	  (status   "resolved"))))))

(defun rt-liber-report-scan-ticket (ticket-alist)
  (let ((date-resolved (cdr (assoc "Resolved" ticket-alist)))
	(owner         (cdr (assoc "Owner" ticket-alist))))
    `(,(format-time-string "%d-%m-%Y" (date-to-time date-resolved))
      . ,owner)))

(defun rt-liber-report-scan-interval (interval)
  "Convert the list of tickets into an ordered format."
  (when (not interval)
    (error "no tickets in interval"))
  (let ((l (copy-tree interval))
	(r nil))
    (while l
      (setq r (append r `(,(rt-liber-report-scan-ticket (car l)))))
      (setq l (cdr l)))
    (setq r (sort r
		  #'(lambda (a b)
		      (string-lessp (car a) (car b)))))
    r))

(defun rt-liber-report-count-by-date (l)
  (let ((h (make-hash-table :test 'equal)))
    (while l
      (let ((d (or (gethash (caar l) h) 0)))
	(puthash (caar l) (+ d 1) h))
      (setq l (cdr l)))
    h))

(defun rt-liber-report-count-by-owner (l)
  (let ((h (make-hash-table :test 'equal)))
    (while l
      (let ((d (or (gethash (cdar l) h) 0)))
	(puthash (cdar l) (+ d 1) h))
      (setq l (cdr l)))
    h))

(defun rt-liber-report (rt-queue start-date end-date)
  (let ((tickets (rt-liber-report-scan-interval
		  (rt-liber-report-get-interval
		   rt-queue start-date end-date)))
	by-date by-owner)
    (when (not tickets)
      (error (concat "no tickets in interval between "
		     start-date " and " end-date)))
    (setq by-date  (rt-liber-report-count-by-date tickets)
	  by-owner (rt-liber-report-count-by-owner tickets))))


(provide 'rt-report)

;;; rt-liberation-report.el ends here.
