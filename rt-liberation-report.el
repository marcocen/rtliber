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

(defun rt-liberation-report-resolved-interval (rt-queue start-date end-date)
  "Return tickets resolved between START-DATE and END-DATE.

The tickets must have their current status be Resolved in order
to be returned by this function. If no tickets match the query,
return `nil'."
  (rt-liber-rest-run-show-base-query
   (rt-liber-rest-run-ls-query
    (rt-liber-compile-query
     (and (queue    rt-queue)
	  (resolved start-date end-date)
	  (status   "resolved"))))))

;; (rt-liberation-report-resolved-interval "licensing" "2015-09-18" "2015-09-17")


;; ((("TimeLeft" . "0")
;;   ("TimeWorked" . "0")
;;   ("TimeEstimated" . "0")
;;   ("Told" . "Thu Oct 08 00:53:59 2015")
;;   ("Resolved" . "Thu Oct 08 00:53:59 2015")
;;   ("Due" . "Not set")
;;   ("Started" . "Wed Oct 07 06:52:03 2015")
;;   ("Starts" . "Not set")
;;   ("Created" . "Wed Oct 07 06:39:22 2015")
;;   ("Requestors" . "ian.macintosh@gtxweb.uk")
;;   ("FinalPriority" . "0")
;;   ("InitialPriority" . "0")
;;   ("Priority" . "0")
;;   ("Status" . "resolved")
;;   ("Subject" . "Badly worded and misleading paragraph")
;;   ("Creator" . "ian.macintosh@gtxweb.uk")
;;   ("Owner" . "jgay@fsf.org")
;;   ("Queue" . "licensing")
;;   ("id" . "ticket/1052419"))
;;  (("TimeLeft" . "0")
;;   ("TimeWorked" . "0")
;;   ("TimeEstimated" . "0")
;;   ("Told" . "Thu Oct 08 07:54:47 2015")
;;   ("Resolved" . "Thu Oct 08 07:54:47 2015")
;;   ("Due" . "Not set")
;;   ("Started" . "Wed Oct 07 12:03:20 2015")
;;   ("Starts" . "Not set")
;;   ("Created" . "Mon Sep 21 13:33:24 2015")
;;   ("Requestors" . "juan.balderas.0@gmail.com")
;;   ("FinalPriority" . "0")
;;   ("InitialPriority" . "0")
;;   ("Priority" . "0")
;;   ("Status" . "resolved")
;;   ("Subject" . "Softare donation to FSF")
;;   ("Creator" . "juan.balderas.0@gmail.com")
;;   ("Owner" . "donald")
;;   ("Queue" . "licensing")
;;   ("id" . "ticket/1048183")))


(provide 'rt-report)

;;; rt-liberation-report.el ends here.
