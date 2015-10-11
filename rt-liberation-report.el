;;; rt-report.el --- Free from RT

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



(provide 'rt-report)

;;; rt-report.el ends here.
