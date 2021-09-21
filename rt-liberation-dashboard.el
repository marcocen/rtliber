;;; rt-liberation-dashboard.el --- A dashboard for rt-liberation

;; Copyright (C) 2021 Marco Centurión
;;
;; Authors: Marco Centurión <mcenturion@fing.edu.uy>
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

;;; Installation:
;;
;; For installation instructions and detailed help please see the
;; wonderful rt-liberation manual located in the "doc/" directory of
;; the rt-liberation distribution.

(defgroup rt-liber-dash nil
  "*Dashboard for rt-liberation."
  :prefix "rt-liber-dash-"
  :group 'rt-liber-dash)

;;; Customizable Variables
(defcustom rt-liber-dash-saved-queries
  '((:name "My Opened Tickets"
	   :query "Status = 'open'"))
  "*A plist of saved searches to show in the dashboard"
  :type '(plist)
  :group 'rt-liber-dash)

(defcustom rt-liber-dash-queues nil
  "A list of queues to add in the queues section of the dashboard"
  :type '(list)
  :group 'rt-liber-dash)

(require 'widget)
(require 'rt-liberation)
(require 'magit)

(defun rt-liber-dashboard ()
  "Create a dashboard with saved rt-liberation queries."
  (interactive)
  (switch-to-buffer "*RT Dashboard*")
  (setq-local revert-buffer-function
	      '(lambda (&optional ignore-auto noconfirm)
		(rt-liber-dashboard)))
  (let ((magit-insert-section--parent magit-root-section)
	(inhibit-read-only t))
    (erase-buffer)
    (widget-insert "RT Dashboard\n\n")
    (magit-insert-section (saved-queries nil t)
      (magit-insert-heading "Saved Queries")
      (rt-liber-dash-insert-queries))
    (magit-section-mode)))


(defun rt-liber-dash-insert-queries ()
  "Inserts all query sections"
  (dolist (q rt-liber-dash-saved-queries)
    (rt-liber-dash-insert-query q)))

(defun rt-liber-dash-insert-query (q)
  (let* ((name   (plist-get q :name))
	 (query  (plist-get q :query))
	 (key    (plist-get q :key))
	 (ticketlist (rt-liber-rest-run-subject-query query)))
    (magit-insert-section (magit-section name t)
      (magit-insert-heading (propertize name 'query query))
	(dolist (ticket ticketlist)
	  (apply 'rt-liber-dash-insert-ticket-link ticket)))))

(defun rt-liber-dash-insert-ticket-link (id subject)
  "Insert a link to view the ticket with ID history"
  (magit-insert-section (ticket nil t)
    (widget-create 'link
		   :notify `(lambda (&rest ignore)
			      (rt-liber-dash-browse-ticket-by-id ',id))
		   :button-face "bold"
		   :button-prefix ""
		   :button-suffix ""
		   (format "  [#%s] %s" id subject))
    (widget-insert "\n")))


(defun rt-liber-dash-insert-queues ()
  "Inserts all saved queues links"
  (dolist (queue rt-liber-dash-queues)
    (rt-liber-dash-insert-queue-link queue)))

(defun rt-liber-dash-insert-queue-link (queue)
  "Insert a link to view a queue"
  (magit-insert-section (queue nil t)
    (widget-create 'link
		   :notify `(lambda (&rest ignore)
			      (rt-liber-browse-query
			       (format "Queue = '%s' and Status = 'open'"
				       queue)))
		   :button-prefix ""
		   :button-suffix ""
		   (format "%s\n" queue))))

(defun rt-liber-dash-insert-search ()
  "Insert a search widget."
  (widget-insert "Search: ")
  (widget-create 'editable-field
		 :action (lambda (widget &rest ignore)
			   (rt-liber-dash-search (widget-value widget))))
  (widget-insert ".")
  (put-text-property (1- (point)) (point) 'invisible t)
  (widget-insert "\n"))

(defun rt-liber-dash-insert-queue-button (q)
  "Inserts a button to see all tickets in QUEUE"
  (widget-create 'link
		 :notify `(lambda (&rest ignore)
			    (rt-liber-browse-query
			     (format "Queue = '%s'" ,q)))
		 :button-prefix ""
		 :button-suffix ""
		 q))

(defun rt-liber-dash-insert-query-buttons (queries)
  "Adds a button for every query in the queries plist"
  (dolist (q queries)
    (rt-liber-dash-insert-query-button q)))

(defun rt-liber-dash-insert-query-button (q)
  "Inserts a query widget for Q"
  (let* ((name   (plist-get q :name))
	 (query  (plist-get q :query))
	 (key    (plist-get q :key))
	 (count  (length (rt-liber-rest-run-ls-query query))))
    ;; to left-align the ticket counts.  3 is a magic number and should
    ;; eventually be replaced by the max length of the query counts
    (widget-insert (make-string (- 3 (length (number-to-string count)))
				? ))
    (widget-insert (number-to-string count))
    (widget-insert " ")
    (widget-create 'link
		   :notify `(lambda (&rest ignore)
			      (rt-liber-browse-query ,query))
		   :button-prefix ""
		   :button-suffix ""
		   name)
    (if key
	(widget-insert (format " (%s %s)\n" rt-liber-dash-jump-key key))
      (widget-insert "\n"))))

;;; Auxiliary functions
(defun rt-liber-dash-search (term)
  "Searches for the given TERM.

If TERM is a number, show the ticket with that number.

If TERM is anything other, run a search based on subject"
  (interactive "MSearch: ")
  (if (string= term (number-to-string (string-to-number term)))
      (rt-liber-dash-browse-ticket-by-id term)
    (rt-liber-browse-query (format "subject LIKE '%s'" term))))

(defun rt-liber-dash-browse-ticket-by-id (id)
  "Opens the TICKET-ID ticket"
  (interactive "MTicket id: ")
  (let ((ticket-alist `((,id "."))))
    (rt-liber-display-ticket-history (car (rt-liber-rest-run-show-base-query ticket-alist)))))

(provide 'rt-liberation-dashboard)
