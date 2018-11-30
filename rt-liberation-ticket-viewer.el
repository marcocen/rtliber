;;; rt-liberation-ticket-viewer.el --- Ticket Viewer for rt-liberation

;;; Commentary:

;; Copyright (C) 2016 Juan Diego Campo
;;
;; Authors: Juan Diego Campo <jdcampo@fing.edu.uy>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(defun rt-liber-ticker-viewer-highlight-string (str)
  "Highlight a string, using bold face"
  (put-text-property 0 (length str)
		     'font-lock-face 'bold
		     str)
  str)


(defun rt-liber-ticket-headers (ticket-alist)
  "Return a string to show as header in ticket viewer."
  (if (not ticket-alist)
      (error "Not viewing a ticket!!"))
  (let* ((id         (rt-liber-ticket-id-only ticket-alist))
	 (subject    (cdr (assoc "Subject" ticket-alist)))
	 (status     (cdr (assoc "Status" ticket-alist)))
	 (queue      (cdr (assoc "Queue" ticket-alist)))
	 ; People
	 (owner      (rt-liber-ticket-owner-only ticket-alist))
	 (requestors (cdr (assoc "Requestors" ticket-alist)))
	 (cc    (cdr (assoc "CC" ticket-alist)))
	 (admincc    (cdr (assoc "AdminCC" ticket-alist)))
	 ; Dates
	 (created    (format-time-string
		      rt-liber-browser-time-format-string
		      (date-to-time
		       (cdr (assoc "Created" ticket-alist)))))
	 (updated    (format-time-string
		      rt-liber-browser-time-format-string
		      (date-to-time
		       (cdr (assoc "LastUpdated" ticket-alist)))))
	 (resolved   (if (string= status "resolved")
			 (format-time-string
			  rt-liber-browser-time-format-string
			  (date-to-time
			   (cdr (assoc "Resolved" ticket-alist))))
		       "N/A")))
    (concat (rt-liber-ticker-viewer-highlight-string (concat "#" id ": " subject))
	    "Basics:\n"
	    "     Status: " status "\n"
	    "      Queue: " queue "\n"

	    "\nPeople:\n"
	    "      Owner: " owner "\n"
	    " Requestors: " requestors "\n"
	    "         Cc: " cc "\n"
	    "    AdminCC: " admincc "\n"

	    "\nDates:\n"
	    "    Created: " created "\n"
	    "    Updated: " updated "\n"
	    "   Resolved: " resolved "\n"
    )))


(defvar rt-liber-viewer-ticket-type-format
  '(("^\\(Create\\|Comment\\|Correspond\\)$" . "%r %u - %D\n%C\n--\n")
    ("^\\(EmailRecord\\|CommentEmailRecord\\|SetWatcher\\)$" . "")
    (".*" . "%r %u - %D\n--\n"))
  "Format for each type of history entry.")

(defun rt-liber-ticket-viewer-parse-history ()
  "Parse a ticket history in the current buffer.
Returns a list of history entries."
  (let (ticketbase-list
	ticketbase
	(continue t))
    (while (save-excursion
	     (re-search-forward "^#" (point-max) t))
      (while (and continue
		  (re-search-forward
		   "^\\(\\([\.{}#[:alpha:]]+\\): \\(.*\\)\\)$\\|^--$"
		   (point-max) t))
	(cond
	 ((string= (match-string-no-properties 0) "--")
	  (setq continue nil))
	 ((string= (match-string-no-properties 2) "Content")
	  (let* ((start
		  (save-excursion
		    (beginning-of-line)
		    (re-search-forward "Content: " (point-max) t)
		    (point)))
		 (contents (concat
			    "       "
			    (buffer-substring-no-properties
			     start
			     (or (progn (re-search-forward
					 "^\\(\\([\.{}#[:alpha:]]+\\): \\(.*\\)\\)$\\|^--$"
					 (point-max) t)
					(beginning-of-line)
					(point))
				 (point-max))))))
	    (push (cons "Content" (replace-regexp-in-string "^[ ]+" "" contents))
	 	  ticketbase)))
	 ;; TODO: Handle attachments
	 (t
	  (push (cons (match-string-no-properties 2)
	 	      (match-string-no-properties 3))
	 	ticketbase))))
      (push (copy-sequence ticketbase) ticketbase-list)
      (setq ticketbase nil
	    continue t))
    ticketbase-list))
    
;; accept a history-alist object and return an alist mapping ticket
;; properties to format characters for use in `rt-liber-ticket-format'.
(defun rt-liber-ticket-format-function (history-alist)
  "Return a pairing of HISTORY-ALIST values to %-sequences."
  (let* ((id         (cdr (assoc "id" history-alist)))
	 (ticket     (cdr (assoc "Ticket" history-alist)))
	 (time-taken (cdr (assoc "TimeTaken" history-alist)))
	 (type       (cdr (assoc "Type" history-alist)))
	 (field      (cdr (assoc "Field" history-alist)))
	 (old-value  (cdr (assoc "OldValue" history-alist)))
	 (new-value  (cdr (assoc "NewValue" history-alist)))
	 (data       (cdr (assoc "Data" history-alist)))
	 (desc       (cdr (assoc "Description" history-alist)))
	 (content    (cdr (assoc "Content" history-alist)))
	 (creator    (cdr (assoc "Creator" history-alist)))
	 ;; For some reason this is in GMT
	 (created    (format-time-string
		      rt-liber-browser-time-format-string
		      (date-to-time
		       (concat (cdr (assoc "Created" history-alist)) " GMT"))))
	 (attachments (cdr (assoc "" history-alist))))
    (list (cons ?i (or id "N/A"))
	  (cons ?t (or ticket "N/A"))
	  (cons ?T (or time-taken "N/A"))
	  (cons ?y (or type "N/A"))
	  (cons ?f (or field "N/A"))
	  (cons ?o (or old-value "N/A"))
	  (cons ?n (or new-value "N/A"))
	  (cons ?d (or data "N/A"))
	  (cons ?D (or desc "N/A"))
	  (cons ?C (or content ""))
	  (cons ?u (or creator "N/A"))
	  (cons ?r (or created "N/A")))))

(defun rt-liber-ticket-format (format history-alist)
  "Substitute %-sequences in FORMAT for the corrsponding vlaue in HISTORY-ALIST."
  (let ((alist (rt-liber-ticket-format-function history-alist)))
    (replace-regexp-in-string
     "%."
     (lambda (str)
       (rt-liber-browser-assoc (aref str 1) alist))
     format t t)))

(defun rt-liber-ticket-find-entry-format (entry-type)
  "Return the format string for ENTRY-TYPE."
  (let ((format nil)
	(l rt-liber-viewer-ticket-type-format))
    (while (and l (not format))
      (if (string-match (caar l) entry-type)
	  (setq format (cdar l)))
      (setq l (cdr l)))
    format))
  

(defun rt-liber-ticket-print-history (ticket-alist entries)
  "Print ticket history ENTRIES in the current buffer."
  (insert (rt-liber-ticket-headers ticket-alist))
  (insert "\n--\n")
  (dolist (entry entries)
    (let* ((entry-type (cdr (assoc "Type" entry)))
	   (format-str (rt-liber-ticket-find-entry-format entry-type))
	   (start (point)))
      (insert (rt-liber-ticket-format format-str entry))
      (if (/= start (point))
	  (progn
	    (add-text-properties start
				 (point)
				 (list 'rt-entry entry))
	    (save-excursion
	      (goto-char start)
	      (put-text-property (point-at-bol)
				 (point-at-eol)
				 'font-lock-face 'rt-liber-ticket-face))))))
  (goto-char (point-min)))


(defun rt-liber-ticket-parse-history (ticket-alist)
  "Parse ticket history in current buffer."
  (interactive)
  (goto-char (point-min))
  (let ((entries (rt-liber-ticket-viewer-parse-history))
	(inhibit-read-only t))
    (erase-buffer)
    (rt-liber-ticket-print-history ticket-alist (reverse entries))))

;(add-hook 'rt-liber-viewer-hook 'rt-liber-ticket-parse-history)

(provide 'rt-liberation-ticket-viewer)
;;; rt-liberation-ticket-viewer.el ends here
