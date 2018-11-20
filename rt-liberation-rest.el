;;; rt-liberation-rest.el --- Interface to the RT REST API

;; Copyright (C) 2014, 2015  Yoni Rabkin
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
;;
;; Note: Licensed under GPLv2+ and not GPLv3+ in order to be
;; compatible with the license of RT.

;;; History:
;;
;; Started in May of 2014 in order to remove rt-liberation's
;; dependency on a local copy of the RT CLI.

;;; Code:

(require 'url)
(require 'url-util)
(require 'auth-source)

(defgroup rt-liber-rest nil
  "Interface to the RT REST API."
  :prefix "rt-liber-rest-"
  :group 'rt-liber)

(defcustom rt-liber-rest-use-auth-source-p nil
  "Whether to use auth-source for authentication."
  :type 'boolean
  :group 'rt-liber-rest)

(defvar rt-liber-rest-debug-buffer-name "*rt-liber-rest debug log*"
  "Buffer name of debug capture.")

(defvar rt-liber-rest-debug-p nil
  "If non-nil, record traffic in a debug buffer.")

(defvar rt-liber-rest-scheme "https"
  "Scheme used for transport. Is one of http or https.")

(defvar rt-liber-rest-url ""
  "URL of RT installation.")

(defvar rt-liber-rest-username ""
  "Username of RT account.")

(defvar rt-liber-rest-password ""
  "Password of RT account.")

(defvar rt-liber-rest-verbose-p t
  "If non-nil, be verbose about what's happening.")


(defun rt-liber-rest-write-debug (str)
  "Write to debug buffer."
  (when (not (stringp str))
    (error "argument not string"))
  (when rt-liber-rest-debug-p
    (with-current-buffer
	(get-buffer-create rt-liber-rest-debug-buffer-name)
      (goto-char (point-max))
      (insert str))))

(defun rt-liber-rest-search-string (scheme url query)
  "Return the search query string."
  (concat scheme
	  "://"
	  url
	  "/REST/1.0/search/ticket" "?"
	  "query=" (url-encode-url query) "&"
	  "format=i" "&"
	  "orderby=+Created"))

(defun rt-liber-rest-show-string (scheme url ticket-id-list query)
  "Return the ticket show string."
  (concat scheme
	  "://"
	  url
	  "/REST/1.0/ticket/" ticket-id-list
	  "/show"))

(defun rt-liber-rest-history-string (scheme url ticket-id)
  "Return the ticket show string."
  (concat scheme
	  "://"
	  url
	  "/REST/1.0/ticket/" ticket-id
	  "/history" "?"
	  "format=l"))

(defun rt-liber-rest-command-edit-string (scheme url ticket-id)
  "Return the ticket edit string."
  (concat scheme
	  "://"
	  url
	  "/REST/1.0/ticket/" ticket-id
	  "/edit"))

(defun rt-liber-rest-get-auth-source-secret (username)
  "Returns the password for USERNAME, using auth-source."
  (let* ((auth-source-creation-prompts
	  '((user . "RT user at %h: ")
	    (secret . "RT password for %u@%h: ")))
	 (auth (nth 0 (auth-source-search :max 1
					  :host (car (split-string rt-liber-rest-url "/"))
					  :user username
					  :require '(:user :secret)
					  :create t)))
	 (secret (plist-get auth :secret)))
    (if (functionp secret)
	(funcall secret)
      secret)))

(defun rt-liber-rest-call (url username)
  "Perform a REST call with URL."
  (let* ((user (url-encode-url username))
	 (password (url-encode-url
		    (if rt-liber-rest-use-auth-source-p
			(rt-liber-rest-get-auth-source-secret username)
		      rt-liber-rest-password)))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data (concat "user=" user "&" "pass=" password))
	 (response (url-retrieve-synchronously url))
	 str)
    (setq str
	  (decode-coding-string
	   (with-current-buffer response
	     (buffer-substring-no-properties (point-min)
					     (point-max)))
	   'utf-8))

    (rt-liber-rest-write-debug
     (format "outgoing rest call -->\n%s\n<-- incoming\n%s\n" url str))
    str))

(defun rt-liber-rest-query-runner (op query-string)
  "Run OP on QUERY-STRING."
  (when (or (not (stringp op))
	    (not (stringp query-string)))
    (error "bad arguments"))
  (cond ((string= op "ls")
	 (rt-liber-rest-call
	  (rt-liber-rest-search-string rt-liber-rest-scheme
				       rt-liber-rest-url
				       query-string)
	  rt-liber-rest-username))
	((string= op "show")
	 (rt-liber-rest-call
	  (rt-liber-rest-show-string rt-liber-rest-scheme
				     rt-liber-rest-url
				     query-string
				     query-string)
	  rt-liber-rest-username))
	((string= op "history")
	 (rt-liber-rest-call
	  (rt-liber-rest-history-string rt-liber-rest-scheme
					rt-liber-rest-url
					query-string)
	  rt-liber-rest-username))
	(t (error "unknown op [%s]" op))))

(defun rt-liber-rest-parse-http-header ()
  "Parse the HTTP header from the server."
  (let ((http-ok-regexp "^HTTP.*200 OK$")
	(rt-ok-regexp   "^rt/.*200 ok$"))
    (condition-case excep
	(progn
	  (re-search-forward http-ok-regexp (point-max))
	  (re-search-forward rt-ok-regexp (point-max)))
      (error "bad HTTP response from server"))))

(defun rt-liber-rest-ticketsql-runner-parser-f ()
  "Parser function for a textual list of tickets."
  (let (idsub-list)
    (rt-liber-rest-parse-http-header)
    (while (re-search-forward "ticket/\\([0-9].+\\)" (point-max) t)
      ;; the output should be compatible with the input to
      ;; `rt-liber-create-tickets-string'
      (push (list (match-string-no-properties 1)
		  ".")
	    idsub-list))
    idsub-list))

(defun rt-liber-rest-run-ls-query (query)
  "Run an \"ls\" type query against the server with QUERY."
  (rt-liber-parse-answer
   (rt-liber-rest-query-runner "ls" query)
   'rt-liber-rest-ticketsql-runner-parser-f))

(defun rt-liber-rest-show-process (response)
  "Process and return the show query response."
  (when (not (stringp response))
    (error "argument not a string"))
  (with-temp-buffer
    (save-excursion
      (insert response))
    (rt-liber-rest-parse-http-header)
    (buffer-substring (point) (point-max))))

(defun rt-liber-rest-show-query-runner (idsublist)
  "Iterate over IDSUBLIST and return the collected result."
  (when (not (listp idsublist))
    (error "argument not list"))
  (with-temp-buffer
    (let ((ticket-ids (reverse (copy-tree idsublist)))
	  (c 1)
	  (l (length idsublist)))
      (while ticket-ids

	(when rt-liber-rest-verbose-p
	  (message "retrieving ticket %d/%d" c l)
	  (setq c (1+ c)))

	(insert
	 (rt-liber-rest-show-process
	  (rt-liber-rest-query-runner "show" (caar ticket-ids))))
	(setq ticket-ids (cdr ticket-ids))
	(when ticket-ids
	  (insert "\n--\n")))
      (when rt-liber-rest-verbose-p
	(message "done retrieving %d tickets" l)))
    (buffer-substring (point-min) (point-max))))

(defun rt-liber-rest-run-show-base-query (idsublist)
  "Run \"show\" type query against the server with IDSUBLIST."
  (rt-liber-parse-answer
   (rt-liber-rest-show-query-runner idsublist)
   #'rt-liber-ticket-base-retriever-parser-f))

(defun rt-liber-rest-run-ticket-history-base-query (ticket-id)
  "Run history query against server for TICKET-ID."
  (rt-liber-parse-answer
   (rt-liber-rest-query-runner "history" ticket-id)
   #'(lambda ()
       (rt-liber-rest-parse-http-header)
       (buffer-substring (point) (point-max)))))

(defun rt-liber-rest-handle-response (buffer)
  "Handle the response provided in BUFFER."
  (with-current-buffer response-buffer
    (rt-liber-rest-write-debug (buffer-string))))

(defun rt-liber-rest-edit-runner (ticket-id field value)
  "Run edit comment to set FIELD to VALUE."
  (message "started edit command at %s..." (current-time-string))
  (message "ticket #%s, %s <- %s" ticket-id field value)
  (let ((request-data
	 (format "content=%s: %s"
		 (url-hexify-string field)
		 (url-hexify-string value))))
    (rt-liber-rest-write-debug (concat request-data "\n"))
    (let ((url-request-method "POST")
	  (url-request-data request-data)
	  response-buffer)
      (setq response-buffer
	    (url-retrieve-synchronously
	     (rt-liber-rest-command-edit-string
	      rt-liber-rest-scheme
	      rt-liber-rest-url
	      ticket-id)))
      (rt-liber-rest-handle-response response-buffer)))
  (message "edit command ended at %s" (current-time-string)))

(defun rt-liber-rest-command-set (id field status)
  "Set ticket ID status to be STATUS."
  (rt-liber-parse-answer
   (rt-liber-rest-edit-runner id field status)
   'rt-liber-command-runner-parser-f))


(provide 'rt-liberation-rest)

;;; rt-liberation-rest.el ends here.
