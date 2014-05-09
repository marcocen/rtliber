;;; rt-liberation-rest.el --- Interface to the RT REST API

;; Copyright (C) 2014  Yoni Rabkin
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


(defvar rt-liber-rest-debug ""
  "Debug capture of last HTTP call.")

(defvar rt-liber-rest-scheme "https"
  "Scheme used for transport. Is one of http or https.")

(defvar rt-liber-rest-url ""
  "URL of RT installation.")

(defvar rt-liber-rest-username ""
  "Username of RT account.")

(defvar rt-liber-rest-password ""
  "Password of RT account.")


(defun rt-liber-rest-search-string (scheme url username password query)
  ""
  (let ((user (url-encode-url username))
	(pass (url-encode-url password)))
    (concat scheme
	    "://"
	    url
	    "/REST/1.0/search/ticket" "?"
	    "user=" user "&"
	    "pass=" pass "&"
	    "query=" (url-encode-url query) "&"
	    "format=i" "&"
	    "orderby=+Created")))

(defun rt-liber-rest-call (url)
  ""
  (let ((url-request-method "POST"))
    (let ((response
	   (url-retrieve-synchronously url))
	  str)
      (setq str
	    (with-current-buffer response
	      (buffer-substring-no-properties (point-min)
					      (point-max))))
      (setq rt-liber-rest-debug str)
      str)))

(defun rt-liber-rest-query-runner (op query-string)
  (message "starting REST '%s' query at %s..." op (current-time-string))
  (when (or (not (stringp op))
	    (not (stringp query-string)))
    (error "bad arguments"))
  (cond ((string= op "ls")
	 (rt-liber-rest-call
	  (rt-liber-rest-search-string rt-liber-rest-scheme
				       rt-liber-rest-url
				       rt-liber-rest-username
				       rt-liber-rest-password
				       query-string)))
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


(provide 'rt-liber-rest)

;;; rt-liberation-rest.el ends here.
