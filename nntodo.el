;;; nntodo.el --- Manage todo items with Gnus

;; Copyright (C) 1999 by Kai Grossjohann.

;; Authors: Jaime Casanova <jcasanov@systemguards.com.ec>
;;          Jacques Wainer wainer@ic.unicamp.br
;;          Kai.Grossjohann@CS.Uni-Dortmund.DE,
;;          John Wiegley <johnw@gnu.org>
;; Keywords: news, mail, calendar, convenience
;; Version: $Id: nntodo.el,v 1.6 2011/06/29 22:12:42 jcasanov Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Warning: this is beta code!  Use at your own risk!  

;; This file provides a new Gnus backend, nntodo, for storing todo
;; items.  Each todo item is a message but has two special headers
;; `X-Todo-Priority' and `X-Todo-DueDate' for the priority and the 
;; due date of a todo item, respectively.  It is possible
;; to sort todo items by due date and by priority.  

;;; Kudos:

;; Dan Nicolaescu <dann@ics.uci.edu> for providing me with a first
;; implementation to look at and steal from.


;; This code is a merge between Jacques Wainer and Kai.Grossjohann@CS.Uni-Dortmund.DE,
;; versions. 
;; Jacques version changes nnmbox by nnml, but i returned to nnmbox at 
;; least until i understand why i should use nnml


;;; How to use it

;; Set up

;;  1) in gnus, go to the server buffer (^)
;;  2) create a new server (a)
;;  2.1)   method : nntodo
;;  2.2)   server:  any name but you could just hit enter
;;  2.3)   open the new server (O)
;;  2.4)   leave the server buffer (q)
;;  3) in the group buffer, create a new group (G-m)
;;  4) group name: whatever you want, by why not todo ?
;;  4.1) from method: nntodo (+ the server name you entered in 2.2)
;;  5) set the properties of this new group (subscription level, always visible
;;       etc
;;  6) It should be possible to create more than one todo group (one
;;  for work one for personal, or whatever your heart desires) 
;;  But one of them must be teh group for short todo items (see
;;  bellow)
;;  Set the variable  "todo-short-group" to this group (there should
;;  be a better way to do this but I dont know)
;;  The format is nntodo+"server-name":"group-name"

;; Daily use

;; 1) if you receive an e-mail that should go to your todo list just 
;; copy or move it to the todo group (B-m or B-c)
;; 2) you will be ask for a delta - that is the time interval for the due date
;; 2.1) you can enter just a number - to be interpreted as days
;;      or a number and a character that indicates either weeks or months
;;      see variables "todo-week-const" and "todo-month-const"
;;      They are initially set to "s" for weeks and "m" from months
;;  or enter the third character (see "todo-date-const") to enter a date
;;  explicitly
;;
;; 3) Most, but not all todo items arive by mail, at least for me. So there 
;;    a function (todo-short) that allows you to create a todo item from scratch
;;    Just bind it to some nice key. Enter the time interval for the
;;    due date and the subject - a todo item will show up in the
;;    approriate group
;;
;; 4) when browsing your todo group, the items will be ordered by due
;; date. Since procastination is a fact of life, there is a function
;; "gnus-summary-add-time" that will add some time to the item's due
;; date. Bind it to some nice key (why not "+"?)



;;; Code:

;;; Backend definition:

(require 'nnheader)
(require 'nnmail)
(require 'gnus-start)
(require 'nnmbox)
(require 'nnoo)
(require 'cl)
(require 'calendar)
(require 'gnus-msg)


;; nntodo variables

;; how to print a calendar date - see 
(defvar todo-calendar-format '((format "%02d-%02d-%4d" (string-to-number day) (string-to-number month) (string-to-number  year))))

;; charaters to indicate week and month
(defvar todo-week-const "s")
(defvar todo-month-const "m")

;; character to indicate that I want to enter an explicit date
(defvar todo-date-const "d")


(nnoo-declare nntodo nnmbox)

;; If this variable isn't named nntodo-mbox-file, strange things
;; happen.  This seems to be a nnoo-ey problem.
(defvoo nntodo-mbox-file
    (expand-file-name (nnheader-concat gnus-home-directory ".nntodo"))
  "Name of the todo file in the user's home directory." nnmbox-mbox-file)
;; Similar.
(defvoo nntodo-active-file
    (expand-file-name (nnheader-concat gnus-home-directory ".nntodo-active"))
  "Name of the actile file for the todo file." nnmbox-active-file)

;; Can we protect better?
(defvoo nntodo-get-new-mail nil
  "Whether nntodo should get new mail.  MUST be nil!"
  nnmbox-get-new-mail)

(defvoo nntodo-current-group "" nil nnmbox-current-group)

(defconst nntodo-version "1.6")
(defvoo nntodo-status-string "" nil nnmbox-status-string)

(nnoo-define-basics nntodo)

;; Too bad that nnmbox-create-mbox-file isn't nnoo'd.
(deffoo nntodo-open-server (server &optional defs)
  (nnoo-change-server 'nntodo server defs)
  (nntodo-create-mbox-file)
  (cond
   ((not (file-exists-p nntodo-mbox-file))
    (nntodo-close-server)
    (nnheader-report 'nntodo "No such file: %s" nntodo-mbox-file))
   ((file-directory-p nntodo-mbox-file)
    (nntodo-close-server)
    (nnheader-report 'nntodo "Not a regular file: %s" nntodo-mbox-file))
   (t
    (nnheader-report 'nntodo "Opened server %s using mbox %s" server
		     nntodo-mbox-file)
    t)))

;; Copy of nnmbox-create-mbox-file, except for file name.
(defun nntodo-create-mbox-file ()
  (when (not (file-exists-p nntodo-mbox-file))
    (nnmail-write-region 1 1 nntodo-mbox-file t 'nomesg)))

;; When creating a group, it gets some special settings
(deffoo nntodo-request-create-group (group &optional server args)
  "Do whatever the nnmbox function does, then set a few group params."
  (nnoo-parent-function 'nntodo
			'nnmbox-request-create-group
			(list group server args))
  ;; Instead of setting these parameters for each group, isn't there a
  ;; way of somehow putting this into the server spec or into the
  ;; backend code?
  ;; Summary line looks different for todo items.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gnus-summary-line-format
   (list "%5N %U%R %7uT: %s\n"))
  ;; Why does the following not work?  `gnus-post-method' is nil or
  ;; something like this in the message buffer after hitting `a' in an
  ;; nntodo group.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gnus-post-method
   '('current))
  ;; Because the post method thing doesn't work, we need this.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gcc-self t)
  ;; default is to sort by due date, then by priority and then by number
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'gnus-thread-sort-functions
   '('(gnus-thread-sort-by-number
       gnus-thread-sort-by-priority
       gnus-thread-sort-by-duedate)))
  ;; Enter gnus-todo-mode in nntodo summaries.
  (gnus-group-set-parameter
   (gnus-group-prefixed-name group (list "nntodo" server))
   'dummy
   '( (gnus-todo-mode 1) )))

;; Ask for priority and duedate when entering articles.
(deffoo nntodo-request-accept-article (group &optional server last)
  "Add/modify priority and due date header before storing the message."
  (let (prio)
    (save-restriction
      (message-narrow-to-headers-or-head)
      (unless (message-fetch-field "X-Todo-Priority")
	(setq prio (todo-gnus-get-priority prio))
	(message-remove-header "X-Todo-Priority" nil nil)
	(mail-position-on-field "X-Todo-Priority")
	(insert prio))))
  (let (date)
    (save-restriction
      (message-narrow-to-headers-or-head)
      (unless (message-fetch-field "X-Todo-DueDate")
	(setq date (todo-gnus-get-duedate))
	(message-remove-header "X-Todo-DueDate" nil nil)
	(mail-position-on-field "X-Todo-DueDate")
	(insert (todo-aux-date-string date))
	)))
  (nnoo-parent-function 'nntodo
			'nnmbox-request-accept-article
			(list group server last)))

(nnoo-import nntodo
  (nnmbox))

;;; Utility code:

;; Hook nntodo backend into Gnus

(unless (assoc "nntodo" gnus-valid-select-methods)
  (gnus-declare-backend "nntodo" 'post 'respool 'address))

;;; Creating todo items:

(defvar todo-gnus-priority-alist
  '(("high" . 0) ("medium" . 1) ("low" . 2))
  "Association between prio names and values.")

(defun todo-gnus-get-priority (&optional prio)
  "Read a priority from the minibuffer."
  (interactive)
  (unless prio (setq prio "medium"))
  (completing-read "Priority (medium): "
		   todo-gnus-priority-alist
		   nil                  ;predicate
		   t                    ;require-match
		   nil nil prio))


(defun todo-gnus-get-duedate ()
  (interactive)
  (todo-read-delta (calendar-current-date))
  )

(defun todo-read-delta (today)
  (let* ((in (read-string "Delta: "))
     (n (string-to-number in))
     )
    (cond
     ((string-match todo-date-const in) (calendar-read-date nil))
     ((string-match todo-month-const in) (todo-add-month today n))
     ((string-match todo-week-const in) (todo-add-week today n))
     (t (todo-add-day today n))
     )
    ))


;; The following section is gross.  Isn't there a better way to do
;; this?  Maybe specify a special sending function for nntodo groups?
;; But how?
(defun todo-gnus-message-send-hook ()
  "Inserts required headers in todo item."
  (when (and (boundp 'gnus-message-group-art)
	     gnus-message-group-art
	     (car gnus-message-group-art)
	     (string-match "^nntodo\\>"
			   (car gnus-message-group-art)))
    (message-remove-header "Newsgroups")))

(add-hook 'message-send-hook 'todo-gnus-message-send-hook)

(add-to-list 'gnus-extra-headers 'X-Todo-Priority)
(add-to-list 'nnmail-extra-headers 'X-Todo-Priority)
(add-to-list 'gnus-extra-headers 'X-Todo-DueDate)
(add-to-list 'nnmail-extra-headers 'X-Todo-DueDate)

;;; Summary buffer:

;; This function is used in nntodo-request-create-group to set
;; `gnus-summary-line-format'.
(defun gnus-user-format-function-T (head)
  (let* ((extra-headers (mail-header-extra head)))
    (cdr (assoc 'X-Todo-Priority extra-headers)))
  (let* ((extra-headers (mail-header-extra head))
	 (calendar-date-display-form todo-calendar-format)
	 )
    (calendar-date-string (todo-aux-string-date 
			   (cdr (assoc 'X-Todo-DueDate extra-headers))))
    ))

;; Sorting by priority.  Code pretty much gleaned from gnus-sum.el
;; without any deeper understanding at all.
(defun gnus-article-sort-by-priority (h1 h2)
  (let* ((e1 (mail-header-extra h1))
	 (e2 (mail-header-extra h2))
	 (p1 (cdr (assoc 'X-Todo-Priority e1)))
	 (p2 (cdr (assoc 'X-Todo-Priority e2)))
	 (n1 (cdr (assoc p1 todo-gnus-priority-alist)))
	 (n2 (cdr (assoc p2 todo-gnus-priority-alist))))
    (unless n1
      (error "Unknown priority: %s" p1))
    (unless n2
      (error "Unknown priority: %s" p2))
    (if (= n1 n2)
	(< (mail-header-number h1) (mail-header-number h2))
      (< n1 n2))))

(defun gnus-thread-sort-by-priority (h1 h2)
  (gnus-article-sort-by-priority
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defun gnus-summary-sort-by-priority (&optional reverse)
  "Sort the summary buffer by priority.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'priority reverse))


;; sorting by due date
(defun gnus-article-sort-by-duedate (h1 h2)
  (let* ((e1 (mail-header-extra h1))
	 (e2 (mail-header-extra h2))
	 (d1 (todo-aux-string-date (cdr (assoc 'X-Todo-DueDate e1))))
	 (d2 (todo-aux-string-date (cdr (assoc 'X-Todo-DueDate e2))))
	 )
    (unless d1
      (error "Unknown due date: %s" d1))
    (unless d2
      (error "Unknown due date: %s" d2))
    (if (equal d1 d2)
	(< (mail-header-number h1) (mail-header-number h2))
      (< (calendar-absolute-from-gregorian d1)
	 (calendar-absolute-from-gregorian d2))
      )
    ))

(defun gnus-thread-sort-by-duedate (h1 h2)
  (gnus-article-sort-by-duedate
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defun gnus-summary-sort-by-duedate (&optional reverse)
  "Sort the summary buffer by due date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'duedate reverse))


;; Todo minor mode.

;; Gee, this seems to be simple with easy-mmode!
(require 'easy-mmode)

(defvar gnus-todo-mode-map
  (easy-mmode-define-keymap
   (list (cons (kbd "i n")
	       (cons "Sort by number" 'gnus-summary-sort-by-number))
	 (cons (kbd "i p")
	       (cons "Sort by priority" 'gnus-summary-sort-by-priority))
	 (cons (kbd "i i")
	       (cons "Add new todo item" 'gnus-summary-post-news))
     (cons (kbd "i +") 
	       (cons "Add time to due date" 'gnus-summary-add-time))
	 (cons (kbd "i d")
	       (cons "Delete todo item" 'gnus-summary-delete-article)))
   "Todo"))

(easy-mmode-define-minor-mode
 gnus-todo-mode
 "Minor mode for nntodo summary buffers.
Without ARG, toggle gnus-todo-mode.
With ARG, turn on iff ARG is positive, else turn off."
 nil
 " Todo"
 gnus-todo-mode-map)


;; add some time to the due date of the todo item

(defun gnus-summary-add-time (article)
  (interactive (list (gnus-summary-article-number)))
  (gnus-with-article article
    (let* ((val  (message-field-value "X-Todo-DueDate"))
	   (date (todo-aux-string-date val))
	   (nn (todo-read-delta date))
	   )
      (message-remove-header "X-Todo-DueDate" nil nil)
      (mail-position-on-field "X-Todo-DueDate")
      (insert (todo-aux-date-string nn))
      )
    ))
      

;;; short todo

;; this allow to create a todo item from stratch
;; just enter the duedate and the subject

;; where the short todo messages go to

(defvar todo-short-group "nntodo:todolist")

(defun todo-short ()
  (interactive)
  (let* ((due (todo-read-delta (calendar-current-date)))
	 (subj (read-string "Subject: "))
	 (buff (get-buffer-create "*TODO Temp"))
	 )
    (save-restriction
      (save-excursion
	(set-buffer buff)
	(message-setup `( (Subject . ,subj)
			  (X-Todo-DueDate . ,(todo-aux-date-string due))
			  (Gcc .  ,todo-short-group)
			  )
		       )
	(gnus-inews-do-gcc)
	(erase-buffer)
	))
    ))


;; calendar stuff

(defun todo-add-day (today n)
  "returns a calendar-day n days from today"
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian today) n))
  )

(defun todo-add-week (today n)
"returns a calendar-day n weeks from today"
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian today) (* n 7)))
  )
  
(defun todo-add-month (today n)
"returns a calendar-day n months from today"
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian today) (* n 30)))
  )
  
(defun todo-aux-date-string (date)
  (if (null date) "01-01-2001"  ; just a fail safe
    (let ((d (cadr date))
	  (m (car date))
	  (y (caddr date)))
      (if (null d) (setq d 15))
      (format "%02d-%02d-%4d" m d y)
      )))
    
(defun todo-aux-string-date (s)
  (if (null s) '(01 01 2001)  ; just a fail safe
    (let* ((m (string-to-number s))
	   (ss (substring s (1+ (string-match "-" s))))
	   (d (string-to-number ss))
	   (sss (substring ss (1+ (string-match "-" ss))))
	   (y (string-to-number sss))
	   )
      (list m d y))
  ))
   
;;; Epilog:

(provide 'nntodo)

;;; nntodo.el ends here
