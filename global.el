;;; global.el --- elisp wrapper functions and Emacs integration for GNU global  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Felipe Lema

;; Author: Felipe Lema <felipel@cl01cats24>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; WIP:
;; - wrapper calls
;; - project.el integration
;; - xref.el integration

;;; Commentary:

;;

;;; Code:

;;; Static definitions
(defgroup global nil "GNU global integration"
  :group 'tools)

(require 'cl-lib)
(require 'generator)
(require 'project)
(require 'subr-x)
(require 'xref)

;;;; variables

(defvar global--global-command
  "global"
  "Name/path to Global executable.")

;;;; utility functions:

(defun global--command-flag (command)
  "Get command line flag for COMMAND as string-or-nil.

Flags are not contracted.  Returned as list to compose command."
  (pcase command
    (`tag nil)
    ((or `completion `file `grep `idutils `path `print-dbpath `update)
     (list (format "--%s" (symbol-name command))))
    (_ (error "Unknown command: %s" (symbol-name command)))))

(defun global--option-requires-extra-flag? (option)
  "Whether command line OPTION requires an extra flag."
  (if (cl-find option '(color encode-path from-here gtagsconf gtagslabel file-list
			      match-part ;; 'nearness is handled outside
			      path-style result single-update
			      scope))
      t))

(defun global--option-sans-extra-flag? (option)
  "Whether command line OPTION does not require an extra flag."
  (if (not (eq option 'nearness));; 'nearness is handled outside
      (not (global--option-requires-extra-flag? option))))

;;; API

(defun global--option-flag (flag &optional value)
  "Get command line option flag for FLAG as string-or-nil.

When FLAG requires an extra parameter, this is passed in VALUE.
Flags are not contracted.  Result is a list of arguments."
  (pcase flag
    ((pred symbolp)
     (pcase (list (global--option-sans-extra-flag? flag) flag value)
       (`(,_ nearness  ,start) (list (format "--nearness=%s" start)))
       (`(t ;; no extra option
	  ,actualflag nil)
	(list (format "--%s" (replace-regexp-in-string "^:"
						  "" (symbol-name actualflag)))))
       (`(nil ;; --some-param some-value
	  ,actualflag ,value)
	(list (format "--%s" (replace-regexp-in-string "^:"
						  "" (symbol-name actualflag)))
	      (progn
		(cl-assert (stringp value)
			   (format "extra parameter for %s must be string, found "
			      value))
		value)))
       (_ (error "Unknown option combination: %s %s" (symbol-name flag) value))))
    ((or (pred stringp)
	 (pred stringp))
     ;; flag is actualy a query parameter
     (list flag))))

(defun global--arguments-as-pairs (flags)
  "Parse arguments from FLAGS as pairs.

:parameter-with-argument string → (:parameter-with-argument string)
:single-parameter               → (:single-parameter nil).
To do this, we look at the next argument in the list."
  (cl-assert (listp flags))
  (if-let ((head (car flags)))
      (let ((rest (cdr flags)))
	(pcase (car rest) ;; is next one …
	  ((or (pred null)
	       (pred symbolp)) ;; a symbol
	   (append
	    (list `(,head . nil)) ;; parameter doesn't pop an arg from list
	    (global--arguments-as-pairs rest)))
	  (_ ;; a number or string
	   (append
	    (list `(,head . ,(car rest))) ;; parameter pops an arg from list
	    (global--arguments-as-pairs
	     ;; ↓ list without popped arg
	     (cdr rest))))))))

(defun global--get-arguments (command &rest flags)
  "Get arguments to global as list per COMMAND and flags.

FLAGS must be plist like (global--get-arguments … :absolute :color \"always\")."
  (append
   (global--command-flag command)
   (cl-loop for (key value) in (global--arguments-as-pairs flags)
	    append (global--option-flag key value))))

;;; Convenience functions (for developers of global.el)

(defun global--get-as-string (command &rest flags)
  "Execute global COMMAND with FLAGS.

FLAGS is a plist.  See `global--get-arguments'.

If inner global command returns non-0, then this function returns nil."
  (let* ((program-and-args (append `(,global--global-command)
				   (global--get-arguments command flags)))
	 (program (car program-and-args))
	 (program-args (cdr program-and-args))
	 (command-return-code)
	 (command-output-str (with-output-to-string
			       (setq command-return-code
				     (apply (apply-partially
					     'call-process
					     program
					     nil
					     `(,standard-output nil)
					     nil
					     )
					    program-args)))))
    (if (= command-return-code 0)
	command-output-str)))

(defun global--get-dbpath (dir)
  "Filepath for database from DIR or nil."
  (if-let* ((default-directory dir)
	    (maybe-dbpath (global--get-as-string 'print-dbpath)))
      (if (file-exists-p maybe-dbpath)
	  maybe-dbpath)))

;;; project.el integration

(defun global-try-project-root (dir)
  "Project root for DIR if it exists."
  (if-let* ((dbpath (global--get-dbpath dir)))
      (cons 'global dbpath)))

(cl-defmethod project-roots ((project (head global)))
  "Default implementation.

See `project-roots' for 'transient."
  (list (cdr project)))

(cl-defmethod project-file-completion-table ((project (head global)) dirs)
  "Same as generic `project-file-completion-table', but replacing find command.

TODO: cache call (see `tags-completion-table' @ etags.el)"
  (let ((all-files
	 (cl-mapcan
	  (lambda (dir)
	    (split-string
	     (let* ((project-root (cdr project))
		    (default-directory project-root))
	       (global--get-as-string 'path :print0))
	     "\0" t))
	  dirs)))
    (lambda (string pred action)
      (cond
       ((eq action 'metadata)
	'(metadata . ((category . project-file))))
       (t
	(complete-with-action action all-files string pred))))))

;; No need to implement unless necessary
;;(cl-defmethod project-external-roots ((project (head global)))
;;  )
;;(cl-defmethod project-files ((project (head global)) &optional dirs)
;;  )
;;(cl-defgeneric project-ignores (_project _dir)

;;; xref.el integration

(defun global-xref-backend ()
  "global backend for Xref."
  (if (global--get-dbpath default-directory)
      'global))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql global)))
  (if-let ((symbol (thing-at-point 'symbol)))
      (symbol-name symbol)))

;;;; TODO
;;;; cache calls (see `tags-completion-table' @ etags.el)
;;;; `xref-backend-identifier-at-point',
;;;; `xref-backend-identifier-completion-table',
;;;; `xref-backend-definitions'
;;;; `xref-backend-references',
;;;; `xref-backend-apropos'

(provide 'global)
;;; global.el ends here
