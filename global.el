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
(require 'project)
(require 'subr-x)

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
  (if (not (null flag))
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
	(_ (error "Unknown option combination: %s %s" (symbol-name flag) value)))))

(defun global--get-arguments (command &rest flags)
  "Returns arguments to global as list per COMMAND and flags.

FLAGS must be plist like (global--get-arguments … :absolute :color \"always\")."
  (append
   (global--command-flag command)
   (cl-loop for (key value) on flags
	    append (global--option-flag key value))))

(defun global--get-as-string (command &rest flags)
  "Execute global COMMAND with FLAGS.

FLAGS is a plist.  See `global--get-arguments'"
  (shell-command-to-string
   (mapconcat #'shell-quote-argument
	      (append `(,global--global-command)
		      (global--get-arguments command flags))
	      " ")))

;;; project.el integration

(defun global-try-project-root (dir)
  "Project root for DIR if it exists."
  (if-let* ((default-directory dir)
	    (dbpath (global--get-as-string 'print-db)))
      (cons 'global dbpath)))

(cl-defmethod project-roots ((project (head global)))
  "Default implementation.

See `project-roots' for 'transient."
  (list (cdr project)))

(cl-defmethod project-file-completion-table ((project (head global)) dirs)
  "Same as generic `project-file-completion-table', but replacing find command."
  (let ((all-files
	 (cl-mapcan
	  (lambda (dir)
	    (split-string
	     (let* ((project-root (cdr project))
		    (default-directory project-root))
	       (global--get-as-string 'path))
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

(provide 'global)
;;; global.el ends here
