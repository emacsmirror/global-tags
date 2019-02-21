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
(require 'rx)
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

;;; API

(defun global--option-flags (flags)
  "Support several formats of to-cli-ize FLAGS.

Will recurse on lists."
  (if (not (null flags))
      (let ((head-flag (car flags))
	    (rest-flags (cdr flags)))
	(pcase head-flag
	  ((pred listp)
	   ;; received a list of list
	   ;; if you know elisp and know how to handle ⎡&rest⎦, then you can
	   ;; help remove this section
	   (let ((l head-flag))
	     (cl-assert (= 1 (length flags)))
	     (append (global--option-flags l))))
	  ((pred null)
	   (global--option-flags rest-flags))
	  ('nearness
	   (let ((next-flag (cadr flags))
		 (rest-flags (cddr flags))) ;; diferent "rest"
	     (append
	      `(,(format "--nearness=%s" next-flag))
	      (global--option-flags rest-flags))))
	  ((pred symbolp)
	   (append `(,(format "--%s" (symbol-name head-flag)))
		   (global--option-flags rest-flags)))
	  ((pred stringp)
	   (append `(,head-flag)
		   (global--option-flags rest-flags)))))))



(defun global--get-arguments (command &rest flags)
  "Get arguments to global as list per COMMAND and flags.

FLAGS must be plist like (global--get-arguments … :absolute :color \"always\")."
  (append
   (global--command-flag command)
   (global--option-flags flags)))

;;; Convenience functions (for developers of global.el)

(defun global--get-as-string (command &rest flags)
  "Execute global COMMAND with FLAGS.

FLAGS is a plist.  See `global--get-arguments'.

If inner global command returns non-0, then this function returns nil."
  (let* ((program-and-args (append `(,global--global-command)
				   (global--get-arguments
				    command flags)))
	 (program (car program-and-args))
	 (program-args (cdr program-and-args))
	 (command-return-code)
	 (command-output-str
	  (with-output-to-string
	    (setq command-return-code
		  ;; `call-process', but forwarding program-args
		  ;; 🙄
		  (apply (apply-partially
			  'call-process
			  program
			  nil
			  `(,standard-output nil)
			  nil)
			 program-args)))))
    (if (= command-return-code 0)
	command-output-str)))

(defun global--get-lines (command &rest flags)
  "Break global COMMAND FLAGS output into lines.

Adds (:print0) to flags."
  (split-string
   (global--get-as-string command (append '(print0)
					  flags))
   "\0" t))

(defun global--get-location (line)
  "Parse location from LINE.

Assumes (:result \"grep\").
Column is always 0."
  (save-match-data
    (if (string-match (rx line-start
			  (group (+ (any print))) ;; file
			  ?: ;; separator
			  (group (+ (any digit))) ;; line
			  ?: ;; separator
			  (group (+ (any print))) ;; function
			  line-end)
		      line)
	`((file . ,(match-string 1 line))
	  (line . ,(string-to-number (match-string 2 line)))
	  (description . ,(match-string 3 line))
	  (column . 0)))))

(defun global--as-xref-location (location-description)
  "Map LOCATION-DESCRIPTION from `global--get-locations' to xref's representation."
  (let-alist location-description
    (xref-make .description
	       (xref-make-file-location .file
					.line
					.column))))

(defun global--get-locations (symbol &optional kind)
  "Get locations according to SYMBOL and KIND.

If KIND is omitted, will do \"tag\" search."
  (let ((lines (global--get-lines kind
				  ;; ↓ see `global--get-location'
				  'result "grep"
				  symbol)))
    (cl-loop for line in lines
	     collect (global--get-location line))))

(defun global--get-xref-locations (symbol &optional kind)
  "Get xref locations according to SYMBOL and KIND.

If KIND is omitted, will do \"tag\" search.
See `global--get-locations'."
  (if-let ((results (global--get-locations kind symbol)))
      (cl-loop for result in results
	       collect (global--as-xref-location result))))

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
  "Same as generic `project-file-completion-table', but replacing find command."
  (let ((all-files
	 (cl-mapcan
	  (lambda (dir)
	    (let* ((default-directory dir))
	      (global--get-lines 'path
				 ;; ↓ project.el sorts out presenting long names
				 'absolute)))
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

(cl-defmethod xref-backend-definitions ((_backend (eql global)) symbol)
  "See `global--get-locations'."
  (global--get-xref-locations 'tag symbol))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql global)))
  (if-let ((symbol-str (thing-at-point 'symbol)))
      symbol-str))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql global)))
  (global--get-lines 'completion))

(cl-defmethod xref-backend-references ((_backend (eql global)) symbol)
  (global--get-xref-locations 'reference symbol))

(cl-defmethod xref-backend-apropos ((_backend (eql global)) symbol)
  (global--get-xref-locations 'grep symbol))

;;;; TODO
;;;; cache calls (see `tags-completion-table' @ etags.el)

(provide 'global)
;;; global.el ends here
