;;; global-tags.el --- Elisp API and editor integration for GNU global  -*- lexical-binding: t; -*-

;; Copyright © 2019  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
;; Keywords: convenience, matching, tools
;; Package-Requires: ((emacs "26.1"))
;; URL: https://launchpad.net/global-tags.el
;; Version: 0.1

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


;;; Commentary:

;; Testeable elisp API that wraps GNU global calls and integration to editor
;; using this API with project.el and xref.el
;; To use with project.el and xref.el, add their "recognize this global handled
;; project" to the proper places like so:
;;
;; ;; for xref
;; (add-to-list 'xref-backend-functions 'global-tags-xref-backend)
;; ;; for project.el
;; (add-to-list 'project-find-functions 'global-tags-try-project-root)

;;; Code:

;;; Static definitions
(defgroup global-tags nil "GNU global integration"
  :group 'tools)

(require 'cl-lib)
(require 'generator)
(require 'project)
(require 'rx)
(require 'subr-x)
(require 'xref)

(declare-function with-parsed-tramp-file-name 'tramp)
(declare-function tramp-make-tramp-file-name 'tramp)

;;;; variables

(defcustom global-tags-global-command
  "global"
  "Name/path to Global executable."
  :type 'string
  :group 'global)

;;;; utility functions:

(defun global-tags--command-flag (command)
  "Get command line flag for COMMAND as string-or-nil.

Flags are not contracted.  Returned as list to compose command."
  (pcase command
    (`tag nil)
    ((or `completion `file `grep `idutils `path `print-dbpath `reference `update)
     (list (format "--%s" (symbol-name command))))
    (_ (error "Unknown command: %s" (symbol-name command)))))

;;; API

(defun global-tags--option-flags (flags)
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
	     (append (global-tags--option-flags l))))
	  ((pred null)
	   (global-tags--option-flags rest-flags))
	  ('nearness
	   (let ((next-flag (cadr flags))
		 (rest-flags (cddr flags))) ;; diferent "rest"
	     (append
	      `(,(format "--nearness=%s" next-flag))
	      (global-tags--option-flags rest-flags))))
	  ((pred symbolp)
	   (append `(,(format "--%s" (symbol-name head-flag)))
		   (global-tags--option-flags rest-flags)))
	  ((pred stringp)
	   (append `(,head-flag)
		   (global-tags--option-flags rest-flags)))))))



(defun global-tags--get-arguments (command &rest flags)
  "Get arguments to global as list per COMMAND and flags.

FLAGS must be plist like
\(global-tags--get-arguments … :absolute :color \"always\"\)."
  (append
   (global-tags--command-flag command)
   (global-tags--option-flags flags)))

;;; Convenience functions (for developers of this package)

(defun global-tags--get-as-string (command &rest flags)
  "Execute global COMMAND with FLAGS.

FLAGS is a plist.  See `global-tags--get-arguments'.

If inner global command returns non-0, then this function returns nil."
  (let* ((program-and-args (append `(,global-tags-global-command)
				   (global-tags--get-arguments
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
			  #'process-file
			  program
			  nil ;; infile
			  `(,standard-output nil) ;; dest, ???
			  nil) ;; display
			 program-args)))))
    (if (= command-return-code 0)
	command-output-str)))

(defun global-tags--get-lines (command &rest flags)
  "Break global COMMAND FLAGS output into lines.

Adds (print0) to flags.

If COMMAND is completion, no print0 is added (global ignores it underneath)."
  (pcase command
    (`completion
     (split-string
      (global-tags--get-as-string command (delete 'print0
					     flags))
      "\n" t))
    (_ (split-string
	(global-tags--get-as-string command (append '(print0)
					       flags))
	"\0" t))))

(defun global-tags--get-location (line)
  "Parse location from LINE.

Assumes (:result \"grep\").
Column is always 0."
  (save-match-data
    (if (string-match (rx line-start
			  (group (+ (any print))) ;; file
			  ?: ;; separator
			  (group (+ (any digit))) ;; line
			  ?: ;; separator
			  (group (* (any print))) ;; function
			  line-end)
		      line)
	`((file . ,(match-string 1 line))
	  (line . ,(string-to-number (match-string 2 line)))
	  (description . ,(match-string 3 line))
	  (column . 0)))))

(defun global-tags--as-xref-location (location-description)
  "Map LOCATION-DESCRIPTION from `global-tags--get-locations' to xref's repr."
  (let-alist location-description
    (xref-make .description
	       (xref-make-file-location .file
					.line
					.column))))

(defun global-tags--get-locations (symbol &optional kind)
  "Get locations according to SYMBOL and KIND.

If KIND is omitted, will do \"tag\" search."
  (let ((lines (global-tags--get-lines kind
                                       ;; ↓ see `global-tags--get-location'
                                       'result "grep"
                                       symbol)))
    (cl-mapcar #'global-tags--get-location lines)))

(defun global-tags--get-xref-locations (symbol kind)
  "Get xref locations according to KIND using SYMBOL as query.

See `global-tags--get-locations'."
  (cl-mapcar #'global-tags--as-xref-location
             (global-tags--get-locations symbol kind)))

(defun global-tags--get-dbpath (dir)
  "Filepath for database from DIR or nil."
  (if-let* ((maybe-dbpath (let ((default-directory dir))
			    (global-tags--get-as-string 'print-dbpath)))
	    ;; db path is *always* printed with trailing newline
	    (trimmed-dbpath (substring maybe-dbpath 0
				       (- (length maybe-dbpath) 1)))
	    (maybe-remote-dbpath
	     (if (file-remote-p default-directory)
		 (let (method user domain host port hop) ;; not recognized in some versions
		   (with-parsed-tramp-file-name default-directory nil
		     (tramp-make-tramp-file-name
		      method
		      user
		      domain
		      host
		      port
		      trimmed-dbpath
		      hop)))
	       trimmed-dbpath)))
      (if (file-exists-p maybe-remote-dbpath)
	  maybe-remote-dbpath)))

;;; project.el integration

(defun global-tags-try-project-root (dir)
  "Project root for DIR if it exists."
  (if-let* ((dbpath (global-tags--get-dbpath dir)))
      (cons 'global dbpath)))

(cl-defmethod project-roots ((project (head global)))
  "Default implementation.

See `project-roots' for 'transient."
  (list (cdr project)))

(cl-defmethod project-file-completion-table ((project (head global)) dirs)
  "See documentation for `project-file-completion-table'."
  (ignore project)
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata . ((category . project-file))))
     (t
      (let ((all-files
             (cl-mapcan
              (lambda (dir)
                (let* ((default-directory dir))
                  (global-tags--get-lines 'path
                                          ;; ↓ project.el deals w/long names
                                          'absolute
                                          pred)))
              dirs)))
        (complete-with-action action all-files string pred))))))

;; No need to implement unless necessary
;;(cl-defmethod project-external-roots ((project (head global)))
;;  )
;;(cl-defmethod project-files ((project (head global)) &optional dirs)
;;  )
;;(cl-defgeneric project-ignores (_project _dir)

;;; xref.el integration

(defun global-tags-xref-backend ()
  "Xref backend for using global."
  (if (global-tags--get-dbpath default-directory)
      'global))

(cl-defmethod xref-backend-definitions ((_backend (eql global)) symbol)
  "See `global-tags--get-locations'."
  (global-tags--get-xref-locations symbol 'tag))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql global)))
  (if-let ((symbol-str (thing-at-point 'symbol)))
      symbol-str))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql global)))
  (global-tags--get-lines 'completion))

(cl-defmethod xref-backend-references ((_backend (eql global)) symbol)
  (global-tags--get-xref-locations symbol 'reference))

(cl-defmethod xref-backend-apropos ((_backend (eql global)) symbol)
  (global-tags--get-xref-locations symbol 'grep))

;;;; TODO
;;;; cache calls (see `tags-completion-table' @ etags.el)

(provide 'global-tags)
;;; global-tags.el ends here
