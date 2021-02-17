;;; global-tags.el --- Elisp API and editor integration for GNU global  -*- lexical-binding: t; -*-

;; Copyright © 2019  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
;; Keywords: convenience, matching, tools
;; Package-Requires: ((emacs "26.1") (async "1.9.4") (project "0.5.2") (ht "2.3"))
;; URL: https://launchpad.net/global-tags.el
;; Version: 0.3

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
;; You can setup using GNU global as backend with any of the following two
;; lines:
;;
;; ;; to use GNU Global automagically, regardless of Emacs default configuration
;; (add-hook 'ruby-mode-hook #'global-tags-exclusive-backend-mode)
;; ;; to use GNU Global automagically, respecting other backends
;; (add-hook 'ruby-mode-hook #'global-tags-shared-backend-mode)
;;
;; Alternatively, you can manually configure project.el and xref.el, add their
;; "recognize this global handled project" to the proper places like so:
;;
;; ;; xref (finding definitions, references)
;; (add-to-list 'xref-backend-functions 'global-tags-xref-backend)
;; ;; project.el (finding files)
;; (add-to-list 'project-find-functions 'global-tags-try-project-root)
;; ;; to update database after save
;; (add-hook 'c++-mode-hook (lambda ()
;;                            (add-hook 'after-save-hook
;;                                      #'global-tags-update-database-with-buffer
;;                                      nil
;;                                      t)))

;;; Code:

;;; Static definitions
(defgroup global-tags nil "GNU global integration"
  :group 'tools)

(require 'async)
(require 'cl-lib)
(require 'generator)
(require 'ht)
(require 'project)
(require 'rx)
(require 'subr-x)
(require 'xref)

;;;; variables

(defcustom global-tags-global-command
  "global"
  "Name/path to Global executable."
  :type 'string
  :group 'global-tags)

(defcustom global-tags-tags-generation-command
  "gtags"
  "Name/path to executable to generate tags"
  :type 'string
  :group 'global-tags)

(defcustom global-tags-generate-tags-command
  "gtags"
  "Name/path to executable to generate tags"
  :type 'string
  :group 'global-tags)

(defcustom global-tags-generate-tags-flags
  '()
  "command-line flags to `global-tags-tags-generation-command'"
  :type '(repeat string)
  :group 'global-tags)

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

(defun global-tags--ensured-correct-separator (command flags)
  "Return FLAGS with correct separator parameter.

'completion is always separated by \\n.  Everything else is separated by \\0."
  (pcase command
    (`completion
     (delete 'print0 flags))
    (_
     (append '(print0) flags))))

;;; Convenience functions (for developers of this package)
(defun global-tags--line-separator (command)
  "Get line (candidate) separator chararcter for the command output.

'completion will _always_ be separated by \\n.  The rest is assumed to be
separated by \\0.
"
  (pcase command
    (`completion
     "\n")
    (_
     "\0")))
(defun global-tags--translate-to-command-line (command command-flags)
  "Translate COMMAND COMMAND-FLAGS to a command line.

Returns a list with (global-binary ARG0 ARG1 …)."
  (cons
   global-tags-global-command
   (global-tags--get-arguments
    command
    (global-tags--ensured-correct-separator
     command
     command-flags))))

(defun global-tags--get-lines-future (command ignore-result &rest command-flags)
  "Return an `async' future that will hold the returned lines of running COMMAND COMMAND-FLAGS.

The future returns (cons return-code list-of-lines)

If you don't want the results, set IGNORE-RESULT to non-nil."
  (pcase-let* ((line-separator (global-tags--line-separator command))
               (`(,program . ,program-args)
                (global-tags--translate-to-command-line command command-flags))
               (future
                (async-start
                 `(lambda ()
                    (require 'simple)
                    (let ((tramp-use-ssh-controlmaster-options nil) ;; avoid race conditions
                          (default-directory ,default-directory)
	                  (command-return-code))
                      (let ((command-output-str
	                     (with-output-to-string
	                       (setq command-return-code
		                     ;; `call-process', but forwarding program-args
		                     ;; 🙄
		                     (process-file
		                      ,program
		                      nil ;; infile
		                      (list standard-output nil) ;; dest, ???
		                      nil ;; display
		                      ,@program-args)))))
                        (cons
                         command-return-code
                         (when command-output-str
                           (split-string command-output-str ,line-separator t))))))
                 (when ignore-result
                   'ignore))))
    ;; add parameter to future buffers to help debugging
    ;; see async package for more info
    (when-let ((actually-process-buffer
                (process-buffer future)))
      (with-current-buffer actually-process-buffer
        (rename-buffer (format "*global future %s %s @ %s*"
                               command
                               command-flags
                               default-directory)
                       t)))
    future))

(defun global-tags--get-lines (command &rest flags)
  "Break global COMMAND FLAGS output into lines.

Adds (print0) to flags.

If COMMAND is completion, no print0 is added (global ignores it underneath)."
  (pcase-let* ((lines-future (apply #'global-tags--get-lines-future
                                    (append
                                     (list
                                      command
                                      nil)
                                     flags)))
               (`(,command-return-code . ,lines)
                (async-get lines-future)))
    (when (= command-return-code 0)
      lines)))

(defun global-tags--get-location (line)
  "Parse location from LINE.

Assumes (:result \"grep\").
Column is always 0."
  (save-match-data
    (when (string-match (rx line-start
			    (group (+ (any print))) ;; file
			    ?: ;; separator
			    (group (+ (any digit))) ;; line
			    ?: ;; separator
			    (group (* (any print blank))) ;; function or line with code
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

If KIND is omitted, global will do a \"tag\" search."
  (thread-last
      (global-tags--get-lines kind
                              ;; ↓ see `global-tags--get-location'
                              'result "grep"
                              symbol)
    (cl-mapcar #'global-tags--get-location)))

(defun global-tags--get-xref-locations (symbol kind)
  "Get xref locations according to KIND using SYMBOL as query.

See `global-tags--get-locations'."
  (cl-mapcar #'global-tags--as-xref-location
             (global-tags--get-locations symbol kind)))

(defun global-tags--get-dbpath (dir)
  "Filepath for database from DIR or nil."
  (when-let* ((maybe-dbpath (let ((default-directory dir))
                              (car
                               (global-tags--get-lines 'print-dbpath))))
              ;; db path is *always* printed with trailing newline
              (trimmed-dbpath (substring maybe-dbpath 0
                                         (- (length maybe-dbpath) 1)))
              (dbpath (concat
                       (file-remote-p dir) ;; add remote file prefix when dir is remote
                       trimmed-dbpath)))
    (if (file-exists-p dbpath)
        dbpath)))

;;; project.el integration
;;;; utilities
(defun global-tags--remote-file-names (local-files)
  "Like `project--remote-file-names', but without having to require Emacs 27."
  (let ((remote-prefix
         (file-remote-p default-directory)))
    (seq-map
     (lambda (local-file)
       (concat
        remote-prefix
        local-file))
     local-files)))

(defclass global-tags-project ()
  ((root
    :initarg :root))
  "Base class for all project.el and xref defmethods.")
(defclass global-tags-project-with-pre-fetched-lines (global-tags-project)
  ()
  "Backend / project type to mark usage of pre-fetched data.")

(defvar global-tags--pre-fetching-futures
  (make-hash-table :test 'equal)
  "(project-root . command-and-args) → lines future.

Root is path returned by `global-tags--get-dbpath'.  Lines future is returned by
`global-tags--get-lines-future'")

(defun global-tags--pre-fetch-key (project command args)
  "Get a key for `global-tags--pre-fetching-futures'."
  (cons
   (project-root project)
   (append (list command)
           args)))

(defun global-tags--queue-next (project command args)
  "Always set next future for arguments."
  (ht-set
   global-tags--pre-fetching-futures
   (global-tags--pre-fetch-key project command args)
   (let ((default-directory (oref project root)))
     (apply #'global-tags--get-lines-future
            (append (list command nil)
                    args)))))

(cl-defgeneric global-tags--ensure-next-fetch-is-queued (project command args)
  "Queue next future only on selected (defmethod) parameters.")
(cl-defmethod global-tags--ensure-next-fetch-is-queued ((project global-tags-project) command args)
  "Don't queue any «next fetch»")

(defconst global-tags--commands-and-args-that-allow-prefetch
  '((path absolute)
    (completion))
  "Each (list COMMAND ARG0 ARG1 …) that will be pre-fetched and stored in `global-tags--pre-fetching-futures'.")

(cl-defmethod global-tags--ensure-next-fetch-is-queued ((project global-tags-project-with-pre-fetched-lines) command args)
  (let ((command-and-args
         (append (list command)
                 args)))
    (when (member command-and-args global-tags--commands-and-args-that-allow-prefetch)
      (global-tags--queue-next project command args))))

(defun global-tags--ensure-queued (project command args)
  "If key is empty in `global-tags--pre-fetching-futures', call `global-tags--queue-next'."
  (unless (ht-get global-tags--pre-fetching-futures (global-tags--pre-fetch-key project command args))
    (global-tags--queue-next project command args)))


(defun global-tags--project-get-lines (project command &rest flags)
  "Get lines for running COMMAND FLAGS.

Forward PROJECT to `global-tags--ensure-next-fetch-is-queued'.

If a future for COMMAND FLAGS was previously queued in
`global-tags--pre-fetching-futures', it will use that.

Whatever is used, `global-tags--ensure-next-fetch-is-queued' is called to
(maybe, according to method) ensure the next call for COMMAND FLAGS will be
pre-fetched."
  (let* ((this-key
          (global-tags--pre-fetch-key project command flags))
         (this-prefetched
          (ht-get global-tags--pre-fetching-futures
                  this-key)))
    ;; set next future before returning
    (global-tags--ensure-next-fetch-is-queued project command flags)
    (unless this-prefetched
      ;; set to an actual future if there wasn't a previous one running in background
      (setq this-prefetched
            (let ((default-directory (oref project root)))
              (apply #'global-tags--get-lines-future
                     (append (list command nil)
                             flags)))))
    ;; return from whatever future we had
    (pcase-let* ((`(,command-return-code . ,lines)
                  (async-get this-prefetched)))
      (when (= command-return-code 0)
        lines))))

;;;; connect to API
(defun global-tags-try-project-root (dir)
  "Project root for DIR if it exists."
  (when-let* ((dbpath (global-tags--get-dbpath dir))
              (project
               ;; ↓ default to launch pre-fetches
               (global-tags-project-with-pre-fetched-lines
                :root dbpath)))
    ;; ↓ won't prefetch if `global-tags--ensure-next-fetch-is-queued'
    ;;   method for project does not says so
    (cl-loop for command-and-flags in global-tags--commands-and-args-that-allow-prefetch
             do
             (pcase-let* ((`(,command ,flags) command-and-flags))
               (global-tags--ensure-next-fetch-is-queued project command flags)))
    project))

(cl-defmethod project-root ((project global-tags-project))
  "Default implementation.

See `project-root' for 'transient."
  (oref project root))

;; No need to implement unless necessary
;;(cl-defmethod project-external-roots ((project (head global)))
;;  )
(cl-defmethod project-files ((project global-tags-project) &optional _dirs)
  "Based off `vc' backend method.

_DIRS is ignored."
  (global-tags--remote-file-names
   (global-tags--project-get-lines project
                                   'path
                                   ;; ↓ project.el deals w/long names
                                   'absolute)))

;;(cl-defgeneric project-ignores (_project _dir)

;;; xref.el integration
(defun global-tags-xref-backend ()
  "Xref backend for using global.

Redirects to `global-tags-try-project-root'"
  (global-tags-try-project-root default-directory))

(cl-defmethod xref-backend-definitions ((_backend global-tags-project) symbol)
  "See `global-tags--get-locations'."
  (global-tags--get-xref-locations (substring-no-properties symbol) 'tag))

(cl-defmethod xref-backend-identifier-at-point ((_backend global-tags-project))
  (if-let ((symbol-str (thing-at-point 'symbol)))
      symbol-str))

(cl-defmethod xref-backend-identifier-completion-table ((backend global-tags-project))
  (global-tags--project-get-lines backend 'completion))

(cl-defmethod xref-backend-references ((_backend global-tags-project) symbol)
  (global-tags--get-xref-locations (substring-no-properties symbol) 'reference))

(cl-defmethod xref-backend-apropos ((_backend global-tags-project) symbol)
  (global-tags--get-xref-locations (substring-no-properties symbol) 'grep))

;;; update database
(cl-defun global-tags-update-database-with-buffer (&optional
                                                   (buffer (current-buffer)))
  "Update BUFFER entry in database.

Requires BUFFER to have a file name (path to file exists)."
  (with-current-buffer buffer
    (unless (buffer-file-name)
      ;; don't update a buffer that doesn't exist
      (error "Cannot update %s (no filename)"
             buffer))
    (global-tags--get-lines-future 'update
                                   t ;; ignore result
                                   'single-update
                                   (file-local-name
                                    (expand-file-name
                                     (buffer-file-name))))))

;;; creating database
(defun global-tags-create-database (directory)
  "Create tags database at DIRECTORY."
  (interactive "DCreate database at: ")
  (let ((default-directory directory))
    (apply 'process-file
           (append
            (list
             global-tags-generate-tags-command
             nil nil nil)
            global-tags-generate-tags-flags))))

(defun global-tags-ensure-database ()
  "Calls `global-tags-create-database' if one db does not exist."
  (unless (global-tags--get-dbpath default-directory)
    (call-interactively #'global-tags-create-database)))

(cl-defun global-tags-update-database ()
  "Calls «global --update» in the background."
  (interactive)
  (global-tags--get-lines-future 'update
                                 t ;; ignore result
                                 ))

(define-minor-mode global-tags-exclusive-backend-mode
  "Use GNU Global as exclusive backend for several Emacs features."
  nil nil nil
  (cond
   (global-tags-exclusive-backend-mode
    (setq-local xref-backend-functions '(global-tags-xref-backend))
    (setq-local project-find-functions '(global-tags-try-project-root))
    (add-hook 'after-save-hook
              #'global-tags-update-database-with-buffer
              nil
              t))
   (t
    (setq-local xref-backend-functions (default-value 'xref-backend-functions))
    (setq-local project-find-functions (default-value 'project-find-functions))
    (remove-hook 'after-save-hook
                 #'global-tags-update-database-with-buffer))))

(define-minor-mode global-tags-shared-backend-mode
  "Use GNU Global as backend for several Emacs features in this buffer."
  nil nil nil
  (cond
   (global-tags-shared-backend-mode
    (add-hook 'xref-backend-functions 'global-tags-xref-backend 80 t)
    (add-hook 'project-find-functions 'global-tags-try-project-root 80 t)
    (add-hook 'after-save-hook
              #'global-tags-update-database-with-buffer
              nil
              t))
   (t
    (remove-hook 'xref-backend-functions 'global-tags-xref-backend t)
    (remove-hook 'project-find-functions 'global-tags-try-project-root t)
    (remove-hook 'after-save-hook
                 #'global-tags-update-database-with-buffer))))

(provide 'global-tags)
;;; global-tags.el ends here
