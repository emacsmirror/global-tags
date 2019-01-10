;;; global.el --- elisp wrapper functions and Emacs integration for GNU global  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Felipe Lema

;; Author: Felipe Lema <felipel@cl01cats24>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; WIP:
;; - wrapper calls
;; - project.el integration
;; - xref.el integration

;;; Commentary:

;;

;;; Code:

(defgroup global nil "GNU global integration"
  :group 'tools)

;;;; utility functions:

(defun global--command-flag (command)
  "Get command line flag for COMMAND as string-or-nil.

Flags are not contracted."
  (pcase command
    (`tag nil)
    (`completion "--completion")
    (`file "--file")
    (`grep "--grep")
    (`idutils "--idutils")
    (`path "--path")
    (`print-dbpath "--print-dbpath")
    (`update "--update")
    (_ (error "Unknown command: %s" (symbol-name command)))))

(defun global--option-requires-extra-flag? (option)
  "Whether command line OPTION requires an extra flag."
  (if (find option '(color encode-path from-here gtagsconf gtagslabel file-list
                           match-part ;; 'nearness is handled outside
                           path-style result single-update
                           scope))
      t))

(defun global--option-sans-extra-flag? (option)
  "Whether command line OPTION does not require an extra flag."
  (if (not (eq option 'nearness));; 'nearness is handled outside
      (not (global--option-sans-extra-flag? option))))

(defun global--option-flag (flag &optional value)
  "Get command line option flag for FLAG as string-or-nil.

When FLAG requires an extra parameter, this is passed in VALUE.
Flags are not contracted.  Result is a list of arguments."
  (pcase (list (global--option-sans-extra-flag? flag) flag value)
    (`(,_ 'nearness  ,start) (error "nearness option not implemented"))
    (`(t ,actualflag nil) ;; no option
     (list (format "--%s" (symbol-name actualflag))))
    (`(nil ,actualflag ,value) ;; --some-param some-value
     (list (format "--%s" (symbol-name actualflag))
           (progn
             (assert (stringp value)
                     (format "extra parameter for %s must be string, found " value))
             value)))
    (_ (error "Unknown option combination: %s %s" (symbol-name flag) value))))
(provide 'global)
;;; global.el ends here
