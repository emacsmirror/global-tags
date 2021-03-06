;;; global-tests.el --- tests for global.el          -*- lexical-binding: t; -*-

;; Copyright © 2019  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
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

;;; Commentary:

;;

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'f)
(require 'global-tags)

(defconst global-tags--all-tags-in-tests
  '("another_global_fun" "called_fun" "global_fun")
  "All tags present in current tests.

See `global-tags--create-mock-project' for source code that produces these
tags.")

(describe "commands and flags"
  (it "commands with no extra flag"
    (expect (global-tags--option-flags (list 'definition)) :to-equal '("--definition"))
    (expect (global-tags--option-flags (list 'absolute)) :to-equal '("--absolute")))

  (it "nearness"
    (expect
     (global-tags--option-flags (list 'nearness "start")) :to-equal '("--nearness=start")))

  (it "tag command has no flag"
    (expect (global-tags--command-flag 'tag) :to-equal '())
    (expect (global-tags--command-flag 'completion) :to-equal '("--completion")))

  (it "compose commands and flag"
    (expect
     (global-tags--get-arguments 'path 'absolute) :to-equal '("--path" "--absolute"))
    (expect (global-tags--get-arguments 'tag 'absolute) :to-equal '("--absolute"))
    (expect (global-tags--get-arguments 'print-dbpath) :to-equal '("--print-dbpath")))

  (it "arguments make sense"
    (expect (mapconcat #'shell-quote-argument
                       (append `(,global-tags-global-command)
                               (global-tags--get-arguments 'print-dbpath))
                       " ")
            :to-equal (format "%s --print-dbpath" global-tags-global-command))
    (expect
     (global-tags--get-lines 'print-dbpath) :to-equal nil))
  (it "nil return from invalid command"
    (expect (global-tags--get-lines 'file "not-an-existing-file") :to-equal nil))
  (it "no dbpath"
    (expect (global-tags--get-dbpath "/") :to-equal nil)))

(describe "internals"
  (it "parse line"
    (let-alist (global-tags--get-location "some/file/path/src.cpp:423:static void some_fun(uint32_t const& arg1, SomeStruct& _struct, uint32_t& some_value, uint32_t& something)")
      (expect .description :to-equal "static void some_fun(uint32_t const& arg1, SomeStruct& _struct, uint32_t& some_value, uint32_t& something)")
      (expect .file :to-equal "some/file/path/src.cpp")
      (expect .line :to-equal 423)))
  (it "line with empty description (string size 0)"
    (expect (global-tags--get-location "some_file.cpp:50:")
            :not :to-be nil)))

(defvar global-tmp-project-directory
  nil
  "Temporary project directory for running a single test.

See `global-gtags--create-temporary-mock-project'")

(describe "reading output"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)))
  (after-each
    (delete-directory global-tmp-project-directory t))
  (it "read files"
    (let ((default-directory global-tmp-project-directory))
      (expect (global-tags--get-lines 'path)
              :to-equal '("main.c" "main.h"))))
  (it "dbpath read correctly"
    (expect (global-tags--get-dbpath global-tmp-project-directory)
            :to-equal global-tmp-project-directory)))

(describe "quirks"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)))
  (after-each
    (delete-directory global-tmp-project-directory t))
  (it "global --completion does not respect --print0"
    ;; we're explicitly testing --completion to make sure it works
    ;; _regardless_ of not using --print0 underneath
    (let ((default-directory global-tmp-project-directory)
          (completion-tags global-tags--all-tags-in-tests))
      (expect (global-tags--get-lines 'completion)
              :to-equal completion-tags))))

(describe "usage/API"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)))
  (after-each
    (delete-directory global-tmp-project-directory t))
  (it "reference"
    (let ((default-directory global-tmp-project-directory))
      (expect (global-tags--get-xref-locations "called_fun" 'reference)
              :not :to-be nil)))
  (it "update-db-single-file"
    (let ((default-directory global-tmp-project-directory))
      (expect (global-tags-update-database-with-buffer
               (find-file (f-join default-directory "main.c")))
              :not :to-throw))))

(describe "border case"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)))
  (after-each
    (delete-directory global-tmp-project-directory t))
  (it "empty return" ;; https://bugs.launchpad.net/global-tags.el/+bug/1850641
    (let ((default-directory global-tmp-project-directory)
          (symbol "this_symbol_does_not_exist")
          (xref-backend-functions '(global-tags-xref-backend))
          (kind 'reference))
      (expect (global-tags--get-lines kind
                                      ;; ↓ see `global-tags--get-location'
                                      'result "grep"
                                      symbol)
              :to-be nil)
      (expect (global-tags--get-locations symbol kind)
              :to-be nil)
      (expect (xref-backend-references (xref-find-backend) "this_symbol_does_not_exist")
              :to-be nil))))

(describe "project.el integration"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)
          project-find-functions '(global-tags-try-project-root)))
  (after-each
    (delete-directory global-tmp-project-directory t)
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)
          project-find-functions (default-value 'project-find-functions)))
  (it "root"
    (let ((default-directory global-tmp-project-directory))
      (expect (project-current)
              :not :to-be nil)
      (expect (project-root
               (project-current))
              :to-equal default-directory)))
  (it "find files"
    (let ((default-directory global-tmp-project-directory))
      (expect (project-current)
              :not :to-be nil)
      (expect (project-files
               (project-current))
              :to-equal
              (cl-mapcar
               (lambda (f)
                 (f-join
                  global-tmp-project-directory
                  f))
               '("main.c" "main.h"))))))

(describe "tramp"
  ;; use project.el in tramp context
  (before-each
    (setq global-tmp-project-directory
          (concat "/ssh:localhost:"
                  (global-gtags--create-temporary-mock-project))))
  (after-each
    (delete-directory global-tmp-project-directory t))
  (it "root"
    (expect (file-remote-p global-tmp-project-directory)
            :not :to-be nil)
    (let ((default-directory global-tmp-project-directory)
          ;; force the use of only `global-tags-try-project-root' for project root
          (project-find-functions '(global-tags-try-project-root)))
      (expect (project-root
               (project-current))
              :to-equal global-tmp-project-directory))))

(describe "xref integration"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)
          xref-backend-functions '(global-tags-xref-backend)))
  (after-each
    (delete-directory global-tmp-project-directory t)
    (setq xref-backend-functions (default-value 'xref-backend-functions)))
  (it "xref-backend-definitions"
    (let* ((default-directory global-tmp-project-directory)
           (current-xref-backend (xref-find-backend)))
      (expect
       current-xref-backend :not :to-be nil)
      (expect
       (xref-backend-identifier-completion-table current-xref-backend)
       :to-equal
       global-tags--all-tags-in-tests))))

(describe "user provided"
  (before-each
    (let ((user-provided-directory
           (f-join
            (locate-dominating-file
             (symbol-file 'global-gtags--create-temporary-mock-project)
             ".git")
            "tests" "from_tom")))
      (cl-assert (f-exists? user-provided-directory))
      (setq default-directory user-provided-directory
            xref-backend-functions '(global-tags-xref-backend))))
  (after-each
    (setq xref-backend-functions (default-value 'xref-backend-functions)))
  (it "have-xref"
    (expect (xref-find-backend)
            :not :to-be nil))
  (it "special line" ;; this test does not require the user provided directory
    (expect (global-tags--get-location
             "parser.lua:348:	local r = lpeg.match(proto * -1 + exception , text , 1, state )
")
            :not :to-be nil))
  (it "lpeg" ;; https://bugs.launchpad.net/global-tags.el/+bug/1850641
    (expect (xref-backend-references (global-tags-xref-backend) "lpeg")
            :not :to-be nil)))

(defun my/project-dont-prompt-user-get-matching (prompt
                                                 all-files &optional predicate
                                                 hist default)
  (let ((found-files
         (seq-find
          (lambda (full-path)
            (string-equal
             (file-name-nondirectory full-path)
             default))
          all-files)))
    ;; we're not supposed to be handling more than one file
    (cl-assert found-files)
    found-files))

(describe "xref searches for either symbol or include file"
  (before-each
    (setq global-tmp-project-directory
          (global-gtags--create-temporary-mock-project)
          xref-backend-functions '(global-tags-xref-backend)
          ;; force project.el to return matching "default" from "all-files" without prompting anyone
          project-read-file-name-function #'my/project-dont-prompt-user-get-matching))
  (after-each
    (delete-directory global-tmp-project-directory t)
    (setq xref-backend-functions (default-value 'xref-backend-functions)
          project-read-file-name-function (default-value 'project-read-file-name-function)))
  (it "search at point on include"
    (save-window-excursion
      (let ((main.c-buffer
             (find-file (f-join global-tmp-project-directory "main.c"))))
        (with-current-buffer main.c-buffer
          (widen)
          (goto-char (point-min))
          (search-forward-regexp (rx "main.h"))
          ;; ensure we're standing in the middle of ⎡main.h⎦
          (goto-char (- (point) 2))
          (expect (thing-at-point 'filename)
                  :to-equal "main.h")
          ;; correctly captures header file as an xref identifier
          (expect
           (substring-no-properties
            (xref-backend-identifier-at-point
             (xref-find-backend)))
           :to-equal "main.h")
          ;; gets the file as definition
          (expect
           (let* ((identifier (xref-backend-identifier-at-point (xref-find-backend)))
                  (search-result ;; search using project.el opens the file and returns the buffer
                   (xref-backend-definitions (xref-find-backend)
                                             identifier))
                  (file-path-of-result
                   (buffer-file-name search-result)))
             file-path-of-result)
           :to-equal
           (f-join global-tmp-project-directory "main.h")))))))

(describe "async create db background (1922452)"
  (before-each
    ;; create project, but don't setup a database
    (thread-last
        (make-temp-file
         "global-unit-test-mock-project"
         t)
      (setq global-tmp-project-directory)
      (global-tags--create-mock-project)))
  (after-each
    (delete-directory global-tmp-project-directory t))
  (it "create database async-ly"
    (let* ((default-directory global-tmp-project-directory)
           (project-find-functions '(global-tags-try-project-root))
           (db-future
            (global-tags-create-database-in-background
             default-directory)))
      (while (not (async-ready db-future))
        (accept-process-output db-future))
      (expect (global-tags--get-dbpath default-directory)
              :not :to-be nil))))

(defun global-tags--create-mock-project (project-path)
  "Create mock project on PROJECT-PATH.

Does not create Global database."
  (let* ((default-directory (file-name-as-directory project-path))
         (main-file-path (f-join default-directory "main.c"))
         (main-header-path (f-join default-directory "main.h"))
         (main-header-text "char *header_string;
int header_int;
float header_float;
void global_fun();
void another_global_fun();
void called_fun();
")
         (main-file-text  "
#include \"main.h\"

char *global_string;
int global_int;
float global_float;
void global_fun(){
    write(header_string);
    write(header_header_int);
}
void another_global_fun(){
    write(header_header_int);
}
void called_fun(){
    // this function is called at least twice
}
int main{
    int local_int;
    called_fun();
    called_fun();
}"))
    (f-write-text main-file-text 'utf-8 main-file-path)
    (f-write-text main-header-text 'utf-8 main-header-path)
    default-directory))

(defun global-gtags--create-temporary-mock-project ()
  "Create temporary mock project and return its path.

Creates a Global database"
  (let* ((this-global-tmp-project-directory
          (make-temp-file
           "global-unit-test-mock-project"
           t)))

    (global-tags--create-mock-project this-global-tmp-project-directory)
    (global-tags-create-database this-global-tmp-project-directory)
    this-global-tmp-project-directory))



(provide 'global-tags-tests)
;;; global-tags-tests.el ends here
