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
                       (append `(,global-tags--global-command)
                               (global-tags--get-arguments 'print-dbpath))
                       " ")
            :to-equal (format "%s --print-dbpath" global-tags--global-command))
    (expect
     (global-tags--get-as-string 'print-dbpath) :to-equal nil))
  (it "nil return from invalid command"
    (expect (global-tags--get-as-string 'file "not-an-existing-file") :to-equal nil))
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
    (let ((default-directory global-tmp-project-directory)
          (completion-tags global-tags--all-tags-in-tests))
      ;; look how we call global with --print0,
      ;; yet symbols are \n-sepaarated
      (expect (global-tags--get-as-string 'completion '(print0))
              :to-equal
              (format "%s\n" (mapconcat 'identity
                                        completion-tags
                                        "\n")))
      (expect (global-tags--get-lines 'completion)
              :to-equal completion-tags))))


(defun global-tags--create-mock-project (project-path)
  "Create mock project on PROJECT-PATH."
  (let* ((default-directory (file-name-as-directory project-path))
	 (main-file-path (concat
			  (file-name-as-directory
			   default-directory)
			  "main.c"))
	 (main-header-path (concat
			    (file-name-as-directory
			     default-directory)
			    "main.h"))
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
}")
	 )
    (f-write-text main-file-text 'utf-8 main-file-path)
    (f-write-text main-header-text 'utf-8 main-header-path)
    (call-process "gtags")))



(defun global-gtags--create-temporary-mock-project ()
  "Create temporary mock project and return its path."
  (let ((global-tmp-project-directory (make-temp-file
                                       "global-unit-test-mock-project"
                                       t)))
    (global-tags--create-mock-project global-tmp-project-directory)
    global-tmp-project-directory))

(provide 'global-tests)
;;; global-tests.el ends here
