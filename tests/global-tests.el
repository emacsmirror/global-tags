;;; global-tests.el --- tests for global.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Felipe Lema

;; Author: Felipe Lema <felipel@devuan-pega>
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
(require 'global)

(describe "commands and flags"
	  (it "commands with no extra flag"
	      (expect (global--option-flags (list 'definition)) :to-equal '("--definition"))
	      (expect (global--option-flags (list 'absolute)) :to-equal '("--absolute")))

	  (it "nearness"
	      (expect
	       (global--option-flags (list 'nearness "start")) :to-equal '("--nearness=start")))

	  (it "tag command has no flag"
	      (expect (global--command-flag 'tag) :to-equal '())
	      (expect (global--command-flag 'completion) :to-equal '("--completion")))

	  (it "compose commands and flag"
	      (expect
	       (global--get-arguments 'path 'absolute) :to-equal '("--path" "--absolute"))
	      (expect (global--get-arguments 'tag 'absolute) :to-equal '("--absolute"))
	      (expect (global--get-arguments 'print-dbpath) :to-equal '("--print-dbpath")))

	  (it "arguments make sense"
	      (expect (mapconcat #'shell-quote-argument
				 (append `(,global--global-command)
					 (global--get-arguments 'print-dbpath))
				 " ")
		      :to-equal (format "%s --print-dbpath" global--global-command))
	      (expect
	       (global--get-as-string 'print-dbpath) :to-equal nil))
	  (it "nil return from invalid command"
	      (expect (global--get-as-string 'file "not-an-existing-file") :to-equal nil))
	  (it "no dbpath"
	      (expect (global--get-dbpath "/") :to-equal nil)))

(describe "internals"
	  (it "parse line"
	      (let-alist (global--get-location "some/file/path/src.cpp:423:static void some_fun(uint32_t const& arg1, SomeStruct& _struct, uint32_t& some_value, uint32_t& something)")
		(expect .description :to-equal "static void some_fun(uint32_t const& arg1, SomeStruct& _struct, uint32_t& some_value, uint32_t& something)")
		(expect .file :to-equal "some/file/path/src.cpp")
		(expect .line :to-equal 423))))
(describe "reading output"
	  (before-each
	   (setq global-tmp-project-directory
		 (make-temp-file
		  "global-unit-test-mock-project"
		  t))
	   (global--create-mock-project global-tmp-project-directory))
	  (it "read files"
	      (let ((default-directory global-tmp-project-directory))
		(expect (global--get-lines 'path)
			:to-equal '("main.c" "main.h"))))
	  (it "dbpath read correctly"
	      (expect (global--get-dbpath global-tmp-project-directory)
		      :to-equal global-tmp-project-directory)))

(defun global--create-mock-project (project-path)
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
int main{
    int local_int;
}")
	 )
    (f-write-text main-file-text 'utf-8 main-file-path)
    (f-write-text main-header-text 'utf-8 main-header-path)
    (call-process "gtags")))



(provide 'global-tests)
;;; global-tests.el ends here
