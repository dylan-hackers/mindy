;;; -*- Package: PARSER -*-
;;;
;;; **********************************************************************
;;; Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
;;; 
(ext:file-comment
  "$Header: /scm/cvs/src/d2c/compiler/parser/do-parsergen.lisp,v 1.1 1998/05/03 19:55:28 andreas Exp $")
;;;
;;; **********************************************************************

(in-package :user)

(eval-when (compile load eval)
  (or (find-package :parser) (make-package :parser)))

(in-package :parser)
(use-package :ext)

(handler-case
    (progn
      (unless (= (length *command-line-words*) 2)
	(error
	 "usage: cmucl .../parsergen.lisp .../parser.input"))
      (let ((hpf-write-date (file-write-date "parsergen.hpf"))
	    (source-file (first *command-line-words*)))
	(if (or (not hpf-write-date)
		(< hpf-write-date (file-write-date source-file)))
	    (compile-file source-file :load t :output-file "parsergen.hpf")
	    (load "parsergen.hpf"))))
  (error (cond)
    (format *error-output* "~2&It didn't work:~%~A~%" cond)
    (quit 1)))

(handler-case
    (progn
      (grovel-file (second *command-line-words*) "parser.dylan" "parser.log")
      (quit 0))
  (error (cond)
    (format *error-output* "~2&It didn't work:~%~A~%" cond)
    (quit 1)))
