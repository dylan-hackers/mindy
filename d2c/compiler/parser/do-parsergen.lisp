;;; -*- Package: PARSER -*-
;;;
;;; **********************************************************************
;;; Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
;;; 
(ext:file-comment
  "$Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/do-parsergen.lisp,v 1.1 1994/12/12 13:01:40 wlott Exp $")
;;;
;;; **********************************************************************

(in-package :user)

(eval-when (compile load eval)
  (or (find-package :parser) (make-package :parser)))

(in-package :parser)
(use-package :ext)

(handler-case
    (let ((hpf-write-date (file-write-date "parsergen.hpf")))
      (if (or (not hpf-write-date)
	      (< hpf-write-date (file-write-date "parsergen.lisp")))
	  (compile-file "parsergen.lisp" :load t)
	  (load "parsergen")))
  (error (cond)
    (format *error-output* "~2&It didn't work:~%~A~%" cond)
    (quit 1)))

(handler-case
    (progn
      (grovel-file "parser.input" "parser.dylan")
      (quit 0))
  (error (cond)
    (format *error-output* "~2&It didn't work:~%~A~%" cond)
    (quit 1)))
