;;; -*- Package: PARSER -*-
;;;
;;; **********************************************************************
;;; Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
;;; 
(ext:file-comment
  "$Header: /scm/cvs/src/d2c/compiler/parser/parsergen.lisp,v 1.1 1998/05/03 19:55:28 andreas Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains a LALR parser generator.
;;;
(in-package :parser)
(use-package :ext)

(export '(define-parser))



;;;;

(defstruct (grammar
	    (:print-function %print-grammar)
	    (:constructor make-grammar ()))
  ;;
  ;; List of entry-point grammar symbols.
  (entry-points '() :type list)
  ;;
  ;; Hash table of all the grammar symbols.
  (symbols (make-hash-table :test #'eq) :type hash-table :read-only t)
  ;;
  ;; Next available token-id.
  (next-token-id 0 :type fixnum)
  ;;
  ;; List of all the tokens.
  (tokens nil :type list)
  ;;
  ;; List of all the token unions.
  (token-unions nil :type list)
  ;;
  ;; List of all the non-terminals.
  (nonterminals nil :type list)
  ;;
  ;; List of the start productions.  One for each entry point.
  (start-productions nil :type list)
  ;;
  ;; Number of productions.
  (num-productions 0 :type (integer 0 *))
  ;;
  ;; List or start states, one for each entry point.
  (start-states nil :type list)
  ;;
  ;; The complete set of states.
  (all-states nil :type list)
  ;;
  ;; The number of states.
  (num-states 0 :type integer))

(defun %print-grammar (grammar stream depth)
  (declare (ignore depth))
  (print-unreadable-object (grammar stream :type t)))


(defstruct (grammar-symbol
	    (:constructor nil))
  ;;
  ;; List of tokens that can start this grammar symbol.
  (first nil :type list)
  ;;
  ;; True iff there is some (foo -> epsilon) production for this grammar
  ;; symbol.
  (nullable nil :type (member t nil))
  ;;
  ;; The symbol name for this grammar symbol.
  (name (required-argument) :type symbol)
  ;;
  ;; The type for this grammar symbol, or nil if not specified.
  (type nil :type symbol))

(defstruct (terminal
	    (:include grammar-symbol)
	    (:constructor nil)
	    (:print-function %print-terminal))
  )

(defun %print-terminal (symbol stream depth)
  (declare (ignore depth))
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (symbol stream :type t)
	(let ((*print-case* :upcase))
	  (prin1 (grammar-symbol-name symbol) stream)))
      (let ((*print-case* :upcase))
	(prin1 (grammar-symbol-name symbol) stream))))
  

(defstruct (token
	    (:include terminal)
	    (:constructor %make-token (name type id)))
  ;;
  ;; A unique integer corresponding to this terminal.
  (id (required-argument) :type fixnum))

(defun make-token (name type id)
  (let ((result (%make-token name type id)))
    (setf (token-first result) (list result))
    result))

(defstruct (token-union
	    (:include terminal)
	    (:constructor make-token-union (name type members)))
  ;;
  ;; List of the names for the terminals that make up this union.
  (members (required-argument) :type list))

(defstruct (nonterminal
	    (:include grammar-symbol)
	    (:constructor make-nonterminal (name))
	    (:print-function %print-nonterminal))
  ;;
  ;; List of productions with this nonterminal on the left hand side.
  (productions nil :type list))

(defun %print-nonterminal (symbol stream depth)
  (declare (ignore depth))
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (symbol stream :type t)
	(let ((*print-case* :downcase))
	  (prin1 (grammar-symbol-name symbol) stream)))
      (let ((*print-case* :downcase))
	(prin1 (grammar-symbol-name symbol) stream))))


(defstruct (production
	    (:constructor make-production (number left-side right-side body))
	    (:print-function %print-production))
  ;;
  ;; Small integer uniquely identifing this production.
  (number 0 :type (integer 0 *))
  ;;
  ;; The nonterminal on the left.
  (left-side (required-argument) :type nonterminal)
  ;;
  ;; List of grammar symbols on the right.
  (right-side nil :type list)
  ;;
  ;; The forms that make up this production.
  (body nil :type list)
  ;;
  ;; Vector of kernel-items indexed by dot-position, or NIL if we haven't
  ;; allocated it yet. Also, individual elements can be NIL if that
  ;; particular kernel-item yet.
  (kernel-items nil :type (or null simple-vector)))

(defun %print-production (production stream depth)
  (declare (ignore depth))
  (if (or *print-escape* *print-readably*)
      (print-unreadable-object (production stream :type t)
	(format stream "~D: ~A ->~:[ epsilon~;~:*~{ ~A~}~]"
		(production-number production)
		(production-left-side production)
		(production-right-side production)))
      (format stream "~A ->~:[ epsilon~;~:*~{ ~A~}~]"
		(production-left-side production)
		(production-right-side production))))

(defvar *item-counter* 0)

(defstruct (item
	    (:constructor make-item (production))
	    (:print-function %print-item))
  ;;
  ;; The production this item is built from.
  (production (required-argument) :type production)
  ;;
  ;; The position of the dot in this item.
  (dot-position 0 :type (integer 0 *))
  ;;
  ;; Unique index arbitrarily assigned to items as they are crested.
  (counter (incf *item-counter*) :type (integer 0 *)))

(defun %print-item (item stream depth)
  (declare (ignore depth))
  (print-unreadable-object (item stream :type t)
    (let* ((production (item-production item))
	   (right-side (production-right-side production))
	   (dot-position (item-dot-position item)))
      (format stream "~A ->~{ ~A~} .~{ ~A~}"
	      (production-left-side production)
	      (subseq right-side 0 dot-position)
	      (subseq right-side dot-position)))))

(defstruct (kernel-item
	    (:include item)
	    (:constructor make-kernel-item (production dot-position))
	    (:print-function %print-kernel-item))
  ;;
  ;; The lookahead terminals.
  (lookaheads nil :type list)
  ;;
  ;; The lookahead terminals added after the last time we propagated
  ;; terminals.
  (new-lookaheads nil :type list)
  ;;
  ;; List of items our lookaheads get propagated to.
  (propagates-to nil :type list))

(defun %print-kernel-item (item stream depth)
  (declare (ignore depth))
  (print-unreadable-object (item stream :type t)
    (let* ((production (kernel-item-production item))
	   (right-side (production-right-side production))
	   (dot-position (kernel-item-dot-position item)))
      (format stream "~A ->~{ ~A~} .~{ ~A~}~@[, ~{~A~^/~}~]"
	      (production-left-side production)
	      (subseq right-side 0 dot-position)
	      (subseq right-side dot-position)
	      (kernel-item-lookaheads item)))))


(defstruct (item-set
	    (:constructor %make-item-set (items))
	    (:print-function %print-item-set))
  ;;
  ;; List of items, sorted by item-counter.
  (items (required-argument) :type list))

(defun %print-item-set (set stream depth)
  (declare (ignore depth))
  (print-unreadable-object (set stream :type t)
    (prin1 (item-set-items set) stream)))

(defun make-item-set (items)
  (declare (type list items))
  (%make-item-set (sort items #'< :key #'item-counter)))

(defun item-sets-= (set1 set2)
  (declare (type item-set set1 set2))
  (equal (item-set-items set1) (item-set-items set2)))



(defstruct (state
	    (:constructor make-state (number kernels))
	    (:print-function %print-state))
  ;;
  ;; Small integer uniquly identifing this state.
  (number 0 :type (integer 0 *))
  ;;
  ;; List of kernel states.
  (kernels (required-argument) :type item-set)
  ;;
  ;; A-list mapping grammar symbols to next states.
  (gotos nil :type list)
  ;;
  ;; A-list mapping terminals to actions.  Each action is one of:
  ;;  (:accept) -- We are done.
  ;;  (:shift state) -- shift state onto the stack
  ;;  (:reduce production) -- reduce stack using production
  (actions nil :type list))

(defun %print-state (state stream depth)
  (declare (ignore depth))
  (print-unreadable-object (state stream :type t)
    (prin1 (state-number state) stream)))


;;;; Grammar parsing stuff.

(defun find-grammar-symbol (grammar thing)
  (declare (type grammar grammar)
	   (type symbol thing)
	   (values grammar-symbol))
  (or (gethash thing (grammar-symbols grammar))
      (let ((new (make-nonterminal thing)))
	(push new (grammar-nonterminals grammar))
	(setf (gethash thing (grammar-symbols grammar)) new)
	new)))

(defun parse-production (grammar left-side right-side body)
  (declare (type grammar grammar)
	   (type symbol left-side)
	   (type list right-side)
	   (type list body)
	   (values production))
  (let* ((nonterminal (find-grammar-symbol grammar left-side))
	 (num-productions (grammar-num-productions grammar))
	 (production
	  (make-production num-productions
			   nonterminal
			   (mapcar #'(lambda (thing)
				       (declare (type symbol thing)
						(values grammar-symbol))
				       (find-grammar-symbol grammar thing))
				   right-side)
			   body)))
    (setf (grammar-num-productions grammar) (1+ num-productions))
    (push production (nonterminal-productions nonterminal))
    production))


;;;; compute firsts.

(defun compute-token-union-firsts (grammar)
  (labels ((maybe-expand-union (union)
	     (declare (type token-union union))
	     (or (token-union-first union)
		 (let ((results nil))
		   (dolist (member-name (token-union-members union))
		     (let ((member (find-grammar-symbol grammar member-name)))
		       (etypecase member
			 (nonterminal
			  (error "In union ~S, member ~S is a non-terminal."
				 (token-union-name union) member-name))
			 (token
			  (pushnew member results))
			 (token-union
			  (dolist (member-member (maybe-expand-union member))
			    (pushnew member-member results))))))
		   (setf (token-union-first union) results)))))
    (dolist (union (grammar-token-unions grammar))
      (maybe-expand-union union)))
  (undefined-value))

(defun compute-nonterminal-firsts (grammar)
  (loop
    (let ((anything-changed nil))
      (dolist (nonterminal (grammar-nonterminals grammar))
	(dolist (production (nonterminal-productions nonterminal))
	  (dolist (symbol
		   (production-right-side production)
		   (unless (nonterminal-nullable nonterminal)
		     (setf (nonterminal-nullable nonterminal) t)
		     (setf anything-changed t)))
	    (dolist (first (grammar-symbol-first symbol))
	      (unless (member first (nonterminal-first nonterminal))
		(push first (nonterminal-first nonterminal))
		(setf anything-changed t)))
	    (unless (grammar-symbol-nullable symbol)
	      (return)))))
      (unless anything-changed
	(return))))
  (undefined-value))

(defun compute-firsts (grammar)
  (compute-token-union-firsts grammar)
  (compute-nonterminal-firsts grammar)
  (undefined-value))


;;;; Compute-items

;;; MAP-ITEMS -- internal.
;;;
;;; Invoke function on each item in closure(item-set).
;;;
(declaim (inline map-items))
;;;
(defun map-items (function item-set)
  (declare (type item-set item-set))
  (let ((productions-added nil))
    (labels ((grovel (production dot-position)
	       (funcall function production dot-position)
	       (let ((right-side (production-right-side production)))
		 (when (< dot-position (length right-side))
		   (let ((next-symbol (nth dot-position right-side)))
		     (when (nonterminal-p next-symbol)
		       (dolist (prod (nonterminal-productions next-symbol))
			 (unless (member prod productions-added)
			   (push prod productions-added)
			   (grovel prod 0)))))))))
      (dolist (item (item-set-items item-set))
	(grovel (item-production item) (item-dot-position item))))))


(defmacro do-items ((production-var dot-position-var items) &body body)
  `(map-items #'(lambda (,production-var ,dot-position-var) ,@body) ,items))

(defun item-equal (item-1 item-2)
  (and (eq (item-production item-1) (item-production item-2))
       (= (item-dot-position item-1) (item-dot-position item-2))))

(defun find-kernel-item (production dot-position)
  (let ((vector (or (production-kernel-items production)
		    (setf (production-kernel-items production)
			  (make-array (1+ (length (production-right-side
						   production)))
				      :initial-element nil)))))
    (or (aref vector dot-position)
	(setf (aref vector dot-position)
	      (make-kernel-item production dot-position)))))

(defun compute-state (grammar kernel-items)
  (let ((item-set (make-item-set kernel-items)))
    (or (find item-set (grammar-all-states grammar)
	      :key #'state-kernels
	      :test #'item-sets-=)
	(let ((state (make-state (grammar-num-states grammar) item-set))
	      (gotos nil))
	  (push state (grammar-all-states grammar))
	  (incf (grammar-num-states grammar))
	  (do-items (production dot-position item-set)
	    (let* ((right-side (production-right-side production)))
	      (when (< dot-position (length right-side))
		(let* ((new-item
			(find-kernel-item production
					  (1+ dot-position)))
		       (next-symbol (nth dot-position right-side))
		       (entry (assoc next-symbol gotos :test #'eq)))
		  (if entry
		      (push new-item (cdr entry))
		      (push (list next-symbol new-item) gotos))))))
	  (dolist (goto gotos)
	    (push (cons (car goto)
			(compute-state grammar (cdr goto)))
		  (state-gotos state)))
	  state))))

(defun compute-states (grammar)
  (setf (grammar-start-states grammar)
	(mapcar #'(lambda (start-production)
		    (let ((item (find-kernel-item start-production 0)))
		      (compute-state grammar (list item))))
		(grammar-start-productions grammar)))
  (setf (grammar-all-states grammar) (nreverse (grammar-all-states grammar)))
  (undefined-value))



;;;; Compute lookaheads.

(defun compute-initial-lookaheads (grammar)
  (declare (type grammar grammar))
  (let ((eof-symbol (find-grammar-symbol grammar (intern "EOF"))))
    (unless (token-p eof-symbol)
      (error "EOF isn't defined as a token."))
    (mapc #'(lambda (start-production start-state)
	      (push eof-symbol
		    (kernel-item-new-lookaheads
		     (find start-production
			   (item-set-items (state-kernels start-state))
			   :key #'kernel-item-production))))
	  (grammar-start-productions grammar)
	  (grammar-start-states grammar)))
  (dolist (state (grammar-all-states grammar))
    (dolist (kernel-item (item-set-items (state-kernels state)))
      (let ((done nil))
	(labels
	    ((grovel (item lookahead)
	       (let* ((a-prod (item-production item))
		      (right-side (production-right-side a-prod))
		      (dot-position (item-dot-position item)))
		 (when (< dot-position (length right-side))
		   (let ((next-symbol (nth dot-position right-side)))
		     (let* ((goto
			     (cdr (assoc next-symbol (state-gotos state))))
			    (other-item
			     (find-if #'(lambda (item)
					  (and (eq a-prod
						   (item-production item))
					       (= (1+ dot-position)
						  (item-dot-position item))))
				      (item-set-items (state-kernels goto)))))
		       (declare (type state goto)
				(type kernel-item other-item))
		       (if lookahead
			   (push lookahead
				 (kernel-item-new-lookaheads other-item))
			   (push other-item
				 (kernel-item-propagates-to kernel-item))))
		     (when (nonterminal-p next-symbol)
		       (let ((tail (subseq right-side (1+ dot-position))))
			 (dolist (b-prod (nonterminal-productions next-symbol))
			   (dolist (sym tail
					(maybe-grovel b-prod lookahead))
			     (dolist (first (grammar-symbol-first sym))
			       (maybe-grovel b-prod first))
			     (unless (grammar-symbol-nullable sym)
			       (return))))))))))
	     (maybe-grovel (production lookahead)
	       (unless (find-if #'(lambda (x)
				    (and (eq (car x) production)
					 (eq (cdr x) lookahead)))
				done)
		 (push (cons production lookahead) done)
		 (grovel (make-item production) lookahead))))
	  (grovel kernel-item nil)))))
  (undefined-value))

(defun propagate-lookaheads (grammar)
  (declare (type grammar grammar))
  (loop
    (let ((anything-changed nil))
      (dolist (state (grammar-all-states grammar))
	(dolist (item (item-set-items (state-kernels state)))
	  (let ((new (kernel-item-new-lookaheads item)))
	    (when new
	      (setf anything-changed t)
	      (setf (kernel-item-new-lookaheads item) nil)
	      (dolist (lookahead new)
		(push lookahead (kernel-item-lookaheads item))
		(dolist (to (kernel-item-propagates-to item))
		  (unless (member lookahead (kernel-item-lookaheads to))
		    (pushnew lookahead (kernel-item-new-lookaheads to)))))))))
      (unless anything-changed
	(return))))
  (undefined-value))


;;;; Compute actions.

(defvar *conflicts*)

(defun describe-state (state)
  (dolist (item (item-set-items (state-kernels state)))
    (let* ((production (item-production item))
	   (right-side (production-right-side production))
	   (dot-position (item-dot-position item)))
      (format t "  ~A ->~{ ~A~} *~{ ~A~}~%"
	      (production-left-side production)
	      (subseq right-side 0 dot-position)
	      (subseq right-side dot-position)))))  

(defun describe-action (action final-newline)
  (case (second action)
    (:reduce
     (let ((production (third action)))
       (format t " reduction by production ~D:~%  ~A ->~{ ~A~}~%"
	       (production-number production)
	       (production-left-side production)
	       (production-right-side production))))
    (:shift
     (let ((target (third action)))
       (format t " shift to state ~D:~%" (state-number target))
       (describe-state target)))
    (:accept
     (if final-newline
	 (format t " accept~%")
	 (format t " accept ")))
    (t
     (error "Strange action kind."))))

(defun add-action (state action)
  (labels ((add-token-action (token action-kind action-datum)
	     (let ((old-action (find token (state-actions state) :key #'car)))
	       (cond ((not old-action)
		      (push (list token action-kind action-datum)
			    (state-actions state)))
		     ((and (eq action-kind (second old-action))
			   (eq action-datum (third old-action)))
		      nil)
		     (t
		      (format t "~%~A/~A conflict at state ~D:~%"
			      (second old-action) action-kind
			      (state-number state))
		      (describe-state state)
		      (format t "on token ~S between" (token-name token))
		      (describe-action old-action nil)
		      (format t "and")
		      (describe-action action t)
		      (incf *conflicts*))))))
    (let ((terminal (car action))
	  (action-kind (cadr action))
	  (action-datum (caddr action)))
      (etypecase terminal
	(token
	 (add-token-action terminal action-kind action-datum))
	(token-union
	 (dolist (token (token-union-first terminal))
	   (add-token-action token action-kind action-datum))))))
  (undefined-value))

(defun compute-actions (grammar)
  (declare (type grammar grammar))
  (dolist (state (grammar-all-states grammar))
    (dolist (kernel-item (item-set-items (state-kernels state)))
      (let ((done nil))
	(labels
	    ((grovel (item lookahead)
	       (let* ((a-prod (item-production item))
		      (right-side (production-right-side a-prod))
		      (dot-posn (item-dot-position item)))
		 (if (< dot-posn (length right-side))
		     (let ((next-symbol (nth dot-posn right-side)))
		       (if (nonterminal-p next-symbol)
			   (let ((tail (subseq right-side (1+ dot-posn))))
			     (dolist (b-prod
				      (nonterminal-productions next-symbol))
			       (dolist (sym tail
					    (maybe-grovel b-prod lookahead))
				 (dolist (first (grammar-symbol-first sym))
				   (maybe-grovel b-prod first))
				 (unless (grammar-symbol-nullable sym)
				   (return)))))
			   (let ((goto (assoc next-symbol
					      (state-gotos state)
					      :test #'eq)))
			     (when goto
			       (add-action state
					   (list next-symbol
						 :shift
						 (cdr goto)))))))
		     (add-action state
				 (if (member a-prod
					    (grammar-start-productions grammar)
					     :test #'eq)
				     (list lookahead :accept)
				     (list lookahead :reduce a-prod))))))
	     (maybe-grovel (production lookahead)
	       (let ((entry (cons production lookahead)))
		 (unless (member entry done
				 :test #'(lambda (x y)
					   (and (eq (car x) (car y))
						(eq (cdr x) (cdr y)))))
		   (push entry done)
		   (grovel (make-item production) lookahead)))))
	  (dolist (lookahead (kernel-item-lookaheads kernel-item))
	    (grovel kernel-item lookahead))))))
  (undefined-value))
  




;;;; Compute gotos

(defun add-gotos (gotos number new)
  (dolist (goto new)
    (let ((symbol (car goto)))
      (when (nonterminal-p symbol)
	(let* ((target (state-number (cdr goto)))
	       (name (nonterminal-name symbol))
	       (entry (assoc name gotos)))
	  (if entry
	      (push (cons number target) (cdr entry))
	      (push (list name (cons number target)) gotos))))))
  gotos)

(defun compact-gotos (gotos)
  (mapcar
   #'(lambda (goto)
       (let ((nonterm (car goto))
	     (counts nil)
	     (most-common nil)
	     (occurrences 0))
	 (dolist (transition (cdr goto))
	   (let ((entry (assoc (cdr transition) counts)))
	     (if entry
		 (incf (cdr entry))
		 (push (cons (cdr transition) 1) counts))))
	 (dolist (count counts)
	   (when (> (cdr count) occurrences)
	     (setf occurrences (cdr count))
	     (setf most-common (car count))))
	 (let* ((without-default (remove most-common (cdr goto) :key #'cdr))
		(length (length without-default)))
	   (cond ((null without-default)
		  (cons nonterm most-common))
		 ((<= length 8)
		  (cons nonterm
			(append (mapcar #'(lambda (transition)
					    (list (car transition)
						  (cdr transition)))
					without-default)
				`((otherwise ,most-common)))))
		 (t
		  (let ((max-cur 0))
		    (dolist (transition (cdr goto))
		      (when (> (car transition) max-cur)
			(setf max-cur (car transition))))
		    (let ((result (make-array (1+ max-cur))))
		      (dolist (transition (cdr goto))
			(setf (aref result (car transition))
			      (cdr transition)))
		      (cons nonterm result))))))))
   gotos))

(defun compute-gotos (grammar)
  (let* ((gotos nil))
    (dolist (state (grammar-all-states grammar))
      (setf gotos
	    (add-gotos gotos (state-number state) (state-gotos state))))
    (compact-gotos gotos)))

      

;;;; Emitter

(defun dump-constant (thing ofile)
  (etypecase thing
    (simple-vector
     (format ofile "#[")
     (dotimes (i (length thing))
       (unless (zerop i)
	 (format ofile ", "))
       (dump-constant (svref thing i) ofile))
     (format ofile "]"))
    (list
     (format ofile "#(")
     (when thing
       (dump-constant (car thing) ofile)
       (do ((remainder (cdr thing) (cdr remainder)))
	   ((null remainder))
	 (cond ((listp remainder)
		(format ofile ", ")
		(dump-constant (car remainder) ofile))
	       (t
		(format ofile " . ")
		(dump-constant remainder ofile)
		(return)))))
     (format ofile ")"))
    ((or symbol integer string)
     (prin1 thing ofile))))

(defun emit-production (production gotos ofile)
  (let* ((right-side (production-right-side production))
	 (left-side (production-left-side production)))
    (format ofile "define method production-~D~%"
	    (production-number production))
    (format ofile
	    "    (prev-state :: <integer>, srcloc-0 :: <source-location>")
    (let ((index 0))
      (dolist (grammar-sym right-side)
	(incf index)
	(format ofile
		",~%     rhs-~D~@[ :: ~A~], srcloc-~D :: <source-location>"
		index
		(grammar-symbol-type grammar-sym)
		index)))
    (format ofile ")~%")
    (format ofile "    => (new-state :: <integer>, new-symbol~@[ :: ~A~]);~%"
	    (nonterminal-type left-side))
    (format ofile "  // ~A~%" production)
    (format ofile "  values(")
    (etypecase gotos
      (integer
       (format ofile "~D" gotos))
      (list
       (format ofile "select (prev-state)~%")
       (dolist (goto gotos)
	 (format ofile "           ~S => ~S;~%" (car goto) (cadr goto)))
       (format ofile "         end"))
      (vector
       (dump-constant gotos ofile)
       (format ofile "[prev-state]")))
    (format ofile ",~%")
    (format ofile "         begin~%")
    (dolist (form (production-body production))
      (format ofile "         ~A~%" form))
    (format ofile "         end);~%")
    (format ofile "end method production-~D;~2%"
	    (production-number production)))
  (values))

(defun encode-actions (actions grammar)
  (let ((vec (make-array (grammar-next-token-id grammar)
			 :initial-element 0)))
    (dolist (action actions)
      (multiple-value-bind
	  (action-kind datum)
	  (ecase (second action)
	    (:accept (values 1 0))
	    (:reduce (values 2 (production-number (third action))))
	    (:shift (values 3 (state-number (third action)))))
	(setf (aref vec (token-id (first action)))
	      (logior (ash datum 2) action-kind))))
    vec))

(defun emit-parser (grammar &optional (ofile *standard-output*))
  (let* ((num-tokens (grammar-next-token-id grammar))
	 (tokens (make-array num-tokens)))
    (dolist (token (grammar-tokens grammar))
      (setf (aref tokens (token-id token)) token))
    (format ofile "define constant $action-bits = 2;~%")
    (format ofile
	    "define constant $action-mask = ash(1, $action-bits) - 1;~2%")
    (format ofile "define constant $error-action = 0;~%")
    (format ofile "define constant $accept-action = 1;~%")
    (format ofile "define constant $reduce-action = 2;~%")
    (format ofile "define constant $shift-action = 3;~2%")
    (format ofile
	    "define constant $tokens-table :: <simple-object-vector>~
	     ~%  = vector(")
    (dotimes (i num-tokens)
      (unless (zerop i)
	(format ofile ",~%           "))
      (format ofile "$~A-token" (token-name (aref tokens i))))
    (format ofile ");~2%"))
  
  (format ofile
	  "define constant $action-table~
	   ~%  = #[")
  (let ((index 0))
    (dolist (state (grammar-all-states grammar))
      (unless (= index (state-number state))
	(error "State numbers got out of sync."))
      (unless (zerop index)
	(format ofile ",~%      "))
      (dump-constant (encode-actions (state-actions state) grammar) ofile)
      (incf index)))
  (format ofile "];~2%")
  
  (let* ((num-productions (grammar-num-productions grammar))
	 (productions-vector
	  (make-array num-productions :initial-element nil))
	 (num-pops-vector
	  (make-array num-productions :initial-element 0))
	 (gotos-table (compute-gotos grammar)))
    (dolist (nonterminal (grammar-nonterminals grammar))
      (let* ((name (nonterminal-name nonterminal))
	     (gotos (cdr (assoc name gotos-table))))
	(if gotos
	    (dolist (production (nonterminal-productions nonterminal))
	      (let ((prod-num (production-number production)))
		(unless (zerop prod-num)
		  (setf (aref productions-vector prod-num)
			(cons production gotos))
		  (setf (aref num-pops-vector prod-num)
			(length (production-right-side production))))))
	    (unless (find nonterminal (grammar-start-productions grammar)
			  :key #'production-left-side)
	      (warn "Nonterminal ~S can't appear." name)))))
    (dotimes (i num-productions)
      (let ((info (aref productions-vector i)))
	(when info
	  (emit-production (car info) (cdr info) ofile))))
    (format ofile "define constant $number-of-pops~%  = ");
    (dump-constant num-pops-vector ofile)
    (format ofile ";~2%")
    (format ofile
	    "define constant $production-table :: <simple-object-vector>~
	     ~%  = vector(")
    (dotimes (i num-productions)
      (unless (zerop i)
	(format ofile ", "))
      (if (aref productions-vector i)
	  (format ofile "production-~D" i)
	  (format ofile "#f")))
    (format ofile ");~2%"))
  
  (mapc #'(lambda (entry-point start-state)
	    (format ofile "define constant $~(~A~)-start-state = ~D;~%"
		    entry-point
		    (state-number start-state)))
	(grammar-entry-points grammar)
	(grammar-start-states grammar))

  (values))


;;;; Log file emitter.


(defun print-state (state line-prefix line-suffix file)
  (dolist (item (item-set-items (state-kernels state)))
    (let* ((production (item-production item))
	   (right-side (production-right-side production))
	   (dot-position (item-dot-position item)))
      (format file "~A~A ->~{ ~A~} *~{ ~A~}~%~A"
	      line-prefix
	      (production-left-side production)
	      (subseq right-side 0 dot-position)
	      (subseq right-side dot-position)
	      line-suffix))))

(defun emit-log-file (grammar file)
  (format file "~D tokens, ~D non-terminals, ~D productions, ~D states.~2%"
	  (grammar-next-token-id grammar)
	  (length (grammar-nonterminals grammar))
	  (grammar-num-productions grammar)
	  (grammar-num-states grammar))
  
  (format file "ID Token Type~%")
  (dolist (token (grammar-tokens grammar))
    (format file "~D ~A ~A~%"
	    (token-id token) (token-name token) (token-type token)))
  (terpri file)


  (dolist (state (grammar-all-states grammar))
    (format file "State ~D:~%" (state-number state))
    (print-state state "    " "" file)
    (terpri file)
    (let ((actions nil))
      (dolist (action (state-actions state))
	(let ((entry (assoc (cdr action) actions :test #'equal)))
	  (if entry
	      (push (car action) (cdr entry))
	      (push (list (cdr action) (car action)) actions))))
      (dolist (entry actions)
	(format file "  on:")
	(dolist (token (sort (cdr entry) #'< :key #'token-id))
	  (format file " ~A" token))
	(let ((action (car entry)))
	  (ecase (first action)
	    (:accept (format file "~%   accept~%"))
	    (:shift
	     (format file "~%   shift to:~%")
	     (print-state (second action) "     " "" file))
	    (:reduce
	     (format file "~%   reduce by:~%     ~A~%" (second action)))))
	(terpri file))
      (terpri file))))



;;;; Source file groveler.

(defun grovel-header (ifile ofile)
  (loop
    (let ((line (read-line ifile)))
      (when (string= line "%%")
	(return))
      (write-line line ofile))))

(defun parse-grammar (grammar ifile)
  (loop
    (let ((lhs (read ifile)))
      (when (eq lhs '%%)
	(return))
      (case lhs
	(:entry-point
	 (let* ((ep (read ifile))
		(prime (intern (concatenate 'string (symbol-name ep)
					    "-PRIME"))))
	   (push ep (grammar-entry-points grammar))
	   (push (parse-production grammar prime (list ep) nil)
		 (grammar-start-productions grammar))))
	(:token
	 (let ((name (read ifile))
	       (type (read ifile)))
	   (let ((old (gethash name (grammar-symbols grammar))))
	     (typecase old
	       (token
		(error "token ~S multiply defined." name))
	       (token-union
		(error "~S previously defined to be a token-union" name))
	       (nonterminal
		(if (nonterminal-productions old)
		    (error "~S was previously defined to be a non-terminal"
			   name)
		    (error "~S was previously assumed to be a non-terminal"
			   name)))))
	   (let* ((id (grammar-next-token-id grammar))
		  (new (make-token name type id)))
	     (push new (grammar-tokens grammar))
	     (setf (grammar-next-token-id grammar) (1+ id))
	     (setf (gethash name (grammar-symbols grammar)) new))))
	(:union
	 (let ((name (read ifile))
	       (type (read ifile))
	       (members (read ifile)))
	   (let ((old (gethash name (grammar-symbols grammar))))
	     (typecase old
	       (token
		(error "~S previously defined to be an atomic token" name))
	       (token-union
		(error "token-union ~S multiply defined" name))
	       (nonterminal
		(if (nonterminal-productions old)
		    (error "~S was previously defined to be a non-terminal"
			   name)
		    (error "~S was previously assumed to be a non-terminal"
			   name)))))
	   (let ((new (make-token-union name type members)))
	     (push new (grammar-token-unions grammar))
	     (setf (gethash name (grammar-symbols grammar)) new))))
	(:type
	 (let ((name (read ifile))
	       (type (read ifile)))
	   (let ((nonterminal (find-grammar-symbol grammar name)))

	     (unless (typep nonterminal 'nonterminal)
	       (error "~S is a terminal, change change its type now." name))
	     (when (nonterminal-type nonterminal)
	       (warn "Multiple types defined for nonterminal ~S" name))
	     (setf (nonterminal-type nonterminal) type))))
	(t
	 (let ((rhs (read ifile)))
	   (collect ((body))
	     (loop
	       (let ((line (read-line ifile)))
		 (when (string= line "%")
		   (return))
		 (body (expand-percents-and-ats line))))
	     (parse-production grammar lhs rhs (body))))))))
  (let ((undefined (remove nil (grammar-nonterminals grammar)
			   :key #'nonterminal-productions :test-not #'eq)))
    (when undefined
      (error "Undefined nonterminals:~{ ~S~}"
	     (mapcar #'nonterminal-name undefined))))
  (setf (grammar-entry-points grammar)
	(nreverse (grammar-entry-points grammar)))
  (setf (grammar-tokens grammar)
	(nreverse (grammar-tokens grammar)))
  (setf (grammar-start-productions grammar)
	(nreverse (grammar-start-productions grammar)))
  (values))

(defun expand-percents-and-ats (line)
  (declare (type simple-string line))
  (let ((end (length line))
	(escaped nil)
	(in-string nil))
    (declare (type fixnum end)
	     (type (member t nil) escaped in-string))
    (do ((index 0 (1+ index)))
	((= index end))
      (declare (type fixnum index))
      (let ((char (schar line index)))
	(cond ((eql char #\%)
	       (unless in-string
		 (setf line
		       (concatenate 'string
				    (subseq line 0 index)
				    "rhs-"
				    (subseq line (1+ index))))
		 (setf end (length line)))
	       (setf escaped nil))
	      ((eql char #\@)
	       (unless in-string
		 (setf line
		       (concatenate 'string
				    (subseq line 0 index)
				    "srcloc-"
				    (subseq line (1+ index))))
		 (setf end (length line)))
	       (setf escaped nil))
	      ((eql char #\\)
	       (setf escaped (not escaped)))
	      ((eql char #\")
	       (unless escaped
		 (setf in-string (not in-string)))
	       (setf escaped nil))))))
  line)

(defun grovel-trailer (ifile ofile)
  (loop
    (let ((line (read-line ifile nil)))
      (unless line
	(return))
      (write-line line ofile))))

(defun grovel-file (iname &optional oname logname)
  (let ((*conflicts* 0))
    (with-open-file (ifile iname)
      (with-open-stream (ofile (if oname
				   (open oname
					 :direction :output
					 :if-exists :supersede
					 :if-does-not-exist :create)
				   (make-broadcast-stream *standard-output*)))
	(grovel-header ifile ofile)
	(let ((grammar (make-grammar)))
	  (parse-grammar grammar ifile)
	  (compute-firsts grammar)
	  (compute-states grammar)
	  (compute-initial-lookaheads grammar)
	  (propagate-lookaheads grammar)
	  (compute-actions grammar)
	  (emit-parser grammar ofile)
	  (when logname
	    (with-open-stream (logfile (open logname
					     :direction :output
					     :if-exists :supersede
					     :if-does-not-exist :create))
	      (emit-log-file grammar logfile))))
	(grovel-trailer ifile ofile)))
    (unless (zerop *conflicts*)
      (warn "~D conflicts." *conflicts*))))

