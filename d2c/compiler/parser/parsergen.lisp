;;; -*- Package: PARSER -*-
;;;
;;; **********************************************************************
;;; Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
;;; 
(ext:file-comment
  "$Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/parsergen.lisp,v 1.3 1996/02/05 01:17:31 wlott Exp $")
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
	    (:print-function %print-grammar))
  ;;
  ;; List of entry-point grammar symbols.
  (entry-points (required-argument) :type list)
  ;;
  ;; Hash table of all the grammar symbols.
  (symbols (make-hash-table :test #'eq) :type hash-table :read-only t)
  ;;
  ;; Next available terminal-id.
  (next-terminal-id 0 :type fixnum)
  ;;
  ;; List of all the terminals.
  (terminals nil :type list)
  ;;
  ;; List of all the non-terminals.
  (nonterminals nil :type list)
  ;;
  ;; List of the start productions.  One for each entry point.
  (start-productions nil :type list)
  ;;
  ;; List or start states, one for each entry point.
  (start-states nil :type list)
  ;;
  ;; Number of productions.
  (num-productions 0 :type (integer 0 *)))

(defun %print-grammar (grammar stream depth)
  (declare (ignore depth))
  (print-unreadable-object (grammar stream :type t)))


(defstruct (grammar-symbol
	    (:constructor nil))
  ;;
  ;; List of non-terminals that can start this grammar symbol.
  (first nil :type list)
  ;;
  ;; True iff there is some (foo -> epsilon) production for this grammar
  ;; symbol.
  (nullable nil :type (member t nil)))

(defstruct (terminal
	    (:include grammar-symbol)
	    (:constructor %make-terminal (kind id))
	    (:print-function %print-terminal))
  ;;
  ;; The kind of token this terminal corresponds to.
  (kind (required-argument) :type symbol)
  ;;
  ;; A unique integer corresponding to this terminal.
  (id (required-argument) :type fixnum))

(defun %print-terminal (terminal stream depth)
  (declare (ignore depth))
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (terminal stream :type t)
	(prin1 (terminal-kind terminal) stream))
      (prin1 (terminal-kind terminal) stream)))

(defun make-terminal (kind id)
  (let ((result (%make-terminal kind id)))
    (setf (terminal-first result) (list result))
    result))


(defstruct (nonterminal
	    (:include grammar-symbol)
	    (:constructor make-nonterminal (name))
	    (:print-function %print-nonterminal))
  ;;
  ;; The symbol name for this nonterminal.
  (name (required-argument) :type symbol)
  ;;
  ;; The type for this nonterminal, or nil if not specified.
  (type nil :type symbol)
  ;;
  ;; List of productions with this nonterminal on the left hand side.
  (productions nil :type list))

(defun %print-nonterminal (nonterminal stream depth)
  (declare (ignore depth))
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (nonterminal stream :type t)
	(prin1 (nonterminal-name nonterminal) stream))
      (prin1 (nonterminal-name nonterminal) stream)))

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
  (print-unreadable-object (production stream :type t)
    (format stream "~D: ~A ->~{ ~A~}"
	    (production-number production)
	    (production-left-side production)
	    (production-right-side production))))



(defstruct (item
	    (:constructor make-item (production dot-position))
	    (:print-function %print-item))
  ;;
  ;; The production this item is built from.
  (production (required-argument) :type production)
  ;;
  ;; The position of the dot in this item.
  (dot-position 0 :type (integer 0 *)))

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


(defstruct (state
	    (:constructor make-state (number kernels))
	    (:print-function %print-state))
  ;;
  ;; Small integer uniquly identifing this state.
  (number 0 :type (integer 0 *))
  ;;
  ;; List of kernel states.
  (kernels nil :type list)
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
      (if (char= (schar (symbol-name thing) 0) #\<)
	  (let* ((id (grammar-next-terminal-id grammar))
		 (new (make-terminal thing id)))
	    (push new (grammar-terminals grammar))
	    (setf (grammar-next-terminal-id grammar) (1+ id))
	    (setf (gethash thing (grammar-symbols grammar)) new)
	    new)
	  (let ((new (make-nonterminal thing)))
	    (push new (grammar-nonterminals grammar))
	    (setf (gethash thing (grammar-symbols grammar)) new)
	    new))))

(defun parse-production (grammar production)
  (destructuring-bind
      (left-side (&rest right-side) &rest body)
      production
    (let* ((nonterminal (find-grammar-symbol grammar left-side))
	   (num-productions (grammar-num-productions grammar))
	   (production
	    (make-production num-productions
			     nonterminal
			     (mapcar #'(lambda (thing)
					 (find-grammar-symbol grammar thing))
				     right-side)
			     body)))
      (setf (grammar-num-productions grammar) (1+ num-productions))
      (push production (nonterminal-productions nonterminal))
      production)))

(defun parse-productions (entry-points productions)
  (let ((grammar (make-grammar :entry-points entry-points)))
    (setf (grammar-start-productions grammar)
	  (mapcar #'(lambda (entry-point)
		      (parse-production
		       grammar
		       (list (intern (concatenate 'string
						  (symbol-name entry-point)
						  "-PRIME"))
			     (list entry-point))))
		  entry-points))
    (dolist (production productions)
      (parse-production grammar production))
    (let ((undefined (remove nil (grammar-nonterminals grammar)
			     :key #'nonterminal-productions :test-not #'eq)))
      (when undefined
	(error "Undefined nonterminals:~{ ~S~}"
	       (mapcar #'nonterminal-name undefined))))
    grammar))


;;;; compute firsts.

(defun compute-firsts (grammar)
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
	(return)))))


;;;; Compute-items

;;; MAP-ITEMS -- internal.
;;;
;;; Invoke function on each item in closure(item-set).
;;;
(declaim (inline map-items))
;;;
(defun map-items (function item-set)
  (let ((productions-added nil))
    (labels ((grovel (item)
	       (funcall function item)
	       (let* ((production (item-production item))
		      (dot-position (item-dot-position item))
		      (right-side (production-right-side production)))
		 (when (< dot-position (length right-side))
		   (let ((next-symbol (nth dot-position right-side)))
		     (when (nonterminal-p next-symbol)
		       (dolist (prod (nonterminal-productions next-symbol))
			 (unless (member prod productions-added)
			   (push prod productions-added)
			   (grovel (make-item prod 0))))))))))
      (dolist (item item-set)
	(grovel item)))))


(defmacro do-items ((var items) &body body)
  `(map-items #'(lambda (,var) ,@body) ,items))

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

(defun compute-states (grammar)
  (let ((results nil)
	(number 0))
    (labels ((add (item-set)
	       (or (find item-set results
			 :key #'state-kernels
			 :test #'(lambda (set1 set2)
				   (null (set-exclusive-or
					  set1 set2
					  :test #'item-equal))))
		   (let ((state (make-state number item-set))
			 (gotos nil))
		     (push state results)
		     (incf number)
		     (do-items (item item-set)
		       (let* ((production (item-production item))
			      (right-side (production-right-side production))
			      (dot-position (item-dot-position item)))
			 (when (< dot-position (length right-side))
			   (let* ((new-item
				   (make-kernel-item production
						     (1+ dot-position)))
				  (next-symbol (nth dot-position right-side))
				  (entry (assoc next-symbol gotos :test #'eq)))
			     (if entry
				 (push new-item (cdr entry))
				 (push (list next-symbol new-item) gotos))))))
		     (dolist (goto gotos)
		       (push (cons (car goto) (add (cdr goto)))
			     (state-gotos state)))
		     state))))
      (setf (grammar-start-states grammar)
	    (mapcar #'(lambda (start-production)
			(add (list (make-kernel-item start-production 0))))
		    (grammar-start-productions grammar))))
    results))


;;;; Compute lookaheads.

(defun compute-initial-lookaheads (grammar states)
  (declare (type grammar grammar) (type list states))
  (let ((eof-symbol (find-grammar-symbol grammar '<eof-token>)))
    (mapc #'(lambda (start-production start-state)
	      (push eof-symbol
		    (kernel-item-new-lookaheads
		     (find start-production
			   (state-kernels start-state)
			   :key #'kernel-item-production))))
	  (grammar-start-productions grammar)
	  (grammar-start-states grammar)))
  (dolist (state states)
    (dolist (kernel-item (state-kernels state))
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
				      (state-kernels goto))))
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
		 (grovel (make-item production 0) lookahead))))
	  (grovel kernel-item nil)))))
  (undefined-value))

(defun propagate-lookaheads (states)
  (declare (type list states))
  (loop
    (let ((anything-changed nil))
      (dolist (state states)
	(dolist (item (state-kernels state))
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

(defun add-action (state action)
  (unless (find action (state-actions state) :test #'equal)
    (let* ((terminal (car action))
	   (old-action (find terminal (state-actions state) :key #'car)))
      (when old-action
	(format t "~%~A/~A conflict at ~A on ~A:~% ~A~%with~% ~A~%"
		(second old-action) (second action) state terminal
		(cdr old-action) (cdr action))
	(incf *conflicts*)))
    (push action (state-actions state)))
  action)

(defun compute-actions (grammar states)
  (declare (type grammar grammar) (type list states))
  (dolist (state states)
    (dolist (kernel-item (state-kernels state))
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
		   (grovel (make-item production 0) lookahead)))))
	  (dolist (lookahead (kernel-item-lookaheads kernel-item))
	    (grovel kernel-item lookahead))))))
  (undefined-value))
  


;;;; Compute tables

(defun encode-actions (actions grammar)
  (let ((terminal-bits
	 (integer-length (1- (grammar-next-terminal-id grammar)))))
    (flet ((encode-action (action)
	     (multiple-value-bind
		 (action-kind datum)
		 (ecase (second action)
		   (:accept (values 0 0))
		   (:reduce (values 1 (production-number (third action))))
		   (:shift (values 2 (state-number (third action)))))
	       (logior (ash (logior (ash datum terminal-bits)
				    (terminal-id (first action)))
			    2)
		       action-kind))))
      (coerce (sort (mapcar #'encode-action actions) #'<) 'vector))))

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

(defun compute-tables (grammar)
  (compute-firsts grammar)
  (let ((states (compute-states grammar)))
    (compute-initial-lookaheads grammar states)
    (propagate-lookaheads states)
    (compute-actions grammar states)
    (let* ((number-states (length states))
	   (state-name-table (make-array number-states :initial-element nil))
	   (action-table (make-array number-states :initial-element nil))
	   (gotos nil))
      (dolist (state states)
	(setf (aref state-name-table (state-number state))
	      (mapcar #'(lambda (item)
			  (let* ((production (item-production item))
				 (right-side
				  (production-right-side production))
				 (dot-position (item-dot-position item)))
			    (format nil "~A ->~{ ~A~} *~{ ~A~}"
				    (production-left-side production)
				    (subseq right-side 0 dot-position)
				    (subseq right-side dot-position))))
		      (state-kernels state)))
	(setf (aref action-table (state-number state))
	      (let ((action (encode-actions (state-actions state) grammar)))
		(or (find action action-table :test #'equalp)
		    action)))
	(setf gotos
	      (add-gotos gotos (state-number state) (state-gotos state))))
      (values action-table
	      (compact-gotos gotos)
	      state-name-table))))
      

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
    (format ofile "define method production-~D (prev-state :: <integer>"
	    (production-number production))
    (let ((index 0))
      (dolist (grammar-sym right-side)
	(let* ((type (etypecase grammar-sym
		       (terminal
			(terminal-kind grammar-sym))
		       (nonterminal
			(nonterminal-type grammar-sym)))))
	  (format ofile ", rhs-~D~@[ :: ~A~]" (incf index) type))))
    (format ofile ")~%")
    (format ofile "    => (new-state :: <integer>, new-symbol~@[ :: ~A~]);~%"
	    (nonterminal-type left-side))
    (format ofile "  // ~S ->~:[ epsilon~;~:*~{ ~S~}~]~%"
	    (nonterminal-name (production-left-side production))
	    (mapcar #'(lambda (sym)
			(etypecase sym
			  (terminal
			   (terminal-kind sym))
			  (nonterminal
			   (nonterminal-name sym))))
		    right-side))
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

(defun emit-parser (entry-points types productions
		    &optional (ofile *standard-output*))
  (let ((grammar (parse-productions entry-points productions))
	(*conflicts* 0))
    (dolist (type types)
      (let ((nonterminal (find-grammar-symbol grammar (car type))))
	(check-type nonterminal nonterminal)
	(setf (nonterminal-type nonterminal) (cdr type))))
    (multiple-value-bind
	(action gotos names)
	(compute-tables grammar)
      (let* ((num-terminals (grammar-next-terminal-id grammar))
	     (terminals (make-array num-terminals)))
	(dolist (terminal (grammar-terminals grammar))
	  (setf (aref terminals (terminal-id terminal)) terminal))
	(format ofile "define constant $action-bits = 2;~%")
	(format ofile "define constant $accept-action = 0;~%")
	(format ofile "define constant $reduce-action = 1;~%")
	(format ofile "define constant $shift-action = 2;~2%")
	(format ofile "define constant $terminal-id-bits = ~D;~%"
		(integer-length (1- num-terminals)))
	(format ofile
		"define constant $terminals-table :: <simple-object-vector>~
		 ~%  = vector(")
	(dotimes (i num-terminals)
	  (unless (zerop i)
	    (format ofile ", "))
	  (format ofile "~A" (terminal-kind (aref terminals i))))
	(format ofile ");~2%"))

      (format ofile
	      "define constant $action-table :: <simple-object-vector>~
	       ~%  = map-as(<simple-object-vector>,~
	       ~%           decode-action-table,~
	       ~%           #[")
      (dotimes (i (length action))
	(unless (zerop i)
	  (format ofile ",~2%             "))
	(format ofile "// Action ~D~%             " i)
	(dolist (name (svref names i))
	  (format ofile "//  ~A~%             " name))
	(dump-constant (svref action i) ofile))
      (format ofile "]);~2%")
      
      (let* ((num-productions (grammar-num-productions grammar))
	     (productions-vector
	      (make-array num-productions :initial-element nil))
	     (num-pops-vector
	      (make-array num-productions :initial-element 0)))
	(dolist (nonterminal (grammar-nonterminals grammar))
	  (let* ((name (nonterminal-name nonterminal))
		 (gotos (cdr (assoc name gotos))))
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
	    (grammar-start-states grammar)))
    (unless (zerop *conflicts*)
      (warn "~D conflicts." *conflicts*)))
  (values))



;;;; Source file groveler.

(defun grovel-header (ifile ofile)
  (loop
    (let ((line (read-line ifile)))
      (when (string= line "%%")
	(return))
      (write-line line ofile))))

(defun grovel-guts (ifile)
  (collect ((entry-points) (types) (productions))
    (loop
      (let ((lhs (read ifile)))
	(when (eq lhs '%%)
	  (return))
	(case lhs
	  (:entry-point
	   (entry-points (read ifile)))
	  (:type
	   (types (cons (read ifile) (read ifile))))
	  (t
	   (let ((rhs (read ifile)))
	     (collect ((body))
	       (loop
		 (let ((line (read-line ifile)))
		   (when (string= line "%")
		     (return))
		   (body (expand-precents line))))
	       (productions `(,lhs ,rhs ,@(body)))))))))
    (values
     (entry-points)
     (types)
     (productions))))

(defun expand-precents (line)
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

(defun grovel-file (iname oname)
  (with-open-file (ifile iname)
    (with-open-stream (ofile (if oname
				 (open oname
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
				 (make-broadcast-stream *standard-output*)))
      (grovel-header ifile ofile)
      (multiple-value-bind
	  (entry-points types productions)
	  (grovel-guts ifile)
	(emit-parser entry-points types productions ofile))
      (grovel-trailer ifile ofile))))
