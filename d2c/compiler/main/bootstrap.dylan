module: dylan-viscera
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/Attic/bootstrap.dylan,v 1.1 1998/05/03 19:55:33 andreas Exp $
copyright: Copyright (c) 1994, 1995, 1996  Carnegie Mellon University
	   All rights reserved.



//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// Primitive macro

define macro %%primitive
    { %%primitive(?:name, ?args) }
      => make-primitive({ ?name }, { ?args } );
  args:
    { } => { }
    { ?:expression, ... } => { ?expression, ... }
end macro;

%%primitive(break);


// Simple statement macros

define macro begin
    { begin ?:body end } => { ?body };
end;

define macro block
    // First, pick off the simple cases.
    { block () ?:body end } => { ?body }
    { block (?:name) ?:body end } => make-bind-exit({ ?name }, { ?body })
    { block () ?:body cleanup ?cleanup:body end }
      => make-unwind-protect({ ?body }, { ?cleanup })
    { block () ?:body afterwards ?afterwards:body end }
      => { let (#rest results) = begin ?body end;
	   ?afterwards;
	   apply(values, results); }

    // Pick of the bind-exit & uwp combo.
    { block (?:name) ?:body cleanup ?cleanup:body end }
      => { block (?name) block () ?body cleanup ?cleanup end end }

    // Pick off the bind-exit & afterwards combo.
    { block (?:name) ?:body afterwards ?afterwards:body end }
      => { block (?name) block () ?body afterwards ?afterwards end end }

    // Pick off the afterwards & uwp combo.
    { block () ?:body afterwards ?afterwards:body cleanup ?cleanup:body end }
      => { block () 
	     block () ?body afterwards ?afterwards end
	   cleanup
	     ?cleanup
	   end }

    // Pick off the bind-exit, afterwards, & uwp combo.
    { block (?:name) ?:body afterwards ?after:body cleanup ?cleanup:body end }
      => { block (?name) 
	     block () ?body afterwards ?after cleanup ?cleanup end
	   end }

    // If we get here, they used exception.  So set up the exception handling
    // context and use ebody to expand the exception clauses into let handlers.
    { block () ?ebody end }
      => { block (done)
	     block (do-handler)
	       mv-call(done, begin ?ebody end);
	     end();
	   end }
    { block (?:name) ?ebody end }
      => { block (?name)
	     let done = ?name;
	     block (do-handler)
	       mv-call(done, begin ?ebody end);
	     end();
	   end }

  ebody:
    // Left recursive so that the first one is innermost.
    { ... exception (?type:expression, #rest ?options:expression)
	    ?:body }
      => { let handler (?type, ?options)
	     = method (condition, next-handler)
		 do-handler(method () ?body end);
	       end;
	    ... }
    { ... exception (?:name :: ?type:expression, #rest ?options:expression)
	    ?:body }
      => { let handler (?type, ?options)
	     = method (?name, next-handler)
		 do-handler(method () ?body end);
	       end;
	    ... }

    // Done with exception clauses.  Deal with cleanups and afterwards.
    { ?abody cleanup ?cleanup:body }
      => { block () ?abody cleanup ?cleanup end }
    { ?abody }
      => { ?abody }
  abody:
    { ?:body }
      => { ?body }
    { ?:body afterwards ?afterwards:body }
      => { block () ?body afterwards ?afterwards end }
end;

define macro case
    { case ?:case-body end } => { ?case-body }
  case-body:
    { ?x:expression => ; ... }
      => { begin let temp = ?x; if (temp) temp else ... end end }
    { ?x:expression => ?:body ; ... } => { if (?x) ?body else ... end }
    { otherwise ?:body } => { ?body }
    { } => { #f }
end;

define macro if
    { if (?x:expression) ?:body ?elses end }
      => make-if({ ?x }, { ?body }, { ?elses })
  elses:
    { else ?:body } => { ?body }
    { elseif (?x:expression) ?:body ... }
      => make-if({ ?x }, { ?body }, { ... })
    { } => { #f };
end;

define macro method
    { method ( ?:parameter-list ) => (?results:variable-list) ; ?:body end }
      => make-anonymous-method({ ?parameter-list }, { ?results }, { ?body })
    { method ( ?:parameter-list ) => (?results:variable-list) ?:body end }
      => make-anonymous-method({ ?parameter-list }, { ?results }, { ?body })
    { method ( ?:parameter-list ) => ?result:variable ; ?:body end }
      => make-anonymous-method({ ?parameter-list }, { ?result }, { ?body })
    { method ( ?:parameter-list ) ; ?:body end }
      => make-anonymous-method({ ?parameter-list }, { #rest res }, { ?body })
    { method ( ?:parameter-list ) ?:body end }
      => make-anonymous-method({ ?parameter-list }, { #rest res }, { ?body })
end;

define macro select
    { select (?what) ?:case-body end }
      => { ?what; ?case-body }
  what:
    { ?:expression by ?fn:expression }
      => { let target = ?expression; let compare :: <function> = ?fn }
    { ?:expression }
      => { let target = ?expression; let compare :: <function> = \== }
  case-body:
    { } => { error("select error") }
    { otherwise ?:body } => { ?body }
    { (?keys:*) => ?:body ; ... } => { if (?keys) ?body else ... end }
    { ?keys:* => ?:body ; ... } => { if (?keys) ?body else ... end }
  keys:
    { ?:expression } => { compare(target, ?expression) }
    { ?:expression , ... } => { compare(target, ?expression) | ... }
end;

define macro unless
    { unless (?:expression) ?:body end }
      => { if (?expression) else ?body end }
end;

define macro until
    { until (?:expression) ?:body end }
      => { begin
	     local method loop () 
		     unless (?expression)
		       ?body;
		       loop();
		     end;
		   end;
	     loop();
	   end }
end;

define macro while
    { while (?:expression) ?:body end }
      => { begin
	     local method loop () 
		     if (?expression)
		       ?body;
		       loop();
		     end;
		   end;
	     loop();
	   end }
end;


// For statement macro

define macro for
    { for (?header) ?fbody end }
      => { for-aux ?fbody, ?header end }

  fbody:
    { ?main:body } => { ?main, #f }
    { ?main:body finally ?val:body } => { ?main, ?val }

  header:
    { ?v:variable in ?c:expression, ... }
      => { for-clause(?v in ?c) ... }
    { ?v:variable = ?e1:expression then ?e2:expression, ... }
      => { for-clause(?v = ?e1 then ?e2) ... }
    { ?v:variable from ?e1:expression ?to, ... }
      => { for-clause(?v from ?e1 ?to) ... }
    { #key ?while:expression }
      => { for-clause(~?while stop) }
    { #key ?until:expression }
      => { for-clause(?until stop) }
    { } => { }

  to:
    { to ?limit:expression by ?step:expression } => { hard ?limit by ?step }
    { to ?limit:expression } => { easy ?limit by 1 test > }
    { above ?limit:expression ?by } => { easy ?limit by ?by test <= }
    { below ?limit:expression ?by } => { easy ?limit by ?by test >= }
    { ?by } => { loop ?by }

  by:
    { by ?step:expression } => { ?step }
    { } => { 1 }
end;

define macro for-clause

    // while: and until: clauses
    { for-clause(?e:expression stop) }
      => {, stop2: ?e }

    // Explicit step clauses
    { for-clause(?v:variable = ?e1:expression then ?e2:expression) }
      => {, var1: ?v, init1: ?e1, next1: ?e2 }

    // Collection clauses
    { for-clause(?v:variable in ?c:expression) }
      => {, init0: [ let collection = ?c;
		     let (initial-state, limit, next-state, finished-state?,
			  current-key, current-element)
		       = forward-iteration-protocol(collection); ]
	  , var1: state, init1: initial-state
	  , next1: next-state(collection, state)
	  , stop1: finished-state?(collection, state, limit)
	  , var2: ?v, next2: current-element(collection, state) }

    // Numeric clauses (three different variants depending on to/by combos)
    { for-clause(?v:name :: ?t:expression from ?e1:expression
		 loop ?by:expression) }
      => {, init0: [ let init = ?e1; let by = ?by; ]
	  , var1: ?v :: ?t, init1: init, next1: ?v + by }

    { for-clause(?v:name :: ?t:expression from ?e1:expression
		 easy ?limit:expression by ?by:expression test ?test:token) }
      => {, init0: [ let init = ?e1; let limit = ?limit; let by = ?by; ]
	  , var1: ?v :: ?t, init1: init, next1: ?v + by
	  , stop1: ?v ?test limit }

    { for-clause(?v:name :: ?t:expression from ?e1:expression
		 hard ?limit:expression by ?by:expression) }
      => {, init0: [ let init = ?e1; let limit = ?limit; let by = ?by; ]
	  , var1: ?v :: ?t, init1: init, next1: ?v + by
	  , stop1: if (by >= 0) ?v > limit else ?v < limit end }
end;

define macro for-aux
    { for-aux ?main:expression, ?value:expression, ?clauses:* end }
      => { for-aux2 ?main, ?value ?clauses end }

  clauses:
    { ?clause:macro ... } => { ?clause ... }
    { } => { }
end;


define macro for-aux2
    { for-aux2 ?main:expression, ?value:expression,
	       #key ??init0:*, ??var1:variable, ??init1:expression,
		    ??next1:expression, ??stop1:expression = #f,
		    ??var2:variable = ignore, ??next2:expression = #f,
		    ??stop2:expression = #f;
      end }
      => { ??init0 ...
	   local method repeat (??var1, ...)
		   block (return)
		     unless (??stop1 | ...)
		       let (??var2, ...) = values(??next2, ...);
		       unless (??stop2 | ...)
			 ?main;
			 mv-call(return, repeat(??next1, ...));
		       end;
		     end;
		     ?value;
		   end;
		 end;
	   repeat(??init1, ...) }

  init0:
    { [ ?stuff:* ] } => { ?stuff }
end;


// Function macros.

define macro \&
    { \& (?left:expression, ?right:expression) }
      => { if (?left) ?right end }
end;

define macro \|
    { \| (?left:expression, ?right:expression) }
      => { let temp = ?left; if (temp) temp else ?right end }
end;

define macro \:=
    { \:= (?place:expression, ?value:expression) }
      => make-assignment({ ?place }, { ?value })
end;

define macro mv-call
    { mv-call(?func:expression) }
      => { ?func() }
    { mv-call(?func:expression, ?arg-expr:expression) }
      => { %%primitive(\mv-call, ?func, ?arg-expr) }
    { mv-call(?func:expression, ?exprs) } 
      => { %%primitive(\mv-call, ?func, %%primitive(merge-clusters, ?exprs)) }
  exprs:
    { } => { }
    { ?:expression, ... } => { ?expression, ... }
end;


// Definition macros.


define macro class-definer
    { define ?class-adjectives class ?:name (?supers) ?slots end }
      => make-define-class({?name}, {?supers}, {?slots}, {?class-adjectives})

  class-adjectives:
    { } => { }
    { abstract ... } => { abstract: #t, ... }
    { concrete ... } => { abstract: #f, ... }
    { primary ... } => { primary: #t, ... }
    { free ... } => { primary: #f, ... }
    { sealed ... } => { sealed: #t, ... }
    { open ... } => { sealed: #f, ... }
    { functional ... } => { functional: #t, ... }

  supers:
    { } => { }
    { ?:expression, ... } => { ?expression, ... }

  slots:
    { } => { }
    { ?slot; ... } => { ?slot, ... }

  slot:
    { inherited slot ?:name, #rest ?inherited-options }
      => make-inherited-slot({?name}, {?inherited-options})
    { inherited slot ?:name = ?:expression, #rest ?inherited-options }
      => make-inherited-slot({?name},
			     {init-expr: ?expression, ?inherited-options})

    { slot ?:name, #rest ?slot-options}
      => make-slot({?name}, {?slot-options})
    { ?slot-adjectives slot ?:name, #rest ?slot-options}
      => make-slot({?name}, {?slot-adjectives, ?slot-options})
    { ?slot-adjectives slot ?:name :: ?type:expression, #rest ?slot-options}
      => make-slot({?name}, {type: ?type, ?slot-adjectives, ?slot-options})
    { ?slot-adjectives slot ?:name :: ?:expression, #rest ?slot-options }
      => make-slot({?name},
		   {init-expr: ?expression, ?slot-adjectives, ?slot-options})
    { ?slot-adjectives slot ?:name :: ?type:expression = ?:expression,
	#rest ?slot-options }
      => make-slot({?name},
		   {type: ?type, init-expr: ?expression,
		    ?slot-adjectives, ?slot-options})

    { keyword ?key:token, #rest ?init-arg-options }
      => make-init-arg({?key}, {?init-arg-options})
    { required keyword ?key:token, #rest ?init-arg-options }
      => make-init-arg({?key}, {required: #t, ?init-arg-options})

  inherited-options:
    { #rest ?all:*, 
      #key ?init-value:expression = #f,
	   ?init-function:expression = #f }
      => {?all}

  slot-adjectives:
    { } => { }
    { constant ... } => { setter: #f, ... }
    { sealed ... } => { sealed: #t, ... }
    { instance } => { allocation: instance }
    { class } => { allocation: class }
    { each-subclass } => { allocation: each-subclass }
    { virtual } => { allocation: virtual }

  slot-options:
    { #rest ?all:*,
      #key ?setter:expression = #f,
	   ?init-keyword:token = foo:,
	   ?required-init-keyword:token = foo:,
	   ?init-value:expression = #f,
	   ?init-function:expression = #f,
	   ?type:expression = #f,
	   ?sizer:expression = #f,
	   ?size-init-keyword:token = foo:,
	   ?required-size-init-keyword:token = foo:,
	   ?size-init-value:expression = #f,
	   ?size-init-function:expression = #f }
      => {?all}

  init-arg-options:
    { #rest ?all:*,
      #key ?init-value:expression = #f,
	   ?init-function:expression = #f,
	   ?type:expression = #f }
      => {?all}

end;

define macro constant-definer
    { define constant ?vars = ?:expression }
      => make-define-constant({ ?vars }, { ?expression })
  vars:
    { ?:variable } => { ?variable }
    { (?:variable-list) } => { ?variable-list }
end;

define macro domain-definer
    { define sealed domain ?:name (?types ) }
      => make-define-sealed-domain({ ?name }, { ?types })
  types:
    { ?type:expression, ... } => { ?type, ...}
    { } => { }
end;

define macro generic-definer
    { define ?adjectives:* generic ?:name ( ?:parameter-list ) ?rest }
      => { define-generic(?name; ?parameter-list; ?rest ?adjectives) }

  adjectives:
    { } => { }
    { sealed ... } => { sealed: #t, ... }
    { open ... } => { sealed: #f, ... }
    { movable ... } => { movable: #t, ... }
    { flushable ... } => { flushable: #t, ... }

  rest:
    { => ?:variable, #key } => { ?variable; }
    { => (?:variable-list), #key } => { ?variable-list; }
    { #key } => { #rest results; }
end;

define macro define-generic
    { define-generic(?:name; ?params:parameter-list; ?results:variable-list;
		     #rest ?options:*,
		     #key ?sealed:expression = #t,
		          ?movable:expression = #f,
		          ?flushable:expression = #f) }
      => make-define-generic({ ?name }, { ?params }, { ?results}, { ?options })
end;

define macro method-definer
    { define ?adjectives:* method ?:name ?rest:* end }
      => make-define-method({?name}, {method ?rest end}, {?adjectives})

  adjectives:
    { } => { }
    { sealed ... } => { sealed: #t, ... }
    { inline ... } => { inline: #t, ... }
    { movable ... } => { movable: #t, ... }
    { flushable ... } => { flushable: #t, ... }
end;

define macro library-definer
    { define library ?:name ?clauses end }
      => make-define-library({ ?name }, { ?clauses })

  clauses:
    { } => { }
    { ?clause; ... } => { ?clause, ... }

  clause:
    {use ?:name, #key ?import = all, ?exclude = {}, ?prefix:token = "", 
		      ?rename = {}, ?export = {} }
      => make-use-clause({ ?name }, { ?import }, { ?exclude }, { ?prefix },
			 { ?rename }, { ?export })
    {export ?names }
      => make-export-clause({ ?names })

  names:
    { } => { }
    { ?:name, ... } => { ?name, ... }

  import:
    { all } => { #t }
    { { ?variable-specs } } => { ?variable-specs }

  variable-specs:
    { } => { }
    { ?:name, ... } => { ?name, ... }
    { ?renaming, ... } => { ?renaming, ... }

  exclude:
    { { ?names } } => { ?names }

  rename:
    { { ?renamings } } => { ?renamings }

  renamings:
    { } => { }
    { ?renaming, ... } => { ?renaming, ... }

  renaming:
    { ?from:name => ?to:name } => make-renaming({ ?from }, { ?to })

  export:
    { all } => { #t }
    { { ?names } } => { ?names }

end;

define macro variable-definer
    { define variable ?vars = ?:expression }
      => make-define-variable({ ?vars }, { ?expression })
  vars:
    { ?:variable } => { ?variable }
    { (?:variable-list) } => { ?variable-list }
end;


// Classes that need to be pre-defined.

define primary abstract open class <object> ()
  //
  // The class of the instance.  Non-abstract classes automatically override
  // the init-value to be the correct class.
  constant slot %object-class :: <class> = <object>;
end;

define abstract class <boolean> (<object>) end;
define class <true> (<boolean>) end;
define class <false> (<boolean>) end;

define abstract class <function> (<object>)
  slot general-entry :: <raw-pointer>,
    required-init-keyword: general-entry:;
end;

define abstract class <closure> (<function>)
  slot closure-var :: <object>,
    init-value: #f,
    sizer: closure-size, size-init-value: 0, size-init-keyword: closure-size:;
end;
define sealed domain initialize (<closure>);

define class <raw-function> (<function>)
end;

define class <raw-closure> (<raw-function>, <closure>)
end;
define sealed domain make (singleton(<raw-closure>));

define class <method> (<function>)
  slot generic-entry :: <raw-pointer>,
    required-init-keyword: generic-entry:;
end;

define class <accessor-method> (<method>)
  slot accessor-slot :: <slot-descriptor>,
    required-init-keyword: slot:;
end class <accessor-method>;

define class <method-closure> (<method>, <closure>)
end;
define sealed domain make (singleton(<method-closure>));

define class <generic-function> (<function>)
end;

define class <symbol> (<object>)
  slot symbol-string :: <string>, setter: #f, required-init-keyword: string:;
end;

define class <type> (<object>) end;
define class <union> (<type>) end;
define class <direct-instance> (<type>) end;
define class <subclass> (<type>) end;
define class <singleton> (<type>) end;
define class <limited-integer> (<type>) end;
define class <byte-character-type> (<type>) end;
define class <class> (<type>)
  slot unique-id :: <integer>, required-init-keyword: unique-id:;
  slot class-functional? :: <boolean>, init-value: #f;
end;
define class <slot-descriptor> (<object>) end;

define abstract open class <number> (<object>) end;
define abstract open class <complex> (<number>) end;
define abstract class <real> (<complex>) end;
define abstract class <rational> (<real>) end;
define abstract class <general-integer> (<rational>) end;

define functional class <integer> (<general-integer>)
  sealed slot value :: <integer>, init-value: 0;
end;

define class <extended-integer> (<general-integer>) end;

define abstract class <float> (<real>) end;

define functional class <single-float> (<float>)
  sealed slot value :: <single-float>, init-value: 0.0s0;
end;

define functional class <double-float> (<float>)
  sealed slot value :: <double-float>, init-value: 0.0d0;
end;

define functional class <extended-float> (<float>)
  sealed slot value :: <extended-float>, init-value: 0.0x0;
end;

define abstract open class <collection> (<object>) end;
define abstract open class <explicit-key-collection> (<collection>) end;
define abstract open class <stretchy-collection> (<collection>) end;
define abstract open class <mutable-collection> (<collection>) end;
define abstract open class <sequence> (<collection>) end;
define abstract open class <mutable-explicit-key-collection>
    (<mutable-collection>, <explicit-key-collection>)
end;
define abstract open class <mutable-sequence>
    (<mutable-collection>, <sequence>)
end;
define abstract open class <array> (<mutable-sequence>) end;
define abstract open class <vector> (<array>) end;
define abstract open class <string> (<mutable-sequence>) end;
define class <simple-object-vector> (<vector>)
  sealed slot %element, init-value: #f, init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;
define class <unicode-string> (<vector>, <string>)
  sealed slot %element :: <character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;
define class <byte-string> (<vector>, <string>)
  sealed slot %element :: <byte-character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;
define abstract class <list> (<mutable-sequence>)
  sealed slot head, required-init-keyword: head:;
  sealed slot tail, required-init-keyword: tail:;
end;
define class <empty-list> (<list>) end;
define class <pair> (<list>) end;

define constant $max-char-code = #xffff;
define constant <char-code>
  = limited(<integer>, min: 0, max: $max-char-code);
define functional class <character> (<object>)
  sealed slot value :: <char-code>, required-init-keyword: code:;
end;

define constant <byte-character> = make(<byte-character-type>);

define class <value-cell> (<object>)
  sealed slot value, required-init-keyword: value:
end;

define functional class <raw-pointer> (<object>)
  sealed slot value :: <raw-pointer>, required-init-keyword: value:;
end;

define class <catcher> (<object>)
end;


// Functions that need to be defined.

define open generic find-slot-offset
    (class :: <class>, slot :: <slot-descriptor>) => res :: <integer>;

define open generic %make-method
    (specializers :: <simple-object-vector>,
     result-types :: <simple-object-vector>,
     rest-result-type :: <type>, entry :: <method>)
    => res :: <method>;
define open generic add-method (gf :: <generic-function>, meth :: <method>)
    => (new :: <method>, old :: type-union(<method>, <false>));
define open generic %instance? (value, type :: <type>) => res :: <boolean>;
define open generic subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
define open generic error (msg, #rest args) => res :: type-union();
define open generic make (class :: <class>, #all-keys) => thing;
define open generic initialize (instance, #all-keys);
define open generic \== (x, y) => res :: <boolean>;
define open generic slow-functional-== (x, y) => res :: <boolean>;
define open generic functional-== (class :: <class>, x, y) => res :: <boolean>;
define open generic \= (x, y) => res :: <boolean>;
define open generic \< (x, y) => res :: <boolean>;
define open generic \+ (x :: <number>, y :: <number>) => res :: <number>;
define open generic \- (x :: <number>, y :: <number>) => res :: <number>;
define open generic even? (x :: <general-integer>) => res :: <boolean>;
define open generic odd? (x :: <general-integer>) => res :: <boolean>;
define open generic %closure-ref
    (closure :: <method>, index :: <integer>) => res :: <object>;
define open generic %make-next-method-cookie
    (next-method-info :: <list>, original-args :: <simple-object-vector>)
    => res :: type-union(<false>, <function>);
define open generic as (class :: <class>, thing :: <object>)
    => result :: <object>;
define open generic element
    (collection :: <collection>, key, #key default) => element;
define open generic element-setter
    (new-value, collection :: <collection>, key);

define open generic make-catcher (saved-state :: <raw-pointer>)
    => res :: <catcher>;
define open generic disable-catcher (catcher :: <catcher>) => ();
define open generic throw
    (catcher :: <catcher>, values :: <simple-object-vector>)
    => res :: type-union();
define open generic make-exit-function (catcher :: <catcher>)
    => res :: <function>;

define open generic push-unwind-protect (cleanup-function :: <function>) => ();
define open generic pop-unwind-protect () => ();

define open generic push-handler
    (type :: <type>, func :: <function>,
     #key test :: false-or(<function>), init-arguments :: <sequence> = #())
    => ();
define open generic pop-handler () => ();

define open generic value (x) => value :: <object>;
define open generic value-setter (x, y) => value;

define open generic verify-keywords
    (keyword-value-arguments :: <simple-object-vector>,
     valid-keywords :: type-union(singleton(#"all"), <simple-object-vector>))
    => ();


// Internal errors.

define method uninitialized-slot-error
    (slot :: <slot-description>, instance :: <object>)
    => res :: type-union();
  error("Accessing uninitialized slot %= in instance %=", slot, instance);
end;

define method missing-required-init-keyword-error
    (keyword :: <symbol>, class :: <class>) => res :: type-union();
  error("Missing required-init-keyword %= in make of %=", keyword, class);
end;

define method wrong-number-of-arguments-error
    (fixed? :: <boolean>, wanted :: <integer>, got :: <integer>)
    => res :: type-union();
  error("Wrong number of arguments.  Wanted %s %d but got %d.",
	if (fixed?) "exactly" else "at least" end,
	wanted, got);
end;

define method odd-number-of-keyword/value-arguments-error ()
    => res :: type-union();
  error("Odd number of keyword/value arguments.");
end;

define method unrecognized-keyword-error (key :: <symbol>)
    => res :: type-union();
  error("Unrecognized keyword: %=.", key);
end;

define method no-applicable-methods-error
    (func :: <generic-function>, arguments :: <simple-object-vector>)
    => res :: type-union();
  error("No applicable methods in call of %= when given arguments:\n  %=",
	func, arguments);
end;

define method ambiguous-method-error (methods :: <list>)
    => res :: type-union();
  error("It is ambiguous which of these methods is most specific:\n  %s",
	methods);
end;

define method type-error (object :: <object>, type :: <type>)
    => res :: type-union();
  error("%= isn't of type %=", object, type);
end;


// Methods that are nice to have by default.

define method check-type 
    (object :: <object>, type :: <type>) => object :: <object>;
  %check-type(object, type);
end;

define inline method %check-type
    (object :: <object>, type :: <type>) => object :: <object>;
  if (instance?(object, type))
    object;
  else
    type-error(object, type);
  end;
end;

define method check-types
    (vector :: <simple-object-vector>, type :: <type>)
    => (checked :: <simple-object-vector>);
end;

define method instance?
    (object :: <object>, type :: <type>) => object :: <object>;
  %instance?(object, type);
end;

define method make (class :: <class>, #rest keys, #all-keys) => res;
  error("make not supported in the bootstrap.");
end;

define inline method initialize (object :: <object>, #rest keys, #all-keys)
    => ();
end;

define constant catch
  = method (saved-state :: <raw-pointer>, thunk :: <function>)
      thunk(saved-state);
    end;

define constant make-rest-arg
  = method (arg-ptr :: <raw-pointer>, count :: <integer>)
	=> res :: <simple-object-vector>;
      let res = make(<simple-object-vector>, size: count);
/*
      for (index :: <integer> from 0,
	   while: index < count)
	%element(res, index) := %%primitive(extract-arg, arg-ptr, index);
      end;
*/
      res;
    end;

define method make-closure
    (func :: <function>, closure-size :: <integer>)
    => res :: <raw-closure>;
  make(<raw-closure>,
       general-entry: func.general-entry,
       closure-size: closure-size);
end;

define method make-closure
    (func :: <method>, closure-size :: <integer>)
    => res :: <method-closure>;
  make(<method-closure>,
       general-entry: func.general-entry,
       generic-entry: func.generic-entry,
       closure-size: closure-size);
end;

define sealed inline method vector (#rest things)
    => res :: <simple-object-vector>;
  things;
end;

define sealed movable method list (#rest things) => res :: <list>;
  as(<list>, things);
end;

define inline method pair (head, tail) => res :: <pair>;
  make(<pair>, head: head, tail: tail);
end;

define sealed inline method \~ (thing :: <object>) => res :: <boolean>;
  %%primitive(not, thing);
end;

define sealed inline method \< (x :: <integer>, y :: <integer>)
    => res :: <boolean>;
  %%primitive(fixnum-<, x, y);
end;

define sealed inline method \== (x :: <integer>, y :: <integer>)
    => res :: <boolean>;
  %%primitive(fixnum-=, x, y);
end;

define sealed inline method \+ (x :: <integer>, y :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-+, x, y);
end;

define sealed inline method \* (x :: <integer>, y :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-*, x, y);
end;

define sealed inline method \- (x :: <integer>, y :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum--, x, y);
end;



// Extra dreck.

define generic values-sequence (sequence :: <sequence>);

define sealed domain values-sequence (<sequence>);

define inline method values-sequence (sequence :: <sequence>)
  values-sequence(as(<simple-object-vector>, sequence));
end;

define inline method values-sequence
    (vector :: <simple-object-vector>)
  %%primitive(values-sequence, vector);
end;


define generic values (#rest values);

define inline method values (#rest values)
  %%primitive(values-sequence, values);
end;


define generic apply (function :: <function>, #rest arguments);

define method apply (function :: <function>, #rest arguments)
  error("Apply wasn't inlined?");
end;


// Might as well.

%%primitive(magic-internal-primitives-placeholder);
