rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/macros.dylan,v 1.22 2003/06/11 18:17:20 housel Exp $
copyright: see below
module: dylan-viscera



//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

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
	     = method (condition :: ?type, next-handler :: <function>)
		 do-handler(method () ?body end);
	       end;
	    ... }
    { ... exception (?:name :: ?type:expression, #rest ?options:expression)
	    ?:body }
      => { let handler (?type, ?options)
	     = method (?name :: ?type, next-handler :: <function>)
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
 => make-anonymous-method({ ?parameter-list }, { ?results }, { ?body }, { })
    
    { method ( ?:parameter-list ) => (?results:variable-list) ?:body end }
 => make-anonymous-method({ ?parameter-list }, { ?results }, { ?body }, { })

    { method ( ?:parameter-list ) => ?result:variable ; ?:body end }
 => make-anonymous-method({ ?parameter-list }, { ?result }, { ?body }, { })

    { method ( ?:parameter-list ) ; ?:body end }
 => make-anonymous-method({ ?parameter-list }, { #rest res }, { ?body }, { })
 
    { method ( ?:parameter-list ) ?:body end }
 => make-anonymous-method({ ?parameter-list }, { #rest res }, { ?body }, { })
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
    { } => { select-error(target) }
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
      => { for-clause(?v in ?c using forward-iteration-protocol) ... }
    { ?v:variable in ?c:expression using ?p:expression, ... }
      => { for-clause(?v in ?c using ?p) ... }
    { ?v:variable keyed-by ?k:variable in ?c:expression, ... }
      => { for-clause(?v keyed-by ?k in ?c using forward-iteration-protocol) 
           ... }
    { ?v:variable keyed-by ?k:variable in ?c:expression using ?p:expression, 
      ... }
      => { for-clause(?v keyed-by ?k in ?c using ?p) ... }

    { ?v:variable = ?e1:expression then ?e2:expression, ... }
      => { for-clause(?v = ?e1 then ?e2) ... }
    { ?v:variable = ?e:expression, ... }
      => { for-clause(?v = ?e) ... }
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

    // Constant or side-effect expression  clause
    { for-clause(?v:variable = ?e:expression) }
      => {, var2: ?v, next2: ?e}

    // Collection clauses without keyed-by
    { for-clause(?v:variable in ?c:expression using ?protocol:expression) }
      => {, init0: [ let collection = ?c;
		     let (initial-state, limit, next-state, finished-state?,
			  current-key, current-element)
		       = ?protocol(collection); ]
	  , var1: state, init1: initial-state
	  , next1: next-state(collection, state)
	  , stop1: finished-state?(collection, state, limit)
	  , var2: ?v, next2: current-element(collection, state) }

    // Collection clauses with keyed-by
    //
    // Because the compiler can't always optimize away extraneous
    // calls to current-key (how does it know the function has no side
    // effects?), we have a separate macro clause for for loops that
    // use keyed-by.
    { for-clause(?v:variable keyed-by ?k:variable in ?c:expression 
		   using ?protocol:expression) }
      => {, init0: [ let collection = ?c;
		     let (initial-state, limit, next-state, finished-state?,
			  current-key, current-element)
		       = ?protocol(collection); ]
	  , var1: state, init1: initial-state
	  , next1: next-state(collection, state)
	  , stop1: finished-state?(collection, state, limit)
	  , var2: ?v, next2: current-element(collection, state)
	  , var3: ?k, next3: current-key(collection, state) }

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
		    ??stop2:expression = #f,
		    ??var3:variable = ignore, ??next3:expression = #f;
                    // no stop3, though
      end }
      => { ??init0 ...
	   local method repeat (??var1, ...)
		   block (return)
		     unless (??stop1 | ...)
		       let (??var2, ...) = values(??next2, ...);
		       let (??var3, ...) = values(??next3, ...);
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

define macro %%primitive
    { %%primitive(?:name, ?args) }
      => make-primitive({ ?name }, { ?args } );
  args:
    { } => { }
    { ?:expression, ... } => { ?expression, ... }
end macro;


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

    { keyword ?key:token ?equals:token ?:expression, #rest ?init-arg-options }
      => make-init-arg({?key}, {init-expr: ?expression, ?init-arg-options})

  inherited-options:
    { #rest ?all:*, 
      #key ?init-value:expression = #f,
	   ?init-function:expression = #f }
      => {?all}

  slot-adjectives:
    { } => { }
    { constant ... } => { setter: #f, ... }
    { sealed ... } => { sealed: #t, ... }
    { instance ... } => { allocation: instance, ... }
    { class ... } => { allocation: class, ... }
    { each-subclass ... } => { allocation: each-subclass, ... }
    { virtual ... } => { allocation: virtual, ... }

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
    { define constant ?:variable = ?:expression }
      => { define-constant ( ?variable; dummy = ?expression ) }
    { define constant ( ?:variable-list ) ?eq:token ?:expression }
      => { define-constant ( ?variable-list; dummy ?eq ?expression ) }
end macro;

define macro define-constant
    { define-constant ( ?:variable-list; ?dummy:name = ?:expression ) }
    => make-define-constant({ ?variable-list }, { ?expression })
end macro;

define macro domain-definer
    { define sealed domain ?:name (?types ) }
      => make-define-sealed-domain({ ?name }, { ?types })
  types:
    { ?type:expression, ... } => { ?type, ...}
    { } => { }
end;

define macro function-definer

  // Result list followed by semicolon
  { define ?adjectives function ?:name ( ?:parameter-list )
     => (?results:variable-list) ; ?:body end }

    => make-define-constant
    ({ ?name },
     make-anonymous-method
       ({ ?parameter-list }, { ?results }, { ?body }, { ?adjectives }))


    // Result list, no semicolon
    { define ?adjectives function ?:name ( ?:parameter-list )
       => (?results:variable-list) ?:body end }

    => make-define-constant
    ({ ?name },
     make-anonymous-method
       ({ ?parameter-list }, { ?results }, { ?body }, {?adjectives}))


    // single result followed by semicolon
    { define ?adjectives function ?:name ( ?:parameter-list )
       => ?result:variable ; ?:body end }

    => make-define-constant
    ({ ?name },
     make-anonymous-method
       ({ ?parameter-list }, { ?result }, { ?body }, {?adjectives}))


    // no result, semicolon
    { define ?adjectives function ?:name ( ?:parameter-list ) ; ?:body end }
    
    => make-define-constant
    ({ ?name },
     make-anonymous-method
       ({ ?parameter-list }, { #rest res }, { ?body }, {?adjectives}))


    // no result, no semicolon
    { define ?adjectives function ?:name ( ?:parameter-list ) ?:body end }

    => make-define-constant
    ({ ?name },
     make-anonymous-method
       ({ ?parameter-list }, { #rest res }, { ?body }, {?adjectives}))

  adjectives:
    { } => { }
    { not-inline ... } => { inline-type: not-inline, ... }
    { default-inline ... } => { inline-type: default-inline, ... }
    { may-inline ... } => { inline-type: may-inline, ... }
    { inline ... } => { inline: #t, inline-type: inline, ... }
    { inline-only ... } => { inline-type: inline-only, ... }
    { movable ... } => { movable: #t, ... }
    { flushable ... } => { flushable: #t, ... }

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
    { sideways ... } => { ... }

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
    { open ... } => { sealed: #f, ... }
    { not-inline ... } => { inline-type: not-inline, ... }
    { default-inline ... } => { inline-type: default-inline, ... }
    { may-inline ... } => { inline-type: may-inline, ... }
    { inline ... } => { inline: #t, inline-type: inline, ... }
    { inline-only ... } => { inline-type: inline-only, ... }
    { movable ... } => { movable: #t, ... }
    { flushable ... } => { flushable: #t, ... }
    { sideways ... } => { ... }
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
    { define ?adjectives variable ?:variable = ?:expression }
      => { define-variable ( ?variable; dummy = ?expression ) }
    { define ?adjectives variable ( ?:variable-list ) ?eq:token ?:expression }
      => { define-variable ( ?variable-list; dummy ?eq ?expression ) }

  adjectives:
    { } => { }
    { thread } => { }
end macro;

define macro define-variable
    { define-variable ( ?:variable-list; ?dummy:name = ?:expression ) }
    => make-define-variable({ ?variable-list }, { ?expression })
end macro;



// Call-out related macros.

define macro call-out
    { call-out (?what:expression, ?result:expression, ?args) }
      => { %%primitive(\call-out, ?what, ?result, ?args) }
  args:
    { } => { }
    { ?arg:expression, ... } => { ?arg, ... }
    { ?key:token ?arg:expression, ... } => { ?key, ?arg, ... }
end;

define macro c-include
    { c-include (?file:expression) } => { %%primitive(\c-include, ?file) }
end;

define macro c-system-include
    { c-system-include (?file:expression) } => { %%primitive(\c-system-include, ?file) }
end;

define macro c-decl
    { c-decl (?decl:expression) } => { %%primitive(\c-decl, ?decl) }
end;

define macro c-local-decl
    { c-local-decl (?decl:expression) } => { %%primitive(\c-local-decl, ?decl) }
end;

define macro c-expr
    { c-expr (?result-type:expression, ?:expression) }
      => { %%primitive(\c-expr, ?result-type, ?expression) }
    { c-expr (?result-type:token ?:expression) }
      => { %%primitive(\c-expr, ?result-type, ?expression) }
end;

define macro c-literal
    { c-literal (?lit:expression) }
      => { %%primitive(\c-literal, #"int", ?lit) }
end;

define macro c-struct-field
    { c-struct-field (?kind:expression, ?ptr:expression,
                      ?struct:expression, ?field:expression) }
      => { %%primitive(\c-struct-field, ?kind, ?ptr, ?struct, ?field) }
end;

define macro c-struct-field-setter
    { c-struct-field-setter (?new-val:expression, ?kind:expression, 
                             ?ptr:expression, ?struct:expression,
                             ?field:expression) }
      => { %%primitive(\c-struct-field-setter, ?new-val, ?kind, ?ptr,
		       ?struct, ?field) }
end;

// Callback-related macros

define macro callback-method
    { callback-method (?:parameter-list) => ( ?result:variable ); ?:body end }
      => make-callback-method({ ?parameter-list }, { ?result }, { ?body })
    { callback-method (?:parameter-list) => ( ); ?:body end }
      => make-callback-method({ ?parameter-list }, { }, { ?body })
end;
