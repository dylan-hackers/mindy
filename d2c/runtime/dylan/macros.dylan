rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/macros.dylan,v 1.6 1996/02/19 20:22:33 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


// Statement macros.

define macro begin
    { begin ?body end } => { %%begin ?body end };
end;

define macro block
    { block () ?body end } => { begin ?body end }
    { block (?name) ?body end } => { %%bind-exit (?name) ?body end }
    { block () ?body cleanup ?body:cleanup end }
      => { %%unwind-protect ?body %%cleanup ?cleanup end }
    { block (?name) ?body cleanup ?body:cleanup end }
      => { %%bind-exit (?name)
	     %%unwind-protect ?body %%cleanup ?cleanup end
	   end }
    { block () ?bbody end }
      => { %%bind-exit (done)
	     %%bind-exit (do-handler)
	       %%mv-call(done, begin ?bbody end);
	     end();
	   end }
    { block (?name) ?bbody end }
      => { %%bind-exit (done)
	     let ?name = done;
	     %%bind-exit(do-handler)
	       %%mv-call(done, begin ?bbody end);
	     end();
	   end }
  bbody:
    { ... cleanup ?body }
      => { %%unwind-protect
	     %%bind-exit (do-handler)
	       %%mv-call(done, begin ... end);
	     end();
	   %%cleanup
	     ?body
	   end }
    { ... exception ?name ?body }
      => { let handler ?name
	     = method (condition, next-handler)
		 do-handler(method () ?body end);
	       end;
	    ... }
    { ... exception (?expr, #rest ?opt, #key ?test (#f), ?init-arguments (#f))
	    ?body }
      => { let handler (?expr, ?opt)
	     = method (condition, next-handler)
		 do-handler(method () ?body end);
	       end;
	    ... }
    { ... exception (?name :: ?expr, #rest ?opt, 
		     #key ?test (#f), ?init-arguments (#f))
	    ?body }
      => { let handler (?expr, ?opt)
	     = method (?name, next-handler)
		 do-handler(method () ?body end);
	       end;
	    ... }
    { ?body } => { ?body };
end;

define macro case
    { case ?case-body end } => { ?case-body }
  case-body:
    { ?expr => ; ... }
      => { begin let temp = ?expr; if (temp) temp else ... end end }
    { ?expr => ?body ; ... } => { if (?expr) ?body else ... end }
    { otherwise ?body } => { ?body }
    { } => { #f }
end;

define macro for
    { for (?header) ?fbody end } => { %%for (?header) ?fbody end }
  fbody:
    { ?body } => { ?body %%finally }
    { ?body:b1 finally ?body:b2 } => { ?b1 %%finally ?b2 }
  header:
    { ?var in ?expr, ... } 
      => { %%in ?var, ?expr, #f, forward-iteration-protocol; ... }
    { ?var in ?expr keyed-by ?var:keyed-by, ... }
       => { %%in ?var, ?expr, ?keyed-by, forward-iteration-protocol; ... }
    { ?var in ?expr using ?expr:using, ... }
       => { %%in ?var, ?expr, #f, ?using; ... }
    { ?var in ?expr keyed-by ?var:keyed-by using ?expr:using, ... }
       => { %%in ?var, ?expr, ?keyed-by, ?using; ... }
    { ?var = ?expr:e1 then ?expr:e2, ... } => { = ?var, ?e1, ?e2; ... }
    { ?var from ?expr ?to, ... } => { %%from ?var, ?expr, ?to; ... }
    { #key ?while } => { %%while ?while }
    { #key ?until } => { %%while ~ ?until }
    { } => { }
  to:
    { to ?expr ?by } => { ?by, to ?expr }
    { above ?expr ?by } => { ?by, above ?expr }
    { below ?expr ?by } => { ?by, below ?expr }
    { ?by } => { ?by }
  by:
    { by ?expr } => { ?expr }
    { } => { 1 }
end;

define macro if
    { if (?expr) ?body ?elses end }
      => { %%if (?expr) ?body ?elses end }
  elses:
    { else ?body } => { %%else ?body };
    { elseif (?expr) ?body ... } => { %%else %%if (?expr) ?body ... end };
    { } => { %%else #f };
end;

define macro method
    { method ?noise end } => { %%method ?noise end }
end;

define macro select
    { select (?what) ?case-body end }
      => { ?what; ?case-body }
  what:
    { ?expr by ?expr:fn } => { let target = ?expr; let fn = ?fn }
    { ?expr } => { let target = ?expr; let fn = \== }
  case-body:
    { } => { error("select error") }
    { otherwise ?body } => { ?body }
    { ?keys => ?body ; ... } => { if (?keys) ?body else ... end }
  keys:
    { ?expr } => { fn(target, ?expr) }
    { ?expr , ... } => { fn(target, ?expr) | ... }
end;

define macro unless
    { unless (?expr) ?body end } => { %%if (?expr) #f %%else ?body end }
end;

define macro until
    { until (?expr) ?body end }
      => { begin
	     local method loop () 
		     unless (?expr)
		       begin ?body end;
		       loop();
		     end;
		   end;
	     loop();
	   end }
end;

define macro while
    { while (?expr) ?body end }
      => { begin
	     local method loop () 
		     if (?expr)
		       begin ?body end;
		       loop();
		     end;
		   end;
	     loop();
	   end }
end;


// Function macros.

define macro \&
    { \& (?exprs) } => { ?exprs }
  exprs:
    { } => { #t }
    { ?expr } => { ?expr }
    { ?expr , ... } => { if (?expr) ... end }
end;

define macro \|
    { \| (?exprs) } => { begin ?exprs end }
  exprs:
    { } => { #f }
    { ?expr } => { ?expr }
    { ?expr, ... } => { let temp = ?expr; if (temp) temp else ... end }
end;

define macro \:=
    { \:= (?expr:place, ?expr:value) } => { %%set (?place, ?value) }
end;


// Define macros

define macro class-definer
    { define ?modifiers class ?name (?supers) ?slots end }
      => { define ?modifiers %%class ?name (?supers) ?slots end }
  modifiers:
    { } => { }
    { open ... } => { open ... }
    { sealed ... } => { sealed ... }
    { free ... } => { free ... }
    { primary ... } => { primary ... }
    { abstract ... } => { abstract ... }
    { concrete ... } => { concrete ... }
    { functional ... } => { functional ... }
  supers:
    { ?expr } => { ?expr }
    { ?expr, ... } => { ?expr, ... }
  slots:
    { } => { }
    { inherited slot ?name, #rest ?options; ... }
      => { inherited ?name, ?options; ... }
    { required keyword ?key, #rest ?options; ... }
      => { keyword ?key, required: #t, ?options; ... }
    { keyword ?key, #rest ?options; ... }
      => { keyword ?key, ?options; ... }
    { ?slot-modifiers slot ?name, #rest ?options; ... }
      => { slot ?name, ?slot-modifiers, ?options; ... }
    { ?slot-modifiers slot ?name = ?expr, #rest ?options; ... }
      => { slot ?name, ?slot-modifiers,
             init-function: method () ?expr end,
             ?options;
           ... }
    { ?slot-modifiers slot ?name :: ?type, #rest ?options; ... }
      => { slot ?name, type: ?type, ?slot-modifiers, ?options; ... }
    { ?slot-modifiers slot ?name :: ?type = ?expr, #rest ?options; ... }
      => { slot ?name, type: ?type, ?slot-modifiers,
	     init-function: method () ?expr end,
	     ?options;
	   ... }
  slot-modifiers:
    { } => { }
    { instance } => { allocation: #"instance" }
    { class } => { allocation: #"class" }
    { each-subclass } => { allocation: #"each-subclass" }
    { virtual } => { allocation: #"virtual" }
    { constant ... } => { setter: #f, ... }
    { open ... } => { open: #t, ... }
    { sealed ... } => { sealed: #t, ...}
end;

define macro constant-definer
    { define constant ?bindings } => { define %%constant ?bindings }
end;

define macro library-definer
    { define library ?name ?clauses end }
      => { define %%library ?name ?clauses end }
  clauses:
    { use ?name, #key ?import (all), ?exclude ({}), ?prefix (#f),
		      ?rename ({}), ?export ({}); ... }
      => { %%use ?name, ?import, ?exclude, ?prefix, ?rename, ?export; ... }
    { export ?names ; ... }
      => { %%export ?names; ... }
    { } => { }
  names:
    { ?name } => { ?name }
    { ?name, ... } => { ?name, ... }
end;

define macro module-definer
    { define module ?name ?clauses end }
      => { define %%module ?name ?clauses end }
  clauses:
    { use ?name, #key ?import (all), ?exclude ({}), ?prefix (#f),
		      ?rename ({}), ?export ({}); ... }
      => { %%use ?name, ?import, ?exclude, ?prefix, ?rename, ?export; ... }
    { export ?names ; ... }
      => { %%export ?names; ... }
    { create ?names ; ... }
      => { %%create ?names; ... }
    { } => { }
  names:
    { ?name } => { ?name }
    { ?name, ... } => { ?name, ... }
end;

define macro method-definer
    { define ?modifiers method ?name ?noise end }
      => { define ?modifiers %%method ?name ?noise end }
  modifiers:
    { } => { }
    { ?name ... } => { ?name ... }
end;

define macro variable-definer
    { define variable ?bindings } => { define %%variable ?bindings }
end;


// Call-out related macros.

define macro call-out
    // Should really match two or more args, but the macro system is broken.
    { call-out (?args) } => { %%primitive call-out (?args) }
end;

define macro c-include
    { c-include (?expr:file) } => { %%primitive c-include (?file) }
end;

define macro c-decl
    { c-decl (?expr:file) } => { %%primitive c-decl (?file) }
end;

define macro c-expr
    // Should really match exactly two args, but the macro system is broken.
    { c-expr (?args) } => { %%primitive c-expr (?args) }
end;

