module: expanders
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/expand.dylan,v 1.6 2003/03/27 17:39:01 housel Exp $
copyright: see below




//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

// split-fragment-at-commas -- exported.
// 
define generic split-fragment-at-commas (fragment :: <fragment>)
    => results :: <simple-object-vector>;

define method split-fragment-at-commas (fragment :: <empty-fragment>)
    => results :: <simple-object-vector>;
  #[];
end method split-fragment-at-commas;

define method split-fragment-at-commas (fragment :: <compound-fragment>)
    => results :: <simple-object-vector>;
  let results = make(<stretchy-vector>);
  let new-head = fragment.fragment-head;
  let stop = fragment.fragment-tail.fragment-next;
  for (sub-fragment = new-head then sub-fragment.fragment-next,
       until: sub-fragment == stop)
    if (instance?(sub-fragment, <token-fragment>)
	  & sub-fragment.fragment-token.token-kind == $comma-token)
      add!(results,
	   if (sub-fragment == new-head)
	     make(<empty-fragment>);
	   elseif (new-head == sub-fragment.fragment-prev)
	     new-head;
	   else
	     make(<compound-fragment>,
		  head: new-head,
		  tail: sub-fragment.fragment-prev);
	   end if);
      new-head := sub-fragment.fragment-next;
    end if;
  end for;
  add!(results,
       if (new-head == stop)
	 make(<empty-fragment>);
       elseif (new-head == fragment.fragment-tail)
	 new-head;
       else
	 make(<compound-fragment>,
	      head: new-head,
	      tail: fragment.fragment-tail);
       end if);
  as(<simple-object-vector>, results);
end method split-fragment-at-commas;

define method split-fragment-at-commas (fragment :: <token-fragment>)
    => results :: <simple-object-vector>;
  if (fragment.fragment-token.token-kind == $comma-token)
    vector(make(<empty-fragment>), make(<empty-fragment>));
  else
    vector(fragment);
  end if;
end method split-fragment-at-commas;

define method split-fragment-at-commas (fragment :: <bracketed-fragment>)
    => results :: <simple-object-vector>;
  vector(fragment);
end method split-fragment-at-commas;


// Other utilities.

define method expression-from-fragment (fragment :: <fragment>)
    => res :: <expression-parse>;
  parse-expression(make(<fragment-tokenizer>, fragment: fragment));
end method expression-from-fragment;



define method extract-name (fragment :: <fragment>)
    => name :: <identifier-token>;
  error("bug in built in macro, %= isn't a name.", fragment);
end method extract-name;

define method extract-name (fragment :: <token-fragment>)
    => name :: <identifier-token>;
  let token = fragment.fragment-token;
  let kind = token.token-kind;
  if (kind < $define-token | kind > $quoted-name-token)
    error("bug in built in macro, %= isn't a name.", fragment);
  end if;
  token;
end method extract-name;


define method extract-boolean (fragment :: <fragment>)
    => res :: <boolean>;
  let expr = expression-from-fragment(fragment);
  let ctv = ct-eval(expr, #f);
  if (instance?(ctv, <literal-boolean>))
    ctv.literal-value;
  else
    error("bug in builtin define macro: %= isn't a constant boolean", expr);
  end if;
end method extract-boolean;

define method extract-identifier-or-false (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  select (token.token-kind)
    $false-token =>
      #f;
    $raw-ordinary-word-token, $ordinary-define-body-word-token,
    $ordinary-define-list-word-token, $quoted-name-token =>
      token;
    otherwise =>
      compiler-fatal-error
	("invalid identifier: %s", token);
  end select;
end method extract-identifier-or-false;

define method extract-identifier (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  select (token.token-kind)
    $raw-ordinary-word-token, $ordinary-define-body-word-token,
    $ordinary-define-list-word-token, $quoted-name-token =>
      token;
    otherwise =>
      compiler-fatal-error
	("invalid identifier: %s", token);
  end select;
end method extract-identifier;

define method extract-properties
    (plist :: <simple-object-vector>, #rest names)
  local method find-property (name :: <symbol>)
	    => res :: false-or(<fragment>);
	  block (return)
	    for (property in plist)
	      if (property.prop-keyword.token-literal.literal-value == name)
		return(property.prop-value);
	      end;
	    end;
	    #f;
	  end;
	end;
  apply(values, map(find-property, names));
end method extract-properties;



define method make-magic-fragment
    (thing :: <object>,
     #key source-location = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: source-location,
       token: make(<pre-parsed-token>,
		   source-location: source-location,
		   kind: $error-token,
		   parse-tree: thing));
end method make-magic-fragment;



define method make-temp (name :: <symbol>) => res :: <identifier-token>;
  make(<identifier-token>, kind: $raw-ordinary-word-token, symbol: name,
       uniquifier: make(<uniquifier>));
end;

define method make-dylan-id (name :: <symbol>) => res :: <identifier-token>;
  make(<identifier-token>, kind: $raw-ordinary-word-token, symbol: name,
       module: $Dylan-Module, uniquifier: make(<uniquifier>));
end;
  
define method bind-temp (name :: <symbol>, expr :: <expression-parse>)
    => (temp :: <identifier-token>, bind-form :: <let-parse>);
  let temp = make-temp(name);
  let param = make(<parameter>, name: temp);
  let varlist = make(<variable-list>, fixed: vector(param));
  values(temp,
	 make(<let-parse>, variables: varlist, expression: expr));
end;



// Simple expanders

define-procedural-expander
  (#"make-bind-exit",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   body-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<bind-exit-parse>,
		// source-location: ???,
		name: extract-name(name-frag),
		body: expression-from-fragment(body-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);

define-procedural-expander
  (#"make-unwind-protect",
   method (generator :: <expansion-generator>, body-frag :: <fragment>,
	   cleanup-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<unwind-protect-parse>,
		// source-location: ???,
		body: expression-from-fragment(body-frag),
		cleanup: expression-from-fragment(cleanup-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);

define-procedural-expander
  (#"make-if",
   method (generator :: <expansion-generator>, cond-frag :: <fragment>,
	   then-frag :: <fragment>, else-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<if-parse>,
		// source-location: ???,
		condition: expression-from-fragment(cond-frag),
		consequent: expression-from-fragment(then-frag),
		alternate: expression-from-fragment(else-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);

define-procedural-expander
  (#"make-anonymous-method",
   method (generator :: <expansion-generator>, parameters-frag :: <fragment>,
           results-frag :: <fragment>, body-frag :: <fragment>,
           options-frag :: <fragment>)
    => ();
     generate-fragment
       (generator,
        make-parsed-fragment
          (make(<method-ref-parse>,
                method:
                  make(<method-parse>,
                       parameters:
                         parse-parameter-list(make(<fragment-tokenizer>,
                                                   fragment: parameters-frag)),
                       returns:
                         parse-variable-list(make(<fragment-tokenizer>,
                                                  fragment: results-frag)),
                       body: expression-from-fragment(body-frag)),
                options: parse-property-list(make(<fragment-tokenizer>,
                                                  fragment: options-frag))),
           source-location: generate-token-source-location(generator)));
   end method);

define-procedural-expander
  (#"make-callback-method",
   method (generator :: <expansion-generator>, parameters-frag :: <fragment>,
	   results-frag :: <fragment>, body-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<callback-method-ref-parse>,
		method:
		  make(<callback-method-parse>,
		       parameters:
			 parse-parameter-list(make(<fragment-tokenizer>,
						   fragment: parameters-frag)),
		       returns:
			 parse-variable-list(make(<fragment-tokenizer>,
						  fragment: results-frag)),
		       body: expression-from-fragment(body-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);

define-procedural-expander
  (#"make-primitive",
   method (generator :: <expansion-generator>, name-frag :: <token-fragment>,
	   args-frag :: <fragment>)
       => ();
     let name = name-frag.fragment-token;
     assert (name.token-kind >= $define-token
	       & name.token-kind <= $quoted-name-token);
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<primitive-parse>,
		source-location: name.source-location,
		name: name,
		operands: map(expression-from-fragment,
			      split-fragment-at-commas(args-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);



// Assignment

define-procedural-expander
  (#"make-assignment",
   method (generator :: <expansion-generator>, place-frag :: <token-fragment>,
	   value-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (expand-assignment(expression-from-fragment(place-frag),
			     expression-from-fragment(value-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);


define generic expand-assignment
    (place :: <expression-parse>, value :: <expression-parse>)
    => res :: <expression-parse>;

define method expand-assignment
    (place :: <varref-parse>, value :: <expression-parse>)
    => res :: <expression-parse>;
  make(<varset-parse>,
       source-location: place.source-location,
       id: place.varref-id,
       value: value);
end;

define method expand-assignment
    (place :: <funcall-parse>, value :: <expression-parse>)
    => res :: <expression-parse>;
  unless (instance?(place.funcall-function, <varref-parse>))
    compiler-fatal-error-location(place, "invalid place for assignment");
  end;
  apply(make-setter-call, place.funcall-function.varref-id, value,
	map(make-parsed-fragment, place.funcall-arguments));
end method expand-assignment;
		   
define method expand-assignment
    (place :: <function-macro-call-parse>, value :: <expression-parse>)
    => res :: <expression-parse>;
  let frag = place.macro-call-fragment;
  if (instance?(frag, <empty-fragment>))
    make-setter-call(place.macro-call-word, value);
  else
    make-setter-call(place.macro-call-word, value, copy-fragment(frag));
  end if;
end method expand-assignment;
			     
define method expand-assignment
    (place :: <dot-parse>, value :: <expression-parse>)
    => res :: <expression-parse>;
  make-setter-call(place.dot-name, value,
		   make-parsed-fragment(place.dot-operand));
end method expand-assignment;
			     
define method expand-assignment
    (place :: <expression-parse>, value :: <expression-parse>)
    => res :: <expression-parse>;
  compiler-fatal-error-location(place, "invalid place for assignment");
end;

define method make-setter-call
    (getter :: <identifier-token>, value :: <expression-parse>,
     #rest argument-fragments)
    => res :: <expression-parse>;
  let (temp, temp-bind) = bind-temp(#"new-value", value);

  let fragment = make(<empty-fragment>);
  local method append-token (token :: <token>) => ();
	  fragment := append-fragments!(fragment,
					make(<token-fragment>, token: token));
	end method append-token;
  append-token(temp);
  for (arg-frag in argument-fragments)
    append-token(make(<token>, kind: $comma-token));
    fragment := append-fragments!(fragment, arg-frag);
  end for;

  let new-name = symcat(getter.token-symbol, "-setter");
  let module = getter.token-module
    | compiler-fatal-error-location(getter, "getter %s has no module", getter);
  let kind = syntax-for-name(module.module-syntax-table, new-name);
  let setter = make(<identifier-token>,
		    source-location: getter.source-location,
		    kind: kind,
		    symbol: new-name,
		    module: module,
		    uniquifier: getter.token-uniquifier);

  make(<body-parse>,
       parts:
	 vector(temp-bind,
		expression-from-fragment
		  (append-fragments!
		     (make(<token-fragment>, token: setter),
		      make(<bracketed-fragment>,
			   left-token: make(<token>,
					    kind: $left-paren-token),
			   contents: fragment,
			   right-token: make(<token>,
					     kind: $right-paren-token)))),
		make(<varref-parse>, id: temp)));
end;

