module: autodump
author: Nick Kramer (nkramer@cs.cmu.edu)

// Program to automatically generate dumpers for all the various
// functions, by using Mindy introspection routines.  Ultimately outputs a 
// .dylan file (or several).

define method generate-dumper-and-loader 
    (stream :: <stream>, cls :: <class>) => ();
  let slot-q = make(<deque>);
  for (slot-descr in cls.slot-descriptors)
    // it should probably be able to handle constant slots...
    if (slot-descr.slot-allocation ~= #"instance")
      error("Slot isn't instance slot; %=.%= is %=", cls, 
	    slot-descr.slot-name, slot-descr.slot-allocation);
    end if;
    let getter = as(<byte-string>, slot-descr.slot-name);
    let keyword 
      = if (slot-descr.init-keyword = #f)
	  "#f";
	else
	  concatenate("#\"", as(<byte-string>, slot-descr.init-keyword), 
		      "\"");
	end if;
    let setter 
      = if (slot-descr.keyword-required? | slot-descr.slot-setter = #f)
	  "#f";
	else
	  let setter-symbol = slot-descr.slot-setter.function-name;
	  if (setter-symbol = #f)
	    error("Can't construct setter name for %= in class %=", 
		  slot-descr.slot-setter, cls);
	  end if;
	  as(<byte-string>, setter-symbol);
	end if;
    push(slot-q, join(", ", getter, keyword, setter));
  end for;
  let list-literal-contents = as(<byte-string>, 
				 apply(join, ",\n       ", slot-q));
  format(stream, 
	 "add-make-dumper(#\"%s\", *compiler-dispatcher*,\n"
	 "  %s, \n"
	 "  list(\n"
	 "       %s\n"
	 "   ));\n\n",
	 copy-sequence(as(<byte-string>, cls.class-name), start: 1, 
		       end: as(<byte-string>, cls.class-name).size - 1),
	 as(<byte-string>, cls.class-name), list-literal-contents);
  force-output(stream);
end method generate-dumper-and-loader;

define method autodump (module-name :: <string>, file-name :: <string>, 
			#rest classes) => ();
  let stream = make(<file-stream>, name: file-name, direction: #"output");
  format(stream, "module: %s\n", module-name);
  format(stream, "author: Autodump\n\n");
  format(stream, "// Don't modify this file by hand, use autodump.\n\n");

  if (next-free-id ~= #f)
    for (cls in classes)
      let name = as(<byte-string>, cls.class-name);
      format(*standard-output*, "register-object-id(#\"%s\", #x%x);\n", 
	     copy-sequence(name, start: 1, end: name.size - 1), next-free-id);
      next-free-id := next-free-id + 1;
    end for;
  end if;

  format(stream, "\n");
  for (cls in classes)
    generate-dumper-and-loader(stream, cls);
  end for;
  close(stream);
end method autodump;

define variable next-free-id = #f;

define method autodump-main
    (component :: <symbol>, id :: union(<integer>, <false>)) => ();
  next-free-id := id;
  select (component)
    #"parse-tree" 
      => autodump("parse-tree", "parse-dump.dylan",
		  <property>, <bindings>, <parameter-list>, <parameter>,
		  <keyword-parameter>, <method-parse>, <case-clause>,
		  <property-set>, <use-clause>, <export-clause>,
		  <create-clause>, <for-clause>, <for-while-clause>,
		  <for-var-clause>, <for-in-clause>, <for-step-clause>,
		  <for-from-clause>, <classopt>, <constituent>,
		  <defining-form>, <define-class-parse>,
		  <define-constant-parse>, <define-generic-parse>,
		  <seal-generic-parse>, <define-library-parse>,
		  <define-method-parse>, <define-module-parse>,
		  <define-variable-parse>, <define-parse>,
		  <define-bindings-parse>, <define-macro-parse>,
		  <define-define-macro-parse>,
		  <define-define-bindings-macro-parse>,
		  <define-statement-macro-parse>,
		  <define-function-macro-parse>, <local-declaration>,
		  <let>, <let-handler>, <local>, <expression>,
		  <literal-ref>, <binop-series>, <funcall>, <dot>,
		  <varref>, <macro-statement>, <assignment>, <begin>,
		  <bind-exit>, <for>, <if>, <method-ref>, <mv-call>,
		  <primitive>, <uwp>, <rule>, <abstract-define-rule>,
		  <define-rule>, <define-bindings-rule>,
		  <statement-rule>, <function-rule>, <pattern>,
		  <pattern-list>, <pattern-sequence>, <simple-pattern>,
		  <variable-pattern>, <bound-variable-pattern>,
		  <identifier-pattern>, <literal-pattern>,
		  <otherwise-pattern>, <arrow-pattern>,
		  <details-pattern>, <pattern-variable>,
		  <property-list-pattern>, <pattern-keyword>,
		  <auxiliary-rule-set>, <auxiliary-rule>, <template>,
		  <paren-template>, <pattern-variable-reference>);
    #"tokens"
      => autodump("tokens", "token-dump.dylan",
		  <token>, <eof-token>, <error-token>, <symbol-token>,
		  <identifier-token>, <word-token>, <name-token>,
		  <simple-name-token>, <quoted-name-token>,
		  <begin-word-token>, <define-word-token>,
		  <define-bindings-word-token>, <constrained-name-token>,
		  <core-word-token>, <begin-token>, <bind-exit-token>,
		  <class-token>, <cleanup-token>, <constant-token>,
		  <create-token>, <define-token>, <else-token>,
		  <end-token>, <export-token>, <finally-token>,
		  <for-token>, <from-token>, <generic-token>,
		  <handler-token>, <if-token>, <in-token>, <let-token>,
		  <library-token>, <local-token>, <macro-token>,
		  <module-token>, <method-token>, <mv-call-token>,
		  <otherwise-token>, <primitive-token>, <seal-token>,
		  <set-token>, <use-token>, <uwp-token>,
		  <variable-token>, <while-token>, <keyword-token>,
		  <abstract-literal-token>, <literal-token>,
		  <string-token>, <operator-token>,
		  <binary-operator-token>,
		  <simple-binary-operator-token>, <unary-operator-token>,
		  <punctuation-token>, <left-paren-token>,
		  <right-paren-token>, <comma-token>, <dot-token>,
		  <semicolon-token>, <left-bracket-token>,
		  <right-bracket-token>, <left-brace-token>,
		  <right-brace-token>, <double-colon-token>,
		  <minus-token>, <tilde-token>, <equal-token>,
		  <double-equal-token>, <arrow-token>,
		  <sharp-paren-token>, <sharp-bracket-token>,
		  <question-token>, <double-question-token>,
		  <ellipsis-token>, <sharp-word-token>, <true-token>,
		  <false-token>, <next-token>, <rest-token>, <key-token>,
		  <all-keys-token>);
    otherwise
      => error("Unknown module %=", component);
  end select;
end method autodump-main;
