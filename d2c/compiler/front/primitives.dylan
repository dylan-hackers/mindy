module: primitives


define class <primitive-info> (<object>)
  //
  // The name of this primitive.
  slot primitive-name :: <symbol>,
    required-init-keyword: name:;
  //
  // List of type specifiers for the arguments.  The second to last element
  // can also be #"rest" which means the last element is repeated.  Elements
  // can also be #"cluster" to indicate a values-cluster argument.
  slot primitive-arg-type-specifiers :: <list>,
    required-init-keyword: arg-types:;
  //
  // The arg-types as ctypes.  Lazily created from arg-type-specifiers.
  // #"rest" and #"cluster" preserved.
  slot %primitive-arg-types :: false-or(<list>),
    init-value: #f;
  //
  // The result-type of the primitive.
  slot primitive-result-type-specifier :: <type-specifier>,
    required-init-keyword: result-type:;
  //
  // The result-type as a ctype.  Lazily created from result-type-specifier.
  slot %primitive-result-type :: false-or(<values-ctype>),
    init-value: #f;
  //
  // A primitive is side-effect-free if it has no effects other than the
  // computation of the result.  In other words, if we can freely delete
  // the primitive if the results arn't used.
  //
  slot primitive-side-effect-free? :: <boolean>,
    required-init-keyword: side-effect-free:;
  //
  // A primitive is pure if it depends on nothing but the arguments.  In other
  // words, can be freely moved around without changing the program execution
  // at all.  Pure implies side-effect-free.
  slot primitive-pure? :: <boolean>,
    required-init-keyword: pure:;
  //
  // Function to ``optimize'' uses of this primitive.  Gets passed
  // the component and the primitive operation.  Any return values are
  // ignored.
  slot primitive-transformer :: false-or(<function>),
    init-value: #f;
  //
  // Function to spew the C code corresponding to the primitive.  Gets
  // passed the primitive and the output-info.
  slot primitive-emitter :: false-or(<function>),
    init-value: #f;
end;

define method print-object
    (info :: <primitive-info>, stream :: <stream>) => ();
  pprint-fields(info, stream, name: info.primitive-name);
end;


define constant $primitives = make(<object-table>);


define method define-primitive
    (name :: <symbol>, arg-types :: <list>, result-type :: <type-specifier>,
     #key pure: pure?, side-effect-free: side-effect-free? = pure?)
    => ();
  let info = make(<primitive-info>, name: name, arg-types: arg-types,
		  result-type: result-type, pure: pure?,
		  side-effect-free: side-effect-free?);
  $primitives[name] := info;
end;

define method primitive-info-or-lose
    (name :: <symbol>)
    => res :: false-or(<primitive-info>);
  element($primitives, name, default: #f)
    | error("Unknown primitive: %s", name);
end;


define method primitive-arg-types (info :: <primitive-info>) => res :: <list>;
  info.%primitive-arg-types
    | (info.%primitive-arg-types
	 := map(method (spec-or-sym)
		  if (spec-or-sym == #"rest" | spec-or-sym == #"cluster")
		    spec-or-sym;
		  else
		    specifier-type(spec-or-sym);
		  end;
		end,
		info.primitive-arg-type-specifiers));
end;

define method primitive-result-type (info :: <primitive-info>)
    => res :: <values-ctype>;
  info.%primitive-result-type
    | (info.%primitive-result-type
	 := specifier-type(info.primitive-result-type-specifier));
end;


define method define-primitive-transformer
    (name :: <symbol>, func :: <function>) => ();
  primitive-info-or-lose(name).primitive-transformer := func;
end;

define method define-primitive-emitter
    (name :: <symbol>, func :: <function>) => ();
  primitive-info-or-lose(name).primitive-emitter := func;
end;



// Magic debugging primitives.

define-primitive
  (#"break", #(), #"<false>");



// Define-primitives for the standard primitives.

define-primitive
  (#"invoke-generic-entry", #(#"<method>", #"<list>", #"cluster"),
   #(values:, rest:, #"<object>"));

define-primitive
  (#"values", #(rest:, #"<object>"), #(values:, rest:, #"<object>"),
   pure: #t);

define-primitive
  (#"values-sequence", #(#"<simple-object-vector>"),
   #(values:, rest:, #"<object>"),
   side-effect-free: #t);

define-primitive
  (#"merge-clusters", #(rest:, #"cluster"),
   #(values:, rest:, #"<object>"),
   side-effect-free: #t);

define-primitive
  (#"vector", #(rest:, #"<object>"), #"<simple-object-vector>",
   pure: #t);

define-primitive
  (#"canonicalize-results", #(#"cluster", #"<fixed-integer>"),
   #(values:, rest:, #"<object>"),
   pure: #t);

define-primitive
  (#"make-closure", #(#"<function>", rest:, #"<object>"), #"<function>",
   side-effect-free: #t);

define-primitive
  (#"extract-args", #(#"<fixed-integer>"), #"<raw-pointer>",
   side-effect-free: #t);

define-primitive
  (#"make-rest-arg",
   #(#"<raw-pointer>", #"<fixed-integer>", #"<fixed-integer>"),
   #"<simple-object-vector>",
   side-effect-free: #t);

define-primitive
  (#"extract-arg", #(#"<raw-pointer>", #"<fixed-integer>"), #"<object>",
   side-effect-free: #t);

define-primitive
  (#"pop-args", #(#"<raw-pointer>"), #(values:));

define-primitive
  (#"initialized?", #(#"<object>"), #"<boolean>",
   pure: #t);

define-primitive
  (#"allocate", #(#"<fixed-integer>"), #"<object>",
   pure: #t);

define-primitive
  (#"make-data-word-instance", #(#"<object>"), #"<object>",
   pure: #t);

define-primitive
  (#"c-string", #(#"<string>"), #"<raw-pointer>",
   pure: #t);

define-primitive
  (#"call-out",
   #(#"<string>", #"<symbol>",
     rest:, #(union:, #"<symbol>", #"<fixed-integer>", #"<raw-pointer>",
	      #"<single-float>", #"<double-float>", #"<extended-float>")),
   #(union:, #"<fixed-integer>", #"<raw-pointer>",
     #"<single-float>", #"<double-float>", #"<extended-float>"));

define-primitive
  (#"as-boolean", #(#"<object>"), #"<boolean>",
   pure: #t);

define-primitive
  (#"not", #(#"<object>"), #"<boolean>",
   pure: #t);

define-primitive
  (#"==", #(#"<object>", #"<object>"), #"<boolean>", pure: #t);


// NLX operations.

define-primitive
  (#"current-sp", #(), #"<raw-pointer>", pure: #t);

define-primitive
  (#"unwind-stack", #(#"<raw-pointer>"), #(values:));

define-primitive
  (#"throw", #(#"<raw-pointer>", #"cluster"), #(union:));


// Fixnum operations.

for (name in #[#"fixnum-=", #"fixnum-<"])
  define-primitive
    (name, #(#"<fixed-integer>", #"<fixed-integer>"), #"<boolean>",
     pure: #t);
end;

for (name in #[#"fixnum-+", #"fixnum-*", #"fixnum--", #"fixnum-logior",
		 #"fixnum-logxor", #"fixnum-logand", #"fixnum-shift-left",
		 #"fixnum-shift-right"])
  define-primitive
    (name, #(#"<fixed-integer>", #"<fixed-integer>"), #"<fixed-integer>",
     pure: #t);
end;

for (name in #[#"fixnum-negative", #"fixnum-lognot"])
  define-primitive
    (name, #(#"<fixed-integer>"), #"<fixed-integer>",
     pure: #t);
end;
  
for (name in #[#"fixnum-floor/", #"fixnum-ceiling/", #"fixnum-round/",
		 #"fixnum-truncate/"])
  // Note: these arn't pure because they have to signal an error for
  // divide by zero.
  define-primitive
    (name,
     #(#"<fixed-integer>", #"<fixed-integer>"),
     #(values:, #"<fixed-integer>", #"<fixed-integer>"));
end;


// Single float operations.

define-primitive
  (#"fixed-as-single", #(#"<fixed-integer>"), #"<single-float>", pure: #t);
   
define-primitive
  (#"double-as-single", #(#"<double-float>"), #"<single-float>", pure: #t);
   
define-primitive
  (#"extended-as-single", #(#"<extended-float>"), #"<single-float>", pure: #t);

for (name in #[#"single-<", #"single-<=", #"single-=",
		 #"single-==", #"single-~="])
  define-primitive
    (name, #(#"<single-float>", #"<single-float>"), #"<boolean>",
     pure: #t);
end;

for (name in #[#"single-+", #"single-*", #"single--"])
  define-primitive
    (name, #(#"<single-float>", #"<single-float>"), #"<single-float>",
     pure: #t);
end;

define-primitive
  (#"single-/", #(#"<single-float>", #"<single-float>"), #"<single-float>");

for (name in #[#"single-abs", #"single-negative"])
  define-primitive
    (name, #(#"<single-float>"), #"<single-float>", pure: #t);
end;

for (name in #[#"single-floor", #"single-ceiling", #"single-round"])
  define-primitive
    (name, #(#"<single-float>"),
     #(values:, #"<fixed-integer>", #"<single-float>"),
     pure: #t);
end;


// Double float operations.

define-primitive
  (#"fixed-as-double", #(#"<fixed-integer>"), #"<double-float>", pure: #t);
   
define-primitive
  (#"single-as-double", #(#"<single-float>"), #"<double-float>", pure: #t);
   
define-primitive
  (#"extended-as-double", #(#"<extended-float>"), #"<double-float>", pure: #t);

for (name in #[#"double-<", #"double-<=", #"double-=",
		 #"double-==", #"double-~="])
  define-primitive
    (name, #(#"<double-float>", #"<double-float>"), #"<boolean>",
     pure: #t);
end;

for (name in #[#"double-+", #"double-*", #"double--"])
  define-primitive
    (name, #(#"<double-float>", #"<double-float>"), #"<double-float>",
     pure: #t);
end;

define-primitive
  (#"double-/", #(#"<double-float>", #"<double-float>"), #"<double-float>");

for (name in #[#"double-abs", #"double-negative"])
  define-primitive
    (name, #(#"<double-float>"), #"<double-float>", pure: #t);
end;

for (name in #[#"double-floor", #"double-ceiling", #"double-round"])
  define-primitive
    (name, #(#"<double-float>"),
     #(values:, #"<fixed-integer>", #"<double-float>"),
     pure: #t);
end;


// Extended float operations.

define-primitive
  (#"fixed-as-extended", #(#"<fixed-integer>"), #"<extended-float>", pure: #t);
   
define-primitive
  (#"single-as-extended", #(#"<single-float>"), #"<extended-float>", pure: #t);
   
define-primitive
  (#"double-as-extended", #(#"<double-float>"), #"<extended-float>", pure: #t);

for (name in #[#"extended-<", #"extended-<=", #"extended-=",
		 #"extended-==", #"extended-~="])
  define-primitive
    (name, #(#"<extended-float>", #"<extended-float>"), #"<boolean>",
     pure: #t);
end;

for (name in #[#"extended-+", #"extended-*", #"extended--"])
  define-primitive
    (name, #(#"<extended-float>", #"<extended-float>"), #"<extended-float>",
     pure: #t);
end;

define-primitive
  (#"extended-/", #(#"<extended-float>", #"<extended-float>"),
   #"<extended-float>");

for (name in #[#"extended-abs", #"extended-negative"])
  define-primitive
    (name, #(#"<extended-float>"), #"<extended-float>", pure: #t);
end;

for (name in #[#"extended-floor", #"extended-ceiling", #"extended-round"])
  define-primitive
    (name, #(#"<extended-float>"),
     #(values:, #"<fixed-integer>", #"<extended-float>"),
     pure: #t);
end;


// raw pointer operations.

define-primitive
  (#"make-raw-pointer", #(#"<fixed-integer>"), #"<raw-pointer>", pure: #t);

define-primitive
  (#"raw-pointer-address", #(#"<raw-pointer>"), #"<fixed-integer>", pure: #t);

define-primitive
  (#"pointer-+", #(#"<raw-pointer>", #"<fixed-integer>"), #"<raw-pointer>",
   pure: #t);

define-primitive
  (#"pointer--", #(#"<raw-pointer>", #"<raw-pointer>"), #"<fixed-integer>",
   pure: #t);

define-primitive
  (#"pointer-<", #(#"<raw-pointer>", #"<raw-pointer>"), #"<boolean>",
   pure: #t);

define-primitive
  (#"pointer-=", #(#"<raw-pointer>", #"<raw-pointer>"), #"<boolean>",
   pure: #t);
