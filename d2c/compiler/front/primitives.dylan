module: primitives
rcs-header: $Header: /scm/cvs/src/d2c/compiler/front/primitives.dylan,v 1.9 2003/03/28 00:42:56 housel Exp $
copyright: see below


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

define class <primitive-info> (<identity-preserving-mixin>)
  //
  // The name of this primitive.
  constant slot priminfo-name :: <symbol>,
    required-init-keyword: name:;
  //
  // List of type specifiers for the arguments.  The second to last element
  // can also be #"rest" which means the last element is repeated.  Elements
  // can also be #"cluster" to indicate a values-cluster argument.
  constant slot priminfo-arg-type-specifiers :: <list>,
    required-init-keyword: arg-types:;
  //
  // The arg-types as ctypes.  Lazily created from arg-type-specifiers.
  // #"rest" and #"cluster" preserved.
  slot %priminfo-arg-types :: false-or(<list>),
    init-value: #f;
  //
  // The result-type of the primitive.
  constant slot priminfo-result-type-specifier :: <type-specifier>,
    required-init-keyword: result-type:;
  //
  // The result-type as a ctype.  Lazily created from result-type-specifier.
  slot %priminfo-result-type :: false-or(<values-ctype>),
    init-value: #f;
  //
  // A primitive is side-effect-free if it has no effects other than the
  // computation of the result.  In other words, if we can freely delete
  // the primitive if the results arn't used.
  //
  constant slot priminfo-side-effect-free? :: <boolean>,
    required-init-keyword: side-effect-free:;
  //
  // A primitive is pure if it depends on nothing but the arguments.  In other
  // words, can be freely moved around without changing the program execution
  // at all.  Pure implies side-effect-free.
  constant slot priminfo-pure? :: <boolean>,
    required-init-keyword: pure:;
  //
  // A primitive is cseable if multiple calls to it with the same arguments
  // are guaranteed to return the same result.  Cseable implies pure.
  constant slot priminfo-cseable? :: <boolean>,
    required-init-keyword: cseable:;
  //
  // Function to dynamically compute the return type of this primitive.
  // Gets passed the primitive operation and a list of the ctypes for each
  // argument.
  slot priminfo-type-deriver :: false-or(<function>) = #f;
  //
  // Function to ``optimize'' uses of this primitive.  Gets passed
  // the component and the primitive operation.  Any return values are
  // ignored.
  slot priminfo-transformer :: false-or(<function>) = #f;
  //
  // Function to spew the C code corresponding to the primitive.  Gets
  // passed the primitive and the file-state.
  slot priminfo-emitter :: false-or(<function>) = #f;
end;

define sealed domain make (singleton(<primitive-info>));
define sealed domain initialize (<primitive-info>);

define method print-object
    (info :: <primitive-info>, stream :: <stream>) => ();
  pprint-fields(info, stream, name: info.priminfo-name);
end;


// Dumping is by name, except that we also dump the arg and result types so
// that we can eagerly initialize them.
// 
define method dump-od (obj :: <primitive-info>, buf :: <dump-buffer>) => ();
  dump-simple-object(#"primitive-info", buf, obj.priminfo-name,
		     obj.priminfo-arg-types, obj.priminfo-result-type);
end method;

add-od-loader(*compiler-dispatcher*, #"primitive-info",
  method (state :: <load-state>) => res :: <primitive-info>;
    let res = primitive-info-or-lose(load-object-dispatch(state));
    res.%priminfo-arg-types := load-object-dispatch(state);
    res.%priminfo-result-type := load-object-dispatch(state);
    assert-end-object(state);
    res;
  end method
);


define constant $primitives = make(<object-table>);


define method define-primitive
    (name :: <symbol>, arg-types :: <list>, result-type :: <type-specifier>,
     #key cseable: cseable? :: <boolean>,
          pure: pure? :: <boolean> = cseable?,
          side-effect-free: side-effect-free? :: <boolean> = pure?)
    => ();
  if (cseable? & ~pure?)
    error("Primitive %s can't be cseable but not pure.", name);
  end if;
  if (pure? & ~side-effect-free?)
    error("Primitive %s can't be pure but not side-effect-free.", name);
  end if;
  let info = make(<primitive-info>, name: name, arg-types: arg-types,
		  result-type: result-type, pure: pure?,
		  side-effect-free: side-effect-free?, cseable: cseable?);
  $primitives[name] := info;
end;

define method primitive-info-or-lose
    (name :: <symbol>)
    => res :: false-or(<primitive-info>);
  element($primitives, name, default: #f)
    | error("Unknown primitive: %s", name);
end;


define method priminfo-arg-types (info :: <primitive-info>) => res :: <list>;
  info.%priminfo-arg-types
    | (info.%priminfo-arg-types
	 := map(method (spec-or-sym)
		  if (spec-or-sym == #"rest" | spec-or-sym == #"cluster")
		    spec-or-sym;
		  else
		    specifier-type(spec-or-sym);
		  end;
		end,
		info.priminfo-arg-type-specifiers));
end;

define method priminfo-result-type (info :: <primitive-info>)
    => res :: <values-ctype>;
  info.%priminfo-result-type
    | (info.%priminfo-result-type
	 := info.priminfo-result-type-specifier.specifier-type.ctype-extent);
end;


define method define-primitive-type-deriver
    (name :: <symbol>, func :: <function>) => ();
  primitive-info-or-lose(name).priminfo-type-deriver := func;
end method define-primitive-type-deriver;

define method define-primitive-transformer
    (name :: <symbol>, func :: <function>) => ();
  primitive-info-or-lose(name).priminfo-transformer := func;
end;

define method define-primitive-emitter
    (name :: <symbol>, func :: <function>) => ();
  primitive-info-or-lose(name).priminfo-emitter := func;
end;



// Magic debugging primitives.

define-primitive
  (#"break", #(), #"<false>");



// Define-primitives for the standard primitives.

define-primitive
  (#"mv-call", #(#"<function>", #"cluster"),
   #(values:, rest:, #"<object>"));

define-primitive
  (#"invoke-generic-entry", #(#"<method>", #"<list>", #"cluster"),
   #(values:, rest:, #"<object>"));

define-primitive
  (#"main-entry", #(#"<method>"), #"<raw-pointer>",
   cseable: #t);

define-primitive
  (#"values", #(rest:, #"<object>"), #(values:, rest:, #"<object>"),
   // This is not cseable, because it isn't really a computation.
   // Besides, cse replaces common subexpressions with uses of values,
   // so if we replace values with values, we would spend a long time
   // compiling.
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
  (#"canonicalize-results", #(#"cluster", #"<integer>"),
   #(values:, rest:, #"<object>"),
   side-effect-free: #t);

define-primitive
  (#"make-next-method", #(#"<list>", #"<simple-object-vector>"),
   #(union:, #"<false>", #"<function>"),
   pure: #t);

define-primitive
  (#"make-closure", #(#"<function>", rest:, #"<object>"), #"<function>",
   side-effect-free: #t);

define-primitive
  (#"extract-args", #(#"<integer>"), #"<raw-pointer>",
   side-effect-free: #t);

define-primitive
  (#"make-rest-arg",
   #(#"<raw-pointer>", #"<integer>", #"<integer>"),
   #"<simple-object-vector>",
   side-effect-free: #t);

define-primitive
  (#"extract-arg", #(#"<raw-pointer>", #"<integer>"), #"<object>",
   side-effect-free: #t);

define-primitive
  (#"pop-args", #(#"<raw-pointer>"), #(values:, rest:, #"<object>"));

define-primitive
  (#"initialized?", #(#"<object>"), #"<boolean>",
   cseable: #t);

define-primitive
  (#"make-immediate", #(#"<class>", rest:, #"<object>"), #"<object>",
   cseable: #t);

define-primitive
  (#"allocate", #(#"<class>", #"<integer>"), #"<object>",
   pure: #t);

define-primitive
  (#"allocate-with-data-word", #(#"<class>", #"<integer>", #"<object>"),
   #"<object>",
   pure: #t);

define-primitive
  (#"c-string", #(#"<string>"), #"<raw-pointer>",
   cseable: #t);

define-primitive
  (#"call-out",
   #(#(union:, #"<string>", #"<raw-pointer>"),
     #"<symbol>", rest:, #"<object>"),
   #(values:, rest:, #"<object>"));

define-primitive
  (#"c-include", #(#"<string>"), #(values:));

define-primitive
  (#"c-system-include", #(#"<string>"), #(values:));

define-primitive
  (#"c-decl", #(#"<string>"), #(values:));

define-primitive
  (#"c-local-decl", #(#"<string>"), #(values:));

define-primitive
  (#"c-expr", #(#"<symbol>", #"<string>"),
   #(values:, rest:, #"<object>"));

define-primitive
  (#"c-literal", #(#"<symbol>", #"<string>"), #"<integer>",
   cseable: #t);

define-primitive
  (#"c-struct-field",
   #(#"<symbol>", #"<raw-pointer>", #"<string>", #"<string>"),
   #"<object>",
   side-effect-free: #t);

define-primitive
  (#"c-struct-field-setter",
   #(#"<object>", #"<symbol>", #"<raw-pointer>", #"<string>", #"<string>"),
   #(values:));

define-primitive
  (#"as-boolean", #(#"<object>"), #"<boolean>",
   cseable: #t);

define-primitive
  (#"not", #(#"<object>"), #"<boolean>",
   cseable: #t);

define-primitive
  (#"==", #(#"<object>", #"<object>"), #"<boolean>", cseable: #t);

define-primitive
  (#"initial-symbols", #(), #(union:, #"<symbol>", #"<false>"), cseable: #t);

define-primitive
  (#"ref-slot", #(#"<object>", #"<symbol>", #"<integer>"),
   #"<object>",
   side-effect-free: #t);

define-primitive
  (#"set-slot", #(#"<object>", #"<object>", #"<symbol>", #"<integer>"),
   #(values:));



// NLX operations.

define-primitive
  (#"current-sp", #(), #"<raw-pointer>", cseable: #t);

define-primitive
  (#"unwind-stack", #(#"<raw-pointer>"), #(values:));

define-primitive
  (#"throw", #(#"<raw-pointer>", #"cluster"), #(union:));


// Fixnum operations.

for (name in #[#"fixnum-=", #"fixnum-<"])
  define-primitive
    (name, #(#"<integer>", #"<integer>"), #"<boolean>",
     cseable: #t);
end;

for (name in #[#"fixnum-+", #"fixnum-*", #"fixnum--", #"fixnum-logior",
		 #"fixnum-logxor", #"fixnum-logand", #"fixnum-shift-left",
		 #"fixnum-shift-right", #"fixnum-logical-shift-right"])
  define-primitive
    (name, #(#"<integer>", #"<integer>"), #"<integer>",
     cseable: #t);
end;

for (name in #[#"fixnum-negative", #"fixnum-lognot"])
  define-primitive
    (name, #(#"<integer>"), #"<integer>",
     cseable: #t);
end;
  
define-primitive
  (#"fixnum-divide", #(#"<integer>", #"<integer>"),
   #(values:, #"<integer>", #"<integer>"),
   cseable: #t);

define-primitive
  (#"dblfix-as-fixed", #(#"<double-integer>"), #"<integer>", cseable: #t);
   

// Double-wide fixnum operations.

for (name in #[#"dblfix-=", #"dblfix-<"])
  define-primitive
    (name, #(#"<double-integer>", #"<double-integer>"), #"<boolean>",
     cseable: #t);
end;

for (name in #[#"dblfix-+", #"dblfix-*", #"dblfix--", #"dblfix-logior",
		 #"dblfix-logxor", #"dblfix-logand"])
  define-primitive
    (name, #(#"<double-integer>", #"<double-integer>"), #"<double-integer>",
     cseable: #t);
end;

for (name in #[#"dblfix-shift-left", #"dblfix-shift-right"])
  define-primitive
    (name, #(#"<double-integer>", #"<integer>"), #"<double-integer>",
     cseable: #t);
end;

for (name in #[#"dblfix-negative", #"dblfix-lognot"])
  define-primitive
    (name, #(#"<double-integer>"), #"<double-integer>",
     cseable: #t);
end;
  
define-primitive
  (#"dblfix-divide", #(#"<double-integer>", #"<double-integer>"),
   #(values:, #"<double-integer>", #"<double-integer>"),
   cseable: #t);

define-primitive
  (#"fixed-as-dblfix", #(#"<integer>"), #"<double-integer>", cseable: #t);
   

// Single float operations.

define-primitive
  (#"fixed-as-single", #(#"<integer>"), #"<single-float>", cseable: #t);
   
define-primitive
  (#"dblfix-as-single", #(#"<double-integer>"), #"<single-float>",
   cseable: #t);
   
define-primitive
  (#"double-as-single", #(#"<double-float>"), #"<single-float>", cseable: #t);
   
define-primitive
  (#"extended-as-single", #(#"<extended-float>"), #"<single-float>",
   cseable: #t);

for (name in #[#"single-<", #"single-<=", #"single-=",
		 #"single-==", #"single-~="])
  define-primitive
    (name, #(#"<single-float>", #"<single-float>"), #"<boolean>",
     cseable: #t);
end;

for (name in #[#"single-+", #"single-*", #"single--"])
  define-primitive
    (name, #(#"<single-float>", #"<single-float>"), #"<single-float>",
     cseable: #t);
end;

define-primitive
  (#"single-/", #(#"<single-float>", #"<single-float>"), #"<single-float>");

for (name in #[#"single-abs", #"single-negative"])
  define-primitive
    (name, #(#"<single-float>"), #"<single-float>", cseable: #t);
end;

for (name in #[#"single-floor", #"single-ceiling", #"single-round"])
  define-primitive
    (name, #(#"<single-float>"),
     #(values:, #"<integer>", #"<single-float>"),
     cseable: #t);
end;


// Double float operations.

define-primitive
  (#"fixed-as-double", #(#"<integer>"), #"<double-float>", cseable: #t);
   
define-primitive
  (#"dblfix-as-double", #(#"<double-integer>"), #"<double-float>",
   cseable: #t);
   
define-primitive
  (#"single-as-double", #(#"<single-float>"), #"<double-float>", cseable: #t);
   
define-primitive
  (#"extended-as-double", #(#"<extended-float>"), #"<double-float>",
   cseable: #t);

for (name in #[#"double-<", #"double-<=", #"double-=",
		 #"double-==", #"double-~="])
  define-primitive
    (name, #(#"<double-float>", #"<double-float>"), #"<boolean>",
     cseable: #t);
end;

for (name in #[#"double-+", #"double-*", #"double--"])
  define-primitive
    (name, #(#"<double-float>", #"<double-float>"), #"<double-float>",
     cseable: #t);
end;

define-primitive
  (#"double-/", #(#"<double-float>", #"<double-float>"), #"<double-float>");

for (name in #[#"double-abs", #"double-negative"])
  define-primitive
    (name, #(#"<double-float>"), #"<double-float>", cseable: #t);
end;

for (name in #[#"double-floor", #"double-ceiling", #"double-round"])
  define-primitive
    (name, #(#"<double-float>"),
     #(values:, #"<integer>", #"<double-float>"),
     cseable: #t);
end;


// Extended float operations.

define-primitive
  (#"fixed-as-extended", #(#"<integer>"), #"<extended-float>", cseable: #t);
   
define-primitive
  (#"dblfix-as-extended", #(#"<double-integer>"), #"<extended-float>",
   cseable: #t);
   
define-primitive
  (#"single-as-extended", #(#"<single-float>"), #"<extended-float>",
   cseable: #t);
   
define-primitive
  (#"double-as-extended", #(#"<double-float>"), #"<extended-float>",
   cseable: #t);

for (name in #[#"extended-<", #"extended-<=", #"extended-=",
		 #"extended-==", #"extended-~="])
  define-primitive
    (name, #(#"<extended-float>", #"<extended-float>"), #"<boolean>",
     cseable: #t);
end;

for (name in #[#"extended-+", #"extended-*", #"extended--"])
  define-primitive
    (name, #(#"<extended-float>", #"<extended-float>"), #"<extended-float>",
     cseable: #t);
end;

define-primitive
  (#"extended-/", #(#"<extended-float>", #"<extended-float>"),
   #"<extended-float>");

for (name in #[#"extended-abs", #"extended-negative"])
  define-primitive
    (name, #(#"<extended-float>"), #"<extended-float>", cseable: #t);
end;

for (name in #[#"extended-floor", #"extended-ceiling", #"extended-round"])
  define-primitive
    (name, #(#"<extended-float>"),
     #(values:, #"<integer>", #"<extended-float>"),
     cseable: #t);
end;


// raw pointer operations.

define-primitive
  (#"make-raw-pointer", #(#"<integer>"), #"<raw-pointer>", cseable: #t);

define-primitive
  (#"raw-pointer-address", #(#"<raw-pointer>"), #"<integer>", cseable: #t);

define-primitive
  (#"pointer-+", #(#"<raw-pointer>", #"<integer>"), #"<raw-pointer>",
   cseable: #t);

define-primitive
  (#"pointer--", #(#"<raw-pointer>", #"<raw-pointer>"), #"<integer>",
   cseable: #t);

define-primitive
  (#"pointer-<", #(#"<raw-pointer>", #"<raw-pointer>"), #"<boolean>",
   cseable: #t);

define-primitive
  (#"pointer-=", #(#"<raw-pointer>", #"<raw-pointer>"), #"<boolean>",
   cseable: #t);

define-primitive
  (#"pointer-deref",
   #(#"<symbol>", #"<raw-pointer>", #"<integer>"),
   #"<object>",
   side-effect-free: #t);

define-primitive
  (#"pointer-deref-setter",
   #(#"<object>", #"<symbol>", #"<raw-pointer>", #"<integer>"),
   #(values:));

define-primitive
  (#"vector-elements",
   #(#(union:, #"<buffer>", #"<byte-vector>", #"<byte-string>",
       #"<unicode-string>")),
   #"<raw-pointer>",
   cseable: #t);

define-primitive
  (#"object-address", #(#"<object>"), #"<raw-pointer>",
   cseable: #t);

