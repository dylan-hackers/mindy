module: main
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/main-unit-state.dylan,v 1.7 2003/07/16 15:03:36 scotek Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
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

// This should have some reasonable association with cback
// <unit-state> (but it doesn't.)
//

define class <main-unit-state> (<object>)
  constant slot unit-command-line-features :: <list>, 
    required-init-keyword: command-line-features:;
  constant slot unit-log-dependencies :: <boolean>, 
    required-init-keyword: log-dependencies:;
  constant slot unit-target :: <platform>,
    required-init-keyword: target:;
  constant slot unit-no-binaries :: <boolean>,
    required-init-keyword: no-binaries:;
  constant slot unit-no-makefile :: <boolean>,
    required-init-keyword: no-makefile:;
  constant slot unit-link-static :: <boolean>,
    required-init-keyword: link-static:;
  constant slot unit-link-rpath :: false-or(<string>),
    required-init-keyword: link-rpath:;
  // Simplistic flags to control debugging (and someday, optimization).
  // We only have one of these right now.
  slot unit-debug? :: <boolean>, init-keyword: debug?:, init-value: #f;
  slot unit-profile? :: <boolean>, init-keyword: profile?:, init-value: #f;

  slot dump-testworks-spec? :: <boolean>,
    init-value: #f, init-keyword: dump-testworks-spec?:;

  slot unit-header :: <header>;
  constant slot unit-init-functions :: <stretchy-vector>
	= make(<stretchy-vector>);

  // how many threads do we want d2c to try to make use of
  constant slot unit-thread-count :: false-or(<integer>),
    init-keyword: thread-count:, init-value: #f;
end class <main-unit-state>;

// Find the library object file (archive) using the data-unit search path.
// There might be more than one possible object file suffix, so we try them
// all, but if we find it under more than one suffix, we error.
//
// If the platform supports shared libraries (as indicated by the presence
// of shared-library-filename-suffix in platforms.descr), and if the user
// didn't specify '-static' on the command line, locate shared library
// version first. 

#if (macos)

// At the moment we don't include compiled libs, so we don't need to look properly
// This will cause problems for MPW and building projects, 
// and so this is a temporary measure

define method find-library-archive
    (unit-name :: <byte-string>, state :: <main-unit-state>)
 => path :: <byte-string>;
	let target = state.unit-target;
	let suffixes = split-at-whitespace( target.library-filename-suffix );
	let libname = concatenate( target.library-filename-prefix, unit-name, first( suffixes ) );
 	libname;
 end method find-library-archive;

#else

define method find-library-archive
    (unit-name :: <byte-string>, state :: <main-unit-state>)
 => path :: <byte-string>;
  let target = state.unit-target;
  let libname = concatenate(target.library-filename-prefix, unit-name);
  let suffixes = split-at-whitespace(target.library-filename-suffix);

  let found = #();

  let find = method (suffixes)
	       let found = #();
	       for (suffix in suffixes)
		 let suffixed = concatenate(libname, suffix);
		 let path = find-file(suffixed, *data-unit-search-path*);
		 if (path)
		   found := pair(path, found);
		 end if;
	       end for;
	       found;
	     end method;

  if (target.shared-library-filename-suffix & ~state.unit-link-static)  
    let shared-suffixes
      = split-at-whitespace(target.shared-library-filename-suffix);
    found := find(shared-suffixes);
    if (empty?(found))
      found := find(suffixes);
    end if;
  else
    found := find(suffixes);
  end if;

  if (empty?(found))
    error("Can't find object file for library %s.", unit-name);
  elseif (found.tail ~== #())
    error("Found more than one type of object file for library %s:\n"
	  "  %=",
	  unit-name,
	  found);
  else
    found.head;
  end if;
end method find-library-archive;

#endif


// The actual meat of compilation.  Does FER conversion, optimizes and emits
// output code.
//
#if (~mindy)
define variable *last-time-flushed* :: <integer> = 0;
#endif

define method compile-1-tlf
    (tlf :: <top-level-form>, file :: <file-state>, state :: <main-unit-state>) 
 => ();
  let name = format-to-string("%s", tlf);
  begin
    let column = *debug-output*.current-column;
    if (column & column > 75)
      format(*debug-output*, "\n");
    end if;
  end;
  format(*debug-output*, ".");
#if (mindy)
  force-output(*debug-output*);
#else
  let now :: <integer> = get-time-of-day();
  if (now ~= *last-time-flushed*)
    force-output(*debug-output*);
    *last-time-flushed* := now;
  end;
#endif
  note-context(name);
  let component = make(<fer-component>);
  let builder = make-builder(component);
  convert-top-level-form(builder, tlf);
  let inits = builder-result(builder);
  let name-obj = make(<anonymous-name>, location: tlf.source-location);
  unless (instance?(inits, <empty-region>))
    let result-type = make-values-ctype(#(), #f);
    let source = make(<source-location>);
    let init-function
      = build-function-body
          (builder, $Default-Policy, source, #f,
	   name-obj,
	   #(), result-type, #t);
    build-region(builder, inits);
    build-return
      (builder, $Default-Policy, source, init-function, #());
    end-body(builder);
    let sig = make(<signature>, specializers: #(), returns: result-type);
    let ctv = make(<ct-function>, name: name-obj, signature: sig);
    make-function-literal(builder, ctv, #"function", #"global",
			  sig, init-function);
    add!(state.unit-init-functions, ctv);
  end;
  optimize-component(*current-optimizer*, component);
  emit-tlf-gunk(tlf, file);
  emit-component(component, file);
end method compile-1-tlf;

define constant $max-inits-per-function = 25;

define method emit-init-functions
    (prefix :: <byte-string>, init-functions :: <vector>,
     start :: <integer>, finish :: <integer>, stream :: <stream>)
    => body :: <byte-string>;
  let string-stream = make(<buffered-byte-string-output-stream>);
  if (finish - start <= $max-inits-per-function)
    for (index from start below finish)
      let init-function = init-functions[index];
      let ep = make(<ct-entry-point>, for: init-function, kind: #"main");
      let name = ep.entry-point-c-name;
      format(stream, "extern void %s(descriptor_t *sp);\n\n", name);
      format(string-stream, "    %s(sp);\n", name);
    end for;
  else
    for (divisions = finish - start
	   then ceiling/(divisions, $max-inits-per-function),
	 while: divisions > $max-inits-per-function)
    finally
      for (divisions from divisions above 0 by -1)
	let count = ceiling/(finish - start, divisions);
	let name = format-to-string("%s_init_%d_%d",
				    prefix, start, start + count - 1);
	let guts = emit-init-functions(prefix, init-functions,
				       start, start + count, stream);
	format(stream, "static void %s(descriptor_t *sp)\n{\n%s}\n\n",
	       name, guts);
	format(string-stream, "    %s(sp);\n", name);
	start := start + count;
      end for;
    end for;
  end if;
  string-stream.stream-contents;
end method emit-init-functions;

define method build-unit-init-function
    (prefix :: <byte-string>, init-functions :: <vector>,
     stream :: <stream>)
    => ();
  let init-func-guts = emit-init-functions(string-to-c-name(prefix), init-functions,
					   0, init-functions.size, stream);
  // The function this generated used to be called simply "%s_init",
  // but that conflicted with the heap object of the same name.  (Of
  // course, on the HP, the linker has separate namespaces for code
  // and data, but most other platforms do not)
  format(stream, "void %s_Library_init(descriptor_t *sp)\n{\n%s}\n",
	 string-to-c-name(prefix), init-func-guts);
end;

define method build-command-line-entry
    (lib :: <library>, entry :: <byte-string>, file :: <file-state>)
    => entry-function :: <ct-function>;
  let (module-name, variable-name) = split-at-colon(entry);
  let module = find-module(lib, as(<symbol>, module-name));
  unless (module)
    compiler-fatal-error("Invalid entry point: %s -- no module %s.",
			 entry, module-name);
  end unless;
  let variable = find-variable(make(<basic-name>,
				    symbol: as(<symbol>, variable-name),
				    module: module));
  unless (variable)
    compiler-fatal-error
      ("Invalid entry point: %s -- no variable %s in module %s.",
       entry, variable-name, module-name);
  end unless;
  let defn = variable.variable-definition;
  unless (defn)
    compiler-fatal-error
      ("Invalid entry point: %s -- it isn't defined.", entry);
  end unless;

  let component = make(<fer-component>);
  let builder = make-builder(component);
  let source = make(<source-location>);
  let policy = $Default-Policy;
  let name = "Command Line Entry";
  let name-obj
    = make(<basic-name>, module: $dylan-module, symbol: #"command-line-entry");

  let int-type = specifier-type(#"<integer>");
  let rawptr-type = specifier-type(#"<raw-pointer>");
  let result-type = make-values-ctype(#(), #f);
  let argc = make-local-var(builder, #"argc", int-type);
  let argv = make-local-var(builder, #"argv", rawptr-type);
  let func
    = build-function-body
        (builder, policy, source, #f,
	 name-obj, list(argc, argv), result-type, #t); 

  let user-func = build-defn-ref(builder, policy, source, defn);
  // ### Should really spread the arguments out, but I'm lazy.
  build-assignment(builder, policy, source, #(),
		   make-unknown-call(builder, user-func, #f,
				     list(argc, argv)));
  build-return(builder, policy, source, func, #());
  end-body(builder);
  let sig = make(<signature>, specializers: list(int-type, rawptr-type),
		 returns: result-type);
  let ctv = make(<ct-function>, name: name-obj, signature: sig);
  make-function-literal(builder, ctv, #"function", #"global", sig, func);
  optimize-component(*current-optimizer*, component);
  emit-component(component, file);
  ctv;
end method build-command-line-entry;


