module: main
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/main.dylan,v 1.1 1998/05/03 19:55:33 andreas Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
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


// This should have some reasonable association with cback
// <unit-state> (but it doesn't.)
//
define class <main-unit-state> (<object>)
    slot unit-lid-file :: <byte-string>, required-init-keyword: lid-file:;
    slot unit-command-line-features :: <list>, 
         required-init-keyword: command-line-features:;
    slot unit-target :: <platform>,
         required-init-keyword: target:;
    slot unit-log-dependencies :: <boolean>, 
         required-init-keyword: log-dependencies:;
    slot unit-no-binaries :: <boolean>,
         required-init-keyword: no-binaries:;

    // A facility for hacking around C compiler bugs by using a different
    // command for particular C compilations.  cc-override is a format string
    // used instead of the normal platform compile-c-command.  It is used
    // whenever compiling one of the files in the override-files list.
    slot unit-cc-override :: false-or(<string>),
         required-init-keyword: cc-override:;
    slot unit-override-files :: <list>,
         required-init-keyword: override-files:;

    slot unit-header :: <header>;
    slot unit-files :: <stretchy-vector>;
    slot unit-lib-name :: <byte-string>;
    slot unit-lib :: <library>;
    // unit-prefix already a <unit-state> accessor
    slot unit-mprefix :: <byte-string>;
    slot unit-tlf-vectors :: <stretchy-vector> = make(<stretchy-vector>);
    slot unit-init-functions :: <stretchy-vector> = make(<stretchy-vector>);
    slot unit-cback-unit :: <unit-state>;
    slot unit-other-cback-units :: <simple-object-vector>;

    slot unit-cc-flags;
    // Makefile generation streams, etc.
    slot unit-all-generated-files :: <list>, init-value: #();
    slot unit-makefile-name :: <byte-string>;
    slot unit-temp-makefile-name :: <byte-string>;
    slot unit-makefile :: <file-stream>;
    slot unit-objects-stream :: <buffered-byte-string-output-stream>;
    slot unit-clean-stream :: <buffered-byte-string-output-stream>;
    slot unit-real-clean-stream :: <buffered-byte-string-output-stream>;

    slot unit-entry-function :: false-or(<ct-function>), init-value: #f;
    slot unit-unit-info :: <unit-info>;

    // All names of the .o files we generated in a string.
    slot unit-objects :: <byte-string>;

    // The name of the .ar file we generated.
    slot unit-ar-name :: <byte-string>;

    // The name of the executable file we generate.
    slot unit-executable :: false-or(<byte-string>);
end class <main-unit-state>;


// Roots registry.

// Information which needs to go into the library dump file.
//
define class <unit-info> (<object>)

  slot unit-name :: <byte-string>,
    required-init-keyword: #"unit-name";

  slot undumped-objects :: <simple-object-vector>,
    required-init-keyword: #"undumped-objects";

  slot extra-labels :: <simple-object-vector>,
    required-init-keyword: #"extra-labels";

  slot unit-linker-options :: false-or(<byte-string>),
    init-value: #f, init-keyword: #"linker-options";
end class <unit-info>;

define sealed domain make (singleton(<unit-info>));
define sealed domain initialize (<unit-info>);

define variable *units* :: <stretchy-vector> = make(<stretchy-vector>);

define method initialize (info :: <unit-info>, #next next-method, #key) => ();
  next-method();
  add!(*units*, info);
end;
  
add-make-dumper(#"unit-info", *compiler-dispatcher*, <unit-info>,
		list(unit-name, unit-name:, #f,
		     undumped-objects, undumped-objects:, #f,
		     extra-labels, extra-labels:, #f,
		     unit-linker-options, linker-options: #f));


// Compilation driver.

define method file-tokenizer
    (lib :: <library>, name :: <byte-string>)
    => (tokenizer :: <tokenizer>, module :: <module>);
  let source = make(<source-file>, name: name);
  let (header, start-line, start-posn) = parse-header(source);
  values(make(<lexer>,
	      source: source,
	      start-posn: start-posn,
	      start-line: start-line),
	 find-module(lib, as(<symbol>, header[#"module"])));
end;


define method test-lexer (file :: <byte-string>) => ();
  block ()
    let (tokenizer, module) = file-tokenizer($dylan-library, file);
    block (return)
      *Current-Module* := module;
      while (#t)
	let token = get-token(tokenizer);
	if (token.token-kind == $eof-token)
	  return();
	else
	  format(*debug-output*, "%=\n", token);
	end if;
      end while;
    cleanup
      *Current-Module* := #f;
    end block;
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method test-lexer;


define method set-module (module :: type-union(<false>, <module>)) => ();
  *current-module* := module;
end method set-module;

define method set-module (module :: <symbol>) => ();
  block ()
    *current-module*
      := find-module(*Current-Library* | $Dylan-library, module);
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method set-module;

define method set-library (library :: type-union(<false>, <library>)) => ();
  *current-library* := library;
end method set-library;

define method set-library (library :: <symbol>) => ();
  block ()
    *current-library* := find-library(library);
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method set-library;



define method test-parse
    (parser :: <function>, file :: <byte-string>,
     #key debug: debug? :: <boolean>)
    => result :: <object>;
  block ()
    let (tokenizer, module) = file-tokenizer($dylan-library, file);
    let orig-library = *current-library*;
    let orig-module = *current-module*;
    block ()
      *current-library* := $dylan-library;
      *current-module* := module;
      parser(tokenizer, debug: debug?);
    cleanup
      *current-library* := orig-library;
      *current-module* := orig-module;
    end block;
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method test-parse;


define method parse-lid (state :: <main-unit-state>) => ();
  let source = make(<source-file>, name: state.unit-lid-file);
  let (header, start-line, start-posn) = parse-header(source);
  
  let contents = source.contents;
  let end-posn = contents.size;
  let files = make(<stretchy-vector>);

  local
    method repeat (posn :: <integer>)
      if (posn < end-posn)
	let char = as(<character>, contents[posn]);
	if (char.whitespace?)
	  repeat(posn + 1);
	elseif (char == '/' & (posn + 1 < contents.size) 
		  & as(<character>, contents[posn + 1]) == '/')
	  repeat(find-newline(posn + 1));
	else
	  let name-end = find-end-of-word(posn);
	  let len = name-end - posn;
	  let name = make(<byte-string>, size: len);
	  copy-bytes(name, 0, contents, posn, len);
	  add!(files, name);
	  repeat(name-end);
	end;
      end;
    end,
    method find-newline (posn :: <integer>)
     => newline :: <integer>;
      if (posn < end-posn)
	let char = as(<character>, contents[posn]);
	if (char == '\n')
	  posn;
	else
	  find-newline(posn + 1);
	end;
      else
	posn;
      end;
    end method,

    // find-end-of-word returns the position of the first character
    // after the word, where "end of word" is defined as whitespace.
    method find-end-of-word (posn :: <integer>)
     => end-of-word :: <integer>;
      if (posn < end-posn)
	let char = as(<character>, contents[posn]);
	if (char.whitespace?)
	  posn;
	else
	  find-end-of-word(posn + 1);
	end;
      else
	posn;
      end;
    end method;
 
  repeat(start-posn);

  state.unit-header := header;
  state.unit-files := files;
end method parse-lid;
// Considers anything with an ASCII value less than 32 (' ') to be
// whitespace.  This includes control characters as well as what we
// normally consider whitespace.
define method split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  split-at(method (x :: <character>) x <= ' ' end, string);
end method split-at-whitespace;


// Split a string at locations where test returns true, removing the delimiter
// characters.
define method split-at (test :: <function>, string :: <byte-string>)
    => res :: <list>;
  let size = string.size;
  local
    method scan (posn :: <integer>, results :: <list>)
	=> res :: <list>;
      if (posn == size)
	results;
      elseif (test(string[posn]))
	scan(posn + 1, results);
      else
	copy(posn + 1, posn, results);
      end;
    end method scan,
    method copy (posn :: <integer>, start :: <integer>,
		 results :: <list>)
	=> res :: <list>;
      if (posn == size | test(string[posn]))
	scan(posn,
	     pair(copy-sequence(string, start: start, end: posn), results));
      else
	copy(posn + 1, start, results);
      end;
    end method copy;
  reverse!(scan(0, #()));
end method split-at;


define method process-feature (feature :: <byte-string>) => ();
  if (feature.empty? | feature[0] ~== '~')
    add-feature(as(<symbol>, feature));
  else
    remove-feature(as(<symbol>, copy-sequence(feature, start: 1)));
  end if;
end method process-feature;


// Find the library object file (archive) using the data-unit search path.
// There might be more than one possible object file suffix, so we try them
// all, but if we find it under more than one suffix, we error.
//
define method find-library-archive
    (unit-name :: <byte-string>, target :: <platform>)
 => path :: <byte-string>;
  let libname = concatenate(target.library-filename-prefix, unit-name);
  let suffixes = split-at-whitespace(target.library-filename-suffix);
  let found = #();
  for (suffix in suffixes)
    let suffixed = concatenate(libname, suffix);
    let path = find-file(suffixed, *data-unit-search-path*);
    if (path)
      found := pair(path, found);
    end if;
  end for;
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


// save-c-file is #t when we don't want the .c file added to the
// real-clean target.  Used when the C file is actually source code,
// rather than the result of Dylan->C.
//
define method output-c-file-rule
    (state :: <main-unit-state>, c-name :: <string>, o-name :: <string>,
     #key save-c-file = #f)
 => ();

  let cc-command
      = if (member?(c-name, state.unit-override-files, test: \=))
          state.unit-cc-override;
	else
	  state.unit-target.compile-c-command;
	end if;

  format(state.unit-makefile, "%s : %s\n", o-name, c-name);
  format(state.unit-makefile, "\t%s\n",
         format-to-string(cc-command, c-name, o-name));
  format(state.unit-objects-stream, " %s", o-name);
  format(state.unit-clean-stream, " %s", o-name);
  format(state.unit-real-clean-stream, " %s", o-name);
  if (~save-c-file)
    format(state.unit-real-clean-stream, " %s", c-name);
  end if;
end method output-c-file-rule;

// makefile rule for assembly files
//
define method output-s-file-rule
    (state :: <main-unit-state>, s-name :: <string>, o-name :: <string>,
     #key save-c-file = #f)
 => ();
  let assemble-string
    = format-to-string(state.unit-target.assembler-command, s-name, o-name);
  format(state.unit-makefile, "%s : %s\n", o-name, s-name);
  format(state.unit-makefile, "\t%s\n", assemble-string);
  format(state.unit-objects-stream, " %s", o-name);
  format(state.unit-clean-stream, " %s", o-name);
  format(state.unit-real-clean-stream, " %s %s", o-name, s-name);
end method output-s-file-rule;

// This function compares old-filename to new-filename.  If they are
// different, or if one doesn't exist (presumably old-filename), then
// new-filename will be renamed old-filename, and what used to be
// old-filename will be deleted.  Otherwise, new-filename will be
// deleted.  This allows us to avoid unnecessary recompilation of .c
// files.
//
define method pick-which-file
    (old-filename :: <string>, new-filename :: <string>, 
     target :: <platform>)
 => (used-new-file :: <boolean>);
  if (files-identical?(old-filename, new-filename))
    delete-file(new-filename);
    #f;
  else
    rename-file(new-filename, old-filename);
    #t;
  end if;
end method pick-which-file;
     
// Look up a header element with a boolean default.  If specified, the option
// must be "yes" or "no".
//
define function boolean-header-element 
    (name :: <symbol>, default :: <boolean>, state :: <main-unit-state>) 
 => res :: <boolean>;
  let found = element(state.unit-header, name, default: #f);
  if (found)
    select (as-uppercase(found) by \=)
      "YES" => #t;
      "NO" => #f;
      otherwise => 
	compiler-error("%s: header option is %s, not \"yes\" or \"no\".",
		       name, found);
    end select;
  else
    default;
  end if;
end function boolean-header-element;
     
define method parse-and-finalize-library (state :: <main-unit-state>) => ();
    parse-lid(state);
    do(process-feature,
       split-at-whitespace(state.unit-target.default-features));
    do(process-feature,
       split-at-whitespace(element(state.unit-header, #"features",
       				   default: "")));
    do(process-feature, state.unit-command-line-features);

    let lib-name = state.unit-header[#"library"];
    state.unit-lib-name := lib-name;
    format(*debug-output*, "Compiling library %s\n", lib-name);
    let lib = find-library(as(<symbol>, lib-name), create: #t);
    state.unit-lib := lib;
    state.unit-mprefix
      := element(state.unit-header, #"unit-prefix", default: #f) 
           | as-lowercase(lib-name);

    *defn-dynamic-default* := boolean-header-element(#"dynamic", #f, state);
    *implicitly-define-next-method*
      := boolean-header-element(#"implicitly-define-next-method", #f, state);

    for (file in state.unit-files)
      let extension = file.filename-extension;
      if (extension = state.unit-target.object-filename-suffix)
	// Add any random crap to the unit-tlf-vectors so that it will
	// have as many elements as there are files mentioned in the
	// .lid file
	add!(state.unit-tlf-vectors, make(<stretchy-vector>));
	let prefixed-filename 
	  = find-file(file, vector(".", state.unit-lid-file.filename-prefix));
	if (prefixed-filename == #f)
	  compiler-fatal-error("Can't find object file %=, and thus can't"
				 " record dependency info.", 
			       file);
	end if;
	log-dependency(prefixed-filename);
      else  // assumed a Dylan file, with or without a ".dylan" extension
	block ()
	  format(*debug-output*, "Parsing %s\n", file);
	  // ### prefixed-filename is still not (necessarily) an absolute
	  // filename, but it's getting closer
	  let prefixed-filename
	    = find-file(file, vector(".", state.unit-lid-file.filename-prefix));
	  if (prefixed-filename == #f)
	    compiler-fatal-error("Can't find source file %=.", file);
	  end if;
	  log-dependency(prefixed-filename);
	  let (tokenizer, mod) = file-tokenizer(state.unit-lib, 
						prefixed-filename);
	  block ()
	    *Current-Library* := state.unit-lib;
	    *Current-Module* := mod;
	    let tlfs = make(<stretchy-vector>);
	    *Top-Level-Forms* := tlfs;
	    add!(state.unit-tlf-vectors, tlfs);
	    parse-source-record(tokenizer);
	  cleanup
	    *Current-Library* := #f;
	    *Current-Module* := #f;
	  end;
	exception (<fatal-error-recovery-restart>)
	  format(*debug-output*, "skipping rest of %s\n", file);
	end block;
      end if;
    end for;
#if (mindy)
    collect-garbage(purify: #t);
#endif
    format(*debug-output*, "Finalizing definitions\n");
    for (tlfs in state.unit-tlf-vectors)
      *Top-Level-Forms* := tlfs;
      for (tlf in copy-sequence(tlfs))
	note-context(tlf);
	finalize-top-level-form(tlf);
	end-of-context();
      end for;
    end;
    format(*debug-output*, "inheriting slots\n");
    inherit-slots();
    format(*debug-output*, "inheriting overrides\n");
    inherit-overrides();
    begin
      let unique-id-base 
          = element(state.unit-header, #"unique-id-base", default: #f);
      if (unique-id-base)
	format(*debug-output*, "assigning unique ids\n");
	assign-unique-ids(string-to-integer(unique-id-base));
      end;
    end;
    format(*debug-output*, "seeding representations\n");
    seed-representations();
    format(*debug-output*, "laying out instances\n");
    layout-instance-slots();
end method parse-and-finalize-library;


// Open various streams used to build the makefiles that we generate to compile
// the C output code.
define method emit-make-prologue (state :: <main-unit-state>) => ();
  let cc-flags
    = getenv("CCFLAGS") 
        | format-to-string(state.unit-target.default-c-compiler-flags,
			   $runtime-include-dir);

  state.unit-cc-flags := cc-flags;

  state.unit-cback-unit := make(<unit-state>, prefix: state.unit-mprefix);
  state.unit-other-cback-units := map-as(<simple-object-vector>, unit-name, 
					 *units*);

  let makefile-name = format-to-string("cc-%s-files.mak", state.unit-mprefix);
  let temp-makefile-name = concatenate(makefile-name, "-temp");
  state.unit-makefile-name := makefile-name;
  state.unit-temp-makefile-name := temp-makefile-name;
  format(*debug-output*, "Creating %s\n", makefile-name);
  let makefile = make(<file-stream>, locator: temp-makefile-name,
		      direction: #"output", if-exists: #"overwrite");
  state.unit-makefile := makefile;
  format(makefile, "# Makefile for compiling the .c and .s files\n");
  format(makefile, "# If you want to compile .dylan files, don't use "
	   "this makefile.\n\n");
  format(makefile, "CCFLAGS = %s\n", cc-flags);

  format(makefile, "# We only know the ultimate target when we've finished"
	   " building the rest\n");
  format(makefile, "# of this makefile.  So we use this fake "
	   "target...\n#\n");
  format(makefile, "all : all-at-end-of-file\n\n");

  // These next three streams gather filenames.  Objects-stream is
  // simply *.o.  clean-stream is the list of files we will delete
  // with the "clean" target--all objects plus the library archive
  // (.a), the library summary (.du), and the executable.
  // real-clean-stream is everything in clean plus *.c, *.s, and
  // cc-lib-files.mak.
  //
  state.unit-objects-stream := make(<buffered-byte-string-output-stream>);
  state.unit-clean-stream := make(<buffered-byte-string-output-stream>);
  state.unit-real-clean-stream := make(<buffered-byte-string-output-stream>);
  format(state.unit-real-clean-stream, " %s", makefile-name);
end method emit-make-prologue;


// The actual meat of compilation.  Does FER conversion, optimizes and emits
// output code.
//
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
  force-output(*debug-output*);
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
    make-function-literal(builder, ctv, #f, #"global", sig, init-function);
    add!(state.unit-init-functions, ctv);
  end;
  optimize-component(component);
  emit-tlf-gunk(tlf, file);
  emit-component(component, file);
end method compile-1-tlf;


// Establish various condition handlers while iterating over all of the source
// files and compiling each of them to an output file.
//
define method compile-all-files (state :: <main-unit-state>) => ();
  for (file in state.unit-files, tlfs in state.unit-tlf-vectors)
    let extension = file.filename-extension;
    if (extension = state.unit-target.object-filename-suffix)
      format(*debug-output*, "Adding %s\n", file);
      format(state.unit-objects-stream, " %s", file);
    else  // assumed a Dylan file, with or without a ".dylan" extension
      block ()
	format(*debug-output*, "Processing %s\n", file);
	let base-name = file.base-filename;
	let c-name = concatenate(base-name, ".c");
	let temp-c-name = concatenate(c-name, "-temp");
	let body-stream
	  = make(<file-stream>, locator: temp-c-name, direction: #"output");
	block ()
	  let file = make(<file-state>, unit: state.unit-cback-unit,
			  body-stream: body-stream);
	  emit-prologue(file, state.unit-other-cback-units);
	  for (tlf in tlfs)
	    block ()
	      compile-1-tlf(tlf, file, state);
	    cleanup
	      end-of-context();
	    exception (<fatal-error-recovery-restart>)
	      #f;
	    end block;
	  end for;
	cleanup
	  close(body-stream);
	  fresh-line(*debug-output*);
	end block;

	pick-which-file(c-name, temp-c-name, state.unit-target);
	let o-name
	  = concatenate(base-name, state.unit-target.object-filename-suffix);
	state.unit-all-generated-files
	  := add!(state.unit-all-generated-files, c-name);
	output-c-file-rule(state, c-name, o-name);

      exception (<fatal-error-recovery-restart>)
	format(*debug-output*, "skipping rest of %s\n", file);
      exception (<simple-restart>,
		   init-arguments:
		   vector(format-string: "Blow off compiling this file."))
	#f;
      end block;
    end if;
  end for;
end method compile-all-files;


// Build initialization function for this library, generate the corresponding
// .c and .o and update the make file.
// 
define method build-library-inits (state :: <main-unit-state>) => ();
    let executable = element(state.unit-header, #"executable", default: #f);
    let executable
     = if (executable)
	 concatenate(executable, state.unit-target.executable-filename-suffix);
       else 
	 #f;
       end if;
    state.unit-executable := executable;
    let entry-point = element(state.unit-header, #"entry-point", default: #f);
    if (entry-point & ~executable)
      compiler-fatal-error("Can only specify an entry-point when producing an "
			     "executable.");
    end if;

    begin
      let c-name = concatenate(state.unit-mprefix, "-init.c");
      let temp-c-name = concatenate(c-name, "-temp");
      let body-stream = make(<file-stream>, 
			     locator: temp-c-name, direction: #"output");
      let file = make(<file-state>, unit: state.unit-cback-unit,
      		      body-stream: body-stream);
      emit-prologue(file, state.unit-other-cback-units);
      if (entry-point)
        state.unit-entry-function
	  := build-command-line-entry(state.unit-lib, entry-point, file);
      end if;
      build-unit-init-function(state.unit-mprefix, state.unit-init-functions,
			       body-stream);
      close(body-stream);

      pick-which-file(c-name, temp-c-name, state.unit-target);
      let o-name = concatenate(state.unit-mprefix, "-init", 
			       state.unit-target.object-filename-suffix);
      output-c-file-rule(state, c-name, o-name);
      state.unit-all-generated-files 
	:= add!(state.unit-all-generated-files, c-name);
    end;
end method build-library-inits;


define method build-local-heap-file (state :: <main-unit-state>) => ();
  format(*debug-output*, "Emitting Library Heap.\n");
  let s-name = concatenate(state.unit-mprefix, "-heap.s");
  let temp-s-name = concatenate(s-name, "-temp");
  let heap-stream = make(<file-stream>, 
			 locator: temp-s-name, direction: #"output");
  let (undumped, extra-labels) = build-local-heap(state.unit-cback-unit, 
						  heap-stream, 
						  state.unit-target);
  close(heap-stream);

  pick-which-file(s-name, temp-s-name, state.unit-target);
  let o-name = concatenate(state.unit-mprefix, "-heap", 
			   state.unit-target.object-filename-suffix);
  output-s-file-rule(state, s-name, o-name);
  state.unit-all-generated-files 
    := add!(state.unit-all-generated-files, s-name);

  let linker-options = element(state.unit-header, #"linker-options", 
			       default: #f);
  state.unit-unit-info := make(<unit-info>, unit-name: state.unit-mprefix,
			       undumped-objects: undumped,
			       extra-labels: extra-labels,
			       linker-options: linker-options);
end method build-local-heap-file;


define method build-ar-file (state :: <main-unit-state>) => ();
  let objects = stream-contents(state.unit-objects-stream);
  let target = state.unit-target;
  let suffix = split-at-whitespace(target.library-filename-suffix).first;
  let ar-name = concatenate(target.library-filename-prefix,
  			    state.unit-mprefix,
			    suffix);

  state.unit-objects := objects;
  state.unit-ar-name := ar-name;
  format(state.unit-makefile, "\n%s : %s\n", ar-name, objects);
  format(state.unit-makefile, "\t%s %s\n",
	 target.delete-file-command, ar-name);
  
  let objects = use-correct-path-separator(objects, target);

  let link-string = format-to-string(target.link-library-command, 
				     ar-name, objects);
  format(state.unit-makefile, "\t%s\n", link-string);
end method;


define method build-da-global-heap (state :: <main-unit-state>) => ();
    format(*debug-output*, "Emitting Global Heap.\n");
    let heap-stream 
      = make(<file-stream>, locator: "heap.s", direction: #"output");
    build-global-heap(apply(concatenate, map(undumped-objects, *units*)),
		      heap-stream, state.unit-target);
    close(heap-stream);
end method;


define method build-inits-dot-c (state :: <main-unit-state>) => ();
  format(*debug-output*, "Building inits.c.\n");
  let stream
    = make(<file-stream>, locator: "inits.c", direction: #"output");
  write(stream, "#include <runtime.h>\n\n");
  write(stream, 
	"/* This file is machine generated.  Do not edit. */\n\n");
  let entry-function-name
    = (state.unit-entry-function
	 & (make(<ct-entry-point>, for: state.unit-entry-function,
	 	 kind: #"main")
	      .entry-point-c-name));
  if (entry-function-name)
    format(stream,
	   "extern void %s(descriptor_t *sp, int argc, void *argv);\n\n",
	   entry-function-name);
  end if;
  write(stream,
	"void inits(descriptor_t *sp, int argc, char *argv[])\n{\n");
  for (unit in *units*)
    format(stream, "    %s_Library_init(sp);\n", unit.unit-name);
  end;
  if (entry-function-name)
    format(stream, "    %s(sp, argc, argv);\n", entry-function-name);
  end if;
  write(stream, "}\n");
  write(stream, "\nextern void real_main(int argc, char *argv[]);\n\n");
  write(stream, "void main(int argc, char *argv[])\n{\n");
  write(stream, "    real_main(argc, argv);\n}\n");
  close(stream);
end method;

define function use-correct-path-separator
    (string :: <byte-string>, target :: <platform>) 
 => new-string :: <byte-string>;
  map(method (c :: <character>) => new-char :: <character>;
	if (c == '/') target.path-separator else c end if;
      end method,
      string);
end function use-correct-path-separator;

define method build-executable (state :: <main-unit-state>) => ();
  let unit-libs = "";
  let dash-small-ells = "";
  let linker-args = concatenate(" ", state.unit-target.link-executable-flags);

  local method add-archive (name :: <byte-string>) => ();
          if (state.unit-no-binaries)
	    // If cross-compiling use -l -L search mechanism.
	    dash-small-ells := stringify(" -l", name, dash-small-ells);
	  else
	    let archive = find-library-archive(name, state.unit-target);
	    unit-libs := stringify(' ', archive, unit-libs);
	  end if;
	end method add-archive;

  // Under Unix, the order of the libraries is significant!  First to
  // be added go at the end of the command line...
  add-archive("gc");
  add-archive("runtime");

  for (unit in *units*)
    if (unit.unit-linker-options)
      linker-args
	:= stringify(' ', unit.unit-linker-options, linker-args);
    end if;
    unless (unit == state.unit-unit-info)
      add-archive(unit.unit-name);
    end unless;
  end;

  // We make sure the linker is not given any source files, only
  // library and object files
  format(state.unit-makefile, "\n");
  let inits-dot-o 
    = concatenate("inits", state.unit-target.object-filename-suffix);
  let heap-dot-o
    = concatenate("heap", state.unit-target.object-filename-suffix);
  output-c-file-rule(state, "inits.c", inits-dot-o);
  output-s-file-rule(state, "heap.s", heap-dot-o);

  let dash-cap-ells = "";
  // If cross-compiling, throw in a bunch of -Ls that will probably help.
  if (state.unit-no-binaries)
    for (dir in *data-unit-search-path*)
      dash-cap-ells := concatenate(dash-cap-ells, " -L", dir);
    end for;
    dash-cap-ells
      := concatenate(" $(LDFLAGS)",
      		     use-correct-path-separator(dash-cap-ells,
		     				state.unit-target),
		     " ");						

  end;

  let unit-libs = use-correct-path-separator(unit-libs, state.unit-target);

  // Again, make sure inits.o and heap.o come first
  let objects = format-to-string("%s %s %s %s", inits-dot-o, heap-dot-o, 
				 state.unit-ar-name, unit-libs);

  // rule to link executable
  format(state.unit-makefile, "\n%s : %s\n", state.unit-executable, objects);
  let link-string
    = format-to-string(state.unit-target.link-executable-command,
		       state.unit-executable,
		       concatenate(dash-cap-ells, dash-small-ells, objects),
		       linker-args);
  format(state.unit-makefile, "\t%s\n", link-string);

  format(state.unit-clean-stream, " %s %s", 
	 state.unit-ar-name, state.unit-executable);
  format(state.unit-real-clean-stream, " %s %s", 
	 state.unit-ar-name, state.unit-executable);
  format(state.unit-makefile, "\nall-at-end-of-file : %s\n", 
	 state.unit-executable);
end method build-executable;


define method dump-library-summary (state :: <main-unit-state>) => ();
  format(*debug-output*, "Dumping library summary.\n");
  let dump-buf
    = begin-dumping(as(<symbol>, state.unit-lib-name),
    		    $library-summary-unit-type);

  for (tlfs in state.unit-tlf-vectors)
    for (tlf in tlfs)
      dump-od(tlf, dump-buf);
    end;
  end;
  dump-od(state.unit-unit-info, dump-buf);
  dump-queued-methods(dump-buf);

  end-dumping(dump-buf);
  format(state.unit-makefile, "\nall-at-end-of-file : %s\n",
  	 state.unit-ar-name);
  format(state.unit-clean-stream, " %s", state.unit-ar-name);
  format(state.unit-real-clean-stream, " %s %s.lib.du", state.unit-ar-name, 
	 as-lowercase(state.unit-lib-name));
end method;


define method do-make (state :: <main-unit-state>) => ();
  let target = state.unit-target;
  format(state.unit-makefile, "\nclean :\n");
  format(state.unit-makefile, "\t%s %s\n", target.delete-file-command, 
	 state.unit-clean-stream.stream-contents);
  format(state.unit-makefile, "\nrealclean :\n");
  format(state.unit-makefile, "\t%s %s\n", target.delete-file-command, 
	 state.unit-real-clean-stream.stream-contents);
  close(state.unit-makefile);

  if (pick-which-file(state.unit-makefile-name,
		      state.unit-temp-makefile-name,
		      target)
	= #t)
    // If the new makefile is different from the old one, then we need
    // to recompile all .c and .s files, regardless of whether they
    // were changed.  So touch them to make them look newer than the
    // object files.
    unless (empty?(state.unit-all-generated-files))
      let touch-command = "touch";
      for (filename in state.unit-all-generated-files)
	touch-command := stringify(touch-command, ' ', filename);
      end for;
      format(*debug-output*, "%s\n", touch-command);
      if (system(touch-command) ~== 0)
	cerror("so what", "touch failed?");
      end if;
    end unless;
  end if;

  if (~state.unit-no-binaries)
    let make-string = format-to-string("%s -f %s", target.make-command, 
				       state.unit-makefile-name);
    format(*debug-output*, "%s\n", make-string);
    unless (zero?(system(make-string)))
      cerror("so what", "gmake failed?");
    end;
  end if;
end method do-make;


define method compile-library (state :: <main-unit-state>)
    => worked? :: <boolean>;
  block (give-up)
    // We don't really have to give-up if we don't want to, but it
    // seems kind of pointless to compile a file that doesn't parse,
    // or create a dump file for library with undefined variables.
    // Thus, we stick some calls to give-up where it seems useful..
    parse-and-finalize-library(state);
    if (~ zero?(*errors*)) give-up(); end if;
    emit-make-prologue(state);
    compile-all-files(state);
    if (~ zero?(*errors*)) give-up(); end if;
    build-library-inits(state);
    build-local-heap-file(state);
    build-ar-file(state);
    if (state.unit-executable)
      log-target(state.unit-executable);
      build-da-global-heap(state);
      build-inits-dot-c(state);
      build-executable(state);
    else
      dump-library-summary(state);
    end if;

    if (state.unit-log-dependencies)
      spew-dependency-log(concatenate(state.unit-mprefix, ".dep"));
    end if;

    do-make(state);

  exception (<fatal-error-recovery-restart>)
    format(*debug-output*, "giving up.\n");
  end block;
  
  format(*debug-output*, "Optimize called %d times.\n", *optimize-ncalls*);

  let worked? = zero?(*errors*);
  format(*debug-output*,
	 "Compilation %s with %d Warning%s and %d Error%s\n",
	 if (worked?) "finished" else "failed" end,
	 *warnings*, if (*warnings* == 1) "" else "s" end,
	 *errors*, if (*errors* == 1) "" else "s" end);

  worked?;
end method compile-library;

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
  let init-func-guts = emit-init-functions(prefix, init-functions,
					   0, init-functions.size, stream);
  // The function this generated used to be called simply "%s_init",
  // but that conflicted with the heap object of the same name.  (Of
  // course, on the HP, the linker has separate namespaces for code
  // and data, but most other platforms do not)
  format(stream, "void %s_Library_init(descriptor_t *sp)\n{\n%s}\n",
	 prefix, init-func-guts);
end;


define method split-at-colon (string :: <byte-string>)
    => (module :: <byte-string>, name :: <byte-string>);
  block (return)
    for (index :: <integer> from 0 below string.size)
      if (string[index] == ':')
	return(copy-sequence(string, end: index),
	       copy-sequence(string, start: index + 1));
      end if;
    end for;
    compiler-fatal-error
      ("Invalid entry point: %s -- must be of the form module:variable.",
       string);
  end block;
end method split-at-colon;


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
  make-function-literal(builder, ctv, #f, #"global", sig, func);
  optimize-component(component);
  emit-component(component, file);
  ctv;
end method build-command-line-entry;

define method incorrect-usage () => ();
  format(*standard-error*, "Usage: \n    compile [-Ldir ...] lid-file\n");
  format(*standard-error*, "Options:\n");
  format(*standard-error*, 
	 "    -Ldir               Search for library files in dir\n");
  format(*standard-error*, 
	 "    -Dfeature           Define feature for #if conditional compilation\n");
  format(*standard-error*, 
	 "    -Ufeature           Undefine feature for #if conditional compilation\n");
  format(*standard-error*, 
	 "    -M                  Generate makefile dependencies\n");
  format(*standard-error*, 
	 "    -no-binaries        Do not compile the generated C code\n");
  format(*standard-error*, 
	 "    -Ttarget            Generate code for the given target machine\n");
  format(*standard-error*, 
	 "                        Often used with -no-binaries\n");
  format(*standard-error*, 
	 "    -pfilename          Get platform information from \n"
	 "                        filename instead of the default platforms.descr\n");
  format(*standard-error*, 
	 "    -d                  Compiler debug mode (for debugging this compiler)\n");
  format(*standard-error*, 
	 "    -Fformat-string     Alternate format string for running the C "
	 "compiler\n"
	 "                        Used when a C file named by -f is compiled.\n");
  format(*standard-error*, 
	 "    -fcfilename         Names a C output file to compile specially.\n");
  format(*standard-error*, 
	 "    -g                  Emit all function objects\n");
  force-output(*standard-error*);
  exit(exit-code: 1);
end method incorrect-usage;

define constant $search-path-seperator =
#if (compiled-for-win32)
  ';';
#else
  ':';
#endif

// Where to find various important files.

// $default-dylan-dir and $default-target-name are defined in
// file-locations.dylan, which is generated by makegen.

// If DYLANDIR is defined, then it is assumed to be the root of the install
// area, and the location of platforms.descr and the libraries are derived from
// there.  Otherwise we use the autoconf prefix @prefix@.  It would be nice to
// use libdir, etc., but the default substitutions contain ${prefix}
// variables, which Dylan doesn't have yet.

define constant $dylan-dir = getenv("DYLANDIR") | $default-dylan-dir;

// Platform parameter database.
define constant $default-targets-dot-descr
  = concatenate($dylan-dir, "/etc/platforms.descr");

// Library search path.
define constant $default-dylan-path
  = concatenate(".:", $dylan-dir, "/lib/dylan/");

// Location of runtime.h
define constant $runtime-include-dir
  = concatenate($dylan-dir, "/include/");

define method main (argv0 :: <byte-string>, #rest args) => ();
  #if (~mindy)
  no-core-dumps();
  #endif
  *random-state* := make(<random-state>, seed: 0);
  define-bootstrap-module();
  let library-dirs = make(<stretchy-vector>);
  let lid-file = #f;
  let features = #();
  let log-dependencies = #f;
  let target-machine = $default-target-name;
  let no-binaries = #f;
  let targets-file = $default-targets-dot-descr;
  let cc-override = #f;
  let override-files = #();
  for (arg in args)
    if (arg.size >= 1 & arg[0] == '-')
      if (arg.size >= 2)
	select (arg[1])
	  'L' =>
	    if (arg.size > 2)
	      add!(library-dirs, copy-sequence(arg, start: 2));
	    else
	      error("Directory not supplied with -L.");
	    end;
	  'D' =>
	    if (arg.size > 2)
	      features := pair(copy-sequence(arg, start: 2), features);
	    else
	      error("Feature to define not supplied with -D.");
	    end;
	  'U' =>
	    if (arg.size > 2)
	      let feature = copy-sequence(arg, start: 1);
	      feature[0] := '~';
	      features := pair(feature, features);
	    else
	      error("Feature to undefine not supplied with -U.");
	    end;
	  'M' =>
	    if (arg.size > 2)
	      error("Bogus switch: %s", arg);
	    end if;
	    log-dependencies := #t;
	  'T' =>
	    if (arg.size > 2)
	      target-machine := as(<symbol>, copy-sequence(arg, start: 2));
	    else
	      error("Target environment not supplied with -T.");
	    end if;
	  'p' =>
	    if (arg.size > 2)
	      targets-file := copy-sequence(arg, start: 2);
	    else
	      error("Name of targets description file not supplied with -p.");
	    end if;
	  'n' =>
	    if (arg = "-no-binaries")  // We need this switch to keep gmake
	                               // from deleting dylan.lib.du when 
	                               // gmake -f cc-dylan-files.mak fails
	      no-binaries := #t;
	    else
	      error("Bogus switch: %s", arg);
	    end if;
	  'd' =>
	    if (arg.size == 2)
	      *break-on-compiler-errors* := #t;
	    else
	      error("Bogus switch: %s", arg);
	    end if;
	  'F' =>
	    cc-override := copy-sequence(arg, start: 2);
	  'f' =>
	    if (arg.size > 2)
	      override-files := pair(copy-sequence(arg, start: 2),
	      			     override-files);
	    else
	      error("Name of override file not supplied with -f.");
	    end if;
	  'g' =>
	    *emit-all-function-objects?* := #t;
	  otherwise =>
	    error("Bogus switch: %s", arg);
	end select;
      else
	error("Bogus switch: %s", arg);
      end;
    else
      if (lid-file)
	error("Multiple LID files: %s and %s", lid-file, arg);
      else
	lid-file := arg;
      end;
    end;
  end;

  unless (lid-file)
    incorrect-usage();
  end;
  if (targets-file == #f)
    error("Can't find platforms.descr");
  end if;
  let possible-targets = get-platforms(targets-file);
  if (~key-exists?(possible-targets, target-machine))
    error("Unknown platform %=.", target-machine);
  end if;
  let target = possible-targets[target-machine];
  *current-target* := target;

  // Stuff in DYLANPATH goes after any explicitly listed directories.
  let dylanpath = getenv("DYLANPATH") | $default-dylan-path;
  let dirs = split-at(method (x :: <character>);
			x == $search-path-seperator;
		      end,
		      dylanpath);
  for (dir in dirs)
    add!(library-dirs, dir);
  end for;
  		       
  *Data-Unit-Search-Path* := as(<simple-object-vector>, library-dirs);

  let state
      = make(<main-unit-state>,
             lid-file: lid-file,
	     command-line-features: reverse!(features), 
	     log-dependencies: log-dependencies,
	     target: target,
	     no-binaries: no-binaries,
	     cc-override: cc-override,
	     override-files: override-files);
  let worked? = compile-library(state);
  exit(exit-code: if (worked?) 0 else 1 end);
end method main;

#if (mindy)
collect-garbage(purify: #t);
#endif
