module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.64 1996/04/10 17:01:56 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


#if (~mindy)

define method import-string (ptr :: <raw-pointer>)
    => string :: <byte-string>;
  for (len :: <integer> from 0,
       until: zero?(pointer-deref(#"char", ptr, len)))
  finally
    let res = make(<byte-string>, size: len);
    for (index :: <integer> from 0 below len)
      res[index] := as(<character>, pointer-deref(#"char", ptr, index));
    end for;
    res;
  end for;
end method import-string;

define method export-string (string :: <byte-string>)
    => ptr :: <raw-pointer>;
  let len = string.size;
  let buffer = make(<buffer>, size: len + 1);
  copy-bytes(buffer, 0, string, 0, len);
  buffer[len] := 0;
  buffer-address(buffer);
end method export-string;

define method getenv (name :: <byte-string>)
    => res :: false-or(<byte-string>);
  let ptr = call-out("getenv", #"ptr", #"ptr", export-string(name));
  if (zero?(as(<integer>, ptr)))
    #f;
  else
    import-string(ptr);
  end if;
end method getenv;

define method system (command :: <byte-string>)
    => res :: <integer>;
  call-out("system", #"int", #"ptr", export-string(command));
end method system;

define method exit (#key exit-code :: <integer> = 0)
    => res :: <never-returns>;
  call-out("exit", void:, int:, exit-code);
end method exit;

#end

define constant $cc-utility = "gcc";   // I don't know if cc would also work
define constant $cc-flags = getenv("CCFLAGS") | "";
define constant $ar-utility = "/bin/ar";
define constant $make-utility = "gmake";  // "make" should also work


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
    (lib :: <library>, name :: <byte-string>, dir :: false-or(<byte-string>))
    => (tokenizer :: <tokenizer>, module :: <module>);
  let source = make(<source-file>, name: name, directory: dir);
  let (header, start-line, start-posn) = parse-header(source);
  values(make(<lexer>,
	      source: source,
	      start-posn: start-posn,
	      start-line: start-line),
	 find-module(lib, as(<symbol>, header[#"module"])));
end;


define method test-lexer (file :: <byte-string>) => ();
  block ()
    let (tokenizer, module) = file-tokenizer($dylan-library, file, #f);
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
    let (tokenizer, module) = file-tokenizer($dylan-library, file, #f);
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



define method strip-extension
    (name :: <byte-string>, extension :: <byte-string>)
    => res :: false-or(<byte-string>);
  if (name.size < extension.size)
    #f;
  else
    block (return)
      for (char in extension, index from name.size - extension.size)
	unless (name[index] == char)
	  return(#f);
	end;
      end;
      copy-sequence(name, start: 0, end: name.size - extension.size);
    end;
  end;
end;



define method parse-lid (lid-file :: <byte-string>)
    => (header :: <header>, files :: <vector>);
  let source = make(<source-file>, name: lid-file);
  let (header, start-line, start-posn) = parse-header(source);
  
  let contents = source.contents;
  let end-posn = contents.size;
  let files = make(<stretchy-vector>);

  local
    method repeat (posn :: <integer>)
      if (posn < end-posn)
	let char = as(<character>, contents[posn]);
	if (char == '\n')
	  repeat(posn + 1);
	elseif (char == '#')
	  repeat(find-newline(posn + 1));
	else
	  let name-end = find-newline(posn + 1);
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
	if (char ~== '\n')
	  find-newline(posn + 1);
	else
	  posn;
	end;
      else
	posn;
      end;
    end method;

  repeat(start-posn);

  values(header, files);
end;


define method split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  let size = string.size;
  local
    method scan (posn :: <integer>, results :: <list>)
	=> res :: <list>;
      if (posn == size)
	results;
      elseif (string[posn] <= ' ')
	scan(posn + 1, results);
      else
	copy(posn + 1, posn, results);
      end;
    end method scan,
    method copy (posn :: <integer>, start :: <integer>,
		 results :: <list>)
	=> res :: <list>;
      if (posn == size | string[posn] <= ' ')
	scan(posn,
	     pair(copy-sequence(string, start: start, end: posn), results));
      else
	copy(posn + 1, start, results);
      end;
    end method copy;
  reverse!(scan(0, #()));
end method split-at-whitespace;

define method process-feature (feature :: <byte-string>) => ();
  if (feature.empty? | feature[0] ~== '~')
    add-feature(as(<symbol>, feature));
  else
    remove-feature(as(<symbol>, copy-sequence(feature, start: 1)));
  end if;
end method process-feature;

define method extract-directory (path :: <byte-string>)
    => dir :: <byte-string>;
  block (return)
    for (index from path.size - 1 to 0 by -1)
      if (path[index] == '/')
	return(copy-sequence(path, end: index + 1));
      end if;
    end for;
    "";
  end block;
end method extract-directory;


define method find-source-file
    (file :: <byte-string>, source-path :: <byte-string>)
    => (name :: <byte-string>, dir :: false-or(<byte-string>));
  block (return)
    local
      method try (name :: <byte-string>, dir :: false-or(<byte-string>)) => ();
	block ()
	  close(make(<file-stream>, name: name));
	  return(name, dir);
	exception (<file-not-found>)
	  #f;
	end block;
      end method try;
    try(file, #f);
    try(concatenate(source-path, file), source-path);
    compiler-fatal-error("Can't find source file %=.", file);
  end block;
end method find-source-file;

define method output-c-file-rule
    (makefile :: <stream>, c-name :: <string>, o-name :: <string>) => ();
  let cc-command
    = format-to-string("$(CC) $(CCFLAGS) -c %s -o %s", c-name, o-name);
  format(makefile, "%s : %s\n", o-name, c-name);
  format(makefile, "\t%s\n", cc-command);
end method output-c-file-rule;

// This function compares old-filename to new-filename.  If they are
// different, or if one doesn't exist (presumably old-filename), then
// new-filename will be renamed old-filename, and what used to be
// old-filename will be deleted.  Otherwise, new-filename will be
// deleted.  This allows us to avoid unnecessary recompilation of .c
// files.
//
define method pick-which-file
    (old-filename :: <string>, new-filename :: <string>)
 => (used-new-file :: <boolean>);
  let cmp-command = format-to-string("cmp -s %s %s", 
				     old-filename, new-filename);
  // cmp will return non-zero if they are different, if a file's not
  // found, or if cmp somehow fails to execute.
  if (system(cmp-command) ~== 0)
    let mv-command = format-to-string("mv -f %s %s", 
				      new-filename, old-filename);
    if (system(mv-command) ~== 0)
      cerror("so what", "mv failed?");
    end if;
    #t;
  else
    let rm-command = format-to-string("rm -f %s", new-filename);
    if (system(rm-command) ~== 0)
      cerror("so what", "rm failed?");
    end if;
    #f;
  end if;
end method pick-which-file;

define method compile-library
    (lid-file :: <byte-string>, command-line-features :: <list>,
     log-dependencies :: <boolean>)
    => worked? :: <boolean>;
  block ()

    let (header, files) = parse-lid(lid-file);

    do(process-feature,
       split-at-whitespace(element(header, #"features", default: "")));
    do(process-feature, command-line-features);

    let lib-name = header[#"library"];
    format(*debug-output*, "Compiling library %s\n", lib-name);
    let lib = find-library(as(<symbol>, lib-name), create: #t);
    let unit-prefix
      = element(header, #"unit-prefix", default: #f) | as-lowercase(lib-name);
    let tlf-vectors = make(<stretchy-vector>);
    let source-path = extract-directory(lid-file);
    for (file in files)
      block ()
	format(*debug-output*, "Parsing %s\n", file);
	let (name, dir) = find-source-file(file, source-path);
	log-dependency(name);
	let (tokenizer, mod) = file-tokenizer(lib, file, dir);
	block ()
	  *Current-Library* := lib;
	  *Current-Module* := mod;
	  let tlfs = make(<stretchy-vector>);
	  *Top-Level-Forms* := tlfs;
	  add!(tlf-vectors, tlfs);
	  parse-source-record(tokenizer);
	cleanup
	  *Current-Library* := #f;
	  *Current-Module* := #f;
	end;
      exception (<fatal-error-recovery-restart>)
	format(*debug-output*, "skipping rest of %s\n", file);
      end block;
    end;
#if (mindy)
    collect-garbage(purify: #t);
#end
    format(*debug-output*, "Finalizing definitions\n");
    for (tlfs in tlf-vectors)
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
      let unique-id-base = element(header, #"unique-id-base", default: #f);
      if (unique-id-base)
	format(*debug-output*, "assigning unique ids\n");
	assign-unique-ids(string-to-integer(unique-id-base));
      end;
    end;
    format(*debug-output*, "seeding representations\n");
    seed-representations();
    format(*debug-output*, "assigning slot representations\n");
    assign-slot-representations();
    format(*debug-output*, "laying out instances\n");
    layout-instance-slots();
    let init-functions = make(<stretchy-vector>);
    let unit = make(<unit-state>, prefix: unit-prefix);
    let objects-stream = make(<byte-string-output-stream>);
    let other-units = map-as(<simple-object-vector>, unit-name, *units*);
    let all-generated-files = #();

    let makefile-name = format-to-string("cc-%s-files.mak", unit-prefix);
    let temp-makefile-name = concatenate(makefile-name, "-temp");
    format(*debug-output*, "Creating %s\n", temp-makefile-name);
    let makefile = make(<file-stream>, name: temp-makefile-name,
			direction: #"output", if-exists: #"overwrite");
    format(makefile, "# Makefile for compiling the .c and .s files\n");
    format(makefile, "# If you want to compile .dylan files, don't use "
	     "this makefile.\n\n");
    format(makefile, "CC = %s\n", $cc-utility);
    format(makefile, "CCFLAGS = %s\n", $cc-flags);
    format(makefile, "AR = %s\n\n", $ar-utility);

    format(makefile, "# We only know the ultimate target when we've finished"
	     " building the rest\n");
    format(makefile, "# of this makefile.  So we use this fake "
	     "target...\n#\n");
    format(makefile, "all : all-at-end-of-file\n\n");

    for (file in files, tlfs in tlf-vectors)
      block ()
	format(*debug-output*, "Processing %s\n", file);
	let base-name = strip-extension(file, ".dylan") | file;
	let c-name = concatenate(base-name, ".c");
	let temp-c-name = concatenate(c-name, "-temp");
	let body-stream
	  = make(<file-stream>, name: temp-c-name, direction: #"output");
	block ()
	  let file = make(<file-state>, unit: unit, body-stream: body-stream);
	  emit-prologue(file, other-units);

	  for (tlf in tlfs)
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
	    unless (instance?(inits, <empty-region>))
	      let result-type = make-values-ctype(#(), #f);
	      let source = make(<source-location>);
	      let init-function
		= build-function-body(builder, $Default-Policy, source, #f, name,
				      #(), result-type, #t);
	      build-region(builder, inits);
	      build-return(builder, $Default-Policy, source, init-function, #());
	      end-body(builder);
	      let sig = make(<signature>, specializers: #(),
			     returns: result-type);
	      let ctv = make(<ct-function>, name: name, signature: sig);
	      make-function-literal(builder, ctv, #f, #"global", sig,
				    init-function);
	      add!(init-functions, ctv);
	    end;
	    optimize-component(component);
	    emit-tlf-gunk(tlf, file);
	    emit-component(component, file);
	    end-of-context();
	  end;
	cleanup
	  close(body-stream);
	  fresh-line(*debug-output*);
	end;

	pick-which-file(c-name, temp-c-name);
	let o-name = concatenate(base-name, ".o");
	output-c-file-rule(makefile, c-name, o-name);
	all-generated-files := add!(all-generated-files, c-name);
	format(objects-stream, " %s", o-name);

      exception (<simple-restart>,
		   init-arguments:
		   vector(format-string: "Blow off compiling this file."))
	#f;
      end block;
    end for;

    let executable = element(header, #"executable", default: #f);
    let entry-point = element(header, #"entry-point", default: #f);
    let entry-function = #f;
    if (entry-point & ~executable)
      compiler-fatal-error("Can only specify an entry-point when producing an "
			     "executable.");
    end if;

    begin
      let c-name = concatenate(unit-prefix, "-init.c");
      let temp-c-name = concatenate(c-name, "-temp");
      let body-stream = make(<file-stream>, 
			     name: temp-c-name, direction: #"output");
      let file = make(<file-state>, unit: unit, body-stream: body-stream);
      emit-prologue(file, other-units);
      if (entry-point)
	entry-function := build-command-line-entry(lib, entry-point, file);
      end if;
      build-unit-init-function(unit-prefix, init-functions, body-stream);
      close(body-stream);

      pick-which-file(c-name, temp-c-name);
      let o-name = concatenate(unit-prefix, "-init.o");
      output-c-file-rule(makefile, c-name, o-name);
      all-generated-files := add!(all-generated-files, c-name);
      format(objects-stream, " %s", o-name);
    end;

    format(*debug-output*, "Emitting Library Heap.\n");
    let s-name = concatenate(unit-prefix, "-heap.s");
    let temp-s-name = concatenate(s-name, "-temp");
    let heap-stream = make(<file-stream>, 
			   name: temp-s-name, direction: #"output");
    let (undumped, extra-labels) = build-local-heap(unit, heap-stream);
    close(heap-stream);

    // Even though this isn't a .c file, we call the compiler the same
    pick-which-file(s-name, temp-s-name);
    let o-name = concatenate(unit-prefix, "-heap.o");
    output-c-file-rule(makefile, s-name, o-name);
    all-generated-files := add!(all-generated-files, s-name);
    format(objects-stream, " %s", o-name);

    let objects = string-output-stream-string(objects-stream);
    
    let ar-name = format-to-string("lib%s.a", unit-prefix);
    format(makefile, "\n%s : %s\n", ar-name, objects);
    format(makefile, "\trm -f %s\n", ar-name);
    format(makefile, "\t$(AR) qc %s%s\n", ar-name, objects);

    let linker-options = element(header, #"linker-options", default: #f);
    let unit-info = make(<unit-info>,
			 unit-name: unit-prefix,
			 undumped-objects: undumped,
			 extra-labels: extra-labels,
			 linker-options: linker-options);

    if (executable)
      log-target(executable);

      begin
	format(*debug-output*, "Emitting Global Heap.\n");
	let heap-stream 
	  = make(<file-stream>, name: "heap.s", direction: #"output");
	build-global-heap(apply(concatenate, map(undumped-objects, *units*)),
			  heap-stream);
	close(heap-stream);
      end;

      begin
	format(*debug-output*, "Building inits.c.\n");
	let stream
	  = make(<file-stream>, name: "inits.c", direction: #"output");
	write("#include <runtime.h>\n\n", stream);
	write("/* This file is machine generated.  Do not edit. */\n\n",
	      stream);
	let entry-function-name
	  = (entry-function
	       & (make(<ct-entry-point>, for: entry-function, kind: #"main")
		    .entry-point-c-name));
	if (entry-function-name)
	  format(stream,
		 "extern void %s(descriptor_t *sp, int argc, void *argv);\n\n",
		 entry-function-name);
	end if;
	write("void inits(descriptor_t *sp, int argc, char *argv[])\n{\n",
	      stream);
	for (unit in *units*)
	  format(stream, "    %s_init(sp);\n", unit.unit-name);
	end;
	if (entry-function-name)
	  format(stream, "    %s(sp, argc, argv);\n", entry-function-name);
	end if;
	write("}\n", stream);
	close(stream);
      end;

      begin
	let flags = concatenate($cc-flags, " -L.");
	for (dir in *data-unit-search-path*)
	  flags := concatenate(flags, " -L", dir);
	end;
	let unit-libs = "";
	for (unit in *units*)
	  if (unit.unit-linker-options)
	    unit-libs := concatenate(" ", unit.unit-linker-options, unit-libs);
	  end if;
	  unit-libs := concatenate(" -l", unit.unit-name, unit-libs);
	end;
	format(makefile, "\n%s : %s %s\n", executable, objects, ar-name);
	format(makefile,
	       "\t$(CC) -z %s -L/lib/pa1.1 -o %s inits.c heap.s %s\n", 
	       flags, executable, unit-libs);
	format(makefile, "\nall-at-end-of-file : %s\n", executable);
      end;

    else
      format(*debug-output*, "Dumping library summary.\n");
      let dump-buf
	= begin-dumping(as(<symbol>, lib-name), $library-summary-unit-type);

      for (tlfs in tlf-vectors)
	for (tlf in tlfs)
	  dump-od(tlf, dump-buf);
	end;
      end;
      dump-od(unit-info, dump-buf);
      dump-queued-methods(dump-buf);

      end-dumping(dump-buf);
      format(makefile, "\nall-at-end-of-file : %s\n", ar-name);
    end;

    if (log-dependencies)
      spew-dependency-log(concatenate(unit-prefix, ".dep"));
    end if;

    close(makefile);
    if (pick-which-file(makefile-name, temp-makefile-name) = #t)
      // If the new makefile is different from the old one, then we need
      // to recompile all .c and .s files, regardless of whether they
      // were changed.  So touch them to make them look newer than the
      // object files.
      unless (empty?(all-generated-files))
	let touch-command = "touch";
	for (filename in all-generated-files)
	  touch-command := stringify(touch-command, ' ', filename);
	end for;
	format(*debug-output*, "%s\n", touch-command);
	if (system(touch-command) ~== 0)
	  cerror("so what", "touch failed?");
	end if;
      end unless;
    end if;

    let make-command = format-to-string("%s -f %s", $make-utility, 
					makefile-name);
    format(*debug-output*, "%s\n", make-command);
    unless (zero?(system(make-command)))
      cerror("so what", "gmake failed?");
    end;
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
    (unit-prefix :: <byte-string>, init-functions :: <vector>,
     start :: <integer>, finish :: <integer>, stream :: <stream>)
    => body :: <byte-string>;
  let string-stream = make(<byte-string-output-stream>);
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
				    unit-prefix, start, start + count - 1);
	let guts = emit-init-functions(unit-prefix, init-functions,
				       start, start + count, stream);
	format(stream, "static void %s(descriptor_t *sp)\n{\n%s}\n\n",
	       name, guts);
	format(string-stream, "    %s(sp);\n", name);
	start := start + count;
      end for;
    end for;
  end if;
  string-stream.string-output-stream-string;
end method emit-init-functions;

define method build-unit-init-function
    (unit-prefix :: <byte-string>, init-functions :: <vector>,
     stream :: <stream>)
    => ();
  let init-func-guts = emit-init-functions(unit-prefix, init-functions,
					   0, init-functions.size, stream);
  format(stream, "void %s_init(descriptor_t *sp)\n{\n%s}\n",
	 unit-prefix, init-func-guts);
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
  let int-type = specifier-type(#"<integer>");
  let rawptr-type = specifier-type(#"<raw-pointer>");
  let result-type = make-values-ctype(#(), #f);
  let argc = make-local-var(builder, #"argc", int-type);
  let argv = make-local-var(builder, #"argv", rawptr-type);
  let func = build-function-body(builder, policy, source, #f, name,
				 list(argc, argv), result-type, #t);
  let user-func = build-defn-ref(builder, policy, source, defn);
  // ### Should really spread the arguments out, but I'm lazy.
  build-assignment(builder, policy, source, #(),
		   make-unknown-call(builder, user-func, #f,
				     list(argc, argv)));
  build-return(builder, policy, source, func, #());
  end-body(builder);
  let sig = make(<signature>, specializers: list(int-type, rawptr-type),
		 returns: result-type);
  let ctv = make(<ct-function>, name: name, signature: sig);
  make-function-literal(builder, ctv, #f, #"global", sig, func);
  optimize-component(component);
  emit-component(component, file);
  ctv;
end method build-command-line-entry;

define method incorrect-usage () => ();
  error("Usage: compile [-Ldir ...] lid-file");
end method incorrect-usage;

define method main (argv0 :: <byte-string>, #rest args) => ();
  *random-state* := make(<random-state>, seed: 0);
  define-bootstrap-module();
  let library-dirs = make(<stretchy-vector>);
  let lid-file = #f;
  let features = #();
  let log-dependencies = #f;
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
	  'd' =>
	    *break-on-compiler-errors* := #t;
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
  *Data-Unit-Search-Path* := as(<simple-object-vector>, library-dirs);
  unless (lid-file)
    incorrect-usage();
  end;
  let worked?
    = compile-library(lid-file, reverse!(features), log-dependencies);
  exit(exit-code: if (worked?) 0 else 1 end);
end method main;


#if (~mindy)
define method %main (argc :: <integer>, argv :: <raw-pointer>) => ();
  let args = make(<vector>, size: argc);
  for (index :: <integer> from 0 below argc)
    let argptr = pointer-deref(#"ptr", argv,
			       index * c-expr(#"int", "sizeof(void *)"));
    args[index] := import-string(argptr);
  end for;
  apply(main, args);
end method %main;
#end


#if (mindy)
collect-garbage(purify: #t);
#end

// Seals for file compiler/main/main.dylan

// <unit-info> -- subclass of <object>
define sealed domain make(singleton(<unit-info>));
define sealed domain initialize(<unit-info>);
