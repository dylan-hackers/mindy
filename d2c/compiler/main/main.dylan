module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.27 1995/11/08 18:35:29 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define constant $cc-flags = "-I.";


// Roots registry.

define variable *roots* :: <stretchy-vector> = make(<stretchy-vector>);

define method here-be-roots
    (unit-prefix :: <byte-string>, roots :: <simple-object-vector>) => ();
  add!(*roots*, vector(unit-prefix, roots));
end;

add-od-loader(*compiler-dispatcher*, #"here-be-roots",
  method (state :: <load-state>) => res :: <byte-string>;
    let prefix = load-object-dispatch(state);
    let roots = load-object-dispatch(state);
    here-be-roots(prefix, roots);
    prefix;
  end method
);


// Compilation driver.

define method file-tokenizer (lib :: <library>, name :: <byte-string>)
  let source = make(<source-file>, name: name);
  let (header, start-line, start-posn) = parse-header(source);
  values(make(<lexer>,
	      source: source,
	      start-posn: start-posn,
	      start-line: start-line),
	 find-module(lib,
		     as(<symbol>, header[#"module"]),
		     create: #t));
end;


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
    method repeat (posn :: <fixed-integer>)
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
    method find-newline (posn :: <fixed-integer>)
	=> newline :: <fixed-integer>;
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


define method compile-library (lid-file :: <byte-string>) => ();
  let (header, files) = parse-lid(lid-file);
  let lib-name = header[#"library"];
  format(*debug-output*, "Compiling library %s\n", lib-name);
  let lib = find-library(as(<symbol>, lib-name), create: #t);
  let unit-prefix
    = element(header, #"unit-prefix", default: #f) | as-lowercase(lib-name);
  let tlf-vectors = make(<stretchy-vector>);
  for (file in files)
    format(*debug-output*, "Parsing %s\n", file);
    let (tokenizer, mod) = file-tokenizer(lib, file);
    block ()
      *Current-Library* := lib;
      *Current-Module* := mod;
      let tlfs = make(<stretchy-vector>);
      *Top-Level-Forms* := tlfs;
      add!(tlf-vectors, tlfs);
      parse-program(tokenizer);
    cleanup
      *Current-Library* := #f;
      *Current-Module* := #f;
    end;
  end;
  format(*debug-output*, "Finalizing definitions\n");
  for (tlfs in tlf-vectors)
    *Top-Level-Forms* := tlfs;
    do(finalize-top-level-form, copy-sequence(tlfs));
  end;
  format(*debug-output*, "inheriting slots\n");
  inherit-slots();
  format(*debug-output*, "inheriting overrides\n");
  inherit-overrides();
  format(*debug-output*, "assigning unique ids\n");
  assign-unique-ids();
  format(*debug-output*, "seeding representations\n");
  seed-representations();
  format(*debug-output*, "assigning slot representations\n");
  assign-slot-representations();
  format(*debug-output*, "laying out instances\n");
  layout-instance-slots();
  let init-functions = make(<stretchy-vector>);
  let unit-info = make(<unit-info>, prefix: unit-prefix);
  let dump-buf
    = begin-dumping(as(<symbol>, lib-name), $library-summary-unit-type);
  let objects-stream = make(<byte-string-output-stream>);
  for (file in files, tlfs in tlf-vectors)
    block ()
      format(*debug-output*, "Processing %s\n", file);
      let base-name = strip-extension(file, ".dylan") | file;
      let c-name = concatenate(base-name, ".c");
      let body-stream
	= make(<file-stream>, name: c-name, direction: #"output");
      block ()
	let output-info
	= make(<output-info>, unit-info: unit-info, body-stream: body-stream);
	emit-prologue(output-info);

	for (tlf in tlfs)
	  let name = format-to-string("%s", tlf);
	  format(*debug-output*, "...Converting %s\n", name);
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
	    add!(init-functions,
		 make-function-literal(builder, ctv, #f, #"global", sig,
				       init-function));
	  end;
	  format(*debug-output*, "...Optimizing %s\n", name);
	  optimize-component(component);
	  format(*debug-output*, "...Emitting %s\n", name);
	  emit-tlf-gunk(tlf, output-info);
	  emit-component(component, output-info);
	  dump-od(tlf, dump-buf);
	end;
      cleanup
	close(body-stream);
      end;

      let o-name = concatenate(base-name, ".o");
      let cc-command
	= format-to-string("gcc %s -c %s -o %s", $cc-flags, c-name, o-name);
      format(*debug-output*, "%s\n", cc-command);
      unless (zero?(system(cc-command)))
	cerror("so what", "cc failed?");
      end;
      format(objects-stream, " %s", o-name);

    exception (<simple-restart>,
		 init-arguments:
		 vector(format-string: "Blow off compiling this file."))
      #f;
    end block;
  end;

  begin
    let c-name = concatenate(unit-prefix, "-init.c");
    let body-stream = make(<file-stream>, name: c-name, direction: #"output");
    let output-info
      = make(<output-info>, unit-info: unit-info, body-stream: body-stream);
    emit-prologue(output-info);
    emit-epilogue(init-functions, output-info);
    close(body-stream);

    let o-name = concatenate(unit-prefix, "-init.o");
    let cc-command
      = format-to-string("gcc %s -c %s -o %s", $cc-flags, c-name, o-name);
    format(*debug-output*, "%s\n", cc-command);
    unless (zero?(system(cc-command)))
      cerror("so what", "cc failed?");
    end;
    format(objects-stream, " %s", o-name);
  end;
  let objects = string-output-stream-string(objects-stream);
  
  begin
    let ar-name = format-to-string("lib%s.a", unit-prefix);
    system(concatenate("rm -f ", ar-name));
    let ar-command = format-to-string("ar qc %s%s", ar-name, objects);
    format(*debug-output*, "%s\n", ar-command);
    unless (zero?(system(ar-command)))
      cerror("so what", "ar failed?");
    end;
  end;

  begin
    let roots = as(<simple-object-vector>, unit-info.unit-info-init-roots);
    dump-simple-object(#"here-be-roots", dump-buf, unit-prefix, roots);
    here-be-roots(unit-prefix, roots);
  end;

  end-dumping(dump-buf);

  let executable = element(header, #"executable", default: #f);
  if (executable)
    begin
      format(*debug-output*, "Emitting Initial Heap.\n");
      let heap-stream 
	= make(<file-stream>, name: "heap.s", direction: #"output");
      build-initial-heap(*roots*, heap-stream);
      close(heap-stream);
    end;

    begin
      format(*debug-output*, "Building inits.c.\n");
      let stream = make(<file-stream>, name: "inits.c", direction: #"output");
      write("#include <runtime.h>\n\n", stream);
      write("/* This file is machine generated.  Do not edit. */\n\n", stream);
      write("void inits(descriptor_t *sp)\n{\n", stream);
      for (unit in *roots*)
	format(stream, "    %s_init(sp);\n", unit[0]);
      end;
      write("}\n", stream);
      close(stream);
    end;

    begin
      let command = " -lruntime";
      for (unit in *roots*)
	command := concatenate(" -l", unit[0], command);
      end;
      command := concatenate("gcc ", $cc-flags, " -o ", executable,
			     " inits.c heap.s", command);
      format(*debug-output*, "%s\n", command);
      unless (zero?(system(command)))
	cerror("so what", "cc failed?");
      end;
    end;
  end;

  format(*debug-output*, "Optimize called %d times.\n", *optimize-ncalls*);
end;


  

define method main (argv0, #rest args)
  if (args.size ~== 1)
    error("usage: compile lid-file");
  end;
  compile-library(args[0]);
end;
