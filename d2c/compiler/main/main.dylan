module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.42 1996/01/12 00:58:52 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define constant $cc-flags = getenv("CCFLAGS") | "";


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
    assert-end-object(state);
    here-be-roots(prefix, roots);
    prefix;
  end method
);


// Compilation driver.

define method file-tokenizer (lib :: <library>, name :: <byte-string>)
    => (tokenizer :: <tokenizer>, module :: <module>);
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


define method test-lexer (file :: <byte-string>) => ();
  let (tokenizer, module) = file-tokenizer($dylan-library, file);
  block (return)
    *Current-Module* := module;
    while (#t)
      let token = get-token(tokenizer);
      if (instance?(token, <eof-token>))
	return();
      else
	format(*debug-output*, "%=\n", token);
      end if;
    end while;
  cleanup
    *Current-Module* := #f;
  end block;
end method test-lexer;


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


define method compile-library
    (lid-file :: <byte-string>, command-line-features :: <list>) => ();
  let (header, files) = parse-lid(lid-file);

  do(process-feature,
     split-at-whitespace(element(header, #"features", default: "")));
  do(process-feature, command-line-features);

  let lib-name = header[#"library"];
  format(*debug-output*, "Compiling library %s\n", lib-name);
  let lib = find-library(as(<symbol>, lib-name));
  let unit-prefix
    = element(header, #"unit-prefix", default: #f) | as-lowercase(lib-name);
  let tlf-vectors = make(<stretchy-vector>);
  let source-path = extract-directory(lid-file);
  for (file in files)
    format(*debug-output*, "Parsing %s\n", file);
    let (tokenizer, mod) = file-tokenizer(lib, concatenate(source-path, file));
    complete-module(mod);
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
  let other-units = map-as(<simple-object-vector>, first, *roots*);
  for (file in files, tlfs in tlf-vectors)
    block ()
      format(*debug-output*, "Processing %s\n", file);
      let base-name = strip-extension(file, ".dylan") | file;
      let c-name = concatenate(base-name, ".c");
      let body-stream
	= make(<file-stream>, name: c-name, direction: #"output");
      block ()
	let file = make(<file-state>, unit: unit, body-stream: body-stream);
	emit-prologue(file, other-units);

	for (tlf in tlfs)
	  let name = format-to-string("%s", tlf);
	  format(*debug-output*, "...Compiling %s\n", name);
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
	  optimize-component(component);
	  emit-tlf-gunk(tlf, file);
	  emit-component(component, file);
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
    let file = make(<file-state>, unit: unit, body-stream: body-stream);
    emit-prologue(file, other-units);
    emit-epilogue(init-functions, file);
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
    let ar-command = format-to-string("/bin/ar qc %s%s", ar-name, objects);
    format(*debug-output*, "%s\n", ar-command);
    unless (zero?(system(ar-command)))
      cerror("so what", "ar failed?");
    end;
  end;

  let executable = element(header, #"executable", default: #f);
  if (executable)
    begin
      format(*debug-output*, "Emitting Initial Heap.\n");
      let heap-stream 
	= make(<file-stream>, name: "heap.s", direction: #"output");
      here-be-roots(unit-prefix,
		    as(<simple-object-vector>,
		       unit.unit-init-roots));
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
      let flags = concatenate($cc-flags, " -L.");
      for (dir in *data-unit-search-path*)
	flags := concatenate(flags, " -L", dir);
      end;
      let unit-libs = "";
      for (unit in *roots*)
	unit-libs := concatenate(" -l", unit[0], unit-libs);
      end;
      let command
	= concatenate("gcc ", flags, " -L/lib/pa1.1 -o ", executable,
		      " inits.c heap.s", unit-libs, " -lruntime -lm");
      format(*debug-output*, "%s\n", command);
      unless (zero?(system(command)))
	cerror("so what", "cc failed?");
      end;
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

    dump-simple-object(#"here-be-roots", dump-buf, unit-prefix,
		       as(<simple-object-vector>,
			  unit.unit-init-roots));

    end-dumping(dump-buf);
  end;

  format(*debug-output*, "Optimize called %d times.\n", *optimize-ncalls*);

  format(*debug-output*, "Compilation finished with %d Warning%s\n",
	 *warnings*, if (*warnings* == 1) "" else "s" end);
end;


define method load-library (name :: <symbol>) => ();
  block ()
    *Current-Library* := find-library(name);
    find-data-unit(name, $library-summary-unit-type,
		   dispatcher: *compiler-dispatcher*);
  cleanup
    *Current-Library* := #f;
  end;
end;

define method incorrect-usage () => ();
  format(*standard-error*, "Usage: compile [-Ldir ...] lid-file\n");
  format(*standard-error*,
	 "    or compile -autodump component [next-free-id-in-hex]\n");
  force-output(*standard-error*);
  error("Incorrect usage");
end method incorrect-usage;

define method main (argv0, #rest args)
  if (args.size > 0 & args.first = "-autodump")
    if (args.size < 2 | args.size > 3)
      incorrect-usage();
    end if;
    let component = as(<symbol>, args.second);
    let next-free-id = if (args.size = 3) 
			 string-to-integer(args.third, base: 16);
		       else
			 #f;
		       end if;
    autodump(component, next-free-id);
  else
    let library-dirs = make(<stretchy-vector>);
    let lid-file = #f;
    let features = #();
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
    compile-library(lid-file, reverse!(features));
  end if;
end method main;
