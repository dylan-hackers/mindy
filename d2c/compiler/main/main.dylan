module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.53 1996/02/17 21:25:48 wlott Exp $
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
  let buffer = make(<buffer>, size: len);
  copy-bytes(buffer, 0, string, 0, len);
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

#end

define constant $cc-flags = getenv("CCFLAGS") | "";


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
    use-module(mod);
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
#if (mindy)
  collect-garbage(purify: #t);
#end
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
  let other-units = map-as(<simple-object-vector>, unit-name, *units*);
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
	    make-function-literal(builder, ctv, #f, #"global", sig,
				  init-function);
	    add!(init-functions, ctv);
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

  let executable = element(header, #"executable", default: #f);
  let entry-point = element(header, #"entry-point", default: #f);
  let entry-function = #f;
  if (entry-point & ~executable)
    compiler-error("Can only specify an entry-point when producing an "
		     "executable.");
  end if;

  begin
    let c-name = concatenate(unit-prefix, "-init.c");
    let body-stream = make(<file-stream>, name: c-name, direction: #"output");
    let file = make(<file-state>, unit: unit, body-stream: body-stream);
    emit-prologue(file, other-units);
    if (entry-point)
      entry-function := build-command-line-entry(lib, entry-point, file);
    end if;
    build-unit-init-function(unit-prefix, init-functions, body-stream);
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

  format(*debug-output*, "Emitting Library Heap.\n");
  let s-name = concatenate(unit-prefix, "-heap.s");
  let heap-stream = make(<file-stream>, name: s-name, direction: #"output");
  let (undumped, extra-labels) = build-local-heap(unit, heap-stream);
  close(heap-stream);

  let o-name = concatenate(unit-prefix, "-heap.o");
  let cc-command
    = format-to-string("gcc %s -c %s -o %s", $cc-flags, s-name, o-name);
  format(*debug-output*, "%s\n", cc-command);
  unless (zero?(system(cc-command)))
    cerror("so what", "cc failed?");
  end;
  format(objects-stream, " %s", o-name);

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

  let linker-options = element(header, #"linker-options", default: #f);
  let unit-info = make(<unit-info>,
		       unit-name: unit-prefix,
		       undumped-objects: undumped,
		       extra-labels: extra-labels,
		       linker-options: linker-options);

  if (executable)
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
      let stream = make(<file-stream>, name: "inits.c", direction: #"output");
      write("#include <runtime.h>\n\n", stream);
      write("/* This file is machine generated.  Do not edit. */\n\n", stream);
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
      let command
	= concatenate("gcc -z ", flags, " -L/lib/pa1.1 -o ", executable,
		      " inits.c heap.s", unit-libs);
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
    dump-od(unit-info, dump-buf);
    dump-queued-methods(dump-buf);

    end-dumping(dump-buf);
  end;

  format(*debug-output*, "Optimize called %d times.\n", *optimize-ncalls*);

  format(*debug-output*, "Compilation finished with %d Warning%s\n",
	 *warnings*, if (*warnings* == 1) "" else "s" end);
end;

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
    compiler-error("Invalid entry point: %s -- "
		     "must be of the form module:variable.",
		   string);
  end block;
end method split-at-colon;


define method build-command-line-entry
    (lib :: <library>, entry :: <byte-string>, file :: <file-state>)
    => entry-function :: <ct-function>;
  let (module-name, variable-name) = split-at-colon(entry);
  let module = find-module(lib, as(<symbol>, module-name));
  unless (module)
    compiler-error("Invalid entry point: %s -- no module %s.",
		   entry, module-name);
  end unless;
  let variable = find-variable(make(<basic-name>,
				    symbol: as(<symbol>, variable-name),
				    module: module));
  unless (variable)
    compiler-error("Invalid entry point: %s -- no variable %s in module %s.",
		   entry, variable-name, module-name);
  end unless;
  let defn = variable.variable-definition;
  unless (defn)
    compiler-error("Invalid entry point: %s -- it isn't defined.", entry);
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
  let user-func = fer-convert-defn-ref(builder, policy, source, defn);
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

define method main (argv0 :: <byte-string>, #rest args) => ();
#if (mindy)
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
#end
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
#if (mindy)
  end if;
#end
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
