module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.25 1995/11/06 17:07:24 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


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


define constant $dot-dylan = ".dylan";

define method maybe-strip-dot-dylan
    (name :: <byte-string>) => res :: <byte-string>;
  if (name.size < $dot-dylan.size)
    name;
  else
    block (return)
      for (char in $dot-dylan, index from name.size - $dot-dylan.size)
	unless (name[index] == char)
	  return(name);
	end;
      end;
      copy-sequence(name, start: 0, end: name.size - $dot-dylan.size);
    end;
  end;
end;


define method compile (#rest files) => ();
  let tlf-vectors = make(<stretchy-vector>);
  let lib = $Dylan-Library;
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
  let unit-prefix = "unit";
  let unit-info = make(<unit-info>, prefix: unit-prefix);
  for (file in files, tlfs in tlf-vectors)
    let body-stream
      = make(<file-stream>,
	     name: concatenate(maybe-strip-dot-dylan(file), ".c"),
	     direction: #"output");
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
	let sig = make(<signature>, specializers: #(), returns: result-type);
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
    end;
    close(body-stream);
  end;

  let body-stream
    = make(<file-stream>, name: concatenate(unit-prefix, "-init.c"),
	   direction: #"output");
  let output-info
    = make(<output-info>, unit-info: unit-info, body-stream: body-stream);
  emit-prologue(output-info);
  emit-epilogue(init-functions, output-info);
  close(body-stream);
  
  format(*debug-output*, "Emitting Initial Heap.\n");
  let heap-stream 
    = make(<file-stream>, name: "heap.s", direction: #"output");
  build-initial-heap
    (vector(vector(unit-prefix, unit-info.unit-info-init-roots)),
     heap-stream,
     output-info);
  close(heap-stream);
  format(*debug-output*, "Optimize called %d times.\n", *optimize-ncalls*);
end;


define method main (argv0, #rest files)
  if (empty?(files))
    error("No files supplied.");
    #f;
  else
    apply(compile, files);
  end;
end;
