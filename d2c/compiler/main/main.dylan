module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.23 1995/11/02 16:51:06 wlott Exp $
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


define method compile (#rest files) => ();
  $Top-Level-Forms.size := 0;
  let lib = $Dylan-Library;
  for (file in files)
    format(*debug-output*, "Parsing %s\n", file);
    let (tokenizer, mod) = file-tokenizer(lib, file);
    block ()
      *Current-Library* := lib;
      *Current-Module* := mod;
      parse-program(tokenizer);
    cleanup
      *Current-Library* := #f;
      *Current-Module* := #f;
    end;
  end;
  format(*debug-output*, "Finalizing definitions\n");
  do(finalize-top-level-form, copy-sequence($Top-Level-Forms));
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
  let body-stream
    = make(<file-stream>, name: "output.c", direction: #"output");
  let output-info
    = make(<output-info>, body-stream: body-stream);
  emit-prologue(output-info);
  format(body-stream, "#include \"output.h\"\n\n");
  for (tlf in $Top-Level-Forms)
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
	= build-function-body(builder, $Default-Policy, source, #f, name, #(),
			      result-type, #t);
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
  emit-epilogue(init-functions, output-info);
  close(body-stream);
  format(*debug-output*, "Emitting Initial Heap.\n");
  let heap-stream 
    = make(<file-stream>, name: "heap.s", direction: #"output");
  build-initial-heap(output-info.output-info-init-roots, heap-stream,
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
