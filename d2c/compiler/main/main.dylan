module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.15 1995/05/18 20:07:21 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define method file-tokenizer (lib :: <library>, name :: <byte-string>)
  let source = make(<source-file>, name: name);
  let (header, start-line, start-posn) = parse-header(source);
  *Current-Module* := find-module(lib,
				  as(<symbol>, header[#"module"]),
				  create: #t);
  make(<lexer>,
       source: source,
       start-posn: start-posn,
       start-line: start-line);
end;


define method compile (#rest files) => res :: <component>;
  $Top-Level-Forms.size := 0;
  for (file in files)
    format(*debug-output*, "Parsing %s\n", file);
    parse-program(file-tokenizer($Dylan-Library, file));
  end;
  format(*debug-output*, "Finalizing definitions\n");
  do(finalize-top-level-form, $Top-Level-Forms);
  format(*debug-output*,
	 "inheriting slots, inheriting overrides, assigning unique ids\n");
  inherit-slots();
  inherit-overrides();
  assign-unique-ids();
  format(*debug-output*, "seeding representations\n");
  seed-representations();
  format(*debug-output*, "assigning slot representations\n");
  assign-slot-representations();
  format(*debug-output*, "laying out instances\n");
  layout-instance-slots();
  format(*debug-output*, "Converting in FER\n");
  let component = make(<fer-component>);
  let builder = make-builder(component);
  let init-function
    = build-function-body(builder, $Default-Policy, make(<source-location>),
			  "Top Level Initializations", #(), #"best");
  do(curry(convert-top-level-form, builder), $Top-Level-Forms);
  build-return(builder, $Default-Policy, make(<source-location>),
	       init-function, #());
  end-body(builder);
  format(*debug-output*, "Optimizing\n");
  optimize-component(component);
  format(*debug-output*, "Emitting C code.\n");
  let header-stream
    = make(<file-stream>, name: "output.h", direction: #"output");
  let body-stream
    = make(<file-stream>, name: "output.c", direction: #"output");
  let output-info
    = make(<output-info>, header-stream: header-stream,
	   body-stream: body-stream);
  do(rcurry(emit-tlf-gunk, output-info), $Top-Level-Forms);
  do(rcurry(emit-function, output-info), component.all-function-regions);
  close(header-stream);
  close(body-stream);
  component;
end;

define method main (argv0, #rest files)
  if (empty?(files))
    error("No files supplied.");
    #f;
  else
    apply(compile, files);
  end;
end;
