module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.3 1995/03/23 22:01:44 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define method file-tokenizer (lib :: <library>, name :: <byte-string>)
  let source = make(<source-file>, name: name);
  let (header, start-line, start-posn) = parse-header(source);
  make(<lexer>,
       source: source,
       module: find-module(lib,
			   as(<symbol>, header[#"module"]),
			   create: #t),
       start-posn: start-posn,
       start-line: start-line);
end;


define method compile (#rest files) => res :: <region>;
  $Top-Level-Forms.size := 0;
  for (file in files)
    format(*debug-output*, "Parsing %s\n", file);
    parse-program(file-tokenizer($Dylan-Library, file));
  end;
  format(*debug-output*, "Finalizing definitions\n");
  do(finalize-top-level-form, $Top-Level-Forms);
  format(*debug-output*, "laying out instances\n");
  layout-instance-slots();
  format(*debug-output*, "Converting in FER\n");
  let component = make(<fer-component>);
  let builder = make-builder(component);
  do(curry(convert-top-level-form, builder), $Top-Level-Forms);
  builder-result(builder);
end;

define method main (argv0, #rest files)
  if (empty?(files))
    error("No files supplied.");
    #f;
  else
    apply(compile, files);
  end;
end;
