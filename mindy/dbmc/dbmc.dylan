Module:   dbmc
Synopsis: Dylan-based Mindy Compiler
Author:   Peter S. Housel

define function translate-abstract-filename (abstract-name :: <byte-string>)
 => (physical-name :: <byte-string>)
  // XXX - We should eventually replace this with a routine that checks
  // for foo.dylan and then foo.dyl, preferably using some sort of abstract
  // locator translation. But for now, we keep it simple.
  concatenate(abstract-name, ".dylan");
end;

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

// Considers anything with an ASCII value less than 32 (' ') to be
// whitespace.  This includes control characters as well as what we
// normally consider whitespace.
define method split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  split-at(method (x :: <character>) x <= ' ' end, string);
end method split-at-whitespace;

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

define method main (name, arguments) => ()
  define-bootstrap-module();

  format(*standard-output*, "working in %s\n", working-directory());

  let lid = make(<source-file>, name: arguments[0]);
  let lid-header = parse-header(lid);

  let files = map-as(<stretchy-vector>,
		     translate-abstract-filename,
		     split-at-whitespace(element(lid-header, #"files",
						 default: "")));
  let lib-name = lid-header[#"library"];

  for(file in files)
    block ()
      let (tokenizer, module) = file-tokenizer($dylan-library, file);
      block (return)
	format(*standard-output*, "Parsing %s, module %s\n",
	       file, as(<string>, module.module-name));
	*Current-Module* := module;
	*Current-Library* := find-library(as(<symbol>, lib-name), create: #t);

	parse-source-record(tokenizer);
	format(*standard-output*, "done.\n");
      cleanup
	*Current-Module* := #f;
      end block;
    exception (<fatal-error-recovery-restart>)
      #f;
    end block;
  end for;
end method main;

begin
  main(application-name(), application-arguments());
end;
