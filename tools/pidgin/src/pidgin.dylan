module: pidgin
synopsis: 
author: 
copyright: 

define method help () => ()
  format(*standard-error*,"Usage: pidgin [OPTION]... FILE\n");
  format(*standard-error*, "Translate C headers into Dylan definitions.\n\n");
format(*standard-error*, "-d, --debug: Print constructs as they are translated\n");
  format(*standard-error*, "-I, --includedir <directory>: Scan specified directory for includes\n");
  format(*standard-error*, "-m, --module <module>: Set module name for resulting interface\n");
  format(*standard-error*, "-l, --library <library>: Set library name for resulting interface\n");
end method;

// Is this defined somewhere else?
// Returns a string representing a filename when given a path.
define method filename
    (path :: <string>)
 => (name :: <string>)
  let lastpathsep :: <integer> = 0;
  for (counter :: <integer> from 0 to size(path) - 1 )
    if (as(<character>, path[counter]) = path-separator)
      lastpathsep := counter;
    end if;
  end for;
  as(<string>, subsequence(path, start: lastpathsep + 1, end: size(path)));
end method;

define function output-header
    (library :: <string>, modules :: <deque>)
 => (result :: <string>)
  let result :: <string> = concatenate("define module dylan-user\n\ndefine library ", library, "\n  use common-dylan;\n  use c-ffi;\n  export ", modules[0], ";\nend library;\n");
  result := concatenate(result, "\ndefine module ", modules[0], "\n  use c-ffi;\n");
  for (i :: <integer> from 0 to size(exported-names) - 1 )
    result := concatenate(result, "  export ",exported-names[i], ";\n");
  end for;
  result := concatenate(result, "end module\n\n")
end function;

define function output-interface
    (file :: <string>, library :: <string>, modules :: <deque>, include-path :: <c-include-path>)
 => ( )
  let repository :: <c-type-repository> = make(<c-type-repository>);
  let c-file :: <c-file> = parse-c-file(repository,
    file,
    include-path: include-path,
    platform: $i386-linux-platform);
  let out :: <file-stream> = make(<file-stream>, locator: concatenate(library, ".dylan"), direction: #"output", if-exists: #"overwrite", if-does-not-exist: #"create");
  c-file := c-file.c-file-contents[0];
  // Keeping this all in a string is a bit ugly, but it should work for now.
  let result :: <string> = make(<string>);
  for (i :: <integer> from 0 to size(c-file.c-file-contents) - 1 )
      if (debug == #t)
      debug-print(c-file.c-file-contents[i]);
    end if;
      result := concatenate(result, c-output(c-file.c-file-contents[i]));
  end for;
  if (debug)
    format(*standard-output*, "Exported names: %=\n", exported-names);
  end if;
  write-line(out, output-header(library, modules));
  write-line(out, result);
  close(out);
end function;

define function pidgin(name, arguments)
  if (empty?(arguments))
    help();
    exit-application(1);
  else
    let argp = make(<argument-list-parser>);
    add-option-parser-by-type(argp,
			      <simple-option-parser>,
			      short-options: #("d"),
			      long-options: #("debug"));
    add-option-parser-by-type(argp,
			      <simple-option-parser>,
			      short-options: #("h"),
			      long-options: #("help"));
    add-option-parser-by-type(argp,
			      <repeated-parameter-option-parser>,
			      short-options: #("I"),
			      long-options: #("includedir"));
    add-option-parser-by-type(argp,
			      <repeated-parameter-option-parser>,
			      short-options: #("m"),
			      long-options: #("module"));
    add-option-parser-by-type(argp,
			      <parameter-option-parser>,
			      short-options: #("l"),
			      long-options: #("library"));
    unless (parse-arguments(argp, arguments))
      help();
      exit-application(1);
    end unless;
    if (option-present?-by-long-name(argp, "debug"))
      format(*standard-output*, "Debugging activated.\n");
      debug := #t;
    end if;
    let extra-includes = option-value-by-long-name(argp, "includedir");
    let include-path = construct-include-path(extra-includes);
    let modules = option-value-by-long-name(argp, "module");
    let library = option-value-by-long-name(argp, "library");
    if (size(modules) = 0)
      modules := make(<deque>, size: 1);
      modules[0] := substring-replace(filename(argp.regular-arguments[0]), ".h", "");
    end if;
    if (~library)
      library := substring-replace(filename(argp.regular-arguments[0]), ".h", "");
    end if;
    output-interface(argp.regular-arguments[0], library, modules, include-path);
  end;
  exit-application(0);
end function;

// Invoke our main() function.
pidgin(application-name(), application-arguments());
