module: pidgin
synopsis: 
author: 
copyright: 

define method help () => ()
  format(*standard-error*,"Usage: pidgin [OPTION]... FILE\n");
  format(*standard-error*, "Translate C headers into Dylan definitions.\n\n");
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

define function pidgin(name, arguments)
  if (empty?(arguments))
    help();
    exit-application(1);
  else
    let argp = make(<argument-list-parser>);
    add-option-parser-by-type(argp,
			      <simple-option-parser>,
			      short-options: #("h"),
			      long-options: #("help"));
    add-option-parser-by-type(argp,
			      <repeated-parameter-option-parser>,
			      short-options: #("I"),
			      long-options: #("includedir"));
    add-option-parser-by-type(argp,
			      <parameter-option-parser>,
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
    let extra-includes = option-value-by-long-name(argp, "includedir");
    format(*standard-output*, "Constructing include path.\n");
    let include-path = construct-include-path(extra-includes);
    force-output(*standard-output*);
    let module = option-value-by-long-name(argp, "module");
    let library = option-value-by-long-name(argp, "library");
    if (~module)
      module := substring-replace(filename(argp.regular-arguments[0]), ".h", "");
    end if;
    if (~library)
      library := substring-replace(filename(argp.regular-arguments[0]), ".h", "");
    end if;
    let repository :: <c-type-repository> = make(<c-type-repository>);
    let c-file :: <c-file> = parse-c-file(repository,
					  argp.regular-arguments[0],
					  include-path: include-path,
					  platform: $i386-linux-platform);
    force-output(*standard-output*);
    let out :: <file-stream> = make(<file-stream>, locator: concatenate(library, ".dylan"), direction: #"output", if-exists: #"overwrite", if-does-not-exist: #"create");
    write-line(out, concatenate("define module dylan-user\n\ndefine library ", library, "\n  use common-dylan;\n  use c-ffi;\n  export ", module, ";\nend library;"));
    write-line(out, concatenate("\ndefine module ", module, "\n  use c-ffi;\nend module\n\n"));
    c-file := c-file.c-file-contents[0];
    for (i :: <integer> from 0 to size(c-file.c-file-contents) - 1 )
      if (instance?(c-file.c-file-contents[i], <c-file>))
        write-line(out, concatenate("c-include(\"", c-file.c-file-contents[i].c-file-name, "\");\n"));
      elseif (instance?(c-file.c-file-contents[i], <c-declaration>))
        write-line(out, c-output(c-file.c-file-contents[i]));
      end if;
    end for;
    close(out);
  end;
  exit-application(0);
end function;

// Invoke our main() function.
pidgin(application-name(), application-arguments());
