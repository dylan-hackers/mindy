module: pidgin
synopsis: 
author: 
copyright: 

define method help () => ()
  format(*standard-error*,"Usage: pidgin [OPTION]... FILE\n");
  format(*standard-error*, "Translate C headers into Dylan definitions.\n\n");
  format(*standard-error*, "  -I, --includedir <directory>: Scan specified directory for includes\n");
end method;

define function pidgin(name, arguments)
  if (empty?(arguments))
    help();
    exit-application(1);
  else
    let argp = make(<argument-list-parser>);
    add-option-parser-by-type(argp,
			      <repeated-parameter-option-parser>,
			      short-options: #("I"),
			      long-options: #("includedir"));
    unless (parse-arguments(argp, arguments))
      help();
      exit-application(1);
    end unless;
    let extra-includes = option-value-by-long-name(argp, "includedir");

    format(*standard-output*, "Constructing include path.\n");
    let include-path = construct-include-path(extra-includes);
    force-output(*standard-output*);
    format(*standard-output*, "Running C parser.\n");
    let repository :: <c-type-repository> = make(<c-type-repository>);
    force-output(*standard-output*);
    let c-file :: <c-file> = parse-c-file(repository,
					  argp.regular-arguments[0],
					  include-path: include-path,
					  platform: $i386-linux-platform);
    force-output(*standard-output*);
    format(*standard-output*, "Parser finished.\n");
    force-output(*standard-output*);
  end;
  exit-application(0);
end function;

// Invoke our main() function.
pidgin(application-name(), application-arguments());
