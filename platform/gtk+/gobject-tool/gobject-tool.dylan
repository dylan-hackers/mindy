Module: gobject-tool

define method usage()
  format(*standard-error*, "usage: gobject-tool [options] file.defs ...");
  new-line(*standard-error*);
end method;

define method main(name, arguments)
  *warning-output* := *standard-error*;
  let arg-parser = make(<argument-list-parser>);
  add-option-parser-by-type(arg-parser,
                            <simple-option-parser>,
                            short-options: #("d"),
                            long-options: #("debug"));
  add-option-parser-by-type(arg-parser,
                            <simple-option-parser>,
                            short-options: #("h"),
                            long-options: #("usage"));
  add-option-parser-by-type(arg-parser,
                            <parameter-option-parser>,
                            long-options: #("platform"));
  add-option-parser-by-type(arg-parser,
                            <repeated-parameter-option-parser>,
                            short-options: #("I"),
                            long-options: #("includedir"));
  add-option-parser-by-type(arg-parser,
                            <parameter-option-parser>,
                            short-options: #("m"),
                            long-options: #("module"));
  add-option-parser-by-type(arg-parser,
                            <simple-option-parser>,
                            short-options: #("e"),
                            long-options: #("exports"));
  add-option-parser-by-type(arg-parser,
                            <repeated-parameter-option-parser>,
                            short-options: #("u"),
                            long-options: #("use"));
  add-option-parser-by-type(arg-parser,
                            <simple-option-parser>,
                            long-options: #("flat"));
  unless(parse-arguments(arg-parser, arguments))
    usage();
    exit-application(1);
  end unless;

  if(option-present?-by-long-name(arg-parser, "usage"))
    usage();
    exit-application(0);
  end if;

  let module-name = option-value-by-long-name(arg-parser, "module");
  unless(module-name)
    format(*standard-error*, "The --module flag is required.\n");
    exit-application(1);
  end unless;

  let platform-name = option-value-by-long-name(arg-parser, "platform");
  let platform
    = select(platform-name by \=)
        "x86-linux-gcc",
        "x86-linux-gcc32" =>
          $i386-linux-platform;
        "ppc-linux-gcc" =>
          $ppc-linux-platform;
        "x86-freebsd-elf-gcc" =>
          $i386-freebsd-platform;
        "sparc-solaris-gcc" =>
          $sparc-solaris-platform;
        otherwise =>
          $generic-platform;
      end;

  let includedir = option-value-by-long-name(arg-parser, "includedir");
  let include-path = make(<gcc-include-path>,
                          standard-include-directories:
                            platform.c-platform-default-include-path,
                          extra-include-directories: includedir,
                          extra-user-include-directories: #());
  for(arg in regular-arguments(arg-parser))
    let module-name = option-value-by-long-name(arg-parser, "module");
    let mod = import-defs(arg);

    if(option-present?-by-long-name(arg-parser, "exports"))
      let imports = option-value-by-long-name(arg-parser, "use");
      output-melange-exports(mod, module-name, imports, *standard-output*);
    else
      new-line(*standard-error*);
      force-output(*standard-error*);
      let includes = defs-module-includes(mod);
      let repository :: <c-type-repository> = make(<c-type-repository>);
      for(file in includes)
        format(*standard-error*, "<%s", file);
        force-output(*standard-error*);
        let c-file :: <c-file> = parse-c-file(repository,
                                              file,
                                              include-path: include-path,
                                              platform: platform);
        format(*standard-error*, ">", file);
        new-line(*standard-error*);
        force-output(*standard-error*);
      end for;

      format(*standard-output*, "Module: %s\n\n", module-name);

      output-melange(mod, repository, *standard-output*);
    end if;
  end for;
end method;

// Invoke our main() function.
main(application-name(), application-arguments());
