module: main
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/main.dylan,v 1.80 2003/07/16 15:03:35 scotek Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================


//----------------------------------------------------------------------
// <feature-option-parser> - handles -D, -U
//----------------------------------------------------------------------
// d2c has a delightfully non-standard '-D' flag with a corresponding '-U'
// flag which allows you to undefine things (well, sort of). We create a
// new option parser class to handle these using the option-parser-protocol
// module from the parse-arguments library.

define class <d2c-feature-option-parser> (<negative-option-parser>)
end class <d2c-feature-option-parser>;

define method reset-option-parser
    (parser :: <d2c-feature-option-parser>, #next next-method) => ()
  next-method();
  parser.option-value := make(<deque> /* of: <string> */);
end;

define method parse-option
    (parser :: <d2c-feature-option-parser>,
     arg-parser :: <argument-list-parser>)
 => ()
  let negative? = negative-option?(parser, get-argument-token(arg-parser));
  let value = get-argument-token(arg-parser).token-value;
  push-last(parser.option-value,
	    if (negative?)
	      concatenate("~", value)
	    else
	      value
	    end if);
end method parse-option;

    
//----------------------------------------------------------------------
// Built-in help.
//----------------------------------------------------------------------

define method show-copyright(stream :: <stream>) => ()
  format(stream, "d2c (Gwydion Dylan) %s\n", $version);
  format(stream, "Compiles Dylan source into C, then compiles that.\n");
  format(stream, "Copyright 1994-1997 Carnegie Mellon University\n");
  format(stream, "Copyright 1998-2003 Gwydion Dylan Maintainers\n");
end method show-copyright;

define method show-usage(stream :: <stream>) => ()
  format(stream,
"Usage: d2c [options] -Llibdir... lidfile\n");
end method show-usage;

define method show-usage-and-exit() => ()
  show-usage(*standard-error*);
  exit(exit-code: 1);
end method show-usage-and-exit;

define method show-help(stream :: <stream>) => ()
  show-copyright(stream);
  format(stream, "\n");
  show-usage(stream);
  format(stream,
"       -i, --interactive  Enter interactive command mode.\n"
"       -L, --libdir:      Extra directories to search for libraries.\n"
"       -D, --define:      Define conditional compilation features.\n"
"       -U, --undefine:    Undefine conditional compilation features.\n"
"       -M, --log-deps:    Log dependencies to a file.\n"
"       -T, --target:      Target platform name.\n"
"       -p, --platforms:   File containing platform descriptions.\n"
"       --no-binaries:     Do not compile generated C files.\n"
"       --no-makefile:     Do not create makefile for generated C files. Implies --no-binaries.\n"
"       -g, --debug:       Generate debugging code.\n"
"       --profile:         Generate profiling code.\n"
"       -s, --static:      Force static linking.\n"
"       -j, --thread-count Max threads to use (default 1)\n"
"       -d, --break:       Debug d2c by breaking on errors.\n"
"       -o, --optimizer-option:\n"
"                          Turn on an optimizer option. Prefix option with\n"
"                          'no-' to turn it off.\n"
"       --debug-optimizer <verbosity>:\n"
"                          Display detailed optimizer information.\n"
"                          Integer argument specifies verbosity (0-5)\n"
"       --optimizer-sanity-check:\n"
"                          Sanity check optimizer\n"
"       -F, --cc-overide-command:\n"
"                          Alternate method of invoking the C compiler.\n"
"                          Used on files specified with -f.\n"
"       -f, --cc-overide-file:\n"
"                          Files which need special C compiler invocation.\n"
"       --help:            Show this help text.\n"
"       --version          Show version number.\n"
	   );
end method show-help;

define method show-compiler-info(stream :: <stream>) => ()
  local method p (#rest args)
	  apply(format, stream, args);
	end;

  // This output gets read by ./configure.
  // All output must be of the form "KEY=VALUE". All keys must begin with
  // "_DCI_" (for "Dylan compiler info") and either "DYLAN" (which designates
  // a general purpose value) or "D2C" (which should be used for anything
  // which is necessarily specific to d2c).

  // This value indicates how much of LID we implement correctly.
  //   0: We only support CMU-style LID files.
  //   1: We support everything from 0 plus the 'File:' keyword.
  //   2: 1 but with unit-prefix being ignored
  p("_DCI_DYLAN_LID_FORMAT_VERSION=2\n");

  // Increment CURRENT_BOOTSTRAP_COUNTER in configure.in to force an
  // automatic bootstrap.
  p("_DCI_D2C_BOOTSTRAP_COUNTER=%d\n", $bootstrap-counter);

  // The directory (relative to --prefix) where ./configure can find our
  // runtime libraries. This is used when bootstrapping.
  p("_DCI_D2C_RUNTIME_SUBDIR=%s/%s\n", $version, $default-target-name);

  // The absolute path to where d2c searches for user-installed libraries.
  // This is used by the Makefile generated by make-dylan-lib to install
  // site-local Dylan code in the right directoy.
  p("_DCI_D2C_DYLAN_USER_DIR=%s/lib/dylan/%s/%s/dylan-user\n",
    $dylan-user-dir, $version, $default-target-name);

  // The library search path in effect.
  p("_DCI_D2C_DYLANPATH=");
  for (dir in *Data-Unit-Search-Path*, first? = #t then #f)
    unless (first?) p("%c", $search-path-seperator) end;
    p("%s", dir);
  end for;
  p("\n");

  // Shared library support
  p("_DCI_D2C_SHARED_SUPPORT=%s\n",
    if (*current-target*.shared-library-filename-suffix
          & *current-target*.shared-object-filename-suffix
          & *current-target*.link-shared-library-command)
      "yes";
    else
      "no";
    end if);
end method;

define method show-dylan-user-location(stream :: <stream>) => ()
  format(stream, "%s/lib/dylan/%s/%s/dylan-user\n",
         $dylan-user-dir, $version, $default-target-name);
end method;

//----------------------------------------------------------------------
// Where to find various important files.
//----------------------------------------------------------------------

// $default-dylan-dir and $default-target-name are defined in
// file-locations.dylan, which is generated by Makegen.

// If DYLANDIR is defined, then it is assumed to be the root of the install
// area, and the location of platforms.descr and the libraries are derived from
// there.  Otherwise we use the autoconf prefix @prefix@.  It would be nice to
// use libdir, etc., but the default substitutions contain ${prefix}
// variables, which Dylan doesn't have yet.

#if (macos)

define constant $dylan-dir = $default-dylan-dir;
define constant $dylan-user-dir = $default-dylan-user-dir;

// Platform parameter database.
define constant $default-targets-dot-descr = concatenate($dylan-dir, ":support:platforms.descr" );

// Library search path.
define constant $default-dylan-path = concatenate($dylan-dir, ":support:\t");

// Location of runtime.h
define constant $runtime-include-dir = concatenate($dylan-dir, ":support:runtime-includes" );

#else

define constant $dylan-dir = getenv("DYLANDIR") | $default-dylan-dir;
define constant $dylan-user-dir = getenv("DYLANUSERDIR") | getenv("DYLANDIR") | $default-dylan-user-dir;

// Platform parameter database.
define constant $default-targets-dot-descr
  = concatenate($dylan-dir, "/share/dylan/platforms.descr");

// Library search path.

// Location of runtime.h
define constant $runtime-include-dir
  = concatenate($dylan-dir, "/include");

#endif


//----------------------------------------------------------------------
// Main
//----------------------------------------------------------------------

define method main (argv0 :: <byte-string>, #rest args) => ();
  #if (~mindy)
  no-core-dumps();
  #endif

  #if (~mindy)
  // alter the GC params, if the user wants
  // This supports three levels:
  //   - small  : small initial heap, frequent GC
  //   - default: initial heap 25 MB
  //   - big    : initial heap 25 M, infrequent G
  c-decl("extern unsigned long GC_free_space_divisor;");
  c-decl("extern int GC_expand_hp(size_t number_of_bytes);");
  if (getenv("D2C_SMALL_MACHINE"))
    c-expr(void: "GC_free_space_divisor = 5");
  else
    c-expr(void: "GC_expand_hp(25*1024*1024)");
  end;
  if (getenv("D2C_BIG_MACHINE"))
    c-expr(void: "GC_free_space_divisor = 2");
  end;
  #endif

  *random-state* := make(<random-state>, seed: 0);
 
  // Set up our argument parser with a description of the available options.
  let argp = make(<argument-list-parser>);
  add-option-parser-by-type(argp,
                           <simple-option-parser>,
                           long-options: #("interactive"),
                           short-options: #("i"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("help"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("version"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("compiler-info"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("dylan-user-location"));
  add-option-parser-by-type(argp,
			    <repeated-parameter-option-parser>,
			    long-options: #("libdir"),
			    short-options: #("L"));
  add-option-parser-by-type(argp,
			    <d2c-feature-option-parser>,
			    long-options: #("define"),
			    short-options: #("D"),
			    negative-long-options: #("undefine"),
			    negative-short-options: #("U"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("log-deps"),
			    short-options: #("M"));
  add-option-parser-by-type(argp,
			    <parameter-option-parser>,
			    long-options: #("target"),
			    short-options: #("T"));
  add-option-parser-by-type(argp,
			    <parameter-option-parser>,
			    long-options: #("platforms"),
			    short-options: #("p"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("no-binaries"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("no-makefile"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("break"),
			    short-options: #("d"));
  add-option-parser-by-type(argp,
			    <parameter-option-parser>,
			    long-options: #("cc-override-command"),
			    short-options: #("F"));
  add-option-parser-by-type(argp,
			    <repeated-parameter-option-parser>,
			    long-options: #("cc-override-file"),
			    short-options: #("f"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("debug"),
			    short-options: #("g"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("profile"));
  add-option-parser-by-type(argp,
			    <parameter-option-parser>,
			    short-options: #("j"),
			    long-options: #("thread-count"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("testworks-spec"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("optimizer-sanity-check"));
  add-option-parser-by-type(argp,
			    <simple-option-parser>,
			    long-options: #("static"),
			    short-options: #("s"));
  add-option-parser-by-type(argp,
			    <parameter-option-parser>,
			    long-options: #("rpath"));
  add-option-parser-by-type(argp,
			    <parameter-option-parser>,
			    long-options: #("debug-optimizer",
					    "dump-transforms"));
  add-option-parser-by-type(argp,
			    <repeated-parameter-option-parser>,
			    long-options: #("optimizer-option"),
			    short-options: #("o"));

  // Parse our command-line arguments.
  unless(parse-arguments(argp, args))
    show-usage-and-exit();
  end unless;

  // Handle our informational options.
  if (option-value-by-long-name(argp, "help"))
    show-help(*standard-output*);
    exit(exit-code: 0);
  end if;
  if (option-value-by-long-name(argp, "version"))
    show-copyright(*standard-output*);
    exit(exit-code: 0);
  end if;
  
  // Get our simple options.
  let target-machine-name = option-value-by-long-name(argp, "target") | $default-target-name;
  let target-machine =  as(<symbol>, target-machine-name);
  let library-dirs = option-value-by-long-name(argp, "libdir");
  let features = option-value-by-long-name(argp, "define");
  let log-dependencies = option-value-by-long-name(argp, "log-deps");
  let no-binaries-pre = option-value-by-long-name(argp, "no-binaries");
  let no-makefile = option-value-by-long-name(argp, "no-makefile");
  let no-binaries = no-binaries-pre | no-makefile;
  let cc-override = option-value-by-long-name(argp, "cc-override-command");
  let override-files = option-value-by-long-name(argp, "cc-override-file");
  let link-static = option-value-by-long-name(argp, "static");

  let link-rpath = option-value-by-long-name(argp, "rpath")
       | format-to-string("%s/lib/dylan/%s/%s/dylan-user",
			  $dylan-user-dir,
			  $version, target-machine-name);

  *break-on-compiler-errors* = option-value-by-long-name(argp, "break");
  let debug? = option-value-by-long-name(argp, "debug");
  let profile? = option-value-by-long-name(argp, "profile");
  *emit-all-function-objects?* = debug?;

  // For folks who have *way* too much time (or a d2c bug) on their hands.
  let debug-optimizer = option-value-by-long-name(argp, "debug-optimizer");

  if(debug-optimizer)
    debug-optimizer := string-to-integer(debug-optimizer);
  else
    debug-optimizer := 0;
  end if;

  let dump-testworks-spec? = option-value-by-long-name(argp, "testworks-spec");

  // Determine our compilation target.
  let targets-file = option-value-by-long-name(argp, "platforms") |
    $default-targets-dot-descr;

  // Decide if anyone passed some '-o' flags to our optimizer.
  let optimizer-options = option-value-by-long-name(argp, "optimizer-option");
  let optimizer-option-table = make(<table>);
  for (option :: <string> in optimizer-options)
    let (key, value) =
      if (option.size > 3 & copy-sequence(option, end: 3) = "no-")
	values(copy-sequence(option, start: 3), #f);
      else
	values(option, #t);
      end;
    optimizer-option-table[as(<symbol>, key)] := value;
  end for;

  // How many threads are we using
  let thread-count = option-value-by-long-name(argp, "thread-count");
  if(thread-count)
    thread-count := string-to-integer(thread-count);
  else
    thread-count := #f;
  end if;

  // Figure out which optimizer to use.
  let optimizer-class =
    if (element(optimizer-option-table, #"null", default: #f))
      format(*standard-error*,
	     "d2c: warning: -onull produces incorrect code\n");
      <null-optimizer>;
    else
      <cmu-optimizer>;
    end;
  *current-optimizer* := make(optimizer-class,
			      options: optimizer-option-table,
			      debug-optimizer: debug-optimizer);

  // Set up our target.
  if (targets-file == #f)
    error("Can't find platforms.descr");
  end if;
  parse-platforms-file(targets-file);
  *current-target* := get-platform-named(target-machine);

  define-platform-constants(*current-target*);
  define-bootstrap-module();

  // Stuff in DYLANPATH goes after any explicitly listed directories.
  let dylanpath = getenv("DYLANPATH");
  let dirs = if(dylanpath)
               split-at(method (x :: <character>);
                          x == $search-path-seperator;
                        end,
                        dylanpath);
             else
               list(".", 
                    concatenate($dylan-user-dir, "/lib/dylan/", $version, 
                                "/", target-machine-name, "/dylan-user"),
                    concatenate($dylan-dir, "/lib/dylan/", $version, "/", 
                                target-machine-name));
             end;
  for (dir in dirs)
    push-last(library-dirs, dir);
  end for;
  		       
  *Data-Unit-Search-Path* := as(<simple-object-vector>, library-dirs);

  if (option-value-by-long-name(argp, "compiler-info"))
    show-compiler-info(*standard-output*);
    exit(exit-code: 0);
  end if;
  if (option-value-by-long-name(argp, "dylan-user-location"))
    show-dylan-user-location(*standard-output*);
    exit(exit-code: 0);
  end if;
  if (option-value-by-long-name(argp, "optimizer-sanity-check"))
    enable-sanity-checks();
  end if;

#if(~mindy)
  if (option-value-by-long-name(argp, "interactive"))
    let finished? = #f;
    while(~ finished?)
      format(*standard-output*, "gwydion> ");
      force-output(*standard-output*);
      let line = read-line(*standard-input*, on-end-of-stream: #f);
      if(line)
        block()
          evaluate(line, $empty-environment);
        exception(condition :: <condition>)
          report-condition(condition, *standard-output*);
          format(*standard-output*, "\n");
        end block;
      else
        finished? := #t;
        format(*standard-output*, "\n");
      end if;
    end while;
    exit();
  end if;
#endif

  // Process our regular arguments
  let args = regular-arguments(argp);
  unless (args.size = 1)
    show-usage-and-exit();
  end unless;

  let lid-file = args[0];

  let state
      = if(lid-file.filename-extension = ".dylan")
          format(*standard-output*, "Entering single file mode.\n");
          force-output(*standard-output*);
          make(<single-file-mode-state>,
               source-file: lid-file,
               command-line-features: as(<list>, features), 
               log-dependencies: log-dependencies,
               target: *current-target*,
               no-binaries: no-binaries,
               no-makefile: no-makefile,
               link-static: link-static,
               link-rpath: link-rpath,
               debug?: debug?,
               profile?: profile?);
        else
          make(<lid-mode-state>,
               lid-file: lid-file,
               command-line-features: as(<list>, features), 
               log-dependencies: log-dependencies,
               target: *current-target*,
               no-binaries: no-binaries,
               no-makefile: no-makefile,
               link-static: link-static,
               link-rpath: link-rpath,
               debug?: debug?,
               profile?: profile?,
               dump-testworks-spec?: dump-testworks-spec?,
               cc-override: cc-override,
               override-files: as(<list>, override-files),
	       thread-count: thread-count);
        end if;
  let worked? = compile-library(state);
  exit(exit-code: if (worked?) 0 else 1 end);
end method main;

#if (mindy)
collect-garbage(purify: #t);
#endif
