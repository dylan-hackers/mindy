module: main
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/single-file-mode-state.dylan,v 1.1 2001/09/12 14:39:35 andreas Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define class <single-file-mode-state> (<main-unit-state>)
  slot unit-source-file :: <byte-string>, required-init-keyword: source-file:;
  
  slot unit-name :: <byte-string>; // for single files, name == module == library == executable
  slot unit-lib :: <library>;

  slot unit-mprefix :: <byte-string>;
  slot unit-tlfs :: <stretchy-vector> = make(<stretchy-vector>);
  slot unit-module;
  slot unit-cback-unit :: <unit-state>;
  slot unit-other-cback-units :: <simple-object-vector>;
  
  slot unit-entry-function :: false-or(<ct-function>), init-value: #f;
  slot unit-unit-info :: <unit-info>;
end class <single-file-mode-state>;

define method parse-and-finalize-library (state :: <single-file-mode-state>) => ();
  let source = make(<source-file>, name: state.unit-source-file);
  let (header, start-line, start-posn) = parse-header(source);

  state.unit-header := header;

  do(process-feature,
     split-at-whitespace(state.unit-target.default-features));
  do(process-feature,
     split-at-whitespace(element(state.unit-header, #"features",
				 default: "")));
  do(process-feature, state.unit-command-line-features);
  
  let lib-name = state.unit-header[#"module"];
  state.unit-name := lib-name;
  format(*debug-output*, "Compiling library %s\n", lib-name);
  state.unit-lib    := find-library(as(<symbol>, lib-name), create: #t);
  // XXX: next line is broken
  state.unit-module := find-module(state.unit-lib, as(<symbol>, lib-name), create: #t);
  state.unit-mprefix := as-lowercase(lib-name);

  // XXX these two look suspicious
  // second one is ok, default is now according to DRM
  *defn-dynamic-default* := boolean-header-element(#"dynamic", #f, state);
  *implicitly-define-next-method*
    := boolean-header-element(#"implicitly-define-next-method", #t, state);

  block ()
    format(*debug-output*, "Parsing %s\n", state.unit-source-file);
    let tokenizer = make(<lexer>, 
                         source: source,
                         start-line: start-line,
                         start-posn: start-posn);
    block ()
      *Current-Library* := state.unit-lib;
      *Current-Module*  := state.unit-module;
      *Top-Level-Forms* := state.unit-tlfs;
      parse-source-record(tokenizer);
    cleanup
      *Current-Library* := #f;
      *Current-Module* := #f;
    end;
  exception (<fatal-error-recovery-restart>)
    format(*debug-output*, "skipping rest of %s\n", state.unit-source-file);
  end block;
  format(*debug-output*, "Finalizing definitions\n");
  for (tlf in copy-sequence(state.unit-tlfs))
    note-context(tlf);
    finalize-top-level-form(tlf);
    end-of-context();
  end for;
  format(*debug-output*, "inheriting slots\n");
  inherit-slots();
  format(*debug-output*, "inheriting overrides\n");
  inherit-overrides();
  begin
    let unique-id-base 
      = element(state.unit-header, #"unique-id-base", default: #f);
    if (unique-id-base)
      format(*debug-output*, "assigning unique ids\n");
      assign-unique-ids(string-to-integer(unique-id-base));
    end;
  end;
  format(*debug-output*, "seeding representations\n");
  seed-representations();
  format(*debug-output*, "laying out instances\n");
  layout-instance-slots();
end method parse-and-finalize-library;

/*
// Establish various condition handlers while iterating over all of the source
// files and compiling each of them to an output file.
//
define method compile-all-files (state :: <single-file-mode-state>) => ();
  for (file in state.unit-files,
       tlfs in state.unit-tlf-vectors,
       module in state.unit-modules)
    let extension = file.filename-extension;
    if (extension = state.unit-target.object-filename-suffix)
      if (state.unit-shared?)
	let shared-file
	  = concatenate(file.extensionless-filename,
			state.unit-target.shared-object-filename-suffix);
	format(*debug-output*, "Adding %s\n", shared-file);
	format(state.unit-objects-stream, " %s", shared-file);
      else
	format(*debug-output*, "Adding %s\n", file);
	format(state.unit-objects-stream, " %s", file);
      end;
    else  // assumed a Dylan file, with or without a ".dylan" extension
      block ()
	format(*debug-output*, "Processing %s\n", file);
	let base-name = file.base-filename;
	let c-name = concatenate(base-name, ".c");
        #if (macos)
        let temp-c-name = concatenate(state.unit-lid-file.filename-prefix,
				      c-name);
        #else
        let temp-c-name = concatenate(c-name, "-temp");
        #endif
	let body-stream
	  = make(<file-stream>, locator: temp-c-name, direction: #"output");
	block ()
	  *Current-Module* := module;
	  let file = make(<file-state>, unit: state.unit-cback-unit,
			  body-stream: body-stream);
	  emit-prologue(file, state.unit-other-cback-units);
	  for (tlf in tlfs)
	    block ()
	      compile-1-tlf(tlf, file, state);
	    cleanup
	      end-of-context();
	    exception (<fatal-error-recovery-restart>)
	      #f;
	    end block;
	  end for;
	cleanup
	  close(body-stream);
	  fresh-line(*debug-output*);
	  *Current-Module* := #f;
	end block;

	pick-which-file(c-name, temp-c-name, state.unit-target);
	let o-name
	  = concatenate(base-name,
			if (state.unit-shared?)
			  state.unit-target.shared-object-filename-suffix;
			else
			  state.unit-target.object-filename-suffix;
			end);
	state.unit-all-generated-files
	  := add!(state.unit-all-generated-files, c-name);
	output-c-file-rule(state, c-name, o-name);

      exception (<fatal-error-recovery-restart>)
	format(*debug-output*, "skipping rest of %s\n", file);
      exception (<simple-restart>,
		   init-arguments:
		   vector(format-string: "Blow off compiling this file."))
	#f;
      end block;
    end if;
  end for;
end method compile-all-files;


// Build initialization function for this library, generate the corresponding
// .c and .o and update the make file.
// 
define method build-library-inits (state :: <single-file-mode-state>) => ();
    let executable = element(state.unit-header, #"executable", default: #f);
    let executable
     = if (executable)
	 concatenate(executable, state.unit-target.executable-filename-suffix);
       else 
	 #f;
       end if;
    state.unit-executable := executable;
    let entry-point = element(state.unit-header, #"entry-point", default: #f);
    if (entry-point & ~executable)
      compiler-fatal-error("Can only specify an entry-point when producing an "
			     "executable.");
    end if;

    begin
      let c-name = concatenate(state.unit-mprefix, "-init.c");
      #if (macos)
      let temp-c-name = concatenate(state.unit-lid-file.filename-prefix,
				    c-name);
      #else
      let temp-c-name = concatenate(c-name, "-temp");
      #endif
      let body-stream = make(<file-stream>, 
			     locator: temp-c-name, direction: #"output");
      let file = make(<file-state>, unit: state.unit-cback-unit,
      		      body-stream: body-stream);
      emit-prologue(file, state.unit-other-cback-units);
      if (entry-point)
        state.unit-entry-function
	  := build-command-line-entry(state.unit-lib, entry-point, file);
      end if;
      build-unit-init-function(state.unit-mprefix, state.unit-init-functions,
			       body-stream);
      close(body-stream);

      pick-which-file(c-name, temp-c-name, state.unit-target);
      let o-name
	= concatenate(state.unit-mprefix, "-init", 
		      if (state.unit-shared?)
			state.unit-target.shared-object-filename-suffix
		      else
			state.unit-target.object-filename-suffix
		      end if);
      output-c-file-rule(state, c-name, o-name);
      state.unit-all-generated-files 
	:= add!(state.unit-all-generated-files, c-name);
    end;
end method build-library-inits;


define method build-local-heap-file (state :: <single-file-mode-state>) => ();
  format(*debug-output*, "Emitting Library Heap.\n");
  let c-name = concatenate(state.unit-mprefix, "-heap.c");
  #if (macos)
  let temp-c-name = concatenate(state.unit-lid-file.filename-prefix, c-name);
  #else
  let temp-c-name = concatenate(c-name, "-temp");
  #endif
  let heap-stream = make(<file-stream>, 
			 locator: temp-c-name, direction: #"output");
  let prefix = state.unit-cback-unit.unit-prefix;
  let heap-state = make(<local-heap-file-state>, unit: state.unit-cback-unit,
			body-stream: heap-stream, // target: state.unit-target,
			id-prefix: stringify(prefix, "_L"));

  let (undumped, extra-labels) = build-local-heap(state.unit-cback-unit, 
						  heap-state);
  close(heap-stream);

  pick-which-file(c-name, temp-c-name, state.unit-target);
  let o-name = concatenate(state.unit-mprefix, "-heap", 
			   if (state.unit-shared?)
			     state.unit-target.shared-object-filename-suffix;
			   else
			     state.unit-target.object-filename-suffix
			   end);
  output-c-file-rule(state, c-name, o-name);
  state.unit-all-generated-files 
    := add!(state.unit-all-generated-files, c-name);

  let linker-options = element(state.unit-header, #"linker-options", 
			       default: #f);
  state.unit-unit-info := make(<unit-info>, unit-name: state.unit-mprefix,
			       undumped-objects: undumped,
			       extra-labels: extra-labels,
			       linker-options: linker-options);
end method build-local-heap-file;

define method build-da-global-heap (state :: <single-file-mode-state>) => ();
  format(*debug-output*, "Emitting Global Heap.\n");
  #if (macos)
  let heap-stream 
       = make(<file-stream>, 
	      locator: concatenate(state.unit-lid-file.filename-prefix,
				   "heap.c"), 
	      direction: #"output");
  #else
  let heap-stream 
  	= make(<file-stream>, locator: "heap.c", direction: #"output");
  #endif
  let heap-state = make(<global-heap-file-state>, unit: state.unit-cback-unit,
			body-stream: heap-stream); //, target: state.unit-target);
  build-global-heap(apply(concatenate, map(undumped-objects, *units*)),
		    heap-state);
  close(heap-stream);
end method;


define method build-inits-dot-c (state :: <single-file-mode-state>) => ();
  format(*debug-output*, "Building inits.c.\n");
#if (macos) 
  let stream
   = make(<file-stream>, 
	  locator: concatenate(state.unit-lid-file.filename-prefix,
			       "inits.c"), 
	  direction: #"output");
#else
  let stream
   = make(<file-stream>, locator: "inits.c", direction: #"output");
#endif
  format(stream, "#include <runtime.h>\n\n");
  format(stream, 
	"/* This file is machine generated.  Do not edit. */\n\n");
  let entry-function-name
    = (state.unit-entry-function
	 & (make(<ct-entry-point>, for: state.unit-entry-function,
	 	 kind: #"main")
	      .entry-point-c-name));
  if (entry-function-name)
    format(stream,
	   "extern void %s(descriptor_t *sp, int argc, void *argv);\n\n",
	   entry-function-name);
  end if;
  format(stream,
	 "void inits(descriptor_t *sp, int argc, char *argv[])\n{\n");
  for (unit in *units*)
    format(stream, "    %s_Library_init(sp);\n", string-to-c-name(unit.unit-name));
  end;
  if (entry-function-name)
    format(stream, "    %s(sp, argc, argv);\n", entry-function-name);
  end if;
  format(stream, "}\n");
  format(stream, "\nextern void real_main(int argc, char *argv[]);\n\n");
#if (macos)
  format(stream, "#include<console.h>\n");
#endif
  format(stream, "int main(int argc, char *argv[]) {\n");
#if (macos)
  format(stream, "    argc = ccommand( &argv );\n");
#endif
  format(stream, "    real_main(argc, argv);\n");
  format(stream, "    return 0;\n");
  format(stream, "}\n");
  close(stream);
end method;

define method build-executable (state :: <single-file-mode-state>) => ();
  let target = state.unit-target;
  let unit-libs = "";
  let dash-small-ells = "";
  let linker-args = concatenate(" ", target.link-executable-flags);
  if(state.unit-profile? & target.link-profile-flags)
    linker-args := concatenate(linker-args, " ", target.link-profile-flags);
  end if;

  local method add-archive (name :: <byte-string>) => ();
          if (state.unit-no-binaries)
	    // If cross-compiling use -l -L search mechanism.
	    dash-small-ells := stringify(" -l", name, dash-small-ells);
	  else
	    let archive = find-library-archive(name, state);
	    unit-libs := stringify(' ', archive, unit-libs);
	  end if;
	end method add-archive;

  // Under Unix, the order of the libraries is significant!  First to
  // be added go at the end of the command line...
  add-archive("gc");
  add-archive("runtime");

  for (unit in *units*)
    if (unit.unit-linker-options)
      linker-args
	:= stringify(' ', unit.unit-linker-options, linker-args);
    end if;
    unless (unit == state.unit-unit-info)
      add-archive(concatenate(unit.unit-name, "-dylan"));
    end unless;
  end;

  // We make sure the linker is not given any source files, only
  // library and object files
  format(state.unit-makefile, "\n");
  let inits-dot-o 
    = concatenate("inits", state.unit-target.object-filename-suffix);
  let heap-dot-o
    = concatenate("heap", state.unit-target.object-filename-suffix);
  output-c-file-rule(state, "inits.c", inits-dot-o);
  output-c-file-rule(state, "heap.c", heap-dot-o);

  let dash-cap-ells = "";
  // If cross-compiling, throw in a bunch of -Ls that will probably help.
  if (state.unit-no-binaries)
    for (dir in *data-unit-search-path*)
      dash-cap-ells := concatenate(dash-cap-ells, " -L", dir);
    end for;
    dash-cap-ells
      := concatenate(" $(LDFLAGS)",
      		     use-correct-path-separator(dash-cap-ells,
		     				state.unit-target),
		     " ");						

  end;

  let unit-libs = use-correct-path-separator(unit-libs, state.unit-target);

  // Again, make sure inits.o and heap.o come first
  let objects = format-to-string("%s %s %s %s", inits-dot-o, heap-dot-o, 
				 state.unit-ar-name, unit-libs);

  // rule to link executable
  format(state.unit-makefile, "\n%s : %s\n", state.unit-executable, objects);
  let link-string
    = format-to-string(state.unit-target.link-executable-command,
		       state.unit-executable,
		       concatenate(objects, dash-cap-ells, dash-small-ells," "),
		       linker-args);
  format(state.unit-makefile, "\t%s\n", link-string);

  format(state.unit-clean-stream, " %s %s", 
	 state.unit-ar-name, state.unit-executable);
  format(state.unit-real-clean-stream, " %s %s", 
	 state.unit-ar-name, state.unit-executable);
  format(state.unit-makefile, "\nall-at-end-of-file : %s\n", 
	 state.unit-executable);
end method build-executable;


define method dump-library-summary (state :: <single-file-mode-state>) => ();
  format(*debug-output*, "Dumping library summary.\n");
  let dump-buf
    = begin-dumping(as(<symbol>, state.unit-lib-name),
    		    $library-summary-unit-type);

  for (tlfs in state.unit-tlf-vectors)
    for (tlf in tlfs)
      dump-od(tlf, dump-buf);
    end;
  end;
  dump-od(state.unit-unit-info, dump-buf);
  dump-queued-methods(dump-buf);

  end-dumping(dump-buf);
  format(state.unit-makefile, "\nall-at-end-of-file : %s\n",
  	 state.unit-ar-name);
  format(state.unit-clean-stream, " %s", state.unit-ar-name);
  format(state.unit-real-clean-stream, " %s %s.lib.du", state.unit-ar-name, 
	 as-lowercase(state.unit-lib-name));
end method;


define method do-make (state :: <single-file-mode-state>) => ();
  let target = state.unit-target;
  format(state.unit-makefile, "\nclean :\n");
  format(state.unit-makefile, "\t%s %s\n", target.delete-file-command, 
	 state.unit-clean-stream.stream-contents);
  format(state.unit-makefile, "\nrealclean :\n");
  format(state.unit-makefile, "\t%s %s\n", target.delete-file-command, 
	 state.unit-real-clean-stream.stream-contents);
  close(state.unit-makefile);

  if (pick-which-file(state.unit-makefile-name,
		      state.unit-temp-makefile-name,
		      target)
	= #t)
    // If the new makefile is different from the old one, then we need
    // to recompile all .c and .s files, regardless of whether they
    // were changed.  So touch them to make them look newer than the
    // object files.
    unless (empty?(state.unit-all-generated-files))
      let touch-command = "touch";
      for (filename in state.unit-all-generated-files)
	touch-command := stringify(touch-command, ' ', filename);
      end for;
      format(*debug-output*, "%s\n", touch-command);
      if (system(touch-command) ~== 0)
	cerror("so what", "touch failed?");
      end if;
    end unless;
  end if;

  if (~state.unit-no-binaries)
    let make-string = format-to-string("%s -f %s", target.make-command, 
				       state.unit-makefile-name);
    format(*debug-output*, "%s\n", make-string);
    unless (zero?(system(make-string)))
      cerror("so what", "gmake failed?");
    end;
  end if;
end method do-make;
*/

define method compile-library (state :: <single-file-mode-state>)
    => worked? :: <boolean>;
  block (give-up)
    // We don't really have to give-up if we don't want to, but it
    // seems kind of pointless to compile a file that doesn't parse,
    // or create a dump file for library with undefined variables.
    // Thus, we stick some calls to give-up where it seems useful..
    parse-and-finalize-library(state);
/*
    if (~ zero?(*errors*)) give-up(); end if;
    compile-all-files(state);
    if (~ zero?(*errors*)) give-up(); end if;
    build-library-inits(state);
    build-local-heap-file(state);
    build-ar-file(state);
    if (state.unit-executable)
      log-target(state.unit-executable);
      calculate-type-inclusion-matrix();
      build-da-global-heap(state);
      build-inits-dot-c(state);
      build-executable(state);
    else
      dump-library-summary(state);
    end if;

    if (state.unit-log-dependencies)
      spew-dependency-log(concatenate(state.unit-mprefix, ".dep"));
    end if;

    do-make(state);
*/
  exception (<fatal-error-recovery-restart>)
    format(*debug-output*, "giving up.\n");
  end block;
  
  format(*debug-output*, "Optimize called %d times.\n", *optimize-ncalls*);

  let worked? = zero?(*errors*);
  format(*debug-output*,
	 "Compilation %s with %d Warning%s and %d Error%s\n",
	 if (worked?) "finished" else "failed" end,
	 *warnings*, if (*warnings* == 1) "" else "s" end,
	 *errors*, if (*errors* == 1) "" else "s" end);

  worked?;
end method compile-library;

