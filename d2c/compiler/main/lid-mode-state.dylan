module: main
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/lid-mode-state.dylan,v 1.10 2002/12/20 19:35:00 andreas Exp $
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

define class <lid-mode-state> (<main-unit-state>)
  slot unit-lid-file :: <byte-string>, required-init-keyword: lid-file:;
  
  // A facility for hacking around C compiler bugs by using a different
  // command for particular C compilations.  cc-override is a format string
  // used instead of the normal platform compile-c-command.  It is used
  // whenever compiling one of the files in the override-files list.
  slot unit-cc-override :: false-or(<string>),
    required-init-keyword: cc-override:;
  slot unit-override-files :: <list>,
    required-init-keyword: override-files:;
  
  slot unit-files :: <stretchy-vector>;
  slot unit-lib-name :: <byte-string>;
  slot unit-lib :: <library>;
  // unit-prefix already a <unit-state> accessor
  slot unit-mprefix :: <byte-string>;
  slot unit-tlf-vectors :: <stretchy-vector> = make(<stretchy-vector>);
  slot unit-modules :: <stretchy-vector> = make(<stretchy-vector>);
  slot unit-cback-unit :: <unit-state>;
  slot unit-other-cback-units :: <simple-object-vector>;
  

  slot unit-shared? :: <boolean>, init-keyword: shared?:, init-value: #f;
  
  // Makefile generation streams, etc.
  slot unit-all-generated-files :: <list>, init-value: #();
  slot unit-makefile-name :: <byte-string>;
  slot unit-temp-makefile-name :: <byte-string>;
  slot unit-makefile :: <file-stream>;
  slot unit-objects-stream :: <buffered-byte-string-output-stream>;
  slot unit-clean-stream :: <buffered-byte-string-output-stream>;
  slot unit-real-clean-stream :: <buffered-byte-string-output-stream>;
  
  slot unit-entry-function :: false-or(<ct-function>), init-value: #f;
  slot unit-unit-info :: <unit-info>;
  
  // All names of the .o files we generated in a string.
  slot unit-objects :: <byte-string>;
  
  // The name of the .ar file we generated.
  slot unit-ar-name :: <byte-string>;

  // should this library be a complete embeddable Dylan application
  slot unit-embedded? :: <boolean> = #f;

  // The name of the executable file we generate.
  slot unit-executable :: false-or(<byte-string>);
end class <lid-mode-state>;

// Internal.  escape-pounds returns the string with any '#' characters
// converted to an escaped '\#' character combination for use in Makefiles.
define function escape-pounds (orig :: <string>) => result :: <string>;
  let result = make(<stretchy-vector>, size: orig.size);

  for (from-index :: <integer> from 0 below orig.size,
       to-index from 0)
    if (orig[from-index] == '#')
       result[to-index] := '\\';
       to-index := to-index + 1;
    end if;
    result[to-index] := orig[from-index];
  end for;
  as(<string>, result);
end function escape-pounds;

define method parse-lid (state :: <lid-mode-state>) => ();
  let source = make(<source-file>, name: state.unit-lid-file);
  let (header, start-line, start-posn) = parse-header(source);

  // We support two types of lid files: old "Gwydion LID" and new
  // "official LID". The Gwydion format had a series of file names after
  // the header; the new format has a 'Files:' keyword in the header. We
  // grab the keyword value, transform the filenames in a vaguely appropriate
  // fashion, and then grab anything in the body "as is". This handles both
  // formats. See translate-abstract-filename for details of the new format.
  let contents = source.contents;
  let end-posn = contents.size;

  // Common-Dylan header-file style
  let files = map-as(<stretchy-vector>,
		     translate-abstract-filename,
		     split-at-whitespace(element(header, #"files",
						 default: "")));

  let ofiles = split-at-whitespace(element(header, #"c-object-files",
					   default: ""));

  local
    method repeat (posn :: <integer>)
      if (posn < end-posn)
	let char = as(<character>, contents[posn]);
	if (char.whitespace?)
	  repeat(posn + 1);
	elseif (char == '/' & (posn + 1 < contents.size) 
		  & as(<character>, contents[posn + 1]) == '/')
	  repeat(find-newline(contents, posn + 1));
	else
	  let name-end = find-end-of-word(posn);
	  let len = name-end - posn;
	  let name = make(<byte-string>, size: len);
	  copy-bytes(name, 0, contents, posn, len);
	  add!(files, name);
	  repeat(name-end);
	end;
      end;
    end,

    // find-end-of-word returns the position of the first character
    // after the word, where "end of word" is defined as whitespace.
    method find-end-of-word (posn :: <integer>)
     => end-of-word :: <integer>;
      if (posn < end-posn)
	let char = as(<character>, contents[posn]);
	if (char.whitespace?)
	  posn;
	else
	  find-end-of-word(posn + 1);
	end;
      else
	posn;
      end;
    end method;
 
  repeat(start-posn);

  state.unit-header := header;
  state.unit-files := concatenate(files, ofiles);
  state.unit-executable := element(header, #"executable", default: #f);
  state.unit-embedded? := element(header, #"embedded?", default: #f) & #t;
end method parse-lid;

// save-c-file is #t when we don't want the .c file added to the
// real-clean target.  Used when the C file is actually source code,
// rather than the result of Dylan->C.
//
define method output-c-file-rule
    (state :: <lid-mode-state>, raw-c-name :: <string>, raw-o-name :: <string>,
     #key save-c-file = #f)
 => ();
  let c-name = escape-pounds(raw-c-name);
  let o-name = escape-pounds(raw-o-name);

  let cc-command
      = if (member?(c-name, state.unit-override-files, test: \=))
          state.unit-cc-override;
	elseif(state.unit-shared?
		 & state.unit-target.compile-c-for-shared-command)
	  state.unit-target.compile-c-for-shared-command;
	else
	  state.unit-target.compile-c-command;
	end if;


  format(state.unit-makefile, "%s : %s\n", o-name, c-name);
  format(state.unit-makefile, "\t%s\n",
         format-to-string(cc-command, c-name, o-name));
  format(state.unit-objects-stream, " %s", o-name);
  format(state.unit-clean-stream, " %s", o-name);
  format(state.unit-real-clean-stream, " %s", o-name);
  if (~save-c-file)
    format(state.unit-real-clean-stream, " %s", c-name);
  end if;
end method output-c-file-rule;

define method parse-and-finalize-library (state :: <lid-mode-state>) => ();
  parse-lid(state);
  do(process-feature,
     split-at-whitespace(state.unit-target.default-features));
  do(process-feature,
     split-at-whitespace(element(state.unit-header, #"features",
				 default: "")));
  do(process-feature, state.unit-command-line-features);
  
  let lib-name = state.unit-header[#"library"];
  state.unit-lib-name := lib-name;
  format(*debug-output*, "Compiling library %s\n", lib-name);
  let lib = find-library(as(<symbol>, lib-name), create: #t);
  state.unit-lib := lib;
  state.unit-mprefix := as-lowercase(lib-name);
  if(element(state.unit-header, #"unit-prefix", default: #f))
    format(*debug-output*, "Warning: unit-prefix header is deprecated, ignoring it.\n");
  end if;

  state.unit-shared?
    := ~state.unit-link-static
       & ~state.unit-executable
       & boolean-header-element(#"shared-library", #t, state)
       & state.unit-target.shared-library-filename-suffix
       & state.unit-target.shared-object-filename-suffix
       & state.unit-target.link-shared-library-command
       & #t;

  // XXX these two look suspicious
  // second one is ok, default is now according to DRM
  *defn-dynamic-default* := boolean-header-element(#"dynamic", #f, state);
  *implicitly-define-next-method*
    := boolean-header-element(#"implicitly-define-next-method", #t, state);

  for (file in state.unit-files)
    let extension = file.filename-extension;
    if (extension = state.unit-target.object-filename-suffix)
      // Add any random crap to the unit-tlf-vectors so that it will
      // have as many elements as there are files mentioned in the
      // .lid file
      add!(state.unit-tlf-vectors, make(<stretchy-vector>));
      add!(state.unit-modules, #f);

      if (state.unit-shared?)
	let shared-file
	  = concatenate(file.extensionless-filename,
			state.unit-target.shared-object-filename-suffix);
	let prefixed-filename 
	  = find-file(shared-file,
		      vector($this-dir, state.unit-lid-file.filename-prefix));
	if (prefixed-filename)
	  log-dependency(prefixed-filename);
	else
	  compiler-fatal-error("Can't find object file %=, and thus can't"
				 " record dependency info.", 
			       shared-file);
	end if;
      else
	let prefixed-filename 
	  = find-file(file, vector($this-dir, state.unit-lid-file.filename-prefix));
	if (prefixed-filename)
	  log-dependency(prefixed-filename);
	else
	#if (macos)
		#t;// Do nothing
	#else
	  compiler-fatal-error("Can't find object file %=, and thus can't"
				 " record dependency info.", 
			       file);
	#endif
	end if;
      end if;
    else  // assumed a Dylan file, with or without a ".dylan" extension
      block ()
	format(*debug-output*, "Parsing %s\n", file);
	// ### prefixed-filename is still not (necessarily) an absolute
	// filename, but it's getting closer
	let prefixed-filename
	  = find-file(file, vector($this-dir, state.unit-lid-file.filename-prefix));
	if (prefixed-filename == #f)
	  compiler-fatal-error("Can't find source file %=.", file);
	end if;
	log-dependency(prefixed-filename);
	let (tokenizer, mod) = file-tokenizer(state.unit-lib, 
					      prefixed-filename);
	block ()
	  *Current-Library* := state.unit-lib;
	  *Current-Module* := mod;
	  let tlfs = make(<stretchy-vector>);
	  *Top-Level-Forms* := tlfs;
	  add!(state.unit-tlf-vectors, tlfs);
	  add!(state.unit-modules, mod);
	  parse-source-record(tokenizer);
	cleanup
	  *Current-Library* := #f;
	  *Current-Module* := #f;
	end;
      exception (<fatal-error-recovery-restart>)
	format(*debug-output*, "skipping rest of %s\n", file);
      end block;
    end if;
  end for;
#if (mindy)
  collect-garbage(purify: #t);
#endif
  format(*debug-output*, "seeding representations\n");
  seed-representations();
  format(*debug-output*, "Finalizing definitions\n");
  for (tlfs in state.unit-tlf-vectors)
    *Top-Level-Forms* := tlfs;
    for (tlf in copy-sequence(tlfs))
      note-context(tlf);
      finalize-top-level-form(tlf);
      end-of-context();
    end for;
  end;
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
  format(*debug-output*, "laying out instances\n");
  layout-instance-slots();
end method parse-and-finalize-library;


// Open various streams used to build the makefiles that we generate to compile
// the C output code.
define method emit-make-prologue (state :: <lid-mode-state>) => ();
  let cc-flags
    = getenv("CCFLAGS") 
        | format-to-string(if (state.unit-profile?)
			     state.unit-target.default-c-compiler-profile-flags;
			   elseif (state.unit-debug?)
			     state.unit-target.default-c-compiler-debug-flags;
			   else
			     state.unit-target.default-c-compiler-flags;
			   end if,
			   $runtime-include-dir);

  cc-flags := concatenate(cc-flags, getenv("CCOPTS")|"");

  state.unit-cback-unit := make(<unit-state>, prefix: state.unit-mprefix);
  state.unit-other-cback-units := map-as(<simple-object-vector>, unit-name, 
					 *units*);

  let makefile-name = format-to-string("cc-%s-files.mak", state.unit-mprefix);
#if (macos)
  let temp-makefile-name = "makefile";
#else
  let temp-makefile-name = concatenate(makefile-name, "-temp");
#endif
  state.unit-makefile-name := makefile-name;
  state.unit-temp-makefile-name := temp-makefile-name;
  format(*debug-output*, "Creating %s\n", makefile-name);
  let makefile = make(<file-stream>, locator: temp-makefile-name,
		      direction: #"output", if-exists: #"overwrite");
  state.unit-makefile := makefile;
  format(makefile, "# Makefile for compiling the .c and .s files\n");
  format(makefile, "# If you want to compile .dylan files, don't use "
	   "this makefile.\n\n");
  format(makefile, "CCFLAGS = %s\n", cc-flags);
  let libtool = getenv("LIBTOOL") | state.unit-target.libtool-command;
  if (libtool)
    format(makefile, "LIBTOOL = %s\n", libtool);
  end;

  format(makefile, "# We only know the ultimate target when we've finished"
	   " building the rest\n");
  format(makefile, "# of this makefile.  So we use this fake "
	   "target...\n#\n");
  format(makefile, "all : all-at-end-of-file\n\n");

  // These next three streams gather filenames.  Objects-stream is
  // simply *.o.  clean-stream is the list of files we will delete
  // with the "clean" target--all objects plus the library archive
  // (.a), the library summary (.du), and the executable.
  // real-clean-stream is everything in clean plus *.c, *.s, and
  // cc-lib-files.mak.
  //
  state.unit-objects-stream := make(<buffered-byte-string-output-stream>);
  state.unit-clean-stream := make(<buffered-byte-string-output-stream>);
  state.unit-real-clean-stream := make(<buffered-byte-string-output-stream>);
  format(state.unit-real-clean-stream, " %s", makefile-name);
end method emit-make-prologue;

// Establish various condition handlers while iterating over all of the source
// files and compiling each of them to an output file.
//
define method compile-all-files (state :: <lid-mode-state>) => ();
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
define method build-library-inits (state :: <lid-mode-state>) => ();
    let executable
     = if (state.unit-executable)
	 concatenate(state.unit-executable, state.unit-target.executable-filename-suffix);
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


define method build-local-heap-file (state :: <lid-mode-state>) => ();
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


define method build-ar-file (state :: <lid-mode-state>) => ();
  let objects = stream-contents(state.unit-objects-stream);
  let target = state.unit-target;
  let suffix = split-at-whitespace(if (state.unit-shared?) 
				     target.shared-library-filename-suffix;
				   else
				     target.library-filename-suffix;
				   end if).first;
  let ar-name = concatenate(target.library-filename-prefix,
  			    state.unit-mprefix,
                            "-dylan",
			    suffix);

  state.unit-objects := objects;
  state.unit-ar-name := ar-name;
  format(state.unit-makefile, "\n%s : %s\n", ar-name, objects);
  format(state.unit-makefile, "\t%s %s\n",
	 target.delete-file-command, ar-name);
  
  let objects = use-correct-path-separator(objects, target);

  let link-string = if (state.unit-shared?)
		      format-to-string(target.link-shared-library-command,
				       ar-name, objects,
				       state.unit-link-rpath);
		    else
		      format-to-string(target.link-library-command,
				       ar-name, objects);
		    end;

  format(state.unit-makefile, "\t%s\n", link-string);
  
  if (target.randomize-library-command & ~state.unit-shared?)
    let randomize-string = format-to-string(target.randomize-library-command,
					    ar-name);
    format(state.unit-makefile, "\t%s\n", randomize-string);
  end if;

  format(state.unit-clean-stream, " %s", state.unit-ar-name);
  format(state.unit-real-clean-stream, " %s", state.unit-ar-name);
  format(state.unit-makefile, "\nall-at-end-of-file : %s\n", 
	 state.unit-ar-name);
end method;


define method build-da-global-heap (state :: <lid-mode-state>) => ();
  format(*debug-output*, "Emitting Global Heap.\n");
  let c-name = concatenate(state.unit-lid-file.filename-prefix, "heap.c");
  let o-name = concatenate(state.unit-lid-file.filename-prefix, "heap",
                           if (state.unit-shared?)
			     state.unit-target.shared-object-filename-suffix;
			   else
			     state.unit-target.object-filename-suffix
			   end);
  let heap-stream 
  	= make(<file-stream>, locator: c-name, direction: #"output");
  let heap-state = make(<global-heap-file-state>, unit: state.unit-cback-unit,
			body-stream: heap-stream); //, target: state.unit-target);
  build-global-heap(apply(concatenate, map(undumped-objects, *units*)),
		    heap-state);
  close(heap-stream);
  output-c-file-rule(state, c-name, o-name);
  state.unit-all-generated-files 
    := add!(state.unit-all-generated-files, c-name);
end method;


define method build-inits-dot-c (state :: <lid-mode-state>) => ();
  format(*debug-output*, "Building inits.c.\n");
  let c-name = concatenate(state.unit-lid-file.filename-prefix, "inits.c");
  let o-name = concatenate(state.unit-lid-file.filename-prefix, "inits",
                           if (state.unit-shared?)
			     state.unit-target.shared-object-filename-suffix;
			   else
			     state.unit-target.object-filename-suffix
			   end);
  let stream
   = make(<file-stream>, locator: c-name, direction: #"output");
  format(stream, "#include \"runtime.h\"\n\n");
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
  if(state.unit-embedded?)
  format(stream,
	 "void inits()\n{\n    descriptor_t *sp = allocate(64*1024);\n\n");
  else    
    format(stream,
	 "void inits(descriptor_t *sp, int argc, char *argv[])\n{\n");
  end if;
  for (unit in *units*)
    format(stream, "    %s_Library_init(sp);\n", string-to-c-name(unit.unit-name));
  end;
  if (entry-function-name)
    format(stream, "    %s(sp, argc, argv);\n", entry-function-name);
  end if;
  format(stream, "}\n");
  if(~state.unit-embedded?)
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
 end if;
 close(stream);
 output-c-file-rule(state, c-name, o-name);
 state.unit-all-generated-files 
   := add!(state.unit-all-generated-files, c-name);
end method;

define method build-executable (state :: <lid-mode-state>) => ();
  let target = state.unit-target;
  let unit-libs = "";
  let dash-small-ells = "";
  let linker-args = concatenate(" ", target.link-executable-flags);
  if(state.unit-profile? & target.link-profile-flags)
    linker-args := concatenate(linker-args, " ", target.link-profile-flags);
  end if;
  if(state.unit-debug? & target.link-debug-flags)
    linker-args := concatenate(linker-args, " ", target.link-debug-flags);
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

  let objects = format-to-string("%s %s", 
                                 stream-contents(state.unit-objects-stream),
                                 unit-libs);

  // rule to link executable
  format(state.unit-makefile, "\n%s : %s\n", state.unit-executable, objects);
  let link-string
    = format-to-string(state.unit-target.link-executable-command,
		       state.unit-executable,
		       concatenate(objects, dash-cap-ells, dash-small-ells," "),
		       linker-args);
  format(state.unit-makefile, "\t%s\n", link-string);

  format(state.unit-clean-stream, " %s", state.unit-executable);
  format(state.unit-real-clean-stream, " %s", state.unit-executable);
  format(state.unit-makefile, "\nall-at-end-of-file : %s\n", 
	 state.unit-executable);
end method build-executable;


define method dump-library-summary (state :: <lid-mode-state>) => ();
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


define method do-make (state :: <lid-mode-state>) => ();
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


define method compile-library (state :: <lid-mode-state>)
    => worked? :: <boolean>;
  block (give-up)
    // We don't really have to give-up if we don't want to, but it
    // seems kind of pointless to compile a file that doesn't parse,
    // or create a dump file for library with undefined variables.
    // Thus, we stick some calls to give-up where it seems useful..
    parse-and-finalize-library(state);
    if (~ zero?(*errors*)) give-up(); end if;
    emit-make-prologue(state);
    compile-all-files(state);
    if (~ zero?(*errors*)) give-up(); end if;
    build-library-inits(state);
    build-local-heap-file(state);
    if(state.unit-executable | state.unit-embedded?)
      calculate-type-inclusion-matrix();
      build-da-global-heap(state);
      build-inits-dot-c(state);
    end if;
    if (state.unit-executable)
      log-target(state.unit-executable);
      build-executable(state);
    else
      build-ar-file(state);
      dump-library-summary(state);
    end if;

    if (state.unit-log-dependencies)
      spew-dependency-log(concatenate(state.unit-mprefix, ".dep"));
    end if;

    do-make(state);

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

