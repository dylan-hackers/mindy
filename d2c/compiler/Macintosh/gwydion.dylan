module: warrior
rcs-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/gwydion.dylan,v 1.2.2.2 2004/10/09 02:13:26 gabor Exp $
file: gwydion.dylan
author: gabor@mac.com
status: really need some cleanup first.
copyright: see below

//======================================================================
//
// Copyright (c) 2000 - 2004  Gwydion Dylan Maintainers
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

// This is the compilation driver for the Gwydion Dylan CodeWarrior plugin.
//

// stuff to implement:
// check that all .dylan project files are mentioned in .lid
// check that no two files in .lid point to the same project file (use FindAndLoad with suppressload)
// check that all files in .lid are project files (redundant?) they will be compiled as .dylan files
//	alien .lid files containing .o or other filetypes should be considered!
// change the color of the Stop button depending on GC situation red: little space, yellow: GC, green: plenty

// TODO: line-numbers (sum up), erase library
// merge split-at-whitespace and split-at-colon
// chop "Error:" from messages -- partly done
// <question>, <intention>
// accept target and subtarget notifications to clear *units*
// add seals on initialize & make
// fix duplicated errors/warnings
// factor out stuff from error/warning handlers (curry?)
// correctly analyse OSErrs vs. IDE errors
// *debug-stream* etc. should be nop-streams
// features (default sets etc. too!)
// line-numbers (see: compiled-lines in <object-data> too)
// bug when reimporting XML panel prefs: output folder lost when reopening prefs
// ensure file-id is text, skip otherwise
// use extract-source from errors.dylan


// BUGS to be fixed (in compiler/ runtime lib):
// (convert/deffunc.dylan) <real-define-method-tlf> has no print-object method?
// see: cw-output-streams.dylan line 249					end if;	// compiler ICE if I write "endif" instead of "end if" here
// func.dylan:  cached-classes-setter is unneeded, make slot constant!
// compiler-fatal-error should be marked as <never-returns> this would avoid several calls to type-error (e.g. in variables.c)
// compiler-fatal-error gets swallowed often and my hander not invoked --- bug in d2c!
// it looks like format-out cannot handle <c-strings>
// crash when cancelling compile when searching for units (immediately after beginning compile)


define constant $platform-table
= #(	platform-name:,							#"MacOS-ppc-CodeWarrior",
		integer-length:,						32,
		pointer-size:,							4,
		short-size:,							2,
		long-size:,								4,
		integer-size:,							4,	// get from preferences for 68k
		single-size:,							4,
		double-size:,							8,
		long-double-size:,						8,

long-long-size:, 8,

pointer-alignment:, 4,
integer-alignment:, 4,
short-alignment:, 2,
long-alignment:, 4,
long-long-alignment:, 4,
single-alignment:, 4,
double-alignment:, 4,
long-double-alignment:, 4,

single-mantissa-digits:, 24,
double-mantissa-digits:, 53,
long-double-mantissa-digits:, 53,
minimum-single-float-exponent:, -125,
maximum-single-float-exponent:, 128,
minimum-double-float-exponent:, -1021,
maximum-double-float-exponent:, 1024,
minimum-long-double-float-exponent:, -1021,
maximum-long-double-float-exponent:, 1024,

make-jobs-flag:, "",

		big-endian?:,							#t,
		compile-c-command:,						"",
		default-c-compiler-flags:,				"",
		default-c-compiler-debug-flags:,		"",
		default-c-compiler-profile-flags:,	"",
		assembler-command:,						"",
		link-library-command:,					"",
		link-executable-command:,				"",
		link-executable-flags:,					"",
		path-separator:,						':',
		default-features:,						"compiled-for-macintosh",	// name of the feature set
		make-command:,							"",
		object-filename-suffix:,				"",
		library-filename-prefix:,				"",
		library-filename-suffix:,				"",
		executable-filename-suffix:,			"",
		delete-file-command:,					"",
		compare-file-command:,					"",
		move-file-command:,						"",
		uses-drive-letters?:,					#f,
		environment-variables-can-be-exported?:,#f,
		use-dbclink?:,							#f,
		makefile-name:,							"",
		make-supports-phony-targets?:,			#f,
		makefiles-can-rebuild-themselves?:,		#f,
		recursive-make-command:,				""	);



define function chop-string(s :: <byte-string>, prefix :: <byte-string>) => without :: <byte-string>;
  subsequence-position(s, prefix) == 0
    & copy-sequence(s, start: prefix.size)
    | s;
end function chop-string;

define constant chop-error :: <function> = rcurry(chop-string, "Error: ");
define constant chop-warning :: <function> = rcurry(chop-string, "Warning: ");


// Information which needs to go into the library dump file.
//
define class <unit-info> (<object>)
  slot unit-name :: <byte-string>,
    required-init-keyword: #"unit-name";
  
  slot undumped-objects :: <simple-object-vector>,
    required-init-keyword: #"undumped-objects";
  
  slot extra-labels :: <simple-object-vector>,
    required-init-keyword: #"extra-labels";
  
  slot unit-linker-options :: false-or(<byte-string>),
    init-value: #f, init-keyword: #"linker-options";
end class <unit-info>;

standard-seals-for(<unit-info>);

define variable *units* :: <stretchy-vector> = stretchy-vector();

define method initialize (info :: <unit-info>, #next next-method, #key) => ();
  next-method();
  add!(*units*, info);
end;
  
add-make-dumper(#"unit-info", *compiler-dispatcher*, <unit-info>,
		list(unit-name, unit-name:, #f,
		     undumped-objects, undumped-objects:, #f,
		     extra-labels, extra-labels:, #f,
		     unit-linker-options, linker-options: #f));


// this is defined elsewhere too, but cannot find it...
define inline function stretchy-vector() => vec :: <stretchy-vector>;
 	<stretchy-vector>.make
end function stretchy-vector;


// Considers anything with an ASCII value less than 32 (' ') to be
// whitespace.  This includes control characters as well as what we
// normally consider whitespace.
define function split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  split-at(method (x :: <character>) x <= ' ' end, string);
end function split-at-whitespace;


// Split a string at locations where test returns true, removing the delimiter
// characters.
define function split-at (test :: <function>, string :: <byte-string>)
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
end function split-at;


define function translate-abstract-filename(abstract-name :: <byte-string>)
 => (physical-name :: <byte-string>)
  // XXX - We should eventually replace this with a routine that checks
  // for foo.dylan and then foo.dyl, preferably using some sort of abstract
  // locator translation. But for now, we keep it simple.
  
  concatenate(abstract-name, ".dylan");
end function translate-abstract-filename;


define function parse-lid-file(plug :: <plugin-callback>, text :: <byte-string>) => (header, files :: <stretchy-vector>);
	let main-file = plug.get-main-file-spec;
	let source = make(<cw-source-file>, name: main-file.spec-file-name, buffer: as(<buffer>, text));	// not very efficient! ¥¥¥ pass buffer around!
	let (header, start-line, start-posn) = source.parse-header;

	// We support to types of lid files: old "Gwydion LID" and new
	// "official LID". The Gwydion format had a series of file names after
	// the header; the new format has a 'Files:' keyword in the header. We
	// grab the keyword value, transform the filenames in a vaguely appropriate
	// fashion, and then grab anything in the body "as is". This handles both
	// formats. See translate-abstract-filename for details of the new format.

	let contents = source.contents;
	let end-posn = contents.size;
	let files = map-as(<stretchy-vector>,
									translate-abstract-filename,
									split-at-whitespace(element(header, #"files", default: "")));

	local method repeat(posn :: <integer>) => ();
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
					end if;
				end if;
			end method repeat,

			// find-end-of-word returns the position of the first character
			// after the word, where "end of word" is defined as whitespace.
			method find-end-of-word(posn :: <integer>) => end-of-word :: <integer>;
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
			end method find-end-of-word;

	repeat(start-posn);

	values(header, files)
end function parse-lid-file;

define function ending-line-number(loc) => line :: <integer>;
	select (loc by instance?)
		<source-location-mixin> => loc.source-location.ending-line-number;
		<known-source-location> => loc.end-line;
		otherwise => 0;
	end select;
end function;

define function plausible-line-number(loc :: <source-location>) => line :: <integer>;
	loc.ending-line-number
end function;

#if (want-native-hunks)
define c-enumeration
	HUNK_START = #x4567,
	HUNK_END,
	HUNK_SEGMENT,
	HUNK_LOCAL_CODE,
	HUNK_GLOBAL_CODE,
	HUNK_LOCAL_UDATA,
	HUNK_GLOBAL_UDATA,
	HUNK_LOCAL_IDATA,
	HUNK_GLOBAL_IDATA,
	HUNK_GLOBAL_ENTRY,
	HUNK_LOCAL_ENTRY,
	HUNK_IMPORT,
	HUNK_XREF_16BIT,
	HUNK_XREF_16BIT_IL,
	HUNK_XREF_24BIT,
	HUNK_XREF_32BIT,
	HUNK_XREF_32BIT_REL,
	HUNK_DEINIT_CODE, /* Reserved */
	HUNK_LIBRARY_BREAK, /* Obsolete */
	HUNK_IMPORT_CONTAINER,
	HUNK_SOURCE_BREAK,
	HUNK_XREF_16BIT_REL,
	HUNK_METHOD_REF
end c-enumeration;

define c-enumeration
	XMC_PR = 0, /* Program Code */
XMC_RO = 1, /* Read Only Constant */
XMC_GL = 6, /* Global Linkage */
/* Read/write classes */
XMC_RW = 5, /* Read Write Data */
XMC_TC0 = 15, /* TOC Anchor */
XMC_TC = 3, /* General TOC Entry */
XMC_TD = 16, /* Scalar TOC Data */
XMC_DS = 10 /* Routine Descriptor */
end c-enumeration;

define method hunk-type(hunk :: <data-hunk>) => type :: <integer>;
	HUNK_GLOBAL_IDATA
end;

define method hunk-type(hunk :: <xref-hunk>) => type :: <integer>;
	HUNK_XREF_32BIT
end;

define function align-4(pos :: <integer>, data-vector :: <cw-vector>) => ();
	let pos = pos.odd? & write-1(0, data-vector, pos: pos) | pos;
	ash(pos, -1).odd? & write-2(0, data-vector, pos: pos);
end;

define generic write-hunk(hunk :: <hunk>, data-vector :: <cw-vector>) => ();

define method write-hunk(hunk :: <hunk>, data-vector :: <cw-vector>) => ();
	write-2(hunk.hunk-type, data-vector);
//	write-2(0, data-vector);	// pad
end;

define method write-hunk(hunk :: <simple-hunk>, data-vector :: <cw-vector>, #next next-method) => ();
	next-method();
	write-2(0, data-vector);	// pad
end;


define generic write-hunk-element(stuff, data-vector :: <cw-vector>) => written :: <integer>;

define method write-hunk-element(stuff :: <integer>, data-vector :: <cw-vector>)
	=> written :: <integer>;
	write-4(stuff, data-vector);
	4
end;

define method write-hunk-element(stuff :: <byte-string>, data-vector :: <cw-vector>)
	=> written :: <integer>;
	let before = data-vector.size;
	write-raw-str(stuff, data-vector, zero: stuff.size.odd?, pos: before) - before;
end;

define method write-hunk(hunk :: <data-hunk>, data-vector :: <cw-vector>, #next next-method) => ();
	next-method();
	write-1(XMC_RW, data-vector);
	write-1(4, data-vector);	// word aligned, no flags
	let size-gap = write-4(hunk.hunk-name-id, data-vector);
	write-4(0, data-vector, pos: size-gap);	// size placeholder
	write-4(/* #x80000000 */ -2147483648, data-vector);	// sym-type-id
	write-4(0, data-vector);	// sym-decl-offset
//	write-4(#x0EADBEEF, data-vector);	// payload
	let computed-size = reduce(method (sofar :: <integer>, stuff)
															 sofar + write-hunk-element(stuff, data-vector)
														 end,
														 0,
														 hunk.hunk-payload);
	let curr-cap = data-vector.size;
	write-4(computed-size, data-vector, pos: size-gap, capacity: curr-cap);	// real size
	align-4(curr-cap, data-vector);
	do(rcurry(write-hunk, data-vector), hunk.hunk-xrefs);
end;

define method write-hunk(hunk :: <xref-hunk>, data-vector :: <cw-vector>, #next next-method) => ();
	next-method();
	write-1(XMC_RW, data-vector);
	write-1(0, data-vector);	// unused
	write-4(hunk.hunk-name-id, data-vector);
	write-4(hunk.hunk-xref-offset, data-vector);
end;

/*
define function write-object-data(plug :: <plugin-callback>, name-table, hunks) => ()
//		let object-data = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);


//		let data :: <ppc-object-data> = make(<ppc-object-data>, object-data: plug.new-mem-handle);
		
//	fill-object-data-header(plug, data);
end;
*/

#endif

define function warning-handler
	(problem :: <compiler-warning>,
	 next :: <function>,
	 in-file,
	 plug :: <plugin-callback>)
 => ();
  let stream = make(<byte-string-stream>, direction: #"output");
  report-condition(problem, stream);

  report-message(	plug,
									in-file,
									stream.stream-contents.chop-error,
									problem.condition-at.plausible-line-number,
									$message-type-warning,
									0);	// I not yet have error numbers!
end;

define function error-handler
	(problem :: <compiler-error>,
	 next :: <function>,
	 in-file,
	 plug :: <plugin-callback>)

//  next(); needed for counting?
  let stream = make(<byte-string-stream>, direction: #"output");
  report-condition(problem, stream);

  report-message(	plug,
									in-file,
									stream.stream-contents.chop-error,
									problem.condition-at.plausible-line-number,
									$message-type-error,
									0);	// I not yet have error numbers!
end;


define function compile-lid-file(plug :: <plugin-callback>, text :: <byte-string>)
	let tlf-groups :: <stretchy-vector> = stretchy-vector();
	let tlf-group-modules :: <stretchy-vector> = stretchy-vector();
	
	let lib-name-symbol = #f;
	let lib-name = #f;

	block ()
		*current-target* := apply(make, <platform>, $platform-table);
		define-platform-constants(*current-target*);

		let (lid-header, files :: <stretchy-vector>) = parse-lid-file(plug, text);
		if(element(lid-header, #"unit-prefix", default: #f))
			compiler-warning("unit-prefix header is deprecated, ignoring it");	// maybe with a location somehow?¥¥
		end if;

    let lid-header
		= begin
			let (	always-next-method :: <boolean>,
					command-line :: <boolean>,
					use-group :: <boolean>,
					group-path :: <byte-string>,
					output-folder :: <file-spec>.false-or,
					debug-code :: <boolean>,
					id-base :: <integer>.false-or) = plug.get-gwydion-prefs;

			*implicitly-define-next-method* := boolean-header-element(#"implicitly-define-next-method", always-next-method, lid-header);
			*emit-all-function-objects?* := debug-code;
//better:			id-base & (lid-header[#"unique-id-base"] := id-base.integer-to-string);
//			id-base & (lid-header[#"unique-id-base"] := format-to-string("%d", id-base));
/////// uncommenting above line brings our lovely <happy-buffer> ICE¥¥¥!
// header needs to be mutable!

      // here is how:
      if (id-base)
      	header-add(lid-header, #"unique-id-base", format-to-string("%d", id-base));
//      	header-add(lid-header, #"unique-id-base", id-base.integer-to-string3); // ¥¥¥ brings us double errors!!
      else
      	lid-header
      end if;
		end begin;
		
		lib-name := lid-header[#"library"];
		
		let lib =
			begin
				let handler <compiler-error>	// to get the "Puked loading... " messages
					= rcurry(error-handler, #f, plug);
				
				find-library(lib-name-symbol := as(<symbol>, lib-name), create: #t);
			end;

		for (file in files)

			// ¥¥¥ make sure it is a .dylan file!
			
			plug.user-break;
			
			let handler <plugin-fnf-error> =
				method(err, next)
					report-message(plug, #f /* in-file */, concatenate("Source file '", file, "' not found"), /* line-number :: <integer> */ 0, $message-type-error, /* error-number */ 0 /*, #key line-2 :: <byte-string> = ""*/)
				end;
			
			let (	file-data :: <machine-pointer>,
					file-data-length :: <integer>,
					file-data-type :: <integer>,
					file-id :: <integer>,
					file-spec :: <file-spec>)
				= cw-find-and-load-file(plug, file, dependency-type: $normal-dependency, want-file-spec: #t, full-search: #t);
			
			// ¥¥¥ ensure file-id is text, skip otherwise

			let b :: <buffer> =
				block()
					show-status(plug, concatenate("Parsing: ", file));
				
					let b = make(<buffer>, size: file-data-length, end: file-data-length);
					call-out("BlockMoveData", void:, ptr: file-data.raw-value, ptr: b.buffer-address, int: file-data-length);
					b
				cleanup
					release-file-text(plug, file-data);
				end block;

			let source = make(<cw-source-file>, name: file, buffer: b);

			let (header, start-line, start-posn) = source.parse-header;

			display-lines(plug, start-line);

			block()
				let (tokenizer, module) =
							values(make(<lexer>, source: source, start-posn: start-posn, start-line: start-line),
										find-module(lib, as(<symbol>, header[#"module"]), create: #t));

				*current-module* := module;
				*current-library* := lib;

				let tlfs = stretchy-vector();
				*Top-Level-Forms* := tlfs;
				
				let handler <compiler-warning>
					= method(problem :: <compiler-warning>, next :: <function>)
							// next(); // for now... (we could put it into an object table to signify it has been reported!)
							let stream = make(<byte-string-stream>, direction: #"output");
							report-condition(problem, stream);
							
							report-message(	plug,
														file-spec,
														stream.stream-contents.chop-warning,
														problem.condition-at.plausible-line-number,
														$message-type-warning,
														0);	// I not yet have error numbers!
						end;

				let handler <compiler-error>
					= method(problem :: <compiler-error>, next :: <function>)
						next();
						let stream = make(<byte-string-stream>, direction: #"output");
						report-condition(problem, stream);
						
						report-message(	plug,
													file-spec,
													stream.stream-contents.chop-error,
													problem.condition-at.plausible-line-number,
													$message-type-error,
													0);	// I not yet have error numbers!
					end;

				tokenizer.parse-source-record;

// not yet				scavenge-od-loader-caches();

				unless (tlfs.empty?)
					let last :: <top-level-form> = tlfs.last;
					let end-line = last.ending-line-number;
					display-lines(plug, end-line);
				end;

				add!(tlf-groups, tlfs);
				add!(tlf-group-modules, module);
			cleanup
				*current-module* := #f;
				*current-library* := #f;
			end block;
		end for;	// each .dylan file

		show-status(plug, "Finalizing definitions");
		
		for (tlfs in tlf-groups, file in files)
			let file-spec
				= cw-maybe-find-file(plug, file, full-search: #t);

			let handler <compiler-error>
				= rcurry(error-handler, file-spec, plug);

			let handler <compiler-warning>
				= rcurry(warning-handler, file-spec, plug);

			*Top-Level-Forms* := tlfs;
			for (tlf :: <top-level-form> in tlfs.copy-sequence)
				tlf.note-context;
				tlf.finalize-top-level-form;
				end-of-context();
			end for;
		end for;
		
		let handler <compiler-error>
			= rcurry(error-handler, #f, plug);

		let handler <compiler-warning>
				= rcurry(warning-handler, #f, plug);

		show-status(plug, "Inheriting slots");
		inherit-slots();

		show-status(plug, "Inheriting overrides");
		inherit-overrides();

		begin
			let unique-id-base = element(lid-header, #"unique-id-base", default: #f);
			unique-id-base & show-status(plug, "Assigning unique ids");
			unique-id-base & unique-id-base.string-to-integer.assign-unique-ids;
		end;

		show-status(plug, "Seeding representations");
		seed-representations();

		show-status(plug, "Laying out instance slots");
		layout-instance-slots();
		
		let unit-lid-file = lib-name.as-lowercase;	// misleading name!!!¥¥¥
		let unit-cback-unit = make(<unit-state>, prefix: unit-lid-file);
		let init-functions :: <stretchy-vector> = stretchy-vector();
		let other-cback-units :: <simple-object-vector> = map-as(<simple-object-vector>, unit-name, *units*);
		compile-all-files(plug, files, tlf-groups, tlf-group-modules, unit-cback-unit, other-cback-units, init-functions);
		let (executable :: <boolean>, entry-function)
			= build-library-inits(plug, lid-header, unit-lid-file, concatenate(unit-lid-file, ".lib"), unit-cback-unit, other-cback-units, init-functions, lib);
		let info :: <unit-info>
			= build-local-heap-file(plug, lid-header, unit-lid-file, unit-cback-unit);

		let (name-table, hunks)
			= if (executable)
					calculate-type-inclusion-matrix();
					let (name-table, hunks) = build-da-global-heap(plug, unit-cback-unit);
		//			write-object-data(plug, name-table, hunks);
					build-inits-dot-c(plug, entry-function, element(lid-header, #"entry-point", default: #f));
					values(name-table, hunks);
				else
					dump-library-summary(plug, concatenate(unit-lid-file, ".lib"), info, tlf-groups);
				end if;

		let data :: <ppc-object-data> = make(<ppc-object-data>, object-data: plug.new-mem-handle);
		
		let browser-info = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);
		
		write-browse-header(browser-info);
		
		let main-file-number = plug.get-main-file-number;
		
		let file-table :: <table> = <table>.make;
		let browser-options :: <table> = plug.get-browse-options;
		
		// check browser options¥¥¥
		
		local method file-id-callback(option :: <symbol>, name :: <byte-string>) => id :: <integer>.false-or;
				
				let namesym :: <symbol> = as(<symbol>, name);
				let found = element(file-table, namesym, default: #"none");
				
				if (found == #"none")
					plug.user-break;
					
					file-table[namesym]
						:=block ()
								let (file-data :: <machine-pointer>,
										file-data-length :: <integer>,
										file-data-type :: <integer>,
										file-id :: <integer>,
										file-spec :: #f.singleton,
										already-included :: <boolean>,
										record-browse-info :: <boolean>)
									= cw-find-and-load-file(plug, name, dependency-type: $normal-dependency, /*is-dependent-of-file: main-file-number,*/
																					suppress-load: #t, full-search: #t);
								
								record-browse-info & file-id
							exception(<plugin-fnf-error>)
							end block
				else
					found
				end if
			end method;
		
		do(compose(rcurry(dump-browser, browser-info), rcurry(gather-browser-infos, file-id-callback)),
				tlf-groups);

		write-browse-end(browser-info);
		
//		fill-object-data-header(plug, data);	// use keywords¥¥¥
#if (want-native-hunks)
		local method write-object(data-vector :: <cw-vector>)
					 => (obj-size :: <integer>,
							 nametable-offset :: <integer>,
							 nametable-names :: <integer>,
							 code-size :: <integer>,
							 udata-size :: <integer>,
							 idata-size :: <integer>);

				write-hunk(make(<simple-hunk>, type: HUNK_START), data-vector);
				do(rcurry(write-hunk, data-vector), hunks);
				write-hunk(make(<simple-hunk>, type: HUNK_END), data-vector);

				let nametable-offset = data-vector.size;
				let names = name-table.key-sequence;
				let sorted :: <stretchy-vector> = make(<stretchy-vector>, size: names.size);
/*				for (i from 0 below names.size)
					sorted[i] := i;
				end for;*/
				for (name in names)
					sorted[name-table[name] - 1] := name;
				end for;
				for (name in sorted)
					write-2(name.c-hash, data-vector);
					write-raw-str(name, data-vector);
				end for;
				values(0, nametable-offset, names.size, 0, 0, 0)
			end;

		fill-object-data-header(plug, data, name-table & write-object);	// use keywords¥¥¥
#else
		fill-object-data-header(plug, data, #f);	// use keywords¥¥¥
#endif
		data.browse-data := browser-info;

		store-object-data(plug, main-file-number, data);
	cleanup
/* this is not ripe yet.
		*current-target* := #f;
		*Top-Level-Forms* := #f;
		remove!(*units*, lib-name, test: method(unit :: <unit-info>, name :: <byte-string>) unit.unit-name == name end);
		remove-key!($Libraries, lib-name-symbol);

		do(curry(remove-key!, $class-for-id), choose(loaded?.complement, *all-classes*));
		local method loaded-is(class :: <cclass>, val :: <boolean>) => result :: <boolean>;
												class.loaded? == val
											end,
								remove-unloaded!(classes :: <mutable-sequence>) => ();
										remove!(classes, #f, test: loaded-is);
									end;

		remove-unloaded!(*all-classes*);
		do(compose(remove-unloaded!, subclasses), *all-classes*);
		do(compose(remove-unloaded!, cclass-direct-subclasses), *all-classes*);

		// now clean all slot-infos from positions
		do(compose(curry(do, reset-slot), all-slot-infos), *all-classes*); */
	exception (<fatal-error-recovery-restart>)
		#f
	end block;
end function compile-lid-file;


// The actual meat of compilation.  Does FER conversion, optimizes and emits
// output code.
//
define function compile-1-tlf
    (tlf :: <top-level-form>, file :: <file-state>, init-functions :: <vector>) => ();

	let name = format-to-string("%s", tlf);
	name.note-context;
	let component = <fer-component>.make;
	let builder = component.make-builder;
	convert-top-level-form(builder, tlf);
	let inits = builder.builder-result;
	let name-obj = make(<anonymous-name>, location: tlf.source-location);
	unless (instance?(inits, <empty-region>))
		let result-type = make-values-ctype(#(), #f);
		let source = <source-location>.make;
		let init-function
			= build-function-body(builder, $Default-Policy, source, #f,
													name-obj, #(), result-type, #t);
		build-region(builder, inits);
		build-return(builder, $Default-Policy, source, init-function, #());
		end-body(builder);
		let sig = make(<signature>, specializers: #(), returns: result-type);
		let ctv = make(<ct-function>, name: name-obj, signature: sig);
		make-function-literal(builder, ctv, #"function", #"global", sig, init-function);
		add!(init-functions, ctv);	// ¥¥ better by class <intention>(<condition>)?
	end;
  optimize-component(*current-optimizer*, component);
	emit-tlf-gunk(tlf, file);
	emit-component(component, file);
end function compile-1-tlf;

// Establish various condition handlers while iterating over all of the source
// files and compiling each of them to an output file.
//
define function compile-all-files
 (plug :: <plugin-callback>,
  files :: <vector>,
  tlf-groups :: <stretchy-vector>,
  tlf-group-modules :: <stretchy-vector>,
  unit-cback-unit :: <unit-state>,
  other-cback-units,
  init-functions :: <vector>)
 => ();

	for (file in files, tlfs in tlf-groups, module in tlf-group-modules)

		let handler <compiler-warning>
			= method(problem :: <compiler-warning>, next :: <function>)
					// next();
					let stream = make(<byte-string-stream>, direction: #"output");
					report-condition(problem, stream);
					
					report-message(	plug,
												cw-maybe-find-file(plug, file, full-search: #t),
												stream.stream-contents.chop-warning,
												problem.condition-at.plausible-line-number,
												$message-type-warning,
												0);	// I not yet have error numbers!
				end;

		let handler <compiler-error>
					= method(problem :: <compiler-error>, next :: <function>)
							next();

							let stream = make(<byte-string-stream>, direction: #"output");
							report-condition(problem, stream);
							
							report-message(	plug,
														cw-maybe-find-file(plug, file, full-search: #t),
														stream.stream-contents.chop-error,
														problem.condition-at.plausible-line-number,
														$message-type-error,
														0);	// I not yet have error numbers!
						end method;

		show-status(plug, concatenate("Processing: ", file));
		let extension = file.filename-extension;
		if (/* extension = state.unit-target.object-filename-suffix*/ #f)	// if file mapping points to dylan compiler!
/*			if (state.unit-shared?)
				let shared-file = concatenate(file.extensionless-filename,
																state.unit-target.shared-object-filename-suffix);
//				format(*debug-output*, "Adding %s\n", shared-file);
				format(state.unit-objects-stream, " %s", shared-file);
			else
//				format(*debug-output*, "Adding %s\n", file);
				format(state.unit-objects-stream, " %s", file);
			end if;*/
		else  // assumed a Dylan file, with or without a ".dylan" extension
			block ()
				let storage = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);
				let body-stream = make(<cw-text-output-stream>, backup: storage, locator: plug.dummy-output-file-spec, name: file);
				block ()
					*current-module* := module;
					begin-sub-compile(plug, 0, 0);

					let file = make(<file-state>, unit: unit-cback-unit, body-stream: body-stream);
					emit-prologue(file, other-cback-units);

					for (tlf :: <top-level-form> in tlfs)
						block ()
							display-lines(plug, tlf.ending-line-number);
							compile-1-tlf(tlf, file, init-functions);
							plug.user-break;	// churn the wheel...
						cleanup
							end-of-context();
//						exception (<fatal-error-recovery-restart>)
//							#f;	// needed?
						end block;
					end for;

					show-status(plug, concatenate("Writing: ", body-stream.determine-file-name));
				cleanup
					body-stream.close;
					plug.end-sub-compile;
					*current-module* := #f;
				end block;
			exception (<fatal-error-recovery-restart>)
				report-message(plug, #f, format-to-string("Skipping rest of '%s'", file), 0, $message-type-info, 0);
					/// ¥¥¥ maybe we should find the next "define" and restart the lexer etc. (make a new one)
				#f
			exception (<simple-restart>, init-arguments: #[format-string:, "Blow off compiling this file."])
				#f;
			end block;
		end if;
	end for;
end function compile-all-files;

define function dummy-output-file-spec(plug :: <plugin-callback>) => file-spec :: <file-spec>;
	let (always-next-method :: <boolean>,
			command-line :: <boolean>,
			use-group :: <boolean>,
			group-path :: <byte-string>,
			output-folder :: <file-spec>.false-or) = plug.get-gwydion-prefs;
	if (output-folder)
		relative-path-from(plug, output-folder, ":dummy")
	else
		plug.get-main-file-spec
	end if;
end function dummy-output-file-spec;


// Build initialization function for this library, generate the corresponding .c file contents.
// 
define function build-library-inits(plug :: <plugin-callback>, unit-header, unit-mprefix, unit-lid-file, unit-cback-unit, other-cback-units, init-functions :: <vector>, lib) => (executable :: <boolean>, entry-function);
	let executable = element(unit-header, #"executable", default: #f);

	let entry-point = element(unit-header, #"entry-point", default: #f);

	(entry-point & ~executable)
	& compiler-fatal-error("Can only specify an entry-point when producing an executable.");
	
	let bare-name = concatenate(unit-mprefix, "-init");
	let storage = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);

	let body-stream = make(<cw-text-output-stream>, backup: storage, locator: plug.dummy-output-file-spec, name: bare-name);
	block ()
		let file = make(<file-state>, unit: unit-cback-unit, body-stream: body-stream);
		show-status(plug, concatenate("Writing: ", body-stream.determine-file-name));
		emit-prologue(file, other-cback-units);
	
		let entry-function = entry-point
													& build-command-line-entry(lib, entry-point, file);
	
		build-unit-init-function(unit-mprefix, init-functions, body-stream);
		values(executable & #t, entry-function)
	cleanup
		body-stream.close;
	end block;

end function build-library-inits;


define function build-unit-init-function(prefix :: <byte-string>, init-functions :: <vector>, stream :: <stream>) => ();
	let init-func-guts = emit-init-functions(string-to-c-name(prefix), init-functions, 0, init-functions.size, stream);
	format(stream, "void %s_Library_init(descriptor_t *sp)\n{\n%s}\n", string-to-c-name(prefix), init-func-guts);
end;

define function split-at-colon(string :: <byte-string>) => (module :: <byte-string>, name :: <byte-string>);
	block (return)
		for (index :: <integer> from 0 below string.size)	// better implemented using find-key ?¥¥¥
			if (string[index] == ':')
				return(copy-sequence(string, end: index), copy-sequence(string, start: index + 1));
			end if;
		end for;
		compiler-fatal-error("Invalid entry point: %s -- must be of the form module:variable.", string);
	end block;
end function split-at-colon;


define function build-command-line-entry(lib :: <library>, entry :: <byte-string>, file :: <file-state>) => entry-function :: <ct-function>;

	let (module-name, variable-name) = split-at-colon(entry);
	let module = find-module(lib, as(<symbol>, module-name));
	unless (module)
		compiler-fatal-error("Invalid entry point: %s -- no module %s.",
											entry, module-name);
	end unless;

	let variable = find-variable(make(<basic-name>,
												symbol: as(<symbol>, variable-name),
												module: module));
	unless (variable)
		compiler-fatal-error("Invalid entry point: %s -- no variable %s in module %s.",
											entry, variable-name, module-name);
	end unless;

	let defn = variable.variable-definition;

	unless (defn)
		compiler-fatal-error("Invalid entry point: %s -- it isn't defined.", entry);
	end unless;

	let component = make(<fer-component>);
	let builder = make-builder(component);

	let source = make(<source-location>);
	let policy = $Default-Policy;
	let name = "Command Line Entry";
	let name-obj
			= make(<basic-name>, module: $dylan-module, symbol: #"command-line-entry");

	let int-type = specifier-type(#"<integer>");
	let rawptr-type = specifier-type(#"<raw-pointer>");
	let result-type = make-values-ctype(#(), #f);
	let argc = make-local-var(builder, #"argc", int-type);
	let argv = make-local-var(builder, #"argv", rawptr-type);
	let func
		= build-function-body(builder, policy, source, #f,
												name-obj, list(argc, argv), result-type, #t); 

	let user-func = build-defn-ref(builder, policy, source, defn);
	// ### Should really spread the arguments out, but I'm lazy.
	build-assignment(builder, policy, source, #(),
	make-unknown-call(builder, user-func, #f, list(argc, argv)));
	build-return(builder, policy, source, func, #());
	end-body(builder);

	let sig = make(<signature>, specializers: list(int-type, rawptr-type),
	returns: result-type);
	let ctv = make(<ct-function>, name: name-obj, signature: sig);
	make-function-literal(builder, ctv, #"function", #"global", sig, func);
	optimize-component(*current-optimizer*, component);
	emit-component(component, file);
	ctv;
end function build-command-line-entry;

define constant $max-inits-per-function = 25;

define function emit-init-functions
    (prefix :: <byte-string>, init-functions :: <vector>,
     start :: <integer>, finish :: <integer>, stream :: <stream>)
    => body :: <byte-string>;
	let string-stream = make(<buffered-byte-string-output-stream>);
	if (finish - start <= $max-inits-per-function)
		for (index from start below finish)
			let init-function = init-functions[index];
			let ep = make(<ct-entry-point>, for: init-function, kind: #"main");
			let name = ep.entry-point-c-name;
			format(stream, "extern void %s(descriptor_t *sp);\n\n", name);
			format(string-stream, "    %s(sp);\n", name);
		end for;
	else
		for (divisions = finish - start then ceiling/(divisions, $max-inits-per-function),
				while: divisions > $max-inits-per-function)
		finally
			for (divisions from divisions above 0 by -1)
				let count = ceiling/(finish - start, divisions);
				let name = format-to-string("%s_init_%d_%d", prefix, start, start + count - 1);
				let guts = emit-init-functions(prefix, init-functions, start, start + count, stream);
				format(stream, "static void %s(descriptor_t *sp)\n{\n%s}\n\n", name, guts);
				format(string-stream, "    %s(sp);\n", name);
				start := start + count;
			end for;
		end for;
	end if;
	string-stream.stream-contents;
end function emit-init-functions;

define function build-local-heap-file(plug :: <plugin-callback>, lid-header, unit-mprefix, unit-cback-unit) => info :: <unit-info>;
	show-status(plug, "Emitting library heap");

	let bare-name = concatenate(unit-mprefix, "-heap");

	let storage = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);
	let heap-stream = make(<cw-text-output-stream>, backup: storage, locator: plug.dummy-output-file-spec, name: bare-name);

	let prefix = unit-cback-unit.unit-prefix;
	let heap-state = make(<local-heap-file-state>, unit: unit-cback-unit,
										body-stream: heap-stream,
										// target: state.unit-target,
										id-prefix: stringify(prefix, "_L"));

	let (undumped, extra-labels) = build-local-heap(unit-cback-unit, 
	heap-state);
	// in cleanup??
	heap-stream.close;

	let linker-options = element(lid-header, #"linker-options", default: #f); // needed??? => #f

	make(<unit-info>,	unit-name: unit-mprefix,
									undumped-objects: undumped,
									extra-labels: extra-labels,
									linker-options: linker-options);
end function build-local-heap-file;


define function build-da-global-heap(plug :: <plugin-callback>, unit-cback-unit) => (name-table, heap-hunks);
	show-status(plug, "Emitting global heap");

	let storage = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);
	let heap-stream = make(<cw-text-output-stream>, backup: storage, locator: plug.dummy-output-file-spec, name: "heap");	// later perhaps lidname + heap.c

// better: <cw-global-heap-file-state>¥¥¥¥
// strange "empty" error message in CW if <cw-global-heap-state> not imported ¥¥¥ to be examined...
#if (want-native-hunks)
	let heap-state = make(plug.precompiling? & <global-heap-file-state> | <cw-global-heap-state>,
#else
	let heap-state = make(<global-heap-file-state>,
#endif
			      unit: unit-cback-unit,
			      body-stream: heap-stream); //, target: state.unit-target);

	build-global-heap(apply(concatenate, map(undumped-objects, *units*)), heap-state);
	close(heap-stream);
	
//	values(heap-stream.name-table, heap-stream.heap-hunks)	; /// ICE¥¥¥ "stream" vs. "state"
#if (want-native-hunks)
	if (instance?(heap-state, <cw-global-heap-state>))
	  values(heap-state.name-table, heap-state.heap-hunks);
	end if;
#endif
end function build-da-global-heap;

// Look up a header element with a boolean default.  If specified, the option
// must be "yes" or "no".
//
define function boolean-header-element 
    (name :: <symbol>, default :: <boolean>, lid-header :: <header>) 
 => res :: <boolean>;
  let found = element(lid-header, name, default: #f);
  if (found)
    select (as-uppercase(found) by \=)
      "YES" => #t;
      "NO" => #f;
      otherwise => 
		compiler-error("%s: header option is %s, not \"yes\" or \"no\".", name, found);
    end select;
  else
    default;
  end if;
end function boolean-header-element;
     

define function build-inits-dot-c(plug :: <plugin-callback>,
																	entry-function :: <ct-function>.false-or,
																	entry-point :: <byte-string>.false-or)
								=> ();
	let storage = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);
	let stream = make(<cw-text-output-stream>, backup: storage, locator: plug.dummy-output-file-spec, name: "inits");	// later perhaps lidname + inits.c
	show-status(plug, concatenate("Building ", stream.determine-file-name));

	write(stream, "#include \"runtime.h\"\n\n");
	write(stream, "/* This file is machine generated.  Do not edit. */\n\n");
	let entry-function-name
		= entry-function
			& (make(<ct-entry-point>, for: entry-function, kind: #"main").entry-point-c-name);

	if (entry-function-name)
		format(stream,
					"extern void %s(descriptor_t *sp, int argc, void *argv);\n\n",
					entry-function-name);
	end if;

	write(stream, "void inits(descriptor_t *sp, int argc, char *argv[])\n{\n");

	for (unit in *units*)
		format(stream, "    %s_Library_init(sp);\n", string-to-c-name(unit.unit-name));
	end;
	
	let (	always-next-method :: <boolean>,
			need-command-line-dialog :: <boolean>
			) = plug.get-gwydion-prefs;

	entry-function-name & format(stream, "    %s(sp, argc, argv);\n", entry-function-name);

	write(stream, "}\n");
	
	
	local method fun-o-entry-point() => fun-o :: <boolean>;
					if (entry-point)
						let entry-point = entry-point.as-lowercase;
						subsequence-position(entry-point, ":%main") == entry-point.size - 6
					end if
				end;

	write(stream, "\nextern void real_main(int argc, char *argv[]);\n\n");
	need-command-line-dialog & write(stream, "#include <console.h>\n\n");
	write(stream, "int main(int argc, char *argv[]) {\n");
	if (need-command-line-dialog)
		write(stream, "\targc = ccommand( &argv );\n");
	elseif (fun-o-entry-point())
		write(stream, "\t{\tstatic char *dummy_argv[] = { \"dummy\" };\n");
		write(stream, "\t\targc = 1;\n");
		write(stream, "\t\targv = dummy_argv; }\n");
	end if;
	write(stream, "\treal_main(argc, argv);\n");
	write(stream, "\treturn 0;\n");
	write(stream, "}\n");
	close(stream);
end function build-inits-dot-c;


define function dump-library-summary(plug :: <plugin-callback>, lib-name, unit-info, unit-tlf-vectors) => ();
	show-status(plug, "Dumping library summary");
	let dump-buf = begin-dumping(as(<symbol>, lib-name), $library-summary-unit-type);

	for (tlfs in unit-tlf-vectors)
		for (tlf :: <top-level-form> in tlfs)
			let handler  <simple-warning> = method(problem :: <simple-warning>, next :: <function>)
						next();
						report-ICE(plug, problem);
				end;
			dump-od(tlf, dump-buf);
		end;
	end;

	dump-od(unit-info, dump-buf);
	dump-buf.dump-queued-methods;

	let storage = make(<cw-vector>, opaque: alloc-mem-handle(plug, 0, #t), plugin: plug);
	let unit-stream = make(<cw-binary-output-stream>, backup: storage, locator: plug.dummy-output-file-spec, name: lib-name);
	end-dumping(dump-buf, stream: unit-stream);
end function dump-library-summary;


define function plugin-entry(arg :: <machine-pointer>) => result :: <integer>;

	block(return)
		let cb = make(<plugin-callback>, opaque: arg);
		select (cb.get-plugin-request)
		$cw-req-initialize =>
				*random-state* := make(<random-state>, seed: 0);	// needed??
				define-bootstrap-module();
				$cw-no-error.return;

		$cw-req-compile =>
				for (i from 0 to 5)
					cb.user-break;
				end for;
				
				if (*standard-output*.stream-open?)
					let (t, l) = get-main-file-text(cb /* as: <byte-string> */);
					
					let cs = as(<c-string>, t);	// <cw-vector> ?
					let bs = as(<byte-string>, cs);
					release-file-text(cb, t);

					let handler <compiler-warning>
						= method(problem :: <compiler-warning>, next :: <function>)
								// next();
								let stream = make(<byte-string-stream>, direction: #"output");
								report-condition(problem, stream);
								
								report-message(	cb,
															#f,
															stream.stream-contents.chop-warning,
															problem.condition-at.plausible-line-number,
															$message-type-warning,
															0);	// I not yet have error numbers!
							end;

					*current-optimizer*
						:= make(<cmu-optimizer>, options: <table>.make);

					compile-lid-file(cb, bs);
				end if;
				$cw-no-error;
		otherwise => $cw-no-error.return;
		end select;

	exception(problem :: <plugin-error>)

		problem.cw-error

	exception(problem :: <error>)
		let cb = make(<plugin-callback>, opaque: arg);
		report-ICE(cb, problem);
		$cw-err-silent
	end block
end function;


define function report-ICE(plug :: <plugin-callback>, problem :: <condition>) => ();
	let stream = make(<byte-string-stream>, direction: #"output");
	format(stream, "Internal compiler error: ");
	report-condition(problem, stream);

	select (problem by instance?)
	<type-error> =>
		let stream2 = make(<byte-string-stream>, direction: #"output");
		let in-file :: <byte-string> = as(<byte-string>, make(<c-string>, pointer: c-expr(ptr: "(void*)type__error__FILE__")));
		let in-line :: <integer> = c-expr(int: "type__error__LINE__");
		format(stream2, "File: %=, line: %d", in-file, in-line);
		report-message(plug, cw-maybe-find-file(plug, in-file, full-search: #t), stream.stream-contents, in-line, $message-type-error, 0, line-2: stream2.stream-contents);
	otherwise =>
		report-message(plug, #f, stream.stream-contents, 0, $message-type-error, 0);
	end select;

end function report-ICE;

#if (want-special-od-loader)
/// the below stuff will go into od-format.dylan

define sealed class <boolean-vector-table>(<value-table>)
end class <boolean-vector-table>;

standard-seals-for(<boolean-vector-table>);

define inline method table-protocol(table :: <boolean-vector-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(\=, bool-vector-hash);
end method table-protocol;

define sealed domain table-protocol (<boolean-vector-table>);
define constant $permanent-hash-state = #f;

define inline function bool-vector-hash
    (v :: <simple-object-vector>, initial-state :: <hash-state>)
 => (id :: <integer>, hash-state :: <hash-state>);

//  values(reduce(combine-shift, 0, v), /*¥¥¥$permanent-hash-state << missing from dylan-exports!!!*/#f);
  values(reduce(combine-shift, 0, v), $permanent-hash-state);
end bool-vector-hash;

define inline function combine-shift(sofar :: <integer>, bool :: <boolean>) => combine-shifted :: <integer>;
	let shifted :: <integer> = ash(sofar, 1);
	bool & shifted + 1 | shifted;
end function combine-shift;

define sealed class <obj-resolved?-configuration-table>(<value-table>)
//	constant slot table-predicate :: <function>, init-keyword: predicate:, init-value: identity;
end class <obj-resolved?-configuration-table>;

standard-seals-for(<obj-resolved?-configuration-table>);


define inline method table-protocol(table :: <obj-resolved?-configuration-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(compare-loaded, obj-resolved?-hash);
end method table-protocol;

define sealed domain table-protocol (<obj-resolved?-configuration-table>);

define inline function obj-resolved?-hash
    (s :: <simple-object-vector>, initial-state :: <hash-state>)
 => (id :: <integer>, hash-state :: <hash-state>);

  values(reduce(obj-resolved?-combine-shift, 0, s), $permanent-hash-state);
end obj-resolved?-hash;

define inline function obj-resolved?-combine-shift(sofar :: <integer>, just-loaded :: <object>) => combine-shifted :: <integer>;
	let shifted :: <integer> = ash(sofar, 1);
	just-loaded.obj-resolved? & shifted + 1 | shifted;
end function obj-resolved?-combine-shift;


define inline function compare-loaded(etalon :: <simple-object-vector>, subobjects :: <simple-object-vector>) => same :: <boolean>;
	block (return)
		do(	method(b :: <boolean>, sub-obj) => ();
					b ~== sub-obj.obj-resolved? & return(#f)
				end,
				etalon, subobjects);
		#t
	end block
end function compare-loaded;

define constant class-table :: <object-table> = <object-table>.make;
define constant vector-table :: <boolean-vector-table> = <boolean-vector-table>.make;

define function lookup-loader(obj-class :: <class>, vec :: <simple-object-vector>, new-loader :: <function>.false-or)
	let loaders-for-class = element(class-table, obj-class, default: #f);

	if (new-loader)
		let loaders-for-class :: <table>
			= loaders-for-class | (class-table[obj-class] := make(<obj-resolved?-configuration-table>, size: 4));

		let pattern :: <simple-object-vector> = map(obj-resolved?, vec);
		let known-vector = element(vector-table, pattern, default: #f);

		let known-vector :: <simple-object-vector>
			=	if (known-vector)
					known-vector
				else
					vector-table[pattern] := pattern
				end if;

		loaders-for-class[known-vector] := new-loader;
	else
		if (loaders-for-class)
			element(loaders-for-class, vec, default: #f);
		end if;
	end if;
end function lookup-loader;

*lookup-loader* := lookup-loader;

define inline function scavenge-od-loader-caches() => ();
	// remove-all-keys! ¥¥¥
	local method clear!(tab :: <table>) => ();
					do(curry(remove-key!, tab), tab.key-sequence);
				end method clear!;

	class-table.clear!;
	vector-table.clear!;
end;
#endif