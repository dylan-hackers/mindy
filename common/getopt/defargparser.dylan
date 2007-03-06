module: parse-arguments
synopsis: Interface macros for parser definition and option access.
authors: David Lichteblau <lichteblau@fhtw-berlin.de>
copyright: see below

//======================================================================
//
//  Copyright (c) 1999-2001 David Lichteblau
//  All rights reserved.
// 
//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".
//
//======================================================================


// Introduction
// ============
//
// defargparser is a set of macros designed to work on top of Eric Kidd's
// getopt library.  The idea is to provide a more readable interface for
// parser definition and option access.
//
//
// Examples
// ========
//
// Below you can find a short overview of defargparser's features.
// If you are looking for working examples, have a look at
//   <URL:http://www.inf.fu-berlin.de/~lichtebl/dylan/>
//
//
// Emacs
// =====
//
// You will want to edit your .emacs to recognize defargparser macros and
// keywords:
//
// (add-hook 'dylan-mode-hook
//           (lambda ()
//             (add-dylan-keyword 'dyl-parameterized-definition-words
//                                "argument-parser")
//             (add-dylan-keyword 'dyl-other-keywords "option")
//             (add-dylan-keyword 'dyl-other-keywords "regular-arguments")
//             (add-dylan-keyword 'dyl-other-keywords "synopsis")))
//
//
// Argument parser definition
// ==========================
// 
//     Use ``define argument-parser'' to define a new parser class.  This
//     macro is intended to look similar to ``define class'', but doesn't
//     define slots as such.  Instead it takes ``option'' clauses.  At
//     initialisation time of an instance, corresponding option parsers will 
//     be made automatically.
// 
//         define argument-parser <my-parser> ()
//           option verbose?, long: "verbose", short: "v";
//         end;
// 
//     Notes:
//       - Default superclass is <argument-list-parser>.
// 
//       - Default option class is <simple-option-parser>.
// 
//         You can specify an alternative class with the kind: keyword:
//           option logfile, kind: <parameter-option-parser>;
// 
//       - For the options, default values are possible:
//           option logfile = "default.log",
//             kind: <parameter-option-parser>,
//             long: "logfile", short: "L";
// 
//       - You may want to specify types for an option:
//           option logfile :: false-or(<string>), ...
//         or
//           option logfile :: <string> = "default.log", ...
//         
//         Currently type checking is done, but errors are not handled.
//         Future version will probably provide a facility for automatic
//         error handling and error message generation.
//     
//       - Remaining keywords are handed as initargs to make.
// 
//       - Besides ``option'' there is also ``regular-arguments'':
//           regular-arguments file-names;
// 
// 
// Parsing an argument list
// ========================
// 
//     Originally I had macros to make an argument-list parser and do the
//     parsing transparently.  It wasn't consistent enough, though, and
//     therefore I decided to throw out that code for now.
// 
//     Just do it manually:
// 
//         define method main (appname, #rest args);
//           let parser = make(<my-parser>);
//           parse-arguments(parser, args);
// 
//           // Here we go.
//         end method main;
// 
// 
// Accessing the options
// =====================
// 
//     ``define argument-parser'' defines function to access the options as
//     if they were real slots:
// 
//         define argument-parser <my-parser> ()
//           option verbose?, short: "v";
//         end argument-parser;
//
//         define method main (appname, #rest args);
//           let parser = make(<my-parser>);
//           parse-arguments(parser, args);
//
//           if (parser.verbose?)
//             ...
//           end if;
//         end method main;
//
//
//     If you happen to need the option parsers, they are accessible as
//     slots with "-parser" appended to the name:
//         let option-parser = parser.verbose?-parser;
//
//
// Synopsis generation
// ===================
//
//    Suppose you say
//
//         define argument-parser <main-parser> ()
//           synopsis print-synopsis,
//             usage: "test [options] file...",
//             description: "Stupid test program doing nothing with the args.";
//        
//           ...
//         end argument-parser;
//
//    Then print-synopsis(parser, stream) will print something like:
//
//         Usage: test [options] file...
//         Stupid test program doing nothing with the args.
//
//           -v, --verbose                Explanation
//               --other-option           foo
//
 
 
#if (~mindy) // whole file

// Macro ARGUMENT-PARSER-DEFINER--exported
// =======================================
// Syntax: define argument-parser ?:name (?supers:*) ?options end
//
//  - Let `?supers' default to <argument-list-parser>.
//
//  - Transform human-readable `?options' into patterns of the form
//      [option-name, type, [default-if-any], #rest initargs]
//      [regular-arguments-name]
//      (synopsis-fn-name, usage, description)
// 
//  - Hand it over to `defargparser-rec'.
//
// Explanation: I have no idea what that is for.
//
define macro argument-parser-definer
    { define argument-parser ?:name () ?options end }
      => { defargparser-rec ?name (<argument-list-parser>) () ?options end }
    { define argument-parser ?:name (?supers) ?options end }
      => { defargparser-rec ?name (?supers) () ?options end }

  supers:
    { ?super:expression, ... } => { ?super, ... }
    { } => { }
    
  options:
    { option ?:name :: ?value-type:expression, ?initargs:*; ... }
      => { [?name, ?value-type, [], ?initargs] ... }
    { option ?:name :: ?value-type:expression = ?default:expression,
        ?initargs:*; ... }
      => { [?name, ?value-type, [?default], ?initargs] ... }
    { regular-arguments ?:name; ... }
      => { [?name] ... }
    { synopsis ?fn:name, #rest ?keys:*,
      #key ?usage:expression = #f, ?description:expression = #f,
      #all-keys; ... }
      => { (?fn, ?usage, ?description) ... }
    { } => { }

  initargs:
    { ?syntax:expression, ?docstring:expression, #rest ?realargs:* }
      => { [?syntax, ?docstring], ?realargs }
    { ?docstring:expression, #rest ?realargs:* }
      => { ["", ?docstring], ?realargs }
    { #rest ?realargs:* }
      => { ["", ""], ?realargs }
end macro;

// Macro DEFARGPARSER-REC--internal
// ================================
// Syntax: defargparser-rec ?:name (?supers:*) (?processed:*) ?options end
//
//   - Start out without `?processed' forms.
//   - (Recursively) take each `?options' form and add a pair
//       [?name, ?option]
//     to `?processed'.
//   - Finally, pass the `?processed' forms to `defargparser-aux'.
//
// Explanation: The options will be processed by auxiliary rules.
// However, these need the `?name', which would be available to main
// rules only.  That's why we need the name/option pairs.
//
define macro defargparser-rec
    { defargparser-rec ?:name (?supers:*) (?processed:*) end }
      => { defargparser-aux ?name (?supers) ?processed end }

    { defargparser-rec ?:name (?supers:*) (?processed:*) [?option:*] ?rem:* end }
      => { defargparser-rec ?name (?supers)
	     (?processed [?name, ?option]) ?rem
	   end }
    { defargparser-rec ?:name (?supers:*) (?processed:*) (?usage:*) ?rem:* end }
      => { defargparser-rec ?name (?supers)
	     ((?usage) ?processed) ?rem
	   end }
end macro;

// Macro DEFARGPARSER-AUX--internal
// ================================
// Syntax: defargparser-aux ?:name (?supers:*) ?options end
//
// Explanation: This is rather staightforward; code generation is
// performed by auxillary macros that output
//
//   - (defargparser-class) a class definition for `?name'
//
//   - (defargparser-init) initialize methods that add our option
//     parsers (held in slots named `.*-parser') using add-option-parser
//
//   - (defargparser-accessors) accessors that ask the parsers for the
//     values that were found
//
//  - (defargparser-synopsis) a method printing usage information
//
define macro defargparser-aux
    { defargparser-aux ?:name (?supers:*) ?options:* end }
      => { defargparser-class ?name (?supers) ?options end;
           defargparser-init ?name ?options end;
           defargparser-accessors ?name ?options end;
           defargparser-synopsis ?name ?options end }
end macro;

define macro defargparser-class
    { defargparser-class ?:name (?supers:*) ?slots end }
      => { define class ?name (?supers)
	     ?slots
	   end class }

  slots:
    { [?class:name, ?option:name, ?value-type:expression, [?default:*],
       [?docstrings:*], #rest ?initargs:*,
       #key ?kind:expression = <simple-option-parser>,
            ?short:expression = #(),
            ?long:expression = #(),
       #all-keys] ... }
      => { constant slot ?option ## "-parser"
	     = begin
	         let long = ?long;
	         let short = ?short;
		 make(?kind,
		      long-options: select (long by instance?)
				      <list> => long;
				      otherwise => list(long);
				    end select,
		      short-options: select (short by instance?)
				       <list> => short;
				       otherwise => list(short);
				     end select,
		      ?initargs);
	       end; ... }
    { [?class:name, ?regular-arguments:name] ... }
      => {  ... }
    { (?usage:*) ... }
      => { ... }
    { } => { }
end macro;

define macro defargparser-init
    { defargparser-init ?:name ?adders end }
      => { define method initialize (instance :: ?name,
				     #next next-method, #key, #all-keys)
	    => ();
	     next-method();
	     ?adders
	   end method initialize }

  adders:
    { [?class:name, ?option:name, ?value-type:expression, [?default:*],
       [?docstrings:*], ?initargs:*] ... }
      => { add-option-parser(instance, ?option ## "-parser" (instance)); ... }
    { [?class:name, ?regular-arguments:name] ... }
      => {  ... }
    { (?usage:*) ... }
      => { ... }
    { } => { }
end macro;

define macro defargparser-accessors
    { defargparser-accessors ?:name ?accessors end }
      => { ?accessors }

  accessors:
    { [?class:name, ?option:name, ?value-type:expression,
       [], [?docstrings:*], ?initargs:*] ... }
      => { define method ?option (arglistparser :: ?class)
	    => (value :: ?value-type);
	     let optionparser = ?option ## "-parser" (arglistparser);
	     option-value(optionparser);
	   end method ?option; ... }
    { [?class:name, ?option:name, ?value-type:expression,
       [?default:expression], [?docstrings:*], ?initargs:*] ... }
      => { define method ?option (arglistparser :: ?class)
	    => (value :: ?value-type);
	     let optionparser = ?option ## "-parser" (arglistparser);
	     if (option-present?(optionparser))
	       option-value(optionparser);
	     else
	       ?default;
	     end if;
	   end method ?option; ... }
    { [?class:name, ?regular-arguments:name] ... }
      => { define method ?regular-arguments (arglistparser :: ?class)
	    => (value :: <sequence>);
	     regular-arguments(arglistparser);
	   end method; ... }
    { (?usage:*) ... }
      => { ... }
    { } => { }
end macro;

define macro defargparser-synopsis
    { defargparser-synopsis ?:name
       (?fn:name, ?usage:expression, ?description:expression)
       ?options
      end }
      => { define method ?fn (parser :: ?name, stream :: <stream>) => ();
	     let usage = ?usage;
	     let desc = ?description;
	     if (usage) format(stream, "Usage: %s\n", usage); end if;
	     if (desc) format(stream, "%s\n", desc); end if;
	     if (usage | desc) new-line(stream); end if;
	     local method print-option(short, long, syntax, description);
		     let short = select (short by instance?)
				   <list> => first(short);
				   <string> => short;
				   otherwise => #f;
				 end select;
		     let long = select (long by instance?)
				  <pair> => first(long);
				  <string> => long;
				  otherwise => #f;
				end select;
		     write(stream, "  ");
		     if (short)
		       format(stream, "-%s", short);
		       if (long)
			 write(stream, ", ");
		       else
			 write(stream, "  ");
		       end if;
		     else
		       write(stream, "    ");
		     end if;
		     if (long)
		       format(stream, "--%s%s", long, syntax);
		       for (i from 1 to (28 - 2 - size(long) - size(syntax)))
			 write-element(stream, ' ');
		       end for;
		     else
		       format(stream, "%28s", "");
		     end if;
		     write(stream, description);
		     new-line(stream);
		   end method print-option;
	     ?options
	   end method ?fn; }

    { defargparser-synopsis ?:name ?ignore:* end }
      => { }

  options:
    { [?class:name, ?option:name, ?value-type:expression,
       [?default:*], [?syntax:expression, ?description:expression],
       #rest ?initargs:*,
       #key ?short:expression = #f,
            ?long:expression = #f,
       #all-keys] ... }
      => { print-option(?short, ?long, ?syntax, ?description); ... }
    { [?class:name, ?regular-arguments:name] ... }
      => { ... }
    { } => { }
end macro;

#endif

// EOF
