library: getopttest
module: getopttest
author: Eric Kidd
copyright: see below

//======================================================================
//
//  Copyright (c) 1998 Eric Kidd
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

define method main(progname :: <string>, #rest argv)
  let *parser* = make(<argument-list-parser>);
  add-option-parser-by-type(*parser*,
			    <simple-option-parser>,
			    long-options: #("verbose"),
			    short-options: #("v"),
			    negative-long-options: #("quiet"),
			    negative-short-options: #("q"),
			    default: #t);
  add-option-parser-by-type(*parser*,
			    <simple-option-parser>,
			    long-options: #("foo"),
			    short-options: #("f"),
			    negative-long-options: #("no-foo"),
			    negative-short-options: #("B"),
			    default: #f);
  add-option-parser-by-type(*parser*,
			    <parameter-option-parser>,
			    long-options: #("quux"),
			    short-options: #("Q"));
  add-option-parser-by-type(*parser*,
			    <optional-parameter-option-parser>,
			    long-options: #("optimize"),
			    short-options: #("O"));
  add-option-parser-by-type(*parser*,
			    <repeated-parameter-option-parser>,
			    long-options: #("warning"),
			    short-options: #("W"));
  add-option-parser-by-type(*parser*,
			    <keyed-option-parser>,
			    long-options: #("define"),
			    short-options: #("D"));
  
  unless(parse-arguments(*parser*, argv))
    format-out("usage: getopt-test [-qvfB] [-Q arg] [-W arg]* "
		 "[-Dkey[=value]]*\n");
    exit(exit-code: 1);
  end unless;
  
  let verbose? = option-value-by-long-name(*parser*, "verbose");
  let foo? = option-value-by-long-name(*parser*, "foo");
  let quux = option-value-by-long-name(*parser*, "quux");
  let optimize = option-value-by-long-name(*parser*, "optimize");
  let warnings = option-value-by-long-name(*parser*, "warning");
  let defines = option-value-by-long-name(*parser*, "define");

  format-out("Program name:    %s\n", progname);
  format-out("Verbose?:        %=\n", verbose?);
  format-out("Foo?:            %=\n", foo?);
  format-out("Quux:            %=\n", quux);
  format-out("Optimize:        %=\n", optimize);

  format-out("Warnings:\n");  
  for (arg in warnings)
    format-out("  <%s>\n", arg);
  end for;

  format-out("Defines:\n");
  for (key in key-sequence(defines))
    format-out("  <%s> = <%=>\n", key, defines[key]);
  end for;

  format-out("Regular arguments:\n");  
  for (arg in *parser*.regular-arguments)
    format-out("  <%s>\n", arg);
  end for;

  exit(exit-code: 0);
end method main;
