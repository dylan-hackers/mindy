documented: #t
module: portability
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.
rcs-header: $Header: /scm/cvs/src/tools/melange/macos-portability.dylan,v 1.8 2003/04/09 11:25:02 gabor Exp $

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


//======================================================================
// Module portability is a tiny OS dependent module which defines the
// preprocessor definions and "standard" include directories which would be
// used by a typical C compiler for that OS.  It may, at some future date,
// also include behavioral switches for things like slot allocation or sizes
// of different sorts of numbers.
//
// This particular implementation of module portability corresponds to
// the compilation environment for a MacOS X Macintosh using the Darwin BSD layer.
//======================================================================

define constant $default-defines
  = #[
      // Basics
      "const", "",
      "volatile", "",
      //"__cdecl", "",
      
      // cpp -dM /dev/null
      "GNUC", "(1)",
      
      // cc -E -dM -o - /dev/null
      "__GNUC__", "2",
      "__GNUC_MINOR__", "95",
      "__ppc__", "",
      "__NATURAL_ALIGNMENT__", "",
      "__MACH__", "",
      "__BIG_ENDIAN__", "",
      "__APPLE__", "",
      "__STDC__", "",
      "__APPLE_CC__", "934",
      "__DYNAMIC__", "",
      "__signed__", "",
      "__signed", "",
      "__inline__", "",
      "__inline", "",
      
      // Parameterized macros which remove various GCC extensions from our
      // source code. The last item in the list is the right-hand side of
      // the define; all the items preceding it are named parameters.
      "__attribute__", #(#("x"), "")
      ];

// Set up the search path for .h files
// cc -E -v /dev/null
define constant macos-include-directories
  = #["/usr/local/include",
 			"/usr/include/gcc/darwin/2.95.2/g++/..",
 			"/usr/include"];

for (dir in macos-include-directories)
  push-last(include-path, dir);
end for;

*handle-c++-comments* := #t;

*framework-paths* := #[ "/System/Library/Frameworks/", "/Library/Frameworks/", "~/Library/Frameworks/" ];


// These constants should be moved here in the future.  Until the module
// declarations can be sufficiently rearranged to allow their definition
// here, they will remain commented out.  -- panda
//
// define constant c-type-size = unix-type-size;
// define constant c-type-alignment = unix-type-alignment;
// define constant $default-alignment :: <integer> = 4;


define constant $integer-size :: <integer> = 4;
define constant $short-int-size :: <integer> = 2;
define constant $long-int-size :: <integer> = 4;
define constant $longlong-int-size :: <integer> = 8;
define constant $char-size :: <integer> = 1;
define constant $float-size :: <integer> = 4;
define constant $double-float-size :: <integer> = 8;
define constant $long-double-size :: <integer> = 16;
define constant $enum-size :: <integer> = $long-int-size;	// Some Apple header constants are longs!
define constant $pointer-size :: <integer> = 4;
define constant $function-pointer-size :: <integer> = $pointer-size;
