documented: #t
module: portability
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
// All rights reserved.
//
//======================================================================

//======================================================================
// Module portability is a tiny OS dependent module which defines the
// preprocessor definions and "standard" include directories which would be
// used by a typical C compiler for that OS.  It may, at some future date,
// also include behavioral switches for things like slot allocation or sizes
// of different sorts of numbers.
//
// This particular implementation of module portability corresponds to the
// compilation environment for a MIPS box running IRIX 6.5.
//======================================================================

define constant $default-defines
  = #["const", "",
      "volatile", "",
      "__STDC__", "",

      // The following six declarations should be removed someday, as soon as 
      // we fix a bug in MINDY.
      //"__GNUC__", "2",
      //"__GNUC_MINPR__", "7",
      //"__signed__", "",
      //"__const", "",
      //"__CONSTVALUE", "",
      //"__CONSTVALUE2", "",

      // Parameterized macros which remove various GCC extensions from our
      // source code. The last item in the list is the right-hand side of
      // the define; all the items preceding it are named parameters.
      "__attribute__", #(#("x"), ""), 
      "__signed__", "", 
      "__inline__", "",
      "inline", "",
      "__inline", "",

      "__ELF__", "",
      "unix", "",
      "mips", "",
      "sgi", "",
      "__host_mips__", "",
      "MIPSEB", "",
      "_MIPSEB", "",
      "_SYSTYPE_SVR4", "",
      "_LONGLONG", "",
      "_SVR4_SOURCE", "",
      "_MODERN_C", "",
      "__DSO__", "",
      "__unix", "",
      "__mips", "",
      "__sgi", "",
      "__host_mips", "",
      "__MIPSEB", "",
      "__SYSTYPE_SVR4", "",
      "__CHAR_UNSIGNED__", "",
      "__LANGUAGE_C__", "",
      "_LANGUAGE_C__", "",
      "__SIZE_TYPE__", "unsigned int",
      "__PTRDIFF_TYPE__", "int",
      "__LONG_MAX__", "9223372036854775807LL",
      "__EXTENSIONS__", "",
      "_SGI_SOURCE", "",
      "_MIPS_FPSET", "32",
      "_MIPS_ISA", "_MIPS_ISA_MIPS3",
      "_ABIN32", "2",
      "_MIPS_SIM", "_ABIN32",
      "_MIPS_SZINT", "32",
      "_MIPS_SZLONG", "32",
      "_MIPS_SZPTR", "32",
      "_COMPILER_VERSION", "601",
      "__mips", "3",
      "__mips64", "3"];

define constant irix-include-directories
  = #["/usr/include"];

for (dir in irix-include-directories)
  push-last(include-path, dir);
end for;


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
define constant $enum-size :: <integer> = $integer-size;
define constant $pointer-size :: <integer> = 4;
define constant $function-pointer-size :: <integer> = $pointer-size;
