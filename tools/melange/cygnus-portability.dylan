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
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
// compilation environment for an Intel x86 running Linux 2.x.x.
//======================================================================

define constant $default-defines
  = #[
      // When used in the cygwin environment, gcc defines __CYGWIN__
      // automatically.  We must define it for melange too so that
      // melange will parse the headers the same way as gcc does.
      "__CYGWIN__", "1",

      // gcc defines __GNUC__ automatically.  We must define it for melange
      // too so melange will parse the headers the same way as gcc does.
      "__GNUC__", "3",

      // cygwin made a goof and uses "__const" in one place instead of "const"
      // The problem has been reported to them (or more specifically to newlib).
      "__const", "",

      // It would be nice if cygwin was consistent about usage of declarations,
      // but of course it's not.
      "__IMPORT", "",

      // Microsoft-specific function calling conventions. For more info, see
      // http://msdn.microsoft.com/library/en-us/vclang/html/_core_Calling_Conventions_Topics.asp
      "__cdecl", "",
      "__stdcall", "",
      "__fastcall", "",

      // Microsoft-specific storage-class attributes. For more info, see
      // http://msdn.microsoft.com/library/en-us/vclang/html/_pluslang_extended_attribute_syntax.asp
      "__declspec", #(#("x"), ""),

      "const", "",
      "volatile", "",
      "__STDC__", "",

      // Parameterized macros which remove various GCC extensions from our
      // source code. The last item in the list is the right-hand side of
      // the define; all the items preceding it are named parameters.
      "__attribute__", #(#("x"), ""), 
      "__signed__", "", 
      "__inline__", "",
      "inline", "",
      "__inline", "",
      "__extension__", "",

      "__ELF__", "",
      "unix", "",
      "i386", "",
      "linux", "",
      "__unix__", "",
      "__i386__", "",
      "__linux__", "",
      "__unix", "",
      "__i386", "",
      "__linux", "",
      "__builtin_va_list", "void*"
     ];
  
define constant linux-include-directories
  = #["/usr/include"];

for (dir in linux-include-directories)
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
