documented: #t
module: portability
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: 

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
// compilation environment for an HP 735 running HPUX.
//======================================================================

define module portability
  use dylan;
  use c-lexer, import: {default-cpp-table, include-path};
end module portability;

define constant reserved-words
  = #["hppa", "hp9000s800", "__hp9000s800", "hp9k8", "PWB", "hpux", "unix",
      "_HPUX_SOURCE", "__hppa__", "__hp9000s800__", "__hp9k8__", "__PWB",
      "__hpux__", "__unix__", "___HPUX_SOURCE__", "__hppa", "__hp9000s800",
      "__hp9000s800", "__hp9k8 _" "__PWB", "__hpux", "__unix",
      "___HPUX_SOURCE", "__hp9aaas700", "_PA_RISC1_1", "__STDC__"];

for (word in reserved-words)
  default-cpp-table[word] := #();
end for;

define constant hp-include-directories
  = #["/usr/local/include", "/usr/include"];

for (dir in hp-include-directories)
  push-last(include-path, dir);
end for;
