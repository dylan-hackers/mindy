author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/system.dylan,v 1.1 1996/06/26 14:39:16 nkramer Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

// Some of the functions that go in the System module.  Much of the
// code was moved from the d2c Main module.

// $Newlines-are-CRLF -- exported from System.
//
// Change this if we ever port the compiler to a Microsoft OS like NT
//
define constant $Newlines-are-CRLF = #f;

define method import-string (ptr :: <raw-pointer>)
 => string :: <byte-string>;
  for (len :: <integer> from 0,
       until: zero?(pointer-deref(#"char", ptr, len)))
  finally
    let res = make(<byte-string>, size: len);
    for (index :: <integer> from 0 below len)
      res[index] := as(<character>, pointer-deref(#"char", ptr, index));
    end for;
    res;
  end for;
end method import-string;

define method export-string (string :: <byte-string>)
 => ptr :: <raw-pointer>;
  let len = string.size;
  let buffer = make(<buffer>, size: len + 1);
  copy-bytes(buffer, 0, string, 0, len);
  buffer[len] := 0;
  buffer-address(buffer);
end method export-string;

define method getenv (name :: <byte-string>)
 => res :: false-or(<byte-string>);
  let ptr = call-out("getenv", #"ptr", #"ptr", export-string(name));
  if (zero?(as(<integer>, ptr)))
    #f;
  else
    import-string(ptr);
  end if;
end method getenv;

define method system (command :: <byte-string>)
 => res :: <integer>;
  call-out("system", #"int", #"ptr", export-string(command));
end method system;

define method exit (#key exit-code :: <integer> = 0)
 => res :: <never-returns>;
  call-out("exit", void:, int:, exit-code);
end method exit;

// no-core-dumps
//
// Sets the current limit for core dumps to 0.  Keeps us from dumping 32+ meg
// core files when we hit an error.
//
define method no-core-dumps () => ();
  let buf = make(<buffer>, size: 8);
  call-out("getrlimit", #"void", #"int", 4, #"ptr", buf.buffer-address);
  pointer-deref(#"int", buf.buffer-address, 0) := 0;
  call-out("setrlimit", #"void", #"int", 4, #"ptr", buf.buffer-address);
end method no-core-dumps;
