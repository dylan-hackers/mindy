module: file-descriptors
author: ram+@cs.cmu.edu
synopsis: This file implements Unix FD I/O 
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/streams/fd-io.dylan,v 1.7 2002/06/08 18:02:27 gabor Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
//

c-include("errno.h");
c-include("fcntl.h");
c-include("string.h");

#if (~compiled-for-win32)
  c-include("unistd.h");
#endif

  
// Top-level init code, done in C
begin
  call-out("streams_fd_init", void:);
end;

define /* exported */ constant fd-seek-set :: <integer>
  = c-expr(int:, "SEEK_SET");

define /* exported */ constant fd-seek-current :: <integer>
  = c-expr(int:, "SEEK_CUR");

define /* exported */ constant fd-seek-end :: <integer>
  = c-expr(int:, "SEEK_END");


define /* exported */ constant fd-o_rdonly :: <integer>
 = c-expr(int:, "O_RDONLY");

define /* exported */ constant fd-o_wronly :: <integer>
 = c-expr(int:, "O_WRONLY");

define /* exported */ constant fd-o_rdwr :: <integer>
 = c-expr(int:, "O_RDWR");

define /* exported */ constant fd-o_creat :: <integer>
 = c-expr(int:, "O_CREAT");

define /* exported */ constant fd-o_append :: <integer>
 = c-expr(int:, "O_APPEND");

define /* exported */ constant fd-o_trunc :: <integer>
 = c-expr(int:, "O_TRUNC");

define /* exported */ constant fd-o_excl :: <integer>
 = c-expr(int:, "O_EXCL");

define /* exported */ constant fd-enoent :: <integer>
 = c-expr(int:, "ENOENT");

define /* exported */ constant fd-eexist :: <integer>
 = c-expr(int:, "EEXIST");

define /* exported */ constant fd-eacces :: <integer>
 = c-expr(int:, "EACCES");


define /* exported */ generic fd-open
    (name :: <byte-string>, mode :: <integer>)
 => (fd :: false-or(<integer>), errno :: false-or(<integer>));

define /* exported */ generic fd-close (fd :: <integer>)
 => (success :: <boolean>, errno :: false-or(<integer>));

define /* exported */ generic fd-read
    (fd :: <integer>, buf :: <buffer>, start :: <integer>,
     max-count :: <integer>)
 => (nbytes :: false-or(<integer>), errno :: false-or(<integer>));

define /* exported */ generic fd-write
    (fd :: <integer>, buf :: <buffer>, start :: <integer>,
     max-count :: <integer>)
 => (nbytes :: false-or(<integer>), errno :: false-or(<integer>));

define /* exported */ generic fd-seek
    (fd :: <integer>, offset :: <integer>,
     whence :: <integer>)
 => (newpos :: false-or(<integer>), errno :: false-or(<integer>));


define /* exported */ generic fd-input-available? (fd :: <integer>)
 => (available :: <boolean>, errno :: false-or(<integer>));

define /* exported */ generic fd-sync-output (fd :: <integer>)
 => (success :: <boolean>, errno :: false-or(<integer>));

define /* exported */ generic fd-error-string (num :: <integer>) 
 => res :: <byte-string>;


// Actual methods:


// Fetch errno if the "result" is negative, otherwise return the result & #f
//
define inline method results (okay :: <integer>, result :: <object>) 
    => (res :: <object>, errno :: false-or(<integer>));
  if (okay < 0)
    values(#f, c-expr(int:, "errno"));
  else
    values(result, #f);
  end if;
end method;


// Allocate a buffer to hold a string, and fill it with the string contents and
// a final 0 (null).  Return the address of the data.
//
define method string->c-string (str :: <byte-string>)
    => res :: <raw-pointer>;
  let ssize = str.size;
  let sbuf = make(<buffer>, size: ssize + 1);
  for (i :: <integer> from 0 below ssize)
    sbuf[i] := as(<integer>, str[i]);
  end for;
  sbuf[ssize] := 0;
  buffer-address(sbuf);
end method;


define inline method fd-open
    (name :: <byte-string>, flags :: <integer>)
 => (fd :: false-or(<integer>), errno :: false-or(<integer>));
  let res = call-out("fd_open", int:,
		     ptr: string->c-string(name),
		     int: flags,
		     int: #o666);
  results(res, res);
end method;


define inline method fd-close (fd :: <integer>)
 => (success :: <boolean>, errno :: false-or(<integer>));
  let res = call-out("fd_close", int:, int: fd);
  results(res, #t);
end method;


define inline method fd-read
    (fd :: <integer>, buf :: <buffer>, start :: <integer>,
     max-count :: <integer>)
 => (nbytes :: false-or(<integer>), errno :: false-or(<integer>));
  let res = call-out("fd_read", int:, int: fd, 
		     ptr: buffer-address(buf) + start,
		     int: max-count);
  results(res, res);
end method;


define inline method fd-write
    (fd :: <integer>, buf :: <buffer>, start :: <integer>,
     max-count :: <integer>)
 => (nbytes :: false-or(<integer>), errno :: false-or(<integer>));
  let res = call-out("write", int:, int: fd, ptr: buffer-address(buf) + start,
		     int: max-count);
  results(res, res);
end method;


define inline method fd-seek
    (fd :: <integer>, offset :: <integer>,
     whence :: <integer>)
 => (newpos :: false-or(<integer>), errno :: false-or(<integer>));
  let res = call-out("lseek", int:, int: fd, long: offset, int: whence);
  results(res, res);
end method;


define method fd-input-available? (fd :: <integer>)
 => (available :: <boolean>, errno :: false-or(<integer>));
  let res = call-out("fd_input_available", int:, int: fd);
  values(res ~== 0, #f); // ### errno ignored
end method;

define method fd-sync-output (fd :: <integer>)
 => (success :: <boolean>, errno :: false-or(<integer>));
  values(#t, #f); // ### completely bogus
end method;

define method fd-error-string (num :: <integer>) 
 => res :: <byte-string>;
  let ptr = call-out("strerror", #"ptr", #"int", num);
  let len = call-out("strlen", #"int", #"ptr", ptr);
  let res = make(<byte-string>, size: len);
  for (i from 0 below res.size)
    res[i] := as(<character>, pointer-deref(#"unsigned-char", ptr, i));
  end for;
  res;
end method;
