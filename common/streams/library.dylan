module: Dylan-User
author: chiles@cs.cmu.edu
synopsis: This file defines the Streams library and its modules.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/streams/library.dylan,v 1.1 1996/03/19 23:58:28 nkramer Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================
//


///
/// These definitions go into the Dylan-User module because this is how we
/// jumpstart a library.
///

define library streams
  use dylan;
  export streams, standard-io;
end library;

#if (~mindy)
  define module File-Descriptors 
    use dylan;
    use extensions;
    use system;
    export
      fd-seek-set,
      fd-seek-current,
      fd-seek-end,
      fd-O_RDONLY,
      fd-O_WRONLY,
      fd-O_RDWR,
      fd-O_CREAT,
      fd-O_TRUNC,
      fd-O_EXCL,
      fd-ENOENT,
      fd-EEXIST,
      fd-open,
      fd-close,
      fd-read,
      fd-write,
      fd-seek,
      fd-input-available?,
      fd-sync-output,
      fd-error-string;
  end module;

  define module Threads
    use dylan;
    use extensions;
    export
      <lock>,
      <multilock>,
      <semaphore>,
      grab-lock,
      release-lock,
      locked?;
  end module;
#end


/// The Internals Module exports everything that is necessary to make the
/// code in the Streams Module run, but only stuff that is of an internals
/// nature to a Dylan implementation.
///
define module internals
  use dylan;
  use extensions,
    import: {<byte-character>,
	     $maximum-integer,
	     one-of, false-or,
 	     <byte-vector>, <byte>,
	     report-condition, condition-format,
#if (mindy)
             on-exit,
#else
             <never-returns>,
#end
	     ignore},
    export: all;
  use system,
    import: {<buffer>, <buffer-index>, $maximum-buffer-size, copy-bytes,
	     $Newlines-are-CRLF},
    export: all;
  use threads,
    import: {<multilock>, <semaphore>, grab-lock, release-lock, locked?},
    export: all;
#if (mindy)
  use file-descriptors,
    // This is one of two use file-descriptors clauses.  This one prefixes
    // everything with "fd-"
    import: {// Lseek values for whence argument.
	     seek_set => fd-seek-set,
	     seek_cur => fd-seek-current,
	     seek_end => fd-seek-end,

	     // Open values for flags argument
	     o_rdonly, o_wronly, o_rdwr, o_creat, o_trunc, o_excl,

	     // Open errors.
	     enoent, eexist},
    prefix: "fd-",
    export: all;
  use file-descriptors,
    // This is two of two use file-descriptors clauses.  This one does no
    // prefixing.
    import: {fd-read, fd-write, fd-open, fd-close, fd-seek,
	     fd-input-available?, fd-sync-output, fd-error-string},
    export: all;
#else
  use file-descriptors, export: all;
#end
  export
    <syscall-error>, call-fd-function
#if (~mindy)
   , on-exit
#end
     ;
end module;

define module streams
  use dylan;
  use internals,
    export: {<byte-vector>, <buffer>, <byte>, <buffer-index>,
	     $maximum-buffer-size};
  export
    //
    // Classes and types.
    <stream>,
    <random-access-stream>,
    <file-stream>,
    <string-input-stream>,
    <byte-string-input-stream>,
    <string-output-stream>,
    <byte-string-output-stream>,
    //
    // Conditions.
    <end-of-file>,
    <file-not-found>,
    <file-exists>,
    //
    // Stream Extension Protocol.
    close,
    stream-extension-get-input-buffer,
    stream-extension-release-input-buffer,
    stream-extension-fill-input-buffer,
    stream-extension-input-available-at-source?,
    stream-extension-get-output-buffer,
    stream-extension-release-output-buffer,
    stream-extension-empty-output-buffer,
    stream-extension-force-secondary-buffers,
    stream-extension-synchronize,
    //
    // Basic I/O Protocol.
    read-byte,
    peek-byte,
    read-line,
    input-available?,
    flush-input,
    force-output,
    synchronize-output,
    //
    // Buffer Access Protocol.
    get-input-buffer,
    release-input-buffer,
    fill-input-buffer,
    input-available-at-source?,
    get-output-buffer,
    release-output-buffer,
    empty-output-buffer,
    force-secondary-buffers,
    synchronize,
    //
    // Data Extension Protocol.
    read-as,
    read-into!,
    write,
    write-line,
    //
    // <random-access-stream> protocol.
    stream-position,
    stream-position-setter,
    adjust-stream-position,
    stream-size,
    //
    // <string-output-stream> protocol.
    string-output-stream-string,
    //
    // <buffer> protocol.
    buffer-subsequence,
    copy-from-buffer!,
    copy-into-buffer!,
    //
    // Conditions operations.
    end-of-file-stream,
    file-not-found-filename,
    file-exists-filename,
    //
    // Locking.
    stream-locked?,
    lock-stream,
    unlock-stream,
    //
    // The following are extensions to the standard Streams Library.
    <fd-stream>;
end module;

define module standard-io
  use dylan;
  use streams,
    import: {<fd-stream>};
  export
    *standard-input*, *standard-output*, *standard-error*;
end module
