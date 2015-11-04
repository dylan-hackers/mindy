module: dylan-user

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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

define library io
  use dylan;

  export format,
         format-out,
         piped-exec,
         pprint,
         print,
         standard-io,
         streams;
end library;

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
             report-condition, condition-format, on-exit,
             <never-returns>, ignore},
    export: all;
  use system,
    import: {<buffer>, <buffer-index>, $maximum-buffer-size,
             buffer-end, buffer-end-setter, buffer-next, buffer-next-setter,
             copy-bytes},
    export: all;
  use threads,
    import: {<multilock>, <semaphore>, grab-lock, release-lock, locked?},
    export: all;
  use file-descriptors,
    // This is one of two use file-descriptors clauses.  This one prefixes
    // everything with "fd-"
    import: {// Lseek values for whence argument.
             seek_set => fd-seek-set,
             seek_cur => fd-seek-current,
             seek_end => fd-seek-end,

             // Open values for flags argument
             o_rdonly, o_wronly, o_rdwr, o_creat, o_trunc, o_excl, o_append,

             // Open errors.
             enoent, eexist, eacces},
    prefix: "fd-",
    export: all;
  use file-descriptors,
    // This is two of two use file-descriptors clauses.  This one does no
    // prefixing.
    import: {fd-read, fd-write, fd-open, fd-close, fd-seek,
             fd-input-available?, fd-sync-output, fd-error-string},
    export: all;
  export
    <syscall-error>, call-fd-function;
end module;

define module streams
  use dylan;
  use internals,
    export: {<byte-vector>, <buffer>, <byte>, <buffer-index>,
             $maximum-buffer-size, buffer-end, buffer-end-setter,
             buffer-next, buffer-next-setter };

  export
    // Classes and types.
    //
    <stream>,
    <buffered-stream>,
    <positionable-stream>,
    <file-stream>,
    <sequence-stream>,
    <string-stream>,
    <byte-string-stream>,
    <unicode-string-stream>,

    <unicode-character>,

    // Creating streams
    //
    type-for-sequence-stream,
    type-for-file-stream,
    close,

    // Reading
    //
    read-element,
    unread-element,
    peek,
    read,
    read-into!,
    discard-input,
    stream-input-available?,

    // Convenience functions for reading.
    //
    read-to,
    read-through,
    read-to-end,
    skip-through,

    // Writing
    //
    write-element,
    write,
    force-output,
    synchronize-output,
    discard-output,

    // Reading and Writing by lines
    //
    read-line,
    read-line-into!,
    write-line,
    new-line,

    // Querying
    //
    stream-open?,
    stream-element-type,
    stream-at-end?,

    // Positionable Stream Protocol.
    //
    <stream-position>,
    stream-position,
    stream-position-setter,
    adjust-stream-position,
    stream-size,
    stream-contents,

    // Using File Streams
    //
#if (~mindy)
    with-open-file,
    with-output-to-string,
#endif

    // Locking
    //
    stream-locked?,
    lock-stream,
    unlock-stream,
    // with-stream-locked,

    // Buffer Access Protocol.
    //
    get-input-buffer,
    release-input-buffer,
    // with-input-buffer,
    next-input-buffer,
    input-available-at-source?,

    get-output-buffer,
    release-output-buffer,
    // with-output-buffer,
    next-output-buffer,
    force-output-buffers,
    synchronize,

    buffer-subsequence,
    copy-into-buffer!,
    copy-from-buffer!,

    // Stream Extension Protocol.
    //
    do-get-input-buffer,
    do-release-input-buffer,
    do-next-input-buffer,
    do-input-available-at-source?,

    do-get-output-buffer,
    do-release-output-buffer,
    do-next-output-buffer,
    do-force-output-buffers,
    do-synchronize,

    // Conditions.
    //
    <end-of-stream-error>,
    end-of-stream-stream,
    <incomplete-read-error>,
    incomplete-read-sequence,
    incomplete-read-count,
    <file-error>,
    file-locator,
    <file-exists-error>,
    <file-does-not-exist-error>,
    <invalid-file-permissions-error>,

    // Wrapper Stream Protocol.
    //
    <wrapper-stream>,
    inner-stream,
    inner-stream-setter,
    outer-stream,
    outer-stream-setter,

    // The following are extensions to the Streams library.
    //
    <fd-stream>,
    <buffered-byte-string-output-stream>,
        file-descriptor;
end module;

define module piped-exec
  use dylan;
  use streams;
  use file-descriptors;
  export
    piped-exec;
end module piped-exec;
define module format
  use dylan;
  use extensions,
    import: {false-or, <general-integer>, <extended-integer>,
             $minimum-integer, <byte-character>,
             condition-format, condition-force-output,
             report-condition};
  use introspection;
  use system;
  use streams;
  use print,
    import: {print};
  export
    format,
    print-message,
    format-to-string;
end module format;

define module format-out
  use dylan;
  use format;
  use standard-io;
  use streams, import: { force-output };

  export format-out,
         force-out;

  export format-err,
         force-err;
end module format-out;

define module print-internals
  use dylan;
  use extensions,
    import: {<general-integer>, <extended-integer>,
             false-or, one-of},
    export: all;
  use introspection,
    import: {class-name, function-name, object-address,
             <subclass>, <limited-integer>, <union>,
#if (~mindy)
             <direct-instance>, direct-instance-of, <byte-character-type>,
             union-singletons,
#endif
             singleton-object, subclass-of, limited-integer-base-class,
             limited-integer-minimum, limited-integer-maximum, union-members},
    export: all;
end module print-internals;

define module pprint
  use dylan;
  use print-internals;
  use system, import: { copy-bytes };
  use streams;

  export
    pprint-logical-block, pprint-newline, pprint-indent, pprint-tab,
    *default-line-length*, *print-miser-width*;
end module pprint;

define module print
  use dylan;
  use streams;
  use pprint;
  use print-internals;
#if (~mindy)
  use system, import: { <raw-pointer> };
#endif
  use extensions,
    import: {$minimum-integer, $unsupplied, <byte-character>,
             <ratio>, numerator, denominator};
  export
    print, print-object, print-to-string,
    *print-length*, *print-level*, *print-circle?*, *print-pretty*;
end module print;

define module standard-io
  use dylan;
  use extensions,
     import: {*warning-output*};
  use streams,
    import: {<fd-stream>};
  export
    *standard-input*, *standard-output*, *standard-error*;
end module standard-io;
