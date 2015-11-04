module: Streams
author: Ben Folk-Williams
synopsis: Efficient buffered versions of the reading convenience functions.
copyright: see below

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

/// read-to
///
define method read-to (stream :: <buffered-stream>, element :: <object>,
                       #key on-end-of-stream :: <object>
                              = $unsupplied,
                            test :: <function> = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      if (on-end-of-stream == $unsupplied)
        error(make(<end-of-stream-error>, stream: stream));
      else
        values(on-end-of-stream, #f);
      end if;
    else
      let start :: <integer> = buf.buffer-next;
      let stop :: <integer> = buf.buffer-end;
      let elt :: <byte> = as(<byte>, element);
      for (i from start below stop, until: test(buf[i], elt))
      finally
        let seq-type :: <type> = type-for-sequence(stream.stream-element-type);
        if (i == stop)
          buf.buffer-next := stop;
          values(buffer-subsequence(buf, seq-type, start, stop), #f);
        else
          buf.buffer-next := i + 1; // Consume boundary elt.
          values(buffer-subsequence(buf, seq-type, start, i), #t);
        end if;
      end for;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-to;

/// read-through
///
define method read-through (stream :: <buffered-stream>, element :: <object>,
                                   #key on-end-of-stream :: <object>
                                          = $unsupplied,
                                        test :: <function> = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      if (on-end-of-stream == $unsupplied)
        error(make(<end-of-stream-error>, stream: stream));
      else
        values(on-end-of-stream, #f);
      end if;
    else
      let start :: <integer> = buf.buffer-next;
      let stop :: <integer> = buf.buffer-end;
      let elt :: <byte> = as(<byte>, element);
      for (i from start below stop, until: test(buf[i], elt))
      finally
        let seq-type :: <type> = type-for-sequence(stream.stream-element-type);
        if (i == stop)
          buf.buffer-next := stop;
          values(buffer-subsequence(buf, seq-type, start, stop), #f);
        else
          buf.buffer-next := i + 1; // Consume boundary elt.
          values(buffer-subsequence(buf, seq-type, start, i + 1), #t);
        end if;
      end for;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-through;

/// read-to-end
///
define method read-to-end (stream :: <buffered-stream>)
 => sequence :: <sequence>;
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    let res-type :: <type> = type-for-sequence(stream.stream-element-type);
    if (~ buf)
      make(res-type, size: 0);
    else
      let res :: res-type = buffer-subsequence(buf, res-type,
                                               buf.buffer-next,
                                               buf.buffer-end);
      buf.buffer-next := buf.buffer-end;
      buf := next-input-buffer(stream);
      while (buf)
        res := concatenate(res, buffer-subsequence(buf, res-type,
                                                   buf.buffer-next,
                                                   buf.buffer-end));
        buf.buffer-next := buf.buffer-end;
        buf := next-input-buffer(stream);
      end while;
      res;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-to-end;

/// skip-through
///
define method skip-through  (stream :: <buffered-stream>, element :: <object>,
                                    #key test :: <function> = \==)
 => found? :: <boolean>;
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      #f;
    else
      let start :: <integer> = buf.buffer-next;
      let stop :: <integer> = buf.buffer-end;
      let elt :: <byte> = as(<byte>, element);
      for (i from start below stop, until: test(buf[i], elt))
      finally
        let seq-type :: <type> = type-for-sequence(stream.stream-element-type);
        if (i == stop)
          buf.buffer-next := stop;
          #f;
        else
          buf.buffer-next := i + 1; // Consume boundary elt.
          #t;
        end if;
      end for;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method skip-through;
