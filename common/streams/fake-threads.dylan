module: threads
author: ram+@cs.cmu.edu
synopsis: This file implements some fake thread operations
copyright: See below.

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


define /* exported */ abstract class <lock> (<object>)
end class;

define sealed domain initialize (<lock>);

define /* exported */ generic grab-lock (lock :: <lock>) => ();
define /* exported */ generic release-lock (lock :: <lock>) => ();
define /* exported */ generic locked? (lock :: <lock>) => res :: <boolean>;


define /* exported */ class <multilock> (<lock>)
  slot lock-count :: <integer>, init-value: 0;
end class;

define sealed domain make (singleton(<multilock>));

define method grab-lock (lock :: <multilock>) => ();
  lock.lock-count := lock.lock-count + 1;
end method;

define method release-lock (lock :: <multilock>) => ();
  let cnt = lock.lock-count;
  unless (cnt > 0)
    error("Not locked?");
  end;
  lock.lock-count := cnt - 1;
end method;

define method locked? (lock :: <multilock>) => res :: <boolean>;
  lock.lock-count > 0;
end method;


define /* exported */ class <semaphore> (<lock>)
  slot locked? :: <boolean>, init-value: #f;
end class;

define sealed domain make (singleton(<semaphore>));

define method grab-lock (lock :: <semaphore>) => ();
  if (lock.locked?)
    error("Lock already locked -- grabbing it would deadlock.\n");
  end if;
  lock.locked? := #t;
end method grab-lock;

define method release-lock (lock :: <semaphore>) => ();
  unless (lock.locked?)
    error("Not locked?");
  end unless;
  lock.locked? := #f;
end method release-lock;
