module: threads
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/semaphore.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

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
// This file contains binary semaphores.
//

define class <semaphore> (<lock>)

  // The spinlock we use to make sure operations on the semaphore are atomic.
  slot lock :: <spinlock>, setter: #f,
    init-function: curry(make, <spinlock>);

  // True if locked, false if not.  (surprise!)
  slot locked? :: <boolean>, init-value: #f;

  // The event we use to signal when the semaphore becomes available.
  slot available :: <event>, setter: #f,
    init-function: curry(make, <event>);
end;

define method grab-lock (semaphore :: <semaphore>)
  grab-lock(semaphore.lock);
  while (locked?(semaphore))
    wait-for-event(semaphore.available, semaphore.lock);
    grab-lock(semaphore.lock);
  end;
  semaphore.locked? := #t;
  release-lock(semaphore.lock);
end;

define method release-lock (semaphore :: <semaphore>)
  grab-lock(semaphore.lock);
  if (semaphore.locked?)
    semaphore.locked? := #f;
    signal-event(semaphore.available);
    release-lock(semaphore.lock);
  else
    release-lock(semaphore.lock);
    error("%= isn't locked, hence cannot be unlocked");
  end;
end;

define method wait-for-event (event :: <event>, semaphore :: <semaphore>)
  grab-lock(semaphore.lock);
  if (semaphore.locked?)
    semaphore.locked? := #f;
    signal-event(semaphore.available);
    wait-for-event(event, semaphore.lock);
  else
    release-lock(semaphore.lock);
    error("%= isn't locked, hence cannot be unlocked");
  end;
end;
