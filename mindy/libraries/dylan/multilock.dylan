module: threads
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/multilock.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

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
// This file contains multilocks, locks that the same thread can lock
// multiple times without blocking.
//

define class <multilock> (<lock>)

  // The spin lock we use to make sure operations on the multilock are
  // atomic.
  slot lock :: <spinlock>, setter: #f,
    init-function: curry(make, <spinlock>);

  // The thread currently holding this lock, or #f if currently unlocked.
  slot locker :: false-or(<thread>), init-value: #f;

  // The number of times the multilock has been locked by the locking
  // thread.
  slot count :: limited(<integer>, min: 0), init-value: 0;

  // The event we signal whenever the multilock becomes available.
  slot available :: <event>, setter: #f,
    init-function: curry(make, <event>);
end;

define method locked? (multilock :: <multilock>) => locked? :: <boolean>;
  grab-lock(multilock.lock);
  let res = if (multilock.locker) #t else #f end;
  release-lock(multilock.lock);
  res;
end;

define method grab-lock (multilock :: <multilock>) => res :: <false>;
  let me = current-thread();

  grab-lock(multilock.lock);
  if (multilock.locker == me)
    multilock.count := multilock.count + 1;
  else
    while (multilock.locker)
      wait-for-event(multilock.available, multilock.lock);
      grab-lock(multilock.lock);
    end;
    multilock.locker := me;
    multilock.count := 1;
  end;
  release-lock(multilock.lock);
  #f;
end;

define method release-lock (multilock :: <multilock>) => res :: <false>;
  grab-lock(multilock.lock);
  let locker = multilock.locker;
  unless (locker)
    error("%= isn't currently locked.", multilock);
  end;
  unless (locker == current-thread())
    release-lock(multilock.lock);
    error("%= attempted to unlock %=, but it is held by %=",
	  current-thread(), lock, locker);
  end;
  if (zero?(multilock.count := multilock.count - 1))
    multilock.locker := #f;
    signal-event(multilock.available);
  end;
  release-lock(multilock.lock);
  #f;
end;
