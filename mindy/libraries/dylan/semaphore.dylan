module: threads

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/semaphore.dylan,v 1.1 1994/06/11 03:12:13 wlott Exp $
//
// This file contains general purpose n-ary semaphores.
//

define class <semaphore> (<lock>)

  // The spin-lock we use to make sure operations on the semaphore are atomic.
  slot lock :: <spin-lock>, setter: #f,
    init-function: curry(make, <spin-lock>);

  // The number of times we have been unlocked.
  slot count :: <integer>, init-value: 1, init-keyword: count:;

  // The event we use to signal when the semaphore becomes available.
  slot available :: <event>, setter: #f,
    init-function: curry(make, <event>);
end;

define method locked? (semaphore :: <semaphore>) => locked? :: <boolean>;
  grab-lock(semaphore.lock);
  let res = zero?(semaphore.count);
  release-lock(semaphore.lock);
  res;
end;

define method grab-lock (semaphore :: <semaphore>)
  grab-lock(semaphore.lock);
  while (zero?(semaphore.count))
    wait-for-event(semaphore.available, semaphore.lock);
    grab-lock(semaphore.lock);
  end;
  semaphore.count := semaphore.count - 1;
  release-lock(semaphore.lock);
end;

define method release-lock (semaphore :: <semaphore>)
  grab-lock(semaphore.lock);
  semaphore.count := semaphore.count + 1;
  signal-event(semaphore.available);
  release-lock(semaphore.lock);
end;

define method wait-for-event (event :: <event>, semaphore :: <semaphore>)
  grab-lock(semaphore.lock);
  semaphore.count := semaphore.count + 1;
  signal-event(semaphore.available);
  wait-for-event(event, semaphore.lock);
end;
