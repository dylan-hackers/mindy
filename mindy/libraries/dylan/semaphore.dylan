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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/semaphore.dylan,v 1.2 1994/06/17 15:57:09 wlott Exp $
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
