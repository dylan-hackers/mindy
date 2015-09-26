/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
*  All rights reserved.
*  
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*  
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/interp/driver.c,v 1.6 2003/12/22 06:16:02 housel Exp $
*
* Main driver routines for mindy.
*
\**********************************************************************/

#include "../compat/std-c.h"
#include "../compat/std-os.h"

#include <setjmp.h>
#ifndef HAVE__SETJMP
#   define _setjmp      setjmp
#endif
#ifndef HAVE__LONGJMP
#   define _longjmp	longjmp
#endif

#include "mindy.h"
#include "gc.h"
#include "thread.h"
#include "driver.h"
#include "bool.h"
#include "gc.h"
#if SLOW_FUNCTION_POINTERS
#   include "interp.h"
#endif
#include "fd.h"
#ifdef MACOS
#	include <Events.h>
#endif

static boolean InInterpreter = FALSE;
static jmp_buf Catcher;
static enum pause_reason PauseReason;

#define OPS_PER_TIME_SLICE 100


/* signal handling. */
static void (*SignalHandlers[NSIG])(void) = {0};
static boolean SignalPending[NSIG] = {0};
static boolean SignalAction[NSIG] = {0};
static boolean SignalBlocked[NSIG] = {0};

static void signal_handler(int sig)
{
    if (SignalHandlers[sig]) {
        SignalBlocked[sig] = TRUE;
	SignalHandlers[sig]();
        SignalBlocked[sig] = FALSE;
    } else
	SignalPending[sig] = TRUE;
}  

void set_signal_handler(int sig, void(*handler)(void))
{
    SignalHandlers[sig] = handler;
    if (SignalPending[sig]) {
        SignalPending[sig] = FALSE;
	handler();
    }
    if ( ! SignalAction[sig]) {
        struct sigaction sa = { 0 };
	sa.sa_handler = signal_handler;
	sigaction(sig, &sa, NULL);
	SignalAction[sig] = TRUE;
    }
    if (SignalBlocked[sig])
      unblock_signal_handler(sig);
}

void clear_signal_handler(int sig)
{
    SignalHandlers[sig] = NULL;
    if (SignalBlocked[sig])
      unblock_signal_handler(sig);
}

void unblock_signal_handler(int sig)
{
  sigset_t set;

  SignalBlocked[sig] = FALSE;
  sigemptyset(&set);
  sigaddset(&set, sig);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
}

/* SIGINT handling. */

void set_interrupt_handler(void (*handler)(void))
{
    set_signal_handler(SIGINT, handler);
}

void clear_interrupt_handler(void)
{
    clear_signal_handler(SIGINT);
}

void unblock_interrupt_handler(void)
{
    unblock_signal_handler(SIGINT);
}


/* Waiting on file descriptors. */

static struct waiters {
    fd_set fds;
    obj_t events[FD_SETSIZE];
} Readers, Writers;
static int NumFds;

static void check_fds(boolean block)
{
    fd_set readfds, writefds;
    int nfound, fd;
    struct timeval tv, *tvp;

    if (NumFds == 0) {
	if (block)
	  sigsuspend(0);
	return;
    }

    memcpy(&readfds, &Readers.fds, sizeof(readfds));
    memcpy(&writefds, &Writers.fds, sizeof(writefds));

#ifdef WIN32
    do {
    	for (fd = 0; fd < NumFds; fd++) {
	    if (FD_ISSET(fd, &Readers.fds) && input_available(fd)) {
	        event_broadcast(Readers.events[fd]);
	        FD_CLR(fd, &Readers.fds);
	        block = 0;
	    }
	    if (FD_ISSET(fd, &Writers.fds) && output_writable(fd)) {
		event_broadcast(Writers.events[fd]);
		FD_CLR(fd, &Writers.fds);
		block = 0;
	    }
	}
    } while (block);
    for (fd = NumFds - 1; fd >= 0; fd--)   /* Adjust NumFds */
        if (FD_ISSET(fd, &Readers.fds) || FD_ISSET(fd, &Writers.fds))
	    break;
    NumFds = fd+1;
#else
    if (block)
	tvp = NULL;
    else {
	tv.tv_usec = 0;
	tv.tv_sec = 0;
	tvp = &tv;
    }

    nfound = select(NumFds, &readfds, &writefds, NULL, tvp);

    if (nfound < 0) {
	switch (errno) {
	  case EBADF:
	    /* One of the file descriptors when bad.  Wake everyone up */
	    /* and let the individual threads figure out who is selecting */
	    /* on a bogus fd. */
	    for (fd = 0; fd < NumFds; fd++) {
		if (FD_ISSET(fd, &Readers.fds))
		    event_broadcast(Readers.events[fd]);
		if (FD_ISSET(fd, &Writers.fds))
		    event_broadcast(Writers.events[fd]);
	    }
	    FD_ZERO(&Readers.fds);
	    FD_ZERO(&Writers.fds);
	    NumFds = 0;
	    break;
		
	  case EINTR:
	    break;
	  case EINVAL:
	    lose("select failed with EINVAL?");
	}
    }
    else if (nfound > 0) {
	for (fd = 0; fd < NumFds; fd++) {
	    if (FD_ISSET(fd, &readfds)) {
		event_broadcast(Readers.events[fd]);
		FD_CLR(fd, &Readers.fds);
	    }
	    if (FD_ISSET(fd, &writefds)) {
		event_broadcast(Writers.events[fd]);
		FD_CLR(fd, &Writers.fds);
	    }
	}
	for (fd = NumFds - 1; fd >= 0; fd--)
	    if (FD_ISSET(fd, &Readers.fds) || FD_ISSET(fd, &Writers.fds))
		break;
	NumFds = fd+1;
    }
#endif
}

static void wait_for_fd(struct thread *thread, int fd, struct waiters *waiters,
			void (*advance)(struct thread *thread))
{
    obj_t event;

    FD_SET(fd, &waiters->fds);

    if (NumFds <= fd)
	NumFds = fd+1;

    event = waiters->events[fd];
    if (event == obj_False) {
	event = make_event();
	waiters->events[fd] = event;
    }

    event_wait(thread, event, obj_False, advance);
}

void wait_for_input(struct thread *thread, int fd,
		    void (*advance)(struct thread *thread))
{
    wait_for_fd(thread, fd, &Readers, advance);
}

void wait_for_output(struct thread *thread, int fd,
		     void (*advance)(struct thread *thread))
{
    wait_for_fd(thread, fd, &Writers, advance);
}


/* Driver loop entry points. */

static void set_pause_interrupted(void)
{
    PauseReason = pause_Interrupted;
}

enum pause_reason do_stuff(void)
{
    struct thread *thread;
    volatile int timer;
    volatile boolean do_select = TRUE;

    assert (!InInterpreter);

    do {
	if (do_select)
	    check_fds(FALSE);
	else
	    do_select = TRUE;
	PauseReason = pause_NoReason;
	thread = thread_pick_next();
	if (thread) {
	    timer = OPS_PER_TIME_SLICE;
	    InInterpreter = TRUE;
	    set_interrupt_handler(set_pause_interrupted);
	    _setjmp(Catcher);
	    if (PauseReason == pause_NoReason)
		while (timer-- > 0) {
#if SLOW_FUNCTION_POINTERS
		    if (thread->advance)
			thread->advance(thread);
		    else
			interpret_next_byte(thread);
#else
		    thread->advance(thread);
#endif
		}
#ifdef MACOS
		{
			EventRecord e;
			/* Weak!!!! */
			OSEventAvail(0, &e);
			if( (e.modifiers & controlKey ) && (e.modifiers & cmdKey ) && (e.modifiers & optionKey ) )
			{
				set_pause_interrupted();
			}
		}
#endif
	    InInterpreter = FALSE;
	    clear_interrupt_handler();

	    if (TimeToGC)
		collect_garbage(FALSE);
	}
	else if (all_threads() == NULL)
	    PauseReason = pause_NothingToRun;
	else {
	    set_interrupt_handler(set_pause_interrupted);
	    check_fds(TRUE);
	    do_select = FALSE;
	    clear_interrupt_handler();
	}
    } while (PauseReason == pause_NoReason
	     || PauseReason == pause_PickNewThread);
    return PauseReason;
}

enum pause_reason single_step(struct thread *thread)
{
    assert(!InInterpreter);
    assert(thread->status == status_Running);
    assert(thread->suspend_count == 0);

    check_fds(FALSE);

    thread_set_current(thread);
    InInterpreter = TRUE;
    PauseReason = pause_NoReason;
    set_interrupt_handler(set_pause_interrupted);
    if (_setjmp(Catcher) == 0) {
#if SLOW_FUNCTION_POINTERS
	if (thread->advance)
	    thread->advance(thread);
	else
	    interpret_next_byte(thread);
#else
	thread->advance(thread);
#endif
    }
    InInterpreter = FALSE;
    clear_interrupt_handler();
    if (TimeToGC)
	collect_garbage(FALSE);
    return PauseReason;
}

void go_on(void)
{
    assert(InInterpreter);
    _longjmp(Catcher, 1);
}

void mindy_pause(enum pause_reason reason)
{
    clear_interrupt_handler();
    if (reason != pause_NoReason)
	PauseReason = reason;
    go_on();
}


/* GC stuff. */

static void scav_waiters(struct waiters *waiters)
{
    int fd;

    for (fd = 0; fd < FD_SETSIZE; fd++)
	scavenge(waiters->events + fd);
}

void scavenge_driver_roots(void)
{
    scav_waiters(&Readers);
    scav_waiters(&Writers);
}


/* Init stuff. */

static void init_waiters(struct waiters *waiters)
{
    int fd;

    FD_ZERO(&waiters->fds);
    for (fd = 0; fd < FD_SETSIZE; fd++)
	waiters->events[fd] = obj_False;
}

void init_driver()
{
    /* By using this declaration rather than the constant NULL, we
     * avoid a compiler bug on the Sparc.
     */
    void (*null)(void) = NULL;

    init_waiters(&Readers);
    init_waiters(&Writers);
    NumFds = 0;
    set_interrupt_handler(null);
}
