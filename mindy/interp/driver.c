/**********************************************************************\
*
*  Copyright (C) 1994, Carnegie Mellon University
*  All rights reserved.
*
*  This code was produced by the Gwydion Project at Carnegie Mellon
*  University.  If you are interested in using this code, contact
*  "Scott.Fahlman@cs.cmu.edu" (Internet).
*
***********************************************************************
*
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/driver.c,v 1.4 1994/03/31 10:06:18 wlott Exp $
*
* Main driver routines for mindy.
*
\**********************************************************************/

#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/errno.h>

#include "mindy.h"
#include "gc.h"
#include "thread.h"
#include "driver.h"
#include "bool.h"

static boolean InInterpreter = FALSE;
static jmp_buf Catcher;
static enum pause_reason PauseReason;

#define OPS_PER_TIME_SLICE 100


/* SIGINT handling. */

static void (*InterruptHandler)(void) = NULL;

static void sigint_handler()
{
    InterruptHandler();
}

void set_interrupt_handler(void (*handler)(void))
{
    unsigned oldmask = sigblock(0);

    InterruptHandler = handler;
    sigsetmask(oldmask & ~sigmask(SIGINT));
}

void clear_interrupt_handler(void)
{
    sigblock(sigmask(SIGINT));
    InterruptHandler = NULL;
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
	    sigpause(0);
	return;
    }

    bcopy(&Readers.fds, &readfds, sizeof(readfds));
    bcopy(&Writers.fds, &writefds, sizeof(writefds));

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
    boolean do_select = TRUE;

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
		while (timer-- > 0)
		    thread->advance(thread);
	    InInterpreter = FALSE;
	    clear_interrupt_handler();

	    if (TimeToGC)
		collect_garbage();
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
    if (_setjmp(Catcher) == 0)
	thread->advance(thread);
    InInterpreter = FALSE;
    if (TimeToGC)
	collect_garbage();
    return PauseReason;
}

void go_on(void)
{
    assert(InInterpreter);
    _longjmp(Catcher, 1);
}

void pause(enum pause_reason reason)
{
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
    struct sigvec sv;

    init_waiters(&Readers);
    init_waiters(&Writers);
    NumFds = 0;

    sigblock(sigmask(SIGINT));

    sv.sv_handler = sigint_handler;
    sv.sv_mask = 0;
    sv.sv_flags = 0;
#ifdef hpux
    sigvector(SIGINT, &sv, NULL);
#else
    sigvec(SIGINT, &sv, NULL);
#endif
}
