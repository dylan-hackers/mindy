/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
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
*     University.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports, questions, comments, and suggestions should be sent by
*  E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
*
***********************************************************************
*
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/input.c,v 1.13 1994/07/07 07:14:56 wlott Exp $
*
* This file implements getc.
*
\**********************************************************************/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/errno.h>
#ifdef MACH
extern void bzero(void *ptr, size_t bytes);
extern int select(int nfds, fd_set *readfds, fd_set *write_fds,
		  fd_set *except_fds, struct timeval *timeout);
#endif
#if defined(__osf__) || defined(ultrix)
extern void bzero(char *string, int length);
extern int select(int nfds, fd_set *readfds, fd_set *writefds,
		  fd_set *exceptfds, struct timeval *timeout);
#endif
#ifdef sgi
#define pause buttplug
#include <unistd.h>
#undef pause
#include <sys/types.h>
#include <bstring.h>
#include <sys/time.h>
#include <errno.h>
#endif

#include "mindy.h"
#include "char.h"
#include "list.h"
#include "bool.h"
#include "thread.h"
#include "func.h"
#include "driver.h"
#include "error.h"
#include "def.h"

static void getc_or_wait(struct thread *thread)
{
    if (
#ifdef linux
        stdin->_IO_read_ptr >= stdin->_IO_read_end
#else
        stdin->_cnt == 0
#endif
        && !feof(stdin)) {
	int fd = fileno(stdin);
	fd_set fds;
	struct timeval tv;
	int nfound;

	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	tv.tv_sec = 0;
	tv.tv_usec = 0;
#ifdef hpux
	nfound = select(fd+1, (int *)&fds, NULL, NULL, &tv);
#else
	nfound = select(fd+1, &fds, NULL, NULL, &tv);
#endif

	if (nfound < 0) {
	    switch (errno) {
	      case EBADF:
		error("Tried to getc with stdin broken.");
	      case EINTR:
		wait_for_input(thread, fd, getc_or_wait);
	      case EINVAL:
		lose("select failed with EINVAL?");
	    }
	}
	else if (nfound == 0)
	    wait_for_input(thread, fd, getc_or_wait);
    }

    {
	int c = getchar();
	obj_t *old_sp = pop_linkage(thread);

	if (c != EOF)
	    *old_sp = int_char(c);
	else
	    *old_sp = obj_False;

	thread->sp = old_sp + 1;

	do_return(thread, old_sp, old_sp);
    }
}

static obj_t dylan_getc(void)
{
    getc_or_wait(thread_current());
    go_on();
    /* go_on never returns. */
    lose("go_on actually returned?");
    return NULL;
}

void init_input_functions(void)
{
    define_function("getc", obj_Nil, FALSE, obj_False, FALSE,
		    obj_CharacterClass, dylan_getc);
}
