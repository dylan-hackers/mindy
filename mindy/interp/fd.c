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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/fd.c,v 1.2 1994/03/31 23:00:00 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/errno.h>

#include "mindy.h"
#include "list.h"
#include "bool.h"
#include "thread.h"
#include "func.h"
#include "driver.h"
#include "buf.h"
#include "str.h"
#include "num.h"
#include "obj.h"
#include "def.h"

static void results(struct thread *thread, obj_t *old_sp,
		    int okay, obj_t result)
{
    thread->sp = old_sp + 2;

    if (okay < 0) {
	old_sp[0] = obj_False;
	old_sp[1] = make_fixnum(errno);
    }
    else {
	old_sp[0] = result;
	old_sp[1] = obj_False;
    }

    do_return(thread, old_sp, old_sp);
}

static void unix_close(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t fd = args[0];

    results(thread, args-1, close(fixnum_value(fd)), obj_True);
}

static void unix_lseek(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t fd = args[0];
    obj_t offset = args[1];
    obj_t whence = args[2];
    off_t res;

    res = lseek(fixnum_value(fd), fixnum_value(offset), fixnum_value(whence));

    results(thread, args-1, res, make_fixnum(res));
}

static void unix_open(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t path = args[0];
    obj_t flags = args[1];
    obj_t mode = args[2];
    int res;

    res = open(string_chars(path), fixnum_value(flags), fixnum_value(mode));

    results(thread, args-1, res, make_fixnum(res));
}

static void maybe_read(struct thread *thread)
{
    obj_t *fp = thread->fp;
    int fd = fixnum_value(fp[-8]);
    fd_set fds;
    struct timeval tv;
    int nfound, res;
    obj_t *old_sp;

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    nfound = select(fd+1, &fds, NULL, NULL, &tv);

    if (nfound < 0)
	if (errno != EINTR) {
	    old_sp = pop_linkage(thread);
	    thread->sp = old_sp + 2;
	    old_sp[0] = obj_False;
	    old_sp[1] = make_fixnum(errno);
	    do_return(thread, old_sp, old_sp);
	}
	else
	    wait_for_input(thread, fd, maybe_read);
    else if (nfound == 0)
	wait_for_input(thread, fd, maybe_read);

    res = read(fd,
	       buffer_data(fp[-7]) + fixnum_value(fp[-6]),
	       fixnum_value(fp[-5]));

    results(thread, pop_linkage(thread), res, make_fixnum(res));
}

static void unix_read(obj_t self, struct thread *thread, obj_t *args)
{
    thread->sp = args + 4;
    push_linkage(thread, args);
    maybe_read(thread);
}

static obj_t unix_strerror(obj_t errno)
{
    if (fixnum_value(errno) < 0 || fixnum_value(errno) >= sys_nerr)
	return obj_False;
    else
	return make_string(sys_errlist[fixnum_value(errno)]);
}

static void maybe_write(struct thread *thread)
{
    obj_t *fp = thread->fp;
    int fd = fixnum_value(fp[-8]);
    fd_set fds;
    struct timeval tv;
    int nfound, res;
    obj_t *old_sp;

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    nfound = select(fd+1, NULL, &fds, NULL, &tv);

    if (nfound < 0)
	if (errno != EINTR) {
	    old_sp = pop_linkage(thread);
	    thread->sp = old_sp + 2;
	    old_sp[0] = obj_False;
	    old_sp[1] = make_fixnum(errno);
	    do_return(thread, old_sp, old_sp);
	}
	else
	    wait_for_output(thread, fd, maybe_write);
    else if (nfound == 0)
	wait_for_output(thread, fd, maybe_write);

    res = write(fd,
		buffer_data(fp[-7]) + fixnum_value(fp[-6]),
		fixnum_value(fp[-5]));

    results(thread, pop_linkage(thread), res, make_fixnum(res));
}

static void unix_write(obj_t self, struct thread *thread, obj_t *args)
{
    thread->sp = args + 4;
    push_linkage(thread, args);
    maybe_write(thread);
}


/* Init stuff. */

void init_unix_functions(void)
{
    define_constant("unix-close",
		    make_raw_method("unix-close", list1(obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, unix_close));
    define_constant("unix-lseek",
		    make_raw_method("unix-lseek",
				    list3(obj_IntegerClass, obj_IntegerClass,
					  obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, unix_lseek));
    define_constant("unix-open",
		    make_raw_method("unix-lseek",
				    list3(obj_ByteStringClass,
					  obj_IntegerClass,
					  obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, unix_open));
    define_constant("unix-read",
		    make_raw_method("unix-read",
				    listn(4, obj_IntegerClass, obj_BufferClass,
					  obj_IntegerClass, obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, unix_read));
    define_method("unix-strerror", list1(obj_IntegerClass), FALSE, obj_False,
		  list1(obj_ObjectClass), unix_strerror);
    define_constant("unix-write",
		    make_raw_method("unix-lseek",
				    listn(4, obj_IntegerClass, obj_BufferClass,
					  obj_IntegerClass, obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, unix_write));
}
