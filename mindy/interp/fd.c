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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/fd.c,v 1.7 1994/04/10 16:24:29 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#ifdef MACH
extern void bzero(void *ptr, size_t bytes);
extern int select(int nfds, fd_set *readfds, fd_set *write_fds,
		  fd_set *except_fds, struct timeval *timeout);
extern int open(const void *path, int flags, int mode);
extern int close(int fd);
extern int read(int fd, void *ptr, int bytes);
extern int write(int fd, const void *ptr, int bytes);
extern off_t lseek(int fd, off_t offset, int whence);
extern int fsync(int fd);
#endif
#ifdef hpux
#define pause buttplug
#include <unistd.h>
#undef pause
#include <fcntl.h>
/* hpux doesn't define these for some reason. */
extern int sys_nerr;
extern char *sys_errlist[];
#endif

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

static void fd_close(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t fd = args[0];

    results(thread, args-1, close(fixnum_value(fd)), obj_True);
}

static obj_t fd_error_str(obj_t errno)
{
    if (fixnum_value(errno) < 0 || fixnum_value(errno) >= sys_nerr)
	return obj_False;
    else
	return make_string(sys_errlist[fixnum_value(errno)]);
}

static int input_available(int fd)
{
    fd_set fds;
    struct timeval tv;

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
#ifdef hpux
    return select(fd+1, (int *)&fds, NULL, NULL, &tv);
#else
    return select(fd+1, &fds, NULL, NULL, &tv);
#endif
}

static void fd_input_available(obj_t self, struct thread *thread, obj_t *args)
{
    int fd = fixnum_value(args[0]);
    int res = input_available(fd);

    results(thread, args-1, res, res ? obj_True : obj_False);
}

static void fd_open(obj_t self, struct thread *thread, obj_t *args)
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
    int nfound, res;
    obj_t *old_sp;

    nfound = input_available(fd);
    if (nfound < 0) {
	old_sp = pop_linkage(thread);
	thread->sp = old_sp + 2;
	old_sp[0] = obj_False;
	old_sp[1] = make_fixnum(errno);
	do_return(thread, old_sp, old_sp);
    }
    else if (nfound == 0)
	wait_for_input(thread, fd, maybe_read);
    else {
	res = read(fd,
		   buffer_data(fp[-7]) + fixnum_value(fp[-6]),
		   fixnum_value(fp[-5]));
	
	results(thread, pop_linkage(thread), res, make_fixnum(res));
    }
}

static void fd_read(obj_t self, struct thread *thread, obj_t *args)
{
    thread->sp = args + 4;
    push_linkage(thread, args);
    maybe_read(thread);
}

static void fd_seek(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t fd = args[0];
    obj_t offset = args[1];
    obj_t whence = args[2];
    off_t res;

    res = lseek(fixnum_value(fd), fixnum_value(offset), fixnum_value(whence));

    results(thread, args-1, res, make_fixnum(res));
}

static void fd_sync_output(obj_t self, struct thread *thread, obj_t *args)
{
    int res = fsync(fixnum_value(args[0]));

    if (res < 0 && errno == EINVAL)
	/* EINVAL means the fd is a socket, not a file descriptor.  We don't */
	/* care that you can't fsync sockets. */
	results(thread, args-1, 0, obj_True);
    else
	results(thread, args-1, res, obj_True);
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
#ifdef hpux
    nfound = select(fd+1, NULL, (int *)&fds, NULL, &tv);
#else
    nfound = select(fd+1, NULL, &fds, NULL, &tv);
#endif

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
    else {
	res = write(fd,
		    buffer_data(fp[-7]) + fixnum_value(fp[-6]),
		    fixnum_value(fp[-5]));

	results(thread, pop_linkage(thread), res, make_fixnum(res));
    }
}

static void fd_write(obj_t self, struct thread *thread, obj_t *args)
{
    thread->sp = args + 4;
    push_linkage(thread, args);
    maybe_write(thread);
}


/* Init stuff. */

void init_fd_functions(void)
{
    define_constant("fd-close",
		    make_raw_method("fd-close", list1(obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, fd_close));
    define_method("fd-error-string", list1(obj_IntegerClass), FALSE, obj_False,
		  list1(obj_ObjectClass), fd_error_str);
    define_constant("fd-input-available?",
		    make_raw_method("fd-input-available?",
				    list1(obj_IntegerClass), FALSE, obj_False,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, fd_input_available));
    define_constant("fd-open",
		    make_raw_method("fd-open",
				    list3(obj_ByteStringClass,
					  obj_IntegerClass,
					  obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_open));
    define_constant("fd-read",
		    make_raw_method("fd-read",
				    listn(4, obj_IntegerClass, obj_BufferClass,
					  obj_IntegerClass, obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_read));
    define_constant("fd-seek",
		    make_raw_method("fd-seek",
				    list3(obj_IntegerClass, obj_IntegerClass,
					  obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_seek));
    define_constant("fd-sync-output",
		    make_raw_method("fd-sync-output",
				    list1(obj_IntegerClass), FALSE, obj_False,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, fd_sync_output));
    define_constant("fd-write",
		    make_raw_method("fd-write",
				    listn(4, obj_IntegerClass, obj_BufferClass,
					  obj_IntegerClass, obj_IntegerClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_write));
}
