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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/fd.c,v 1.10 1994/04/29 06:41:53 wlott Exp $
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
#define alloca buttplug1
#define pause buttplug2
#define execvp buttplug3
#include <libc.h>
#undef alloca
#undef pause
#undef execvp
extern int execvp(const char *, char *const []);
#endif MACH
#ifdef hpux
#include <stdlib.h>
#define pause buttplug
#include <unistd.h>
#undef pause
#include <fcntl.h>
/* hpux doesn't define these for some reason. */
extern int sys_nerr;
extern char *sys_errlist[];
#endif hpux

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


/* Function to run an arbitrary program, returning file descriptors for the
   program's stdin and stdout. */
static void fd_exec(obj_t self, struct thread *thread, obj_t *args)
{
    int inpipes[2], outpipes[2], forkresult;
    obj_t *oldargs;

    oldargs = args - 1;
    thread->sp = args + 1;

    if (pipe(inpipes) >= 0 && pipe(outpipes) >= 0 &&
	(forkresult = fork()) != -1)
    {
	if (forkresult == 0) {
	    /* This process is going to exit shortly, so we needn't be too
	       careful about malloc behavior, nor about the fact that we
	       destructively modify the command string. */
	    char *command = string_chars(args[0]);
	    char *p, **args;
	    int argcounter = 1;

	    for (p = command; *p != 0; p++)
		if (*p == ' ') {
		    argcounter++;
		    while (*(++p) == ' ');
		}
	    args = (char **) calloc(argcounter+1, sizeof(char *));
	    args[0] = command;
	    for (p = command, argcounter = 1; *p != 0; p++) {
		if (*p == ' ') {
		    *p = 0;
		    while (*(++p) == ' ');
		    if (*p != 0)
			args[argcounter++] = p;
		}
	    }
	    args[argcounter] = 0;

	    close(0);
	    dup(inpipes[0]);
	    close(inpipes[0]);
	    close(inpipes[1]);
	    close(1);
	    dup(outpipes[1]);
	    close(outpipes[0]);
	    close(outpipes[1]);
	    execvp(args[0], args);
	    /* We never get here.... */
	}
	close(inpipes[0]);
	close(outpipes[1]);
	
	oldargs[0] = make_fixnum(inpipes[1]);
	oldargs[1] = make_fixnum(outpipes[0]);
    } else {
	oldargs[0] = obj_False;
	oldargs[1] = obj_False;
    }

    do_return(thread, oldargs, oldargs);
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
		  obj_ObjectClass, fd_error_str);
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
    define_constant("fd-exec",
		    make_raw_method("fd-exec",
				    list1(obj_ByteStringClass),
				    FALSE, obj_False,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_exec));
}
