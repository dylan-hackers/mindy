/* 
 * protected.c - this file implements four standard system calls
 * with wrappers that ignore interrupts.  The definitions, and
 * the code that uses them, was taken from tcl-7.3.
 *
 * tclUnixUtil.c --
 *
 *	This file contains a collection of utility procedures that
 *	are present in the Tcl's UNIX core but not in the generic
 *	core.  For example, they do file manipulation and process
 *	manipulation.
 *
 *	Parts of this file are based on code contributed by Karl
 *	Lehenbauer, Mark Diekhans and Peter da Silva.
 *
 * Copyright (c) 1991-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "std-c.h"
#include "std-os.h"

/*
 *----------------------------------------------------------------------
 *
 * protect_open, protect_read, protect_waitpid, and protect_write --
 *
 *	Below are a bunch of procedures that are used instead
 *	of system calls.  Each of the procedures executes the
 *	corresponding system call and retries automatically
 *	if the system call was interrupted by a signal.
 *
 * Results:
 *	Whatever the system call would normally return.
 *
 * Side effects:
 *	Whatever the system call would normally do.
 *
 * NOTE:
 *	This should be the last page of this file, since it undefines
 *	the macros that redirect read etc. to the procedures below.
 *
 *----------------------------------------------------------------------
 */

#undef open
int protect_open(path, oflag, mode)
    char *path;
    int oflag;
    int mode;
{
    int result;
    while (1) {
	result = open(path, oflag, mode);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}

#undef read
int protect_read(fd, buf, numBytes)
    int fd;
    VOID *buf;
    size_t numBytes;
{
    int result;
    while (1) {
	result = read(fd, buf, (size_t) numBytes);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}

#undef waitpid
extern pid_t waitpid _ANSI_ARGS_((pid_t pid, int *stat_loc, int options));

/*
 * Note:  the #ifdef below is needed to avoid compiler errors on systems
 * that have ANSI compilers and also define pid_t to be short.  The
 * problem is a complex one having to do with argument type promotion.
 */

#ifdef _USING_PROTOTYPES_
int protect_waitpid _ANSI_ARGS_((pid_t pid, int *statPtr, int options))
#else
int protect_waitpid(pid, statPtr, options)
    pid_t pid;
    int *statPtr;
    int options;
#endif /* _USING_PROTOTYPES_ */
{
    int result;
    while (1) {
	result = waitpid(pid, statPtr, options);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}

#undef write
int protect_write(fd, buf, numBytes)
    int fd;
    VOID *buf;
    size_t numBytes;
{
    int result;
    while (1) {
	result = write(fd, buf, (size_t) numBytes);
	if ((result != -1) || (errno != EINTR)) {
	    return result;
	}
    }
}
