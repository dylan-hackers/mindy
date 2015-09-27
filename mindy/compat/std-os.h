/*
 * std-os.h --
 *
 *	Provide emulation of standard unix where parts might be lacking.
 *
 *	This file is adapted from tcl-7.3 tclUnix.h and should be considered
 *	a highly derivative workd covered by the following copyright.
 *
 *	Parts of this are irrelevant to mindy as it stands, but they're easier
 *	to keep than to go back and dig them up later.
 *
 * tclUnix.h --
 *
 *	This file reads in UNIX-related header files and sets up
 *	UNIX-related macros for Tcl's UNIX core.  It should be the
 *	only file that contains #ifdefs to handle different flavors
 *	of UNIX.  This file sets up the union of all UNIX-related
 *	things needed by any of the Tcl core files.
 *
 *	Much of the material in this file was originally contributed
 *	by Karl Lehenbauer, Mark Diekhans and Peter da Silva.
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
 *
 * $Header: /scm/cvs/src/mindy/compat/std-os.h,v 1.5 2003/01/26 15:35:53 andreas Exp $ SPRITE (Berkeley)
 */

#ifndef _STD_OS_H_
#define _STD_OS_H_	1

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#ifndef NO_PWD_H
#   include <pwd.h>
#endif
#include <signal.h>
#ifndef NO_SYS_PARAM_H
#   include <sys/param.h>
#endif
#include <dirent.h>
#ifndef NO_SYS_FILE_H
#   include <sys/file.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_TIME_H
#    include <time.h>
#else
# ifndef NO_SYS_TIME_H
#    include <sys/time.h>
# else
#    warning Neither time.h nor sys/time.h found
# endif
#endif
#ifndef NO_SYS_WAIT_H
#   include <sys/wait.h>
#endif
#ifdef WIN32
#   include <io.h>
#	  define fsync	_commit
#else
#   include <unistd.h>
#endif

/*
 * On systems without symbolic links (i.e. S_IFLNK isn't defined)
 * define "lstat" to use "stat" instead.
 */

#ifndef S_IFLNK
#   define lstat stat
#endif

#ifndef WIN32
    extern char **environ;
#endif

/*
 * A series of tests to deal with unuseful select() definitions.
 * Tk actually handles the mask bit twiddling itself, maintaining
 * static arrays of fd_masks.
 *
 * Some systems have a useful sys/select.h.
 */
#ifdef HAVE_SYS_SELECT_H
#   include <sys/select.h>
#endif

/* For Windows NT (and '95), we get select functionality from WinSock */

#ifdef WIN32
#   include <winsock.h>
#endif

#ifndef O_NONBLOCK
#   ifdef O_NDELAY
#       define O_NONBLOCK O_NDELAY
#   else
#       define O_NONBLOCK FNDELAY
#   endif
#endif

#endif	/* _STD_OS_H_ */
