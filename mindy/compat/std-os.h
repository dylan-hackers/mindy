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
#ifdef USE_DIRENT2_H
#   include "std-dirent2.h"
#else
#   ifdef NO_DIRENT_H
#	include "std-dirent.h"
#   else
#	include <dirent.h>
#   endif
#endif
#ifndef NO_SYS_FILE_H
#   include <sys/file.h>
#endif
#include <sys/stat.h>
#ifndef NO_SYS_TIME_H
#    include <sys/time.h>
#else
#    include <time.h>
#endif
#ifndef NO_SYS_WAIT_H
#   include <sys/wait.h>
#endif
#define pause	unistd_pause
#ifndef NO_UNISTD_H
#   include <unistd.h>
#else
#	ifdef WIN32
#	    include <io.h>
#	    define fsync	_commit
#	else
#           include "std-unistd.h"
#	endif
#endif
#undef pause

/*
 * Not all systems declare the errno variable in errno.h. so this
 * file does it explicitly.  The list of system error messages also
 * isn't generally declared in a header file anywhere.
 */

#ifndef WIN32
    extern int errno;
#endif

/*
 * The type of the status returned by wait varies from UNIX system
 * to UNIX system.  The macro below defines it:
 */

#ifdef AIX
#   define WAIT_STATUS_TYPE pid_t
#else
#ifndef NO_UNION_WAIT
#   define WAIT_STATUS_TYPE union wait
#else
#   define WAIT_STATUS_TYPE int
#endif
#endif

/*
 * Supply definitions for macros to query wait status, if not already
 * defined in header files above.
 */

#ifndef WIFEXITED
#   define WIFEXITED(stat)  (((*((int *) &(stat))) & 0xff) == 0)
#endif

#ifndef WEXITSTATUS
#   define WEXITSTATUS(stat) (((*((int *) &(stat))) >> 8) & 0xff)
#endif

#ifndef WIFSIGNALED
#   define WIFSIGNALED(stat) (((*((int *) &(stat)))) && ((*((int *) &(stat))) == ((*((int *) &(stat))) & 0x00ff)))
#endif

#ifndef WTERMSIG
#   define WTERMSIG(stat)    ((*((int *) &(stat))) & 0x7f)
#endif

#ifndef WIFSTOPPED
#   define WIFSTOPPED(stat)  (((*((int *) &(stat))) & 0xff) == 0177)
#endif

#ifndef WSTOPSIG
#   define WSTOPSIG(stat)    (((*((int *) &(stat))) >> 8) & 0xff)
#endif

/*
 * Supply macros for seek offsets, if they're not already provided by
 * an include file.
 */

#ifndef SEEK_SET
#   define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#   define SEEK_CUR 1
#endif

#ifndef SEEK_END
#   define SEEK_END 2
#endif

/*
 * The stuff below is needed by the "time" command.  If this
 * system has no gettimeofday call, then must use times and the
 * CLK_TCK #define (from sys/param.h) to compute elapsed time.
 * Unfortunately, some systems only have HZ and no CLK_TCK, and
 * some might not even have HZ.
 */

#ifdef NO_GETTOD
#	ifndef WIN32
#		include <sys/times.h>
#		include <sys/param.h>
#		ifndef CLK_TCK
#			ifdef HZ
#				define CLK_TCK HZ
#			else
#				define CLK_TCK 60
#			endif
#		endif
#	endif
#endif

/*
 * Define access mode constants if they aren't already defined.
 */

#ifndef F_OK
#    define F_OK 00
#endif
#ifndef X_OK
#    define X_OK 01
#endif
#ifndef W_OK
#    define W_OK 02
#endif
#ifndef R_OK
#    define R_OK 04
#endif

/*
 * On systems without symbolic links (i.e. S_IFLNK isn't defined)
 * define "lstat" to use "stat" instead.
 */

#ifndef S_IFLNK
#   define lstat stat
#endif

/*
 * Define macros to query file type bits, if they're not already
 * defined.
 */

#ifndef S_ISREG
#   ifdef S_IFREG
#       define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#   else
#       define S_ISREG(m) 0
#   endif
# endif
#ifndef S_ISDIR
#   ifdef S_IFDIR
#       define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#   else
#       define S_ISDIR(m) 0
#   endif
# endif
#ifndef S_ISCHR
#   ifdef S_IFCHR
#       define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#   else
#       define S_ISCHR(m) 0
#   endif
# endif
#ifndef S_ISBLK
#   ifdef S_IFBLK
#       define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
#   else
#       define S_ISBLK(m) 0
#   endif
# endif
#ifndef S_ISFIFO
#   ifdef S_IFIFO
#       define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#   else
#       define S_ISFIFO(m) 0
#   endif
# endif
#ifndef S_ISLNK
#   ifdef S_IFLNK
#       define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#   else
#       define S_ISLNK(m) 0
#   endif
# endif
#ifndef S_ISSOCK
#   ifdef S_IFSOCK
#       define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#   else
#       define S_ISSOCK(m) 0
#   endif
# endif

/*
 * Make sure that MAXPATHLEN is defined.
 */

#ifndef MAXPATHLEN
#   ifdef PATH_MAX
#       define MAXPATHLEN PATH_MAX
#   else
#       define MAXPATHLEN 2048
#   endif
#endif

/*
 * Make sure that L_tmpnam is defined.
 */

#ifndef L_tmpnam
#   define L_tmpnam 100
#endif

/*
 * Substitute our own versions for several system calls.  The
 * versions retry automatically if interrupted by signals.
 * (see protected.c).
 *
 * The only time a system call could be interrupted is if a
 * signal_handler caught the signal and continued, so the
 * fact that the system call got interrupted, too, is purely
 * a nuisance rather than any kind of feature.
 *
 * I didn't enable the global definitions for mindy because
 * interp/load.c already implements something similar.
 */

#if 0
#   define open(a,b,c) protect_open(a,b,c)
#   define read(a,b,c) protect_read(a,b,c)
#   define waitpid(a,b,c) protect_waitpid(a,b,c)
#   define write(a,b,c) protect_write(a,b,c)
#endif

EXTERN int	protect_open _ANSI_ARGS_((char *path, int oflag, int mode));
EXTERN int	protect_read _ANSI_ARGS_((int fd, VOID *buf, size_t numBytes));
EXTERN int	protect_waitpid _ANSI_ARGS_((pid_t pid, int *statPtr, int options));
EXTERN int	protect_write _ANSI_ARGS_((int fd, VOID *buf, size_t numBytes));

/*
 * Variables provided by the C library:
 */

#if defined(_sgi) || defined(__sgi)
#   define environ _environ
#endif
#ifndef WIN32
    extern char **environ;
#endif

/*
 * Provide for minimal emulation of POSIX signals.
 * Read the source for std-signal.h and sigaction.c
 * before you start getting excited about using this.
 */
#ifdef NO_SIGACTION
#   include "std-signal.h"
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

/*
 * Define OPEN_MAX if it isn't already defined for this system.
 */

#ifndef OPEN_MAX
#   define OPEN_MAX 256
#endif

/* For Windows NT (and '95), we get select functionality from WinSock */

#ifdef WIN32
#   define _INC_STDARG
#   undef VOID
#   include <winsock.h>
#endif

/*
 * The following macro defines the type of the mask arguments to
 * select:
 */

#ifndef NO_FD_SET
#   define SELECT_MASK fd_set
#else
#   ifndef _AIX
	typedef long fd_mask;
#   endif
#   if defined(_IBMR2)
#	define SELECT_MASK void
#   else
#	define SELECT_MASK int
#   endif
#endif

/* Include select and other net stuff for BeOS */
#if defined(__BEOS__)
#include <be/net/socket.h>
#endif

/*
 * Define "NBBY" (number of bits per byte) if it's not already defined.
 */

#ifndef NBBY
#   define NBBY 8
#endif

/*
 * The following macro defines the number of fd_masks in an fd_set:
 */

#if !defined(howmany)
#   define howmany(x, y) (((x)+((y)-1))/(y))
#endif
#ifdef NFDBITS
#   define MASK_SIZE howmany(FD_SETSIZE, NFDBITS)
#else
#   define MASK_SIZE howmany(OPEN_MAX, NBBY*sizeof(fd_mask))
#endif

/*
 * Finally, provide a declaration for select.
 */

#ifndef HAVE_SYS_SELECT_H
#    if !defined(SELECT_IN_TIME_H) && !defined(WIN32)
          extern int select _ANSI_ARGS_((int nfds, SELECT_MASK *readfds,
			    SELECT_MASK *writefds, SELECT_MASK *exceptfds,
			    struct timeval *timeout));
#    endif
#endif

/*
 * Provide an error free implementation of fsync() or a simple
 * declaration.
 */
#if NO_FSYNC
#   define fsync(fd)	0
#else
#   ifndef WIN32
        extern int fsync();
#   endif
#endif

/*
** Following are the remnants of system dependent stuff from
** mindy files.  What's left here was not covered by one of
** the earlier includes in std-c.h or this file.  My inclination
** is to simply write:
**	extern int select(), fsync();
** and leave it at that.  For that matter, fsync() is a bit of
** overkill.
*/

#if defined(MACH) || defined(__osf__) || defined(ultrix)
    extern int select(int nfds, fd_set *readfds, fd_set *write_fds,
		      fd_set *except_fds, struct timeval *timeout);
#endif

/* Was #if defined(__osf__) || defined(ultrix)
 */
#if defined(__osf__)
    extern int fsync(int filedes);
#   include <exc_handling.h>
#endif

/* Define a few POSIX constants in terms of the equivalent non-POSIX
 * constants.
 */

#ifndef SEEK_SET
#   define SEEK_SET L_SET
#endif

#ifndef SEEK_CUR
#   define SEEK_CUR L_INCR
#endif

#ifndef SEEK_END
#   define SEEK_END L_XTND
#endif

#ifndef O_APPEND
#   define O_APPEND FAPPEND
#endif

#ifndef O_CREAT
#   define O_CREAT FCREAT
#endif

#ifndef O_TRUNC
#   define O_TRUNC FTRUNC
#endif

#ifndef O_EXCL
#   define O_EXCL FEXCL
#endif

#ifndef O_NONBLOCK
#   ifdef O_NDELAY
#       define O_NONBLOCK O_NDELAY
#   else
#       define O_NONBLOCK FNDELAY
#   endif
#endif

#endif	/* _STD_OS_H_ */
