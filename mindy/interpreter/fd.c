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
* $Header: /scm/cvs/src/mindy/interp/fd.c,v 1.6 2003/12/22 06:16:03 housel Exp $
*
* This file implements an interface to file descriptors.
*
\**********************************************************************/

#include "../compat/std-c.h"
#include "../compat/std-os.h"

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
#include "fd.h"
#include "../compat/cygwin.h"

/* And now, some large pieces of OS-dependent stuff. */

#ifdef WIN32
#include <io.h>

/* Here, we kluge around the fact that Windows/NT for non-sockets won't 
   tell you if there's input available or not on a file descriptor.  To
   get around this, we spawn a thread for each fd whose job is to try to
   read from the thread.  When the "read 0 bytes" returns, we know there
   is input available.

   Assumes file descriptors are allocated more or less consecutively.
   (We simply keep an array from 0 to 99, and assume all our file 
   descriptors will fall in there)  Also assumes there is never more than 
   one "real" thread trying to read from an fd at any one time.  (Or for
   that matter, reading an fd while at the same time calling 
   input_available on the same fd from another thread)

   Currently, input_checker updates its answer after every read.  
   Another approach would be for input_available to ask input_checker
   to update its answer.  This second approach depends upon the idea that
   it is always safe to safe "no input available" so long as if it's false
   we don't give that answer forever.  This second approach may be more
   appropriate if calls to input_available are rare, but that's not the
   case in Mindy.
 */

#define MAX_FDS FD_SETSIZE
static boolean input_available_array[MAX_FDS]; /* no mutex protecting this */
static HANDLE fd_threads[MAX_FDS]; /* thread handles */
static HANDLE update_input_available[MAX_FDS]; /* handles to events */
static HANDLE input_available_is_updated[MAX_FDS]; /* handles to events */

/* checks for input.  Although it seems like there should be a simpler
   way, this does a full producer/consumer thing with mindy_read.
 */
static DWORD input_checker (LPDWORD param)
{
    int fd = (int) param;  /* so what if we cast a pointer to an int? */
    HANDLE handle = (HANDLE) _get_osfhandle(fd);
    int read_from = 0;
    for (;;) {
	char small_buffer;  /* Ignore the contents of the next two vars */
	int bytes_read;
	DWORD the_error = 0;
	boolean read_result;
	/* Wait until someone invalidates our last answer */
        WaitForSingleObject(update_input_available[fd], INFINITE);
	/* Now loop until we complete a read without getting a
	   broken pipe error */
	/* read 0 bytes, block if not available */
	read_result = ReadFile(handle, &small_buffer,
			       0, &bytes_read, NULL);
	if (!read_result) {
	    the_error = GetLastError();
	    if (the_error != ERROR_BROKEN_PIPE) { 
		/* handle broken pipes later */
		lose("Read error type %d on fd %d (aka handle #x%x)", 
		     the_error, fd, handle);
	    }
	}
	input_available_array[fd] = TRUE;
	SetEvent(input_available_is_updated[fd]);
	if (the_error == ERROR_BROKEN_PIPE) {
	    /* All further read attempts will return this error.  
	       Go to sleep; we'll soon be killed off by mindy_close */
	    SuspendThread(fd_threads[fd]);
	}
	read_from = 1;
    }
    lose("This is not supposed to be reached!");
    return 0;
}

static void setup_input_checker (int fd)
{
    DWORD thread_id;
    if (fd >= MAX_FDS)
	lose("%d is too high a file descriptor number for us.", fd);
    update_input_available[fd] = CreateEvent(NULL,  /* default security */
					    FALSE, /* auto-reset */
					    TRUE,  /* init value true */
					    NULL); /* unnamed */
    input_available_is_updated[fd] 
	= CreateEvent(NULL,  /* default security */
		      FALSE, /* auto-reset */
		      FALSE,  /* init value false */
		      NULL); /* unnamed */
    fd_threads[fd]
	= CreateThread(NULL, 0, /* default security + stack size */
		       (LPTHREAD_START_ROUTINE) input_checker, 
		       (VOID *) fd, /* param */
		       0,  /* default creation flags */
		       &thread_id);
    if (fd_threads[fd] == NULL) 
	lose("Can't create input_checker thread for fd %d", fd);
}
    
static void stop_input_checker (int fd)
{
    TerminateThread(fd_threads[fd], 0);  
    /* 0 is the thread return value -- any value will do */
    CloseHandle(update_input_available[fd]);
    CloseHandle(input_available_is_updated[fd]);
}

static int mindy_open (const char *filename, int flags, int mode)
{
    int fd = open(filename, flags | O_BINARY, mode);
    if (fd != -1) {
	/* You can't directly test O_RDONLY, because
	   it is defined as 0 usually, and you'd have
	   if (flags & 0) ...
	   */
	if (!(flags & O_WRONLY)) {
	    setup_input_checker(fd);
	} else {
	    fd_threads[fd] = NULL;
	}
    }
    return fd;
}

static int mindy_close (int fd)
{
    int res = close(fd);
    if (!res && fd_threads[fd] != NULL) {
	stop_input_checker(fd);
    }
    return res;
}

/* We don't need to synchronize with anything because the only way a 
   false positive can be generated (input_available_array[fd] == TRUE 
   when that really isn't the case) is when someone reads all the
   input without setting input_available_array[fd] to FALSE.  But
   that would mean someone's running this function in parallel
   with mindy_read, which we said isn't allowed.
 */
int input_available (int fd)
{
    return input_available_array[fd];
}

/* Like read(), except it informs check_input.
   Mindy internally uses this for reading from stdin for debugger stuff.
   Won't work if check_input thread isn't running.
   Loading .dbc files bypasses this, because psuedo-threading isn't
   necessary there.
 */
int mindy_read (int fd, char *buffer, int max_chars)
{
    int	res;
    /* We wait for check_input to finish because if we don't,
       we can't safely reset anything. */
    WaitForSingleObject(input_available_is_updated[fd], INFINITE);
    input_available_array[fd] = FALSE;
    res = read(fd, buffer, max_chars);
    if (res < 0 && GetLastError() == ERROR_BROKEN_PIPE) {
	res = 0;  /* treat broken pipes as EOF */
    }
    SetEvent(update_input_available[fd]);
    return res;
}

static void win32_inits (void)
{
    setmode(fileno(stdin), O_BINARY);
    setmode(fileno(stdout), O_BINARY);
    setmode(fileno(stderr), O_BINARY);
    setup_input_checker(0);  /* standard input */
}

/* And now, it's time for fd_exec -- create a child process whose input 
   comes from us and whose output goes to us.
*/

/* This function does file handle black magic.  Here, we create pipes,
   duplicate the ones we're going to pass on to the child process, and
   set the current process's stdin and stdout to be those pipes.  (###
   I'm not sure duplicating them is necessary, but it doesn't hurt...)

   inpipes, outpipes, and old_handles are 2 element arrays.  */
static void pipe_setup (STARTUPINFO *siStartInfo, int inpipes[], 
			int outpipes[], HANDLE old_handles[])
{
  const int pipe_size = 2000;
  HANDLE new_stdin, new_stdout;
  HANDLE parent = GetCurrentProcess();

  /* Create new file handles--in binary mode.
     _pipe sticks the read then the write handle in {in,out}pipes, and
     returns 0 on success and -1 on failure */
  if (_pipe(inpipes, pipe_size, O_BINARY) != 0
      || _pipe(outpipes, pipe_size, O_BINARY) != 0

      /* Duplicate the stdin and stdout handles.  False on failure. */
      || !DuplicateHandle(parent, /* source process */
			  /* next, handle to dup */
			  (HANDLE) _get_osfhandle(inpipes[0]),
			  parent,  /* Proc to give new handles to */
			  &new_stdin, /* Where new handle is stored */
			  0,  /* Parameter ignored */
			  TRUE, /* Make new handle inheritable */
			  DUPLICATE_SAME_ACCESS)
      || !DuplicateHandle(parent,  /* source process */
			  /* next, handle to dup */
			  (HANDLE)_get_osfhandle(outpipes[1]),
			  parent,   /* Proc to give new handles to */
			  &new_stdout,  /* Where new handle is stored */
			  0,  /* Parameter ignored */
			  TRUE, /* Make new handle inheritable */
			  DUPLICATE_SAME_ACCESS)) {
    lose("Failed while doing pipe stuff for fd_exec");
  }

  /* Save the old stdin and stdout handles to some place we can remember */
  old_handles[0] = GetStdHandle(STD_INPUT_HANDLE);
  old_handles[1] = GetStdHandle(STD_OUTPUT_HANDLE);

  /* Set stdin and stdout to the new handles */
  if (!SetStdHandle(STD_INPUT_HANDLE, new_stdin)
      || !SetStdHandle(STD_OUTPUT_HANDLE, new_stdout)) {
    lose("Failed while doing pipe stuff for fd_exec");
  }

  /* Now tell the StartInfo to use the handles we just created.  By
     default, child processes don't inherit the stdin and stdout of
     their parents. */
  siStartInfo->dwFlags = STARTF_USESTDHANDLES;
  siStartInfo->hStdInput = new_stdin;
  siStartInfo->hStdOutput = new_stdout;

  /* nothing funny with stderr, but we still have to initialize 
     the field anyway */
  siStartInfo->hStdError = GetStdHandle(STD_ERROR_HANDLE);
}

/* Here we undo the hackery in the setup, and close any handles we
   know fd_exec doesn't use.
   */
static void pipe_cleanup (int inpipes[], int outpipes[], HANDLE old_handles[])
{
  /* Close unnecessary fd's--the ones the child uses */
  if (close(inpipes[0]) != 0
      || close(outpipes[1]) != 0

  /* close the handles we're pretending are our stdin and stdout */
      || !CloseHandle(GetStdHandle(STD_INPUT_HANDLE))
      || !CloseHandle(GetStdHandle(STD_OUTPUT_HANDLE))

  /* now restore the real stdin and stdout */
      || !SetStdHandle(STD_INPUT_HANDLE, old_handles[0])
      || !SetStdHandle(STD_OUTPUT_HANDLE, old_handles[1])) {
    lose("Failed while doing pipe cleanup for fd_exec");
  }
}

static void fd_exec(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *oldargs;

    oldargs = args - 1;
    thread->sp = args + 1;

    {
	PROCESS_INFORMATION piProcInfo;
	STARTUPINFO siStartInfo;
	SECURITY_ATTRIBUTES saAttr;
	int inpipes[2], outpipes[2];
	HANDLE old_handles[2];
        char *command_line = (char *) string_chars(args[0]);

	siStartInfo.cb = sizeof(STARTUPINFO);
	siStartInfo.lpReserved = NULL;
	siStartInfo.lpReserved2 = NULL;
	siStartInfo.cbReserved2 = 0;
	siStartInfo.lpDesktop = NULL;
        /* pipe_setup initializes the rest of siStartInfo */

	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;

	pipe_setup(&siStartInfo, inpipes, outpipes, old_handles);

	if (! CreateProcess(NULL, command_line, NULL, NULL, TRUE, 0,
	                    NULL, NULL, &siStartInfo, &piProcInfo)) {
	    DWORD debug_info = GetLastError();
	    oldargs[0] = obj_False;
	    oldargs[1] = obj_False;
	} else {
	    oldargs[0] = make_fixnum(inpipes[1]);  /* fd we can write to */
	    oldargs[1] = make_fixnum(outpipes[0]); /* fd we can read from */
	    setup_input_checker(outpipes[0]);
	}

	pipe_cleanup(inpipes, outpipes, old_handles);
    } 
    do_return(thread, oldargs, oldargs);
}

#else
#	ifdef MACOS

static int mindy_open (const char *filename, int flags, int mode)
{
    return MacOpen(filename, flags, mode);	/* From rob's MacTextFileIO.c */
}

static int mindy_close (int fd)
{
    return MacClose(fd);
}

int input_available(int fd)
{
    fd_set fds;
    struct timeval tv;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    return select(fd+1, &fds, NULL, NULL, &tv);
}

int mindy_read (int fd, char *buffer, int max_chars)
{
	int length;
	
    length = MacRead(fd, buffer, max_chars);						/* Read raw data. fread would translate linefeeds but doesn't return!!! */
		
    return length;
}

static void fd_exec(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *oldargs;

    oldargs = args - 1;
    thread->sp = args + 1;

    /* Mac doesn't support forking or piping */

	oldargs[0] = obj_False;
	oldargs[1] = obj_False;
	
    do_return(thread, oldargs, oldargs);
}

#	else
/* Not WIN32 or MACOS -- it's assumed this means Unix */

static int mindy_open (const char *filename, int flags, int mode)
{
    return open(filename, flags_for(flags), mode);
}

static int mindy_close (int fd)
{
    return close(fd);
}

int input_available(int fd)
{
    fd_set fds;
    struct timeval tv;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    return select(fd+1, &fds, NULL, NULL, &tv);
}

int mindy_read (int fd, char *buffer, int max_chars)
{
    return read(fd, buffer, max_chars);
}

static void fd_exec(obj_t self, struct thread *thread, obj_t *args)
{
    int inpipes[2], outpipes[2], forkresult;
    obj_t *oldargs;

    oldargs = args - 1;
    thread->sp = args + 1;

    /* ### Collect some zombie processes before we launch a new
       process.  Ideally, we'd collect them in a more orderly fashion,
       but this will do for now. */
    while (waitpid(-1, NULL, WNOHANG) > 0)
	;

    if (pipe(inpipes) >= 0 && pipe(outpipes) >= 0 &&
	(forkresult = fork()) != -1)
    {
	if (forkresult == 0) {
	    /* This process is going to exit shortly, so we needn't be too
	       careful about malloc behavior, nor about the fact that we
	       destructively modify the command string. */
	    char *command = (char *)string_chars(args[0]);
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

	    /* Put the child in its own session so that signals don't hit it */
	    setsid();
	    
	    execvp(args[0], args);
	    /* If we get here, execvp failed, so shut down as 
	     * gracefully as we can 
	     */
	    exit(1);
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
#	endif
#endif
/* End Unix-specific */
/* End OS-specific stuff */


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

    results(thread, args-1, mindy_close(fixnum_value(fd)), obj_True);
}

static obj_t fd_error_str(obj_t xerrno)
{
  return make_byte_string(strerror(fixnum_value(xerrno)));
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
    int res;

    res = mindy_open((char *)string_chars(path), fixnum_value(flags),
	             0666);

    results(thread, args-1, res, make_fixnum(res));
}

static void maybe_read(struct thread *thread)
{
    obj_t *fp = thread->fp;
    int fd = fixnum_value(fp[-9]);
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
	res = mindy_read(fd,
		         (char *)(buffer_data(fp[-8]) + fixnum_value(fp[-7])),
		         fixnum_value(fp[-6]));
	
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

    if ((res < 0 && errno == EINVAL)
	/* EINVAL means the fd is a socket, not a file descriptor.  We don't */
	/* care that you can't fsync sockets. */
#ifdef WIN32
 	|| (res < 0 && errno == EBADF)
	/* In Windows, EBADF means that the fd is a descriptor for
           the console. */
#endif
		)
	results(thread, args-1, 0, obj_True);
    else
	results(thread, args-1, res, obj_True);
}

/* The output version of input_available
 */
int output_writable(int fd)
{
    fd_set fds;
    struct timeval tv;

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
#ifdef WIN32
    if (isatty(fd)) {
        return 1;   /* You can always output to a tty */
    } else {
        int select_result = select(fd+1, NULL, &fds, NULL, &tv);
	if (select_result < 0) {
	    /* fd is a file rather than a socket, so we can write to 
	       it without blocking */
            return 1;
	} else {
	    return select_result;
	}
    }
#else
    return select(fd+1, NULL, &fds, NULL, &tv);
#endif        
}


static void maybe_write(struct thread *thread)
{
    obj_t *fp = thread->fp;
    int fd = fixnum_value(fp[-9]);
    int nfound, res;
    obj_t *old_sp;

    nfound = output_writable(fd);
    if (nfound < 0) {
	if (errno != EINTR) {
	    old_sp = pop_linkage(thread);
	    thread->sp = old_sp + 2;
	    old_sp[0] = obj_False;
	    old_sp[1] = make_fixnum(errno);
	    do_return(thread, old_sp, old_sp);
	} else {
	    wait_for_output(thread, fd, maybe_write);
	}
    } else if (nfound == 0)
	wait_for_output(thread, fd, maybe_write);
    else {
#ifdef MACOS
		res = MacWrite(fd,   
#else    
		res = write(fd,
#endif
		    buffer_data(fp[-8]) + fixnum_value(fp[-7]),
		    fixnum_value(fp[-6]));
		results(thread, pop_linkage(thread), res, make_fixnum(res));
    }
}

static void fd_write(obj_t self, struct thread *thread, obj_t *args)
{
    thread->sp = args + 4;
    push_linkage(thread, args);
    maybe_write(thread);
}


/* file-write-date */

static obj_t file_write_date(obj_t path)
{
    struct stat buf;

    if (stat((char *)string_chars(path), &buf) < 0)
	return obj_False;
    else
	return make_fixnum(buf.st_mtime);
}


/* Init stuff. */

void init_fd_functions(void)
{
    define_constant("fd-close",
		    make_raw_method("fd-close", list1(obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, fd_close));
    define_method("fd-error-string", list1(obj_FixnumClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, fd_error_str);
    define_constant("fd-input-available?",
		    make_raw_method("fd-input-available?",
				    list1(obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, fd_input_available));
    define_constant("fd-open",
		    make_raw_method("fd-open",
				    list2(obj_ByteStringClass,
					  obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_open));
    define_constant("fd-read",
		    make_raw_method("fd-read",
				    listn(4, obj_FixnumClass,
					  obj_BufferClass,
					  obj_FixnumClass,
					  obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_read));
    define_constant("fd-seek",
		    make_raw_method("fd-seek",
				    list3(obj_FixnumClass,
					  obj_FixnumClass,
					  obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_seek));
    define_constant("fd-sync-output",
		    make_raw_method("fd-sync-output",
				    list1(obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_BooleanClass, obj_ObjectClass),
				    obj_False, fd_sync_output));
    define_constant("fd-write",
		    make_raw_method("fd-write",
				    listn(4, obj_FixnumClass,
					  obj_BufferClass,
					  obj_FixnumClass,
					  obj_FixnumClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_write));
    define_constant("fd-exec",
		    make_raw_method("fd-exec",
				    list1(obj_ByteStringClass),
				    FALSE, obj_False, FALSE,
				    list2(obj_ObjectClass, obj_ObjectClass),
				    obj_False, fd_exec));
    define_function("file-write-date", list1(obj_ByteStringClass), FALSE,
		    obj_False, FALSE, obj_ObjectClass, file_write_date);

    define_constant("SEEK_SET", make_fixnum(SEEK_SET));
    define_constant("SEEK_CUR", make_fixnum(SEEK_CUR));
    define_constant("SEEK_END", make_fixnum(SEEK_END));
    define_constant("O_RDONLY", make_fixnum(O_RDONLY));
    define_constant("O_WRONLY", make_fixnum(O_WRONLY));
    define_constant("O_RDWR", make_fixnum(O_RDWR));
    define_constant("O_APPEND", make_fixnum(O_APPEND));
    define_constant("O_CREAT", make_fixnum(O_CREAT));
    define_constant("O_EXCL", make_fixnum(O_EXCL));
    define_constant("O_TRUNC", make_fixnum(O_TRUNC));
#ifndef WIN32
#ifndef MACOS
    define_constant("O_NONBLOCK", make_fixnum(O_NONBLOCK));
#endif
#endif

    /* This compendium of error numbers comes from Tcl. */
#ifdef E2BIG
    define_constant("E2BIG", make_fixnum(E2BIG)); 
#endif
#ifdef EACCES
    define_constant("EACCES", make_fixnum(EACCES)); 
#endif
#ifdef EADDRINUSE
    define_constant("EADDRINUSE", make_fixnum(EADDRINUSE)); 
#endif
#ifdef EADDRNOTAVAIL
    define_constant("EADDRNOTAVAIL", make_fixnum(EADDRNOTAVAIL)); 
#endif
#ifdef EADV
    define_constant("EADV", make_fixnum(EADV)); 
#endif
#ifdef EAFNOSUPPORT
    define_constant("EAFNOSUPPORT", make_fixnum(EAFNOSUPPORT)); 
#endif
#ifdef EAGAIN
    define_constant("EAGAIN", make_fixnum(EAGAIN)); 
#endif
#ifdef EALIGN
    define_constant("EALIGN", make_fixnum(EALIGN)); 
#endif
#ifdef EALREADY
    define_constant("EALREADY", make_fixnum(EALREADY)); 
#endif
#ifdef EBADE
    define_constant("EBADE", make_fixnum(EBADE)); 
#endif
#ifdef EBADF
    define_constant("EBADF", make_fixnum(EBADF)); 
#endif
#ifdef EBADFD
    define_constant("EBADFD", make_fixnum(EBADFD)); 
#endif
#ifdef EBADMSG
    define_constant("EBADMSG", make_fixnum(EBADMSG)); 
#endif
#ifdef EBADR
    define_constant("EBADR", make_fixnum(EBADR)); 
#endif
#ifdef EBADRPC
    define_constant("EBADRPC", make_fixnum(EBADRPC)); 
#endif
#ifdef EBADRQC
    define_constant("EBADRQC", make_fixnum(EBADRQC)); 
#endif
#ifdef EBADSLT
    define_constant("EBADSLT", make_fixnum(EBADSLT)); 
#endif
#ifdef EBFONT
    define_constant("EBFONT", make_fixnum(EBFONT)); 
#endif
#ifdef EBUSY
    define_constant("EBUSY", make_fixnum(EBUSY)); 
#endif
#ifdef ECHILD
    define_constant("ECHILD", make_fixnum(ECHILD)); 
#endif
#ifdef ECHRNG
    define_constant("ECHRNG", make_fixnum(ECHRNG)); 
#endif
#ifdef ECOMM
    define_constant("ECOMM", make_fixnum(ECOMM)); 
#endif
#ifdef ECONNABORTED
    define_constant("ECONNABORTED", make_fixnum(ECONNABORTED)); 
#endif
#ifdef ECONNREFUSED
    define_constant("ECONNREFUSED", make_fixnum(ECONNREFUSED)); 
#endif
#ifdef ECONNRESET
    define_constant("ECONNRESET", make_fixnum(ECONNRESET)); 
#endif
#if defined(EDEADLK) && (!defined(EWOULDBLOCK) || (EDEADLK != EWOULDBLOCK))
    define_constant("EDEADLK", make_fixnum(EDEADLK)); 
#endif
#ifdef EDEADLOCK
    define_constant("EDEADLOCK", make_fixnum(EDEADLOCK)); 
#endif
#ifdef EDESTADDRREQ
    define_constant("EDESTADDRREQ", make_fixnum(EDESTADDRREQ)); 
#endif
#ifdef EDIRTY
    define_constant("EDIRTY", make_fixnum(EDIRTY)); 
#endif
#ifdef EDOM
    define_constant("EDOM", make_fixnum(EDOM)); 
#endif
#ifdef EDOTDOT
    define_constant("EDOTDOT", make_fixnum(EDOTDOT)); 
#endif
#ifdef EDQUOT
    define_constant("EDQUOT", make_fixnum(EDQUOT)); 
#endif
#ifdef EDUPPKG
    define_constant("EDUPPKG", make_fixnum(EDUPPKG)); 
#endif
#ifdef EEXIST
    define_constant("EEXIST", make_fixnum(EEXIST)); 
#endif
#ifdef EFAULT
    define_constant("EFAULT", make_fixnum(EFAULT)); 
#endif
#ifdef EFBIG
    define_constant("EFBIG", make_fixnum(EFBIG)); 
#endif
#ifdef EHOSTDOWN
    define_constant("EHOSTDOWN", make_fixnum(EHOSTDOWN)); 
#endif
#ifdef EHOSTUNREACH
    define_constant("EHOSTUNREACH", make_fixnum(EHOSTUNREACH)); 
#endif
#ifdef EIDRM
    define_constant("EIDRM", make_fixnum(EIDRM)); 
#endif
#ifdef EINIT
    define_constant("EINIT", make_fixnum(EINIT)); 
#endif
#ifdef EINPROGRESS
    define_constant("EINPROGRESS", make_fixnum(EINPROGRESS)); 
#endif
#ifdef EINTR
    define_constant("EINTR", make_fixnum(EINTR)); 
#endif
#ifdef EINVAL
    define_constant("EINVAL", make_fixnum(EINVAL)); 
#endif
#ifdef EIO
    define_constant("EIO", make_fixnum(EIO)); 
#endif
#ifdef EISCONN
    define_constant("EISCONN", make_fixnum(EISCONN)); 
#endif
#ifdef EISDIR
    define_constant("EISDIR", make_fixnum(EISDIR)); 
#endif
#ifdef EISNAME
    define_constant("EISNAM", make_fixnum(EISNAM)); 
#endif
#ifdef ELBIN
    define_constant("ELBIN", make_fixnum(ELBIN)); 
#endif
#ifdef EL2HLT
    define_constant("EL2HLT", make_fixnum(EL2HLT)); 
#endif
#ifdef EL2NSYNC
    define_constant("EL2NSYNC", make_fixnum(EL2NSYNC)); 
#endif
#ifdef EL3HLT
    define_constant("EL3HLT", make_fixnum(EL3HLT)); 
#endif
#ifdef EL3RST
    define_constant("EL3RST", make_fixnum(EL3RST)); 
#endif
#ifdef ELIBACC
    define_constant("ELIBACC", make_fixnum(ELIBACC)); 
#endif
#ifdef ELIBBAD
    define_constant("ELIBBAD", make_fixnum(ELIBBAD)); 
#endif
#ifdef ELIBEXEC
    define_constant("ELIBEXEC", make_fixnum(ELIBEXEC)); 
#endif
#ifdef ELIBMAX
    define_constant("ELIBMAX", make_fixnum(ELIBMAX)); 
#endif
#ifdef ELIBSCN
    define_constant("ELIBSCN", make_fixnum(ELIBSCN)); 
#endif
#ifdef ELNRNG
    define_constant("ELNRNG", make_fixnum(ELNRNG)); 
#endif
#ifdef ELOOP
    define_constant("ELOOP", make_fixnum(ELOOP)); 
#endif
#ifdef EMFILE
    define_constant("EMFILE", make_fixnum(EMFILE)); 
#endif
#ifdef EMLINK
    define_constant("EMLINK", make_fixnum(EMLINK)); 
#endif
#ifdef EMSGSIZE
    define_constant("EMSGSIZE", make_fixnum(EMSGSIZE)); 
#endif
#ifdef EMULTIHOP
    define_constant("EMULTIHOP", make_fixnum(EMULTIHOP)); 
#endif
#ifdef ENAMETOOLONG
    define_constant("ENAMETOOLONG", make_fixnum(ENAMETOOLONG)); 
#endif
#ifdef ENAVAIL
    define_constant("ENAVAIL", make_fixnum(ENAVAIL)); 
#endif
#ifdef ENET
    define_constant("ENET", make_fixnum(ENET)); 
#endif
#ifdef ENETDOWN
    define_constant("ENETDOWN", make_fixnum(ENETDOWN)); 
#endif
#ifdef ENETRESET
    define_constant("ENETRESET", make_fixnum(ENETRESET)); 
#endif
#ifdef ENETUNREACH
    define_constant("ENETUNREACH", make_fixnum(ENETUNREACH)); 
#endif
#ifdef ENFILE
    define_constant("ENFILE", make_fixnum(ENFILE)); 
#endif
#ifdef ENOANO
    define_constant("ENOANO", make_fixnum(ENOANO)); 
#endif
#if defined(ENOBUFS) && (!defined(ENOSR) || (ENOBUFS != ENOSR))
    define_constant("ENOBUFS", make_fixnum(ENOBUFS)); 
#endif
#ifdef ENOCSI
    define_constant("ENOCSI", make_fixnum(ENOCSI)); 
#endif
#ifdef ENODATA
    define_constant("ENODATA", make_fixnum(ENODATA)); 
#endif
#ifdef ENODEV
    define_constant("ENODEV", make_fixnum(ENODEV)); 
#endif
#ifdef ENOENT
    define_constant("ENOENT", make_fixnum(ENOENT)); 
#endif
#ifdef ENOEXEC
    define_constant("ENOEXEC", make_fixnum(ENOEXEC)); 
#endif
#ifdef ENOLCK
    define_constant("ENOLCK", make_fixnum(ENOLCK)); 
#endif
#ifdef ENOLINK
    define_constant("ENOLINK", make_fixnum(ENOLINK)); 
#endif
#ifdef ENOMEM
    define_constant("ENOMEM", make_fixnum(ENOMEM)); 
#endif
#ifdef ENOMSG
    define_constant("ENOMSG", make_fixnum(ENOMSG)); 
#endif
#ifdef ENONET
    define_constant("ENONET", make_fixnum(ENONET)); 
#endif
#ifdef ENOPKG
    define_constant("ENOPKG", make_fixnum(ENOPKG)); 
#endif
#ifdef ENOPROTOOPT
    define_constant("ENOPROTOOPT", make_fixnum(ENOPROTOOPT)); 
#endif
#ifdef ENOSPC
    define_constant("ENOSPC", make_fixnum(ENOSPC)); 
#endif
#ifdef ENOSR
    define_constant("ENOSR", make_fixnum(ENOSR)); 
#endif
#if defined(ENOSTR) && (!defined(ENOTTY) || (ENOTTY != ENOSTR))
    define_constant("ENOSTR", make_fixnum(ENOSTR)); 
#endif
#ifdef ENOSYM
    define_constant("ENOSYM", make_fixnum(ENOSYM)); 
#endif
#ifdef ENOSYS
    define_constant("ENOSYS", make_fixnum(ENOSYS)); 
#endif
#ifdef ENOTBLK
    define_constant("ENOTBLK", make_fixnum(ENOTBLK)); 
#endif
#ifdef ENOTCONN
    define_constant("ENOTCONN", make_fixnum(ENOTCONN)); 
#endif
#ifdef ENOTDIR
    define_constant("ENOTDIR", make_fixnum(ENOTDIR)); 
#endif
#if defined(ENOTEMPTY) && (!defined(EEXIST) || (ENOTEMPTY != EEXIST))
    define_constant("ENOTEMPTY", make_fixnum(ENOTEMPTY)); 
#endif
#ifdef ENOTNAM
    define_constant("ENOTNAM", make_fixnum(ENOTNAM)); 
#endif
#ifdef ENOTSOCK
    define_constant("ENOTSOCK", make_fixnum(ENOTSOCK)); 
#endif
#ifdef ENOTTY
    define_constant("ENOTTY", make_fixnum(ENOTTY)); 
#endif
#ifdef ENOTUNIQ
    define_constant("ENOTUNIQ", make_fixnum(ENOTUNIQ)); 
#endif
#ifdef ENXIO
    define_constant("ENXIO", make_fixnum(ENXIO)); 
#endif
#ifdef EOPNOTSUPP
    define_constant("EOPNOTSUPP", make_fixnum(EOPNOTSUPP)); 
#endif
#ifdef EPERM
    define_constant("EPERM", make_fixnum(EPERM)); 
#endif
#ifdef EPFNOSUPPORT
    define_constant("EPFNOSUPPORT", make_fixnum(EPFNOSUPPORT)); 
#endif
#ifdef EPIPE
    define_constant("EPIPE", make_fixnum(EPIPE)); 
#endif
#ifdef EPROCLIM
    define_constant("EPROCLIM", make_fixnum(EPROCLIM)); 
#endif
#ifdef EPROCUNAVAIL
    define_constant("EPROCUNAVAIL", make_fixnum(EPROCUNAVAIL)); 
#endif
#ifdef EPROGMISMATCH
    define_constant("EPROGMISMATCH", make_fixnum(EPROGMISMATCH)); 
#endif
#ifdef EPROGUNAVAIL
    define_constant("EPROGUNAVAIL", make_fixnum(EPROGUNAVAIL)); 
#endif
#ifdef EPROTO
    define_constant("EPROTO", make_fixnum(EPROTO)); 
#endif
#ifdef EPROTONOSUPPORT
    define_constant("EPROTONOSUPPORT", make_fixnum(EPROTONOSUPPORT)); 
#endif
#ifdef EPROTOTYPE
    define_constant("EPROTOTYPE", make_fixnum(EPROTOTYPE)); 
#endif
#ifdef ERANGE
    define_constant("ERANGE", make_fixnum(ERANGE)); 
#endif
#if defined(EREFUSED) && (!defined(ECONNREFUSED) || (EREFUSED != ECONNREFUSED))
    define_constant("EREFUSED", make_fixnum(EREFUSED)); 
#endif
#ifdef EREMCHG
    define_constant("EREMCHG", make_fixnum(EREMCHG)); 
#endif
#ifdef EREMDEV
    define_constant("EREMDEV", make_fixnum(EREMDEV)); 
#endif
#ifdef EREMOTE
    define_constant("EREMOTE", make_fixnum(EREMOTE)); 
#endif
#ifdef EREMOTEIO
    define_constant("EREMOTEIO", make_fixnum(EREMOTEIO)); 
#endif
#ifdef EREMOTERELEASE
    define_constant("EREMOTERELEASE", make_fixnum(EREMOTERELEASE)); 
#endif
#ifdef EROFS
    define_constant("EROFS", make_fixnum(EROFS)); 
#endif
#ifdef ERPCMISMATCH
    define_constant("ERPCMISMATCH", make_fixnum(ERPCMISMATCH)); 
#endif
#ifdef ERREMOTE
    define_constant("ERREMOTE", make_fixnum(ERREMOTE)); 
#endif
#ifdef ESHUTDOWN
    define_constant("ESHUTDOWN", make_fixnum(ESHUTDOWN)); 
#endif
#ifdef ESOCKTNOSUPPORT
    define_constant("ESOCKTNOSUPPORT", make_fixnum(ESOCKTNOSUPPORT)); 
#endif
#ifdef ESPIPE
    define_constant("ESPIPE", make_fixnum(ESPIPE)); 
#endif
#ifdef ESRCH
    define_constant("ESRCH", make_fixnum(ESRCH)); 
#endif
#ifdef ESRMNT
    define_constant("ESRMNT", make_fixnum(ESRMNT)); 
#endif
#ifdef ESTALE
    define_constant("ESTALE", make_fixnum(ESTALE)); 
#endif
#ifdef ESUCCESS
    define_constant("ESUCCESS", make_fixnum(ESUCCESS)); 
#endif
#ifdef ETIME
    define_constant("ETIME", make_fixnum(ETIME)); 
#endif
#ifdef ETIMEDOUT
    define_constant("ETIMEDOUT", make_fixnum(ETIMEDOUT)); 
#endif
#ifdef ETOOMANYREFS
    define_constant("ETOOMANYREFS", make_fixnum(ETOOMANYREFS)); 
#endif
#ifdef ETXTBSY
    define_constant("ETXTBSY", make_fixnum(ETXTBSY)); 
#endif
#ifdef EUCLEAN
    define_constant("EUCLEAN", make_fixnum(EUCLEAN)); 
#endif
#ifdef EUNATCH
    define_constant("EUNATCH", make_fixnum(EUNATCH)); 
#endif
#ifdef EUSERS
    define_constant("EUSERS", make_fixnum(EUSERS)); 
#endif
#ifdef EVERSION
    define_constant("EVERSION", make_fixnum(EVERSION)); 
#endif
#if defined(EWOULDBLOCK) && (!defined(EAGAIN) || (EWOULDBLOCK != EAGAIN))
    define_constant("EWOULDBLOCK", make_fixnum(EWOULDBLOCK)); 
#endif
#ifdef EXDEV
    define_constant("EXDEV", make_fixnum(EXDEV)); 
#endif
#ifdef EXFULL
    define_constant("EXFULL", make_fixnum(EXFULL)); 
#endif

#ifdef WIN32
    win32_inits();
#endif

#if 0
#ifdef WIN32
    if (isatty(0)) {   /* If stdin is a tty and not redirected */
    	stdin_buffer_empty     = CreateEvent(NULL, TRUE, TRUE, NULL);
	stdin_buffer_not_empty = CreateEvent(NULL, TRUE, FALSE, NULL);
	       /* These are nameless "manual reset" events */
	InitializeCriticalSection(&stdin_buffer_mutex);
	{
	    DWORD thread_id;
	    HANDLE thread_handle;
	    thread_handle 
		= CreateThread(NULL, 0, 
			       (LPTHREAD_START_ROUTINE) stdin_producer, 
			       NULL, 0, &thread_id);
	    if (thread_handle == NULL) 
		lose("Can't create stdin_producer thread");
	}

    }
#endif
#endif
}
