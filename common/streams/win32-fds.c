/* This file contains Win32 versions of d2c_open, fd_read, fd_close,
   and fd_exec.  In Unix, these are pretty straightforward, but in
   MS-Windows {95|NT} these are pretty disgusting.  This is all
   adapted from Mindy, but beware--it is adapted!

   Some Windows-specific code so we can correctly implement
   input-available? and fd-exec.  Related to the Mindy version of this
   stuff, but not the same.  Here we took an alternate approach, where
   an input-checker thread is only spawned if the user calls
   input-available?.  (And only if the fd is not a socket) Hopefully
   this will result in better performance--less synchronization
   between the reader and the input_checker.

   This code assumes several things.  First of all, there can never be
   more than one thread doing things on a given fd at any one time.
   Second, it is assumed that if fd_input_available? is called, then
   the fd really is open for reading.  Finally, I'm not sure what will
   happen if someone uses lseek on the fd.  */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <io.h>
#include <winsock.h>

#define MAX_FDS FD_SETSIZE

typedef enum {
  thread_killed, thread_still_waiting, thread_finished
} thread_status_t;

static char thread_status[MAX_FDS];
static HANDLE fd_threads[MAX_FDS]; /* thread handles */

/* Tries to read 0 bytes.  As soon as it succeeds, record in
   thread_status that it succeeded, and exit.
   */
static DWORD input_checker (LPDWORD param)
{
    int fd = (int) param;  /* so what if we cast a pointer to an int? */
    HANDLE handle = (HANDLE) _get_osfhandle(fd);
    char small_buffer;  /* Ignore the contents of the next two vars */
    int bytes_read;
    int read_result;
    /* read 0 bytes, block if not available */
    read_result = ReadFile(handle, &small_buffer, 0, &bytes_read, NULL);
    if (!read_result) {
      /* For easier debugging, find out what error */
      DWORD the_error = GetLastError();
      fprintf(stderr, "Read error on fd %d", fd);
      exit(1);
    }
    thread_status[fd] = thread_finished;
    fd_threads[fd] = NULL;
      /* If we don't do this, the file handle may be reused for a different
	 fd, and fd_read() may kill off a thread it didn't mean to */
    return 0;
}

/* Create an input_checker thread, initializing global structures as 
   necessary.
 */
static void spawn_input_checker (int fd)
{
    DWORD thread_id;
    if (fd >= MAX_FDS) {
        fprintf(stderr, "%d is too high a file descriptor for us.", fd);
	exit(1);
    }
    fd_threads[fd]
	= CreateThread(NULL, 0, /* default security + stack size */
		       (LPTHREAD_START_ROUTINE) input_checker, 
		       (VOID *) fd, /* param */
		       0,  /* default creation flags */
		       &thread_id);
    if (fd_threads[fd] == NULL) {
        fprintf(stderr, "Can't create input_checker thread for fd %d", fd);
	exit(1);
    }
    thread_status[fd] = thread_still_waiting;
}
    
/* If the input_checker thread is alive, kill it.
 */
static void kill_checker (int fd)
{
  if (fd_threads[fd] != NULL) {
    /* If thread's already quit, this won't do anything too bad */
    TerminateThread(fd_threads[fd], 0);
    thread_status[fd] = thread_killed;
    fd_threads[fd] = NULL;
  }
}

int fd_open (const char *filename, int flags, int mode)
{
    int fd = open(filename, flags | O_BINARY, mode);
    if (fd != -1) {
        thread_status[fd] = thread_killed;
	fd_threads[fd] = NULL;
    }
    return fd;
}

int fd_close (int fd)
{
    int res = close(fd);
    if (!res)
      kill_checker(fd);
    return res;
}

/* Assume that fd is an input fd
 */
int fd_input_available (int fd)
{
    fd_set fds;
    struct timeval tv;
    int select_result;

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    select_result = select(fd+1, NULL, &fds, NULL, &tv);

    if (select_result >= 0) {
        /* fd is a socket, so select() works */
        return select_result;
    } else {
        /* not a socket; use the input_checker black magic */
      if (thread_status[fd] == thread_still_waiting) {
	return 0;
      } else if (thread_status[fd] == thread_finished) {
	return 1;
      } else if (thread_status[fd] == thread_killed) {
	spawn_input_checker(fd);
	return 0;
      } else {
	fprintf(stderr, "Bad thread status on fd %d\n", fd);
	exit(1);
      }
    }
}

int fd_read (int fd, char *buffer, int max_chars)
{
  kill_checker(fd);
  return read(fd, buffer, max_chars);
}

/* And now, it's time for fd_exec -- create a child process whose input 
   comes from us and whose output goes to us.

   This version of fd_exec is based on Emacs 19.31 ntproc.c.  It is
   more or less unchanged from the Mindy version (it does take
   different parameters, though)

   prepare_standard_handles and reset_standard_handles are ripped straight 
   out of Emacs.  You give it two file descriptors, it sticks them
   into stdin and stdout, and puts the old values of them into
   handles[].  (The original Emacs version also did stderr.  We don't need
   stderr...)

   Original Emacs comment:

   The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are *not* inheritable, we make them
   stdin, stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the std handles to inheritable duplicates of the ones being passed in.
     (Note that _get_osfhandle() is an io.h procedure that retrieves the
     NT file handle for a crt file descriptor.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Close the std handles passed to the child.
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

static void prepare_standard_handles (int in, int out, HANDLE handles[2])
{
  HANDLE parent;
  HANDLE newstdin, newstdout;

  parent = GetCurrentProcess ();

  handles[0] = GetStdHandle (STD_INPUT_HANDLE);
  handles[1] = GetStdHandle (STD_OUTPUT_HANDLE);

  /* make inheritable copies of the new handles */
  if (!DuplicateHandle (parent, 
		       (HANDLE) _get_osfhandle (in),
		       parent,
		       &newstdin, 
		       0, 
		       TRUE, 
			DUPLICATE_SAME_ACCESS)) {
    fprintf(stderr, "Duplicating input handle for child");
    exit(1);
  }
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (out),
		       parent,
		       &newstdout,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS)) {
    fprintf(stderr, "Duplicating output handle for child");
    exit(1);
  }

  /* and store them as our std handles */
  if (!SetStdHandle (STD_INPUT_HANDLE, newstdin)) {
    fprintf(stderr, "Changing stdin handle");
    exit(1);
  }
 
  if (!SetStdHandle (STD_OUTPUT_HANDLE, newstdout)) {
    fprintf(stderr, "Changing stdout handle");
    exit(1);
  }
}

static void reset_standard_handles (HANDLE handles[2])
{
  /* close the duplicated handles passed to the child */
  CloseHandle(GetStdHandle(STD_INPUT_HANDLE));
  CloseHandle(GetStdHandle(STD_OUTPUT_HANDLE));

  /* now restore parent's saved std handles */
  SetStdHandle(STD_INPUT_HANDLE, handles[0]);
  SetStdHandle(STD_OUTPUT_HANDLE, handles[1]);
}

void fd_exec(char *command_line, int *toprog, int *fromprog)
{
    int inpipes[2], outpipes[2];
    /* This code is a combination of the Unix version of this code
     * and code in the Win32 Programmer's Reference volume 2, 
     * pages 40-45, and code in Emacs.
     */
    PROCESS_INFORMATION piProcInfo;
    STARTUPINFO siStartInfo;
    SECURITY_ATTRIBUTES saAttr;
    HANDLE old_handles[2];
    const int pipe_size = 2000;
    
    /* printf("Starting fd_exec\n"); */
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.lpReserved = NULL;
    siStartInfo.lpReserved2 = NULL;
    siStartInfo.cbReserved2 = 0;
    siStartInfo.lpDesktop = NULL;
    /* more siStartInfo in a bit */
    
    saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    saAttr.bInheritHandle = TRUE;
    saAttr.lpSecurityDescriptor = NULL;
    
    /* And now, file handle black magic */
    
    /* Create new file handles--in binary mode.
       _pipe returns the read then the write handle. */
    _pipe(inpipes, pipe_size, O_BINARY);
    _pipe(outpipes, pipe_size, O_BINARY);

    /* Set up things so the new process will use the file handles we 
       just made */
    prepare_standard_handles(inpipes[0], outpipes[1], old_handles);
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    
    /* nothing funny with stderr, but we still have to initialize 
       the field */
    siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    
    if (! CreateProcess(NULL, command_line, NULL, NULL, TRUE, 0,
                        NULL, NULL, &siStartInfo, &piProcInfo)) {
        *toprog = -1;
        *fromprog = -1;
    } else {
        *toprog = inpipes[1];    /* fd we can write to */
        *fromprog = outpipes[0]; /* fd we can read from */
    }
    
    /* Restore the original stdin and stdout */
    reset_standard_handles(old_handles);
    /* Close unnecessary fd's--the ones the child uses */
    close(inpipes[0]);
    close(outpipes[1]);
}
