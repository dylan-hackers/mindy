/* This file contains Mac versions of d2c_open, fd_read, fd_close,
   and fd_exec.  In Unix, these are pretty straightforward, but in
   MS-Windows {95|NT} these are pretty disgusting.  This is all
   adapted from Mindy, but beware--it is adapted!  
 */

#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#ifdef __linux__
  #include <sys/time.h> /* We need this to get struct timeval... */
#endif /* ifdef linux */
#ifdef __FreeBSD__
  #include <sys/time.h>
#endif
//#include <sys/wait.h>

#if defined(__BEOS__)
// For select, etc.
#include <be/net/socket.h>
#include <unistd.h>
#endif


int fd_open (const char *filename, int flags, int mode)
{
    return MacOpen(filename, flags, mode);
}

int fd_close (int fd)
{
    return MacClose(fd);
}

int fd_input_available(int fd)
{
    return 1;
}

int fd_read (int fd, char *buffer, int max_chars)
{
    return MacRead(fd, buffer, max_chars);
}

// Override UNIX write to make sure linefeed conversion occurs
int fd_write (int fd, char *buffer, int num_chars)
{
    return MacWrite(fd, buffer, num_chars);
}

void fd_exec(char *command, int *toprog, int *fromprog)
{
	printf( "mac-fds.c:fd_exec: Mac doesn't fork or pipe, sorry\n" );
	*toprog = -1;
	*fromprog = -1;
}


void streams_fd_init (void) {
    /* Does nothing */
}
