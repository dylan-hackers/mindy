/* This file contains Unix versions of d2c_open, fd_read, fd_close,
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
#include <sys/wait.h>

int fd_open (const char *filename, int flags, int mode)
{
    return open(filename, flags, mode);
}

int fd_close (int fd)
{
    return close(fd);
}

int fd_input_available(int fd)
{
    fd_set fds;
    struct timeval tv;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    return select(fd+1, &fds, NULL, NULL, &tv);
}

int fd_read (int fd, char *buffer, int max_chars)
{
    return read(fd, buffer, max_chars);
}

void fd_exec(char *command, int *toprog, int *fromprog)
{
    int inpipes[2], outpipes[2], forkresult;

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
	
	*toprog = inpipes[1];    /* fd we can write to */
	*fromprog = outpipes[0]; /* fd we can read from */
    } else {
        *toprog = -1;
        *fromprog = -1;
    }
}


void streams_fd_init (void) {
    /* Does nothing */
}
