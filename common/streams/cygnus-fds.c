/* modifications to win32-fds.c to work with cygnus by
 * Douglas M. Auclair, dauclair@hotmail.com */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <io.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>

#define MAX_FDS FD_SETSIZE

int fd_open (const char *filename, int flags, int mode)
{
    int fd = open(filename, flags | O_BINARY, mode);
    return fd;
}

int fd_close (int fd)
{
    int res = close(fd);
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

        return select_result;
}

int fd_read (int fd, char *buffer, int max_chars)
{
  int res;
  res = read(fd, buffer, max_chars);
  if (res < 0) {
    res = 0;  /* treat broken pipes as EOF */
  }
  return res;
}

/* I use the unix fd_exec instead of the windows one. */
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

/* The commented out code below is for
 * compatibility's sake to Function-Developer ... I'm just not
 * too sure about its utility, and it also causes a great deal
 * of heart-ache -- it's gone.  Doug Auclair, dauclair@hotmail.com */
void streams_fd_init (void) {
#ifdef WHY_WOULD_WE_EVER_WANT_THIS
    setmode(fileno(stdin), O_BINARY);
    setmode(fileno(stdout), O_BINARY);
    setmode(fileno(stderr), O_BINARY);
#endif
}

#ifdef TEST_CYGNUS_FDS
/* tests to see if I've got all my includes correct */
int main() { return 0; }
#endif
