/* This file implements mutant_fd_exec().  Something similar to this
   will probably be needed on all Unix machines, but this particular
   file is probably HP specific 
   */

#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <sys/ptyio.h>
#include <errno.h>
#include <hp-dig-support.h>

int process_id = -1;
int master_pty = -1;

/* pass the interrupt on to the process on the other side of the pty
 */
void handle_interrupt (int sig, long code, struct sigcontext *scp)
{
  signal(sig, handle_interrupt);
  ioctl(master_pty, TIOCSIGSEND, SIGINT);
  scp->sc_syscall_action = SIG_RESTART;
  return;
}

          
/* Function to run an arbitrary program, returning file descriptors
   for the program's stdin and stdout.  Does weird things to make sure
   control-C is seen by both the parent and the child process (and
   also sets up an interrupt handler for the parent so that control-C
   is ignored) 
 */
void mutant_fd_exec(char *command, int *toprog, int *fromprog)
{
  int pty = -1, slave_fd, slave_pid, i;
  char master[20], *slave;
  int forkresult, pgrp;

  for (i = 0; pty == -1 && i < 16; i++) {
    sprintf(master, "/dev/ptym/ptyp%1.1x", i);
    pty = open(master, O_RDWR);
  }
  if (pty == -1) {
    *toprog = -1;
    *fromprog = -1;
    return;
  }
  master_pty = pty;
  slave = ptsname(pty);

  if ((forkresult = fork()) != -1) {
      
    /*        if (forkresult != 0) {*/
    if (forkresult == 0) {
      /* This process is going to exit shortly, so we needn't be too
	 careful about malloc behavior, nor about the fact that we
	 destructively modify the command string. */
      char *p, **args;
      int argcounter = 1;
      
      setsid();
      for (p = command; *p != 0; p++)
	if (*p == ' ') {
	  argcounter++;
	  while (*(++p) == ' ');
	}
      args = (char **) GC_malloc((argcounter+1) * sizeof(char *));
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
      open(slave, O_RDONLY);
      close(1);
      open(slave, O_WRONLY);
      close(2);
      open(slave, O_WRONLY);

      execvp(args[0], args);
      /* If we get here, execvp failed, so shut down as 
             * gracefully as we can 
             */
      exit(1);
    }

    /*	printf("controlling process = %d\n", tcgetpgrp(0));
	printf("new process group = %d\n", setsid());*/
    /*    fflush(stdout);*/

    signal(SIGINT, handle_interrupt);
    /*signal(SIGINT, SIG_IGN);*/
    *toprog = pty;
    *fromprog = pty;
  } else {
    *toprog = -1;
    *fromprog = -1;
  }
}
