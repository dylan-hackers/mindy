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

#ifndef MIT_PTHREADS
  /* None of these seem to work under threads, and the one that works best in
  /* a non-threaded world active breaks under threads. */
  ioctl(master_pty, TIOCSIGSEND, SIGINT);
  /*kill(process_id, sig);*/
  /*write(master_pty, "\003", 1);*/
#endif

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
  /*char master[20], *slave;*/
  char master[20], slave[20];
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
  /* We don't use ptsname because it clashes with the threads library */
  sprintf(slave, "/dev/pty/t%s", master + 11);
  slave_fd = open(slave, O_RDWR);


  if ((forkresult = fork()) != -1) {
      
    if (forkresult == 0) {
      /* This process is going to exit shortly, so we needn't be too
	 careful about malloc behavior, nor about the fact that we
	 destructively modify the command string. */
      char *p, *cmd, **args;
      int argcounter = 1;
      char *fork_args[3];
      FILE *tstfile;

      setsid();

      cmd = (char *)malloc(strlen(command) + 1);
      strcpy(cmd, command);
      for (p = cmd; *p != 0; p++)
	if (*p == ' ') {
	  argcounter++;
	  while (*(++p) == ' ');
	}
      args = (char **) malloc((argcounter+1) * sizeof(char *));
      args[0] = cmd;
      for (p = cmd, argcounter = 1; *p != 0; p++) {
	if (*p == ' ') {
	  *p = 0;
	  while (*(++p) == ' ');
	  if (*p != 0)
	    args[argcounter++] = p;
	}
      }
      args[argcounter] = 0;

      dup2(slave_fd, 0);
      dup2(slave_fd, 1);
      dup2(slave_fd, 2);

      /* Hack: By opening a terminal device after doing the setsid, we avoid
      /* problems with lack of a controlling terminal.  We ought to be able to
      /* use the result of this open in out dup2 calls, but they don't seem to
      /* work unless the pty was opend *before* the call to fork.  (This is
      /* likely a bad interaction with the threads library.)  The one thing
      /* which can be said in favor of the current scheme is that it seems to
      /* work.  Don't remove this statement without testing the results. */
      slave_fd = open(slave, O_RDWR);

      execvp(args[0], args);
      exit(0);

      /* If we get here, execvp failed, so shut down as 
             * gracefully as we can 
             */
      exit(1);
    }

    *toprog = pty;
    *fromprog = pty;

    process_id = forkresult;
    signal(SIGINT, handle_interrupt);
    fflush(stdout);
  } else {
    *toprog = -1;
    *fromprog = -1;
  }
}
