#include <signal.h>
#include <stdio.h>
#include <errno.h>

/* Function to run an arbitrary program, returning file descriptors for the
   program's stdin and stdout. */
void fd_exec(char *command, int *toprog, int *fromprog)
{
    int inpipes[2], outpipes[2], forkresult, pgrp;

    if (pipe(inpipes) >= 0 && pipe(outpipes) >= 0 &&
        (forkresult = fork()) != -1)
    {
      /*        if (forkresult != 0) {*/
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
            dup(inpipes[0]);
            close(inpipes[0]);
            close(inpipes[1]);
            close(1);
            dup(outpipes[1]);
	    close(2);
	    dup(outpipes[1]);
            close(outpipes[0]);
            close(outpipes[1]);
          
            execvp(args[0], args);
            /* If we get here, execvp failed, so shut down as 
             * gracefully as we can 
             */
            exit(1);
        }

/*	printf("controlling process = %d\n", tcgetpgrp(0));
	printf("new process group = %d\n", setsid());*/
	fflush(stdout);

	signal(SIGTSTP, SIG_IGN);
        close(inpipes[0]);
        close(outpipes[1]);
        
        *toprog = inpipes[1];
        *fromprog = outpipes[0];
    } else {
        *toprog = -1;
        *fromprog = -1;
    }
}
