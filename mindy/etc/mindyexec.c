/*
** mindyexec - execute dylan sources as scripts
**
** execute dylan sources as scripts by starting a pipeline
** in which mindycomp compiles to standard output and mindy
** loads from standard input.
**
** mindyexec is intended to be run as
**
**	mindyexec source-file [args-for-mindy-main ...]
**
** which is the form in which the #! protocol will start.
**
** mindyexec should be a mindy program when mindy knows how to do all
** this grotty system stuff.
*/

#include "../compat/std-c.h"
#include "../compat/std-os.h"

#define MINDYCOMP "mindycomp"

int main(argc, argv) int argc; char *argv[];
{
  int fd[2], pid;
  char *source;
  
  pipe(fd);

  if (pipe(fd) != 0) {
    fprintf(stderr, "mindyexec: pipe failed: %s\n", strerror(errno));
    exit(1);
  }
  
  pid = fork();

  if (pid < 0) {
    fprintf(stderr, "mindyexec: fork failed: %s\n", strerror(errno));
    exit(1);
  }

  if (argc < 2)
    source = "-";			/* compile from standard input */
  else
    source = argv[1];			/* compile from script */

  if (pid == 0) {
    static char *nargv[] = {
      MINDYCOMP,			/* compile */
      "source-script",			/* <source name here> */
      "-q",				/* no warnings */
      "-o",				/* output to */
      "-",				/* stdout */
      NULL
    };

    nargv[1] = source;

    close(1);
    dup(fd[1]);
    close(fd[0]);
    close(fd[1]);

    execvp(MINDYCOMP, nargv);

    fprintf(stderr, "mindyexec: execvp(\"%s\", ...) failed: %s\n", MINDYCOMP,
	    strerror(errno));
    exit(1);
  }

  if (pid > 0) {
    char **nargv = malloc((argc+4+1) * sizeof(char *));
    int nargc, oargc;

    nargc = 0;
    nargv[nargc++] = "mindy";		/* interpret */
#if ! NO_ARGV_0
    nargv[nargc++] = "-0";		/* command name is */
    nargv[nargc++] = source;		/* source file */
#endif
    nargv[nargc++] = "-x";		/* last file to load is */
    nargv[nargc++] = "-";		/* stdin */
    for (oargc = 2; oargc < argc; )
      nargv[nargc++] = argv[oargc++];	/* rest of args for main() */
    nargv[nargc] = NULL;

    close(0);
    dup(fd[0]);
    close(fd[0]);
    close(fd[1]);

    execvp("mindy", nargv);

    fprintf(stderr, "mindyexec: execvp(\"mindy\", ...) failed:%s\n", strerror(errno));
    exit(1);
  }
}

