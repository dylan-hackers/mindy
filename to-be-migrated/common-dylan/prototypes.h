/* External prototypes needed by common-extensions.

   For d2c, these are implemented in gd/src/d2c/runtime/c-code/main.c.
   XXX - Implement these in Mindy.

   This file exists solely to be processed by Melange.
*/

/* Copy of argc */
extern int application_argc;

/* Copy of argv pointer */
extern char **application_argv;

/* Return CPU time used */
extern long *cpu_time(void);
