/* This file implements delegate_gdb_signals, which is used to pass certain
   signals through to GDB. Currently, we delegate SIGINT and handle other
   signals ourselves.

   Note that we use the SA_RESTART flag when calling sigaction. This is
   present is the Single UNIX Standard Version 2 (and also SVR4 and BSD 4.3+,
   according to Colin Simmonds). It is not, however, present in older versions
   of POSIX.
   */

#include <stddef.h>
#include <sys/types.h>
#include <signal.h>
#include "posix-dig-support.h"

int process_id = -1;

void delegate_signal(int signum)
{
    kill(process_id, signum);
}

int delegate_gdb_signals(void)
{
#ifndef SA_RESTART
    int SA_RESTART = 0;
#endif

    struct sigaction act;

    act.sa_handler  = &delegate_signal;
#if !defined(GD_PLATFORM_BEOS)
    act.sa_flags    = SA_RESTART;
#else
    /* Can't find an SA_RESTART flag in the BeOS posix headers so
       I guess dig support will be broken for now... */
    act.sa_flags    = 0;
#endif
    sigemptyset(&act.sa_mask);

    /* return sigaction(SIGINT, &act, NULL); */
    return 0;
}
