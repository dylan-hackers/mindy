/*
 * This file implements an emulation of POSIX sigaction() and sigsuspend()
 * in terms of BSD sigvec() and sigpause() or SYSV.3 sigset() and sigpause().
 *
 * This remains untested until some poor soul with no POSIX libraries tries
 * to use it.
 */

/*
 * configure may not have figured out which to use.
 */
#if !defined(BSD_SIGNALS) && !defined(USG_SIGNALS) && !defined(WIN32)
#   define BSD_SIGNALS	1
#endif

#if BSD_SIGNALS
#   define _BSD_SIGNALS	1		/* this is how Irix turns bsd signals on */
#   define _BSD_COMPAT	1
#endif

#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include "std-signal.h"

/*
 * these are common to the emulations.
 * Taken from p. 292, Advanced Programming in the Unix Environment 
 * (W. Richard Stevens)
 *
 * On Linux, sigemptyset and sigfillset are macros, so we'd best not
 * try to redeclare them.
 */
#ifndef sigemptyset
    int sigemptyset(sigset_t *set) { return *set = 0; }
    int sigfillset(sigset_t *set) { *set = ~(sigset_t)0; return 0; }
#endif

/* We define a dummy definition of SIGBAD because we can get away
 * with it in Mindy.
 */
#ifndef SIGBAD
#   define SIGBAD(sig) 0
#   define BROKEN_SIGBAD 1
#endif

int sigaddset(sigset_t *set, int sig) 
{
    if (SIGBAD(sig)) { errno = EINVAL; return -1; }
    *set |= (1<<(sig-1)); 
    return 0;
}

int sigdelset(sigset_t *set, int sig) 
{
    if (SIGBAD(sig)) { errno = EINVAL; return -1; }
    *set &= ~(1<<(sig-1)); 
    return 0;
}

int sigismember(sigset_t *set, int sig)
{
    if (SIGBAD(sig)) { errno = EINVAL; return -1; }
    return (*set & (1<<(sig-1))) != 0; 
}

#if BROKEN_SIGBAD
#   undef SIGBAD
#endif

#if BSD_SIGNALS

#   ifdef hpux
#       define sigvec	sigvector
#   endif

int sigaction(sig, sa, osa) int sig; struct sigaction *sa, *osa;
{
  struct sigvec sv = { 0 }, osv;
  sv.sv_mask = sa->sa_mask;
  sv.sv_handler = (int (*)())sa->sa_handler;
  if (osa) {
    sigvec(sig, &sv, &osv);
    osa->sa_handler = (void (*)())osv.sv_handler;
    osa->sa_mask = osv.sv_mask;
  } else {
    sigvec(sig, &sv, NULL);
  }
}

int sigsuspend(set) sigset_t *set;
{
  if (set)
    sigpause(*set);
  else
    sigpause(0);
}

int sigprocmask(int operation, sigset_t *set, sigset_t *oset)
{
  int new, old, sig;
  switch (operation) {
  case SIG_NOP:
    old = sigsetmask(0xFFFFFFFF);
    sigsetmask(old);
    break;
  case SIG_BLOCK:
    if ( ! set) { errno = EINVAL; return -1; }
    new = old = sigsetmask(0xFFFFFFFF);
    for (sig = 1; sig < NSIG; sig += 1)
      if (sigismember(set, sig))
	new |= sigmask(sig);
    sigsetmask(new);
    break;
  case SIG_UNBLOCK:
    if ( ! set) { errno = EINVAL; return -1; }
    new = old = sigsetmask(0xFFFFFFFF);
    for (sig = 1; sig < NSIG; sig += 1)
      if (sigismember(set, sig))
	new &= ~sigmask(sig);
    sigsetmask(new);
    break;
  case SIG_SETMASK:
    if ( ! set) { errno = EINVAL; return -1; }
    new = 0;
    for (sig = 1; sig < NSIG; sig += 1)
      if (sigismember(set, sig))
	new |= sigmask(sig);
    old = sigsetmask(new);
    break;
  }
  if (oset) *oset = old;
  return 0;
}

#elif USG_SIGNALS

int sigaction(sig, sa, osa) int sig; struct sigaction *sa, *osa;
{
  if (osa)
    osa->sa_handler = sigset(sig, sa->sa_handler);
  else
    sigset(sig, sa->sa_handler);
}

int sigsuspend(set) sigset_t *set;
{
  sigpause(0);
}

int sigprocmask(int operation, sigset_t *set, sigset_t *oset)
{
  int new, old, sig;
  switch (operation) {
  case SIG_NOP:
    break;
  case SIG_BLOCK:
    if ( ! set) { errno = EINVAL; return -1; }
    for (sig = 1; sig < NSIG; sig += 1)
      if (sigismember(set, sig))
	sighold(sig);
    break;
  case SIG_UNBLOCK:
    if ( ! set) { errno = EINVAL; return -1; }
    for (sig = 1; sig < NSIG; sig += 1)
      if (sigismember(set, sig))
	sigrelse(sig);
    break;
  case SIG_SETMASK:
    if ( ! set) { errno = EINVAL; return -1; }
    for (sig = 1; sig < NSIG; sig += 1)
      if (sigismember(set, sig))
	sighold(sig);
      else
	sigrelse(sig);
    break;
  }
  if (oset) *oset = 0;
  return 0;
}

#elif WIN32

int sigaction(sig, sa, osa) int sig; struct sigaction *sa, *osa;
{
  if (osa)
    osa->sa_handler = signal(sig, (void(__cdecl*)(int))sa->sa_handler);
  else
    signal(sig, (void(__cdecl*)(int))sa->sa_handler);
  return 0;
}

int sigsuspend(set) sigset_t *set;
{
  return 0;
}

int sigprocmask(int operation, sigset_t *set, sigset_t *oset)
{
  if (oset) *oset = 0;
  return 0;
}
#endif
