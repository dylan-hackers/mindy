/*
 * This file implements an emulation of POSIX sigaction() and sigsuspend()
 * in terms of BSD sigvec() and sigpause().
 */

typedef unsigned long sigset_t;

typedef struct sigaction {
  void (*sa_handler)();
  sigset_t sa_mask;
  int sa_flags;
} sigaction_t;

extern int	sigaction(int, struct sigaction *, struct sigaction *);
extern int	sigsuspend(sigset_t *);
extern int	sigprocmask(int op, sigset_t *set, sigset_t *oset);

extern int	sigaddset(sigset_t *set, int sig);
extern int	sigdelset(sigset_t *set, int sig);
extern int	sigemptyset(sigset_t *set);

/*
 * On Linux, sigemptyset and sigfillset are macros, so we'd best not
 * try to redeclare them.
 */
#ifndef sigemptyset
    extern int	sigfillset(sigset_t *set);
    extern int	sigismember(sigset_t *set, int sig);
#endif

#define SIG_NOP		0
#define SIG_BLOCK	1
#define SIG_UNBLOCK	2
#define SIG_SETMASK	3


