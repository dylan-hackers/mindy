/*
	These do nothing but keep the compiler and the code happy.
	Check that where they are called the dummy values returned have no side effects!
*/

#include "std-c.h"
#include "std-os.h"

extern int	sigaction(int a, struct sigaction * b, struct sigaction * c)
{
	return a;
}

extern int	sigsuspend(sigset_t * a)
{
	return 1;
}

extern int	sigprocmask(int op, sigset_t *set, sigset_t *oset)
{
	return 1;
}

extern int	sigaddset(sigset_t *set, int sig)
{
	return 1;
}

extern int	sigdelset(sigset_t *set, int sig)
{
	return 1;
}

extern int	sigemptyset(sigset_t *set)
{
	return 1;
}