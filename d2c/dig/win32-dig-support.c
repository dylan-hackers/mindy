#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <win32-dig-support.h>

void ignore_interrupts (void) 
{
  signal(SIGINT, SIG_IGN);
}
