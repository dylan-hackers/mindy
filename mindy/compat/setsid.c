/*
 * This file implements an emulation of POSIX setsid(), at least as 
 * much as is necessary for Mindy.
 */

#include "std-c.h"
#include "std-os.h"

int setsid(void)
{
    /* int fd = open("/dev/tty", O_RDWR);
    ioctl(fd, TIOCNOTTY, 0);
    close(fd);
    */
    return setpgrp(0, getpid());
}
