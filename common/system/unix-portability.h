#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <pwd.h>
#include <grp.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

int system_errno(void);
void system_errno_setter(int);

int system_localtime(time_t clock, struct tm *result, long *gmtoff,
		     const char **zone);

int system_open(const char *path, int oflag, mode_t mode);
