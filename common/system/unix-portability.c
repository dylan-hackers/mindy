#include "config.h"
#include "unix-portability.h"

int system_errno(void)
{
  return errno;
}

void system_errno_setter(int new_errno)
{
  errno = new_errno;
}

int
system_localtime(time_t clock, struct tm *result, long *gmtoff,
	     const char **zone) {
  struct tm *p_tm;
#ifdef HAVE_LOCALTIME_R
  p_tm = localtime_r(&clock, result);
#else
  p_tm = localtime(&clock);
  *result = *p_tm;
#endif

#if defined(HAVE_TM_GMTOFF)
  *gmtoff = result->tm_gmtoff;
#elif defined(HAVE_DAYLIGHT)
  *gmtoff = -timezone;
#elif defined(HAVE_CYGNUS_DAYLIGHT)
  *gmtoff = -_timezone;
#else
#error "No implementation provided for obtaining timezone offset"
#endif

#if defined(HAVE_STRUCT_TM_TM_ZONE)
  *zone = result->tm_zone;
#elif defined(HAVE_DAYLIGHT)
  *zone = tzname[daylight];
#elif defined(HAVE_CYGNUS_DAYLIGHT)
  *zone = tzname[_daylight];
#else
#error "No implementation provided for obtaining timzeone name"
#endif

  return 0;
}

int system_open(const char *path, int oflag, mode_t mode)
{
  return (oflag & O_CREAT) ? open(path, oflag, mode) : open(path, oflag);
}
