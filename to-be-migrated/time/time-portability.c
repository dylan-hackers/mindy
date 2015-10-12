#include "time-portability.h"
#include "config.h"

#if defined(HAVE_DAYLIGHT)
int
my_timezone(void)
{
  return timezone;
}

int
my_daylight(void)
{
  return daylight;
}
#elif defined(HAVE_TM_GMTOFF)
int
my_timezone(void)
{
  time_t now;
  time(&now);
  return localtime(&now)->tm_isdst;
}

int
my_daylight(void)
{
  time_t now;
  time(&now);
  return -(localtime(&now)->tm_gmtoff);
}
#elif defined(HAVE_CYGNUS_DAYLIGHT)
int
my_timezone(void)
{
  return _timezone;
}

int
my_daylight(void)
{
  return _daylight;
}
#else
#error "No implementation provided for my_timezone()/my_daylight()"
#endif


