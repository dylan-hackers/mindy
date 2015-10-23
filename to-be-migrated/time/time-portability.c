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
#elif defined(_WIN32)
#include <Windows.h>

int
my_timezone(void)
{
  DYNAMIC_TIME_ZONE_INFORMATION tzinfo;
  DWORD rv = GetDynamicTimeZoneInformation(&tzinfo);
  if (rv == TIME_ZONE_ID_UNKNOWN) // No zone info, just return 0
    return 0;
  
  // Bias is in minutes, POSIX timezone is in seconds west of GMT
  return tzinfo.Bias * 60;
}

int
my_daylight(void)
{
  DYNAMIC_TIME_ZONE_INFORMATION tzinfo;
  DWORD rv = GetDynamicTimeZoneInformation(&tzinfo);
  if (rv == TIME_ZONE_ID_DAYLIGHT) // Alternatively, check tzinfo.DynamicDaylightTimeDisabled
    return 1;
  else
    return 0;
}
#else
#error "No implementation provided for my_timezone()/my_daylight()"
#endif


