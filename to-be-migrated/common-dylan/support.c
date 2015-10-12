#ifndef __MWERKS__
#include "config.h"
#endif
#include "support.h"

#include <time.h>
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

/* This comes from d2c/runtime/c-code/main.c */
extern char** application_argv;

char *dylan_common_application_arg(int index)
{
  if ((index >= 0) && (index < application_argc)) {
    return application_argv[index];
  }
  return NULL;
}

struct dylan_common_timeval *dylan_common_profiling_cpu_time(void)
{
  struct dylan_common_timeval *retval =
    (struct dylan_common_timeval *)allocate(sizeof(struct dylan_common_timeval));
#ifdef HAVE_GETRUSAGE
  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru) == 0) {
    retval->tv_sec
      = ru.ru_utime.tv_sec + ru.ru_stime.tv_sec
      + (ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) / 1000000L;
    retval->tv_usec = (ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) % 1000000L;
  } else {
    retval->tv_sec = retval->tv_usec = 0;
  }
#else
  clock_t runtime = clock();
  if (runtime >= 0) {
    retval->tv_sec = (runtime / CLOCKS_PER_SEC);
#ifdef __MWERKS__
    retval->tv_usec = (runtime - retval->tv_sec * CLOCKS_PER_SEC) * 1000000L / CLOCKS_PER_SEC;
#else
    retval->tv_usec = (runtime % CLOCKS_PER_SEC) * 1000000L / CLOCKS_PER_SEC;
#endif
  } else {
    retval->tv_sec = retval->tv_usec = 0;
  }
#endif
  return retval;
}
