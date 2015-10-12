
extern int application_argc;
extern char *dylan_common_application_arg(int);

struct dylan_common_timeval {
    long tv_sec;
    long tv_usec;
};

extern struct dylan_common_timeval *dylan_common_profiling_cpu_time(void);


