#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#if !defined(__MACH__) || !defined(__APPLE__)
#	include <sys/poll.h>
#endif
