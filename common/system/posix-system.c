/* Unix support routines for the Dylan operating-system library
   Written by Tom Emerson, tree@tiac.net
   
   Copyright (C) 1999 Thomas R. Emerson

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA  02111-1307, USA.

   Bug reports, questions, comments, and suggestions should be sent by
   E-mail to the Internet address "gd-bugs@gwydiondylan.org".
*/

/* $Header: /scm/cvs/src/common/system/Attic/posix-system.c,v 1.7 2003/10/23 07:38:23 housel Exp $ */

#ifdef WIN32
#else
# include <unistd.h>
# include <grp.h>
#endif

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include "posix-system.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

extern char **environ;

/*
** Get the name of the current user's primary group, copying it into the
** the buffer 'outBuf'. The length of 'outBuf' is passed in 'bufLen'. If
** the group name (including the null terminator) is larger than 'bufLen'
** then it is truncated.
**
** primary_group_name returns 0 if the group name was successfully retrieved,
** otherwise it returns 1.
**
** CAVEAT HACKER: this function is *not* re-entrant
**
** [I'm sure this could be written in Dylan using the internal C-FFI routines,
**  but I don't know enough about that yet to dive in: so this is the path
**  of least resistence. Since we'll be switching to the HQN C-FFI in the
**  near future, this will exist until then.]
*/
int primary_group_name(unsigned char *outBuf, long bufLen)
{
#ifdef WIN32
	typedef struct group
	{
		char* gr_name;
	};
#endif
	struct group *gpptr;

    if ((outBuf == NULL) || (bufLen <= 0))
        return 1;

#ifdef WIN32
	if (1) {
		struct group fake;
		gpptr = &fake;
		fake.gr_name = "ROOT";
#else
    if ((gpptr = getgrgid(getgid())) != NULL) {
#endif
        strncpy((char *) outBuf, gpptr->gr_name, bufLen - 1);
        outBuf[bufLen - 1] = '\0';
        return 0;
    }
    return 1;
}

/*
** return the total size (in bytes) of the current 'environ' table, including the
** trailing NULL.
*/
static int environ_size(void)
{
    char **p = environ;
    int size = sizeof(char **);              /* trailing NULL */

    while (*p++ != NULL)
        size += sizeof(char **);
    return size;
}

/*
** The Solaris (and presumably other Unixen) implementation of putenv()
** does not copy the new environment value: rather it puts the name-value
** pair (nvp) directly into the environment table, which is unfortunate.
**
** The following implementation is based the discussion in Stevens'
** "Advanced Programming in the Unix Environment", p. 174.
** 
** FIXME: this function will leak memory when replacing an table entry
**        which was allocated from our heap with malloc; i can't
**        portably determine whether or not a specific pointer is in
**        the heap or not. note too that if i create my own environ
**        table then this too will "leak", though that isn't a huge
**        problem. investigate whether the GC will solve this problem
**        or not.
*/
int safe_putenv(const char *nvp)
{
    static char environ_moved_p = 0;
    int result = 1;
    char *name;
    char **p;
    
    if ((name = strchr(nvp, '=')) != NULL) {
        /* see if this name already exists */
        p = environ;
        while (*p != NULL) {
            if (strncmp(nvp, *p, name - nvp) == 0)
                break;
            p++;
        }

        if (*p != NULL) {
            if (strlen(nvp) <= strlen(*p)) {
                strcpy(*p, nvp);
                result = 0;
            } else {
                char *_nvp;
                if ((_nvp = strdup(nvp)) != NULL) {
                    *p = _nvp;
                    result = 0;
                }
            }
        } else { /* the name doesn't exist */
            int environSize = environ_size();

            if (environ_moved_p) {
                p = (char **) realloc(environ, environSize + sizeof(char **));
            } else {
                if ((p = (char **) malloc(environSize + sizeof(char **))) != NULL) {
                    memcpy(p, environ, environSize);
                    environ_moved_p = 1;
                }
            }

            if (p != NULL) {
                if ((p[environSize / 4 - 1] = strdup(nvp)) != NULL) {
                    p[environSize / 4] = NULL;
                    environ = p;
                    result = 0;
                } else {
                    free(p);
                }
            }
        }
    }
    return result;
}

/*
** This is a bit misnamed: Solaris doesn't actually have an 'unsetenv'
** equivalent in its libraries. This implementation is pretty simplistic,
** but it works and it is not expected that user programs will be
** setting and unsetting environment variables often.
**
** As noted for safe_putenv, this function can leak if an nvp is unset
** which was added by the user.
*/
int safe_unsetenv(const char *name)
{
    int    nameLen = strlen(name);
    int    offset  = 0;
    char **p       = environ;

    while (*p != NULL) {
        int l = strlen(*p);
        offset += sizeof(char **);
        if (strncmp(name, *p, MIN(nameLen, l)) == 0) {
            /* copy all entries below this one up */
            memmove(p, p + 1, environ_size() - offset);
            break;
        }
        p++;
    }
    return *p == NULL;
}

#ifdef TESTING
#include <stdio.h>
int main()
{
    int i;
    char *foo;

    i = safe_putenv("FOO=BAR");
    i = safe_putenv("FOO=QUX");
    i = safe_putenv("FOO=foobar");
    i = safe_putenv("GREAT_QUX=foobarbaz");
    i = safe_putenv("QUX=bal");

    foo = getenv("FOO");
	printf("Foo = %s\n", foo);
    foo = getenv("GREAT_QUX");
	printf("Foo = %s\n", foo);

    i = safe_unsetenv("QUX");
    i = safe_unsetenv("PWD");
    i = safe_unsetenv("MAIRIN");

	printf("returning %d\n", i);

    return i;
}
#endif /* TESTING */
