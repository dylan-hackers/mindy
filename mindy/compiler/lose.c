/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
*  All rights reserved.
*  
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*  
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/comp/lose.c,v 1.2 2000/01/24 04:58:04 andreas Exp $
*
* This file contains lose, the interal flame-out routine.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "lose.h"

static void vlose(char *fmt, va_list ap)
{
    if (fmt != NULL) {
        vfprintf(stderr, fmt, ap);
	fflush(stderr);
	if (fmt[strlen(fmt)-1] != '\n')
	    putc('\n', stderr);
    }
    abort();
}

#if _USING_PROTOTYPES_
void lose(char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vlose(fmt, ap);
    va_end(ap);
}
#else
void lose(va_alist) va_dcl
{
    va_list ap;
    char *fmt;
    va_start(ap);
    fmt = va_arg(ap, char *);
    vlose(fmt, ap);
    va_end(ap);
}
#endif

