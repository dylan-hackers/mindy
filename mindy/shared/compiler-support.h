/**********************************************************************\
*
*  Copyright (c) 2015 Mindy contributors.
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
\**********************************************************************/

#ifndef MINDY_SHARED_COMPILER_SUPPORT_H
#define MINDY_SHARED_COMPILER_SUPPORT_H

#ifdef __GNUC__
#define MINDY_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#define MINDY_NORETURN __declspec(noreturn)
#else
#define MINDY_NORETURN
#endif

#endif
