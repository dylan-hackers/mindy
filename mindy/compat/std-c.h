/*
 * std-c.h --
 *
 *        Provide emulations of standard c where parts might be lacking.
 *
 *        This file is adapted from tcl-7.3 tcl.h and tclInt.h and should
 *        be considered a highly derivative work covered by the following
 *        copyright.
 *
 *        Parts of this are irrelevant to mindy as it stands, but they're
 *        easier to keep than to go back and dig them up later.
 *
 * tcl.h --
 * tclInt.h --
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 */

#ifndef _STD_C_H_
#define _STD_C_H_

#include <config.h>

/*
 * Miscellaneous declarations.
 */

/*
 * Common C language include files are included here, so that
 * system-dependent personalizations for the include files only
 * have to be made in once place.  This results in a few extra
 * includes, but greater modularity.
 */

#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#ifdef WIN32
#   define strcasecmp        _stricmp
#   define strncasecmp        _memicmp
#endif
#include <stdarg.h>

/*
 * Provide a way to determine if there are any characters in a
 * stdio buffer.  This is sure to break many more times.
 */
#ifdef USE_LINUX_FBUFEMPTYP
#   define FBUFEMPTYP(fp)        (fp->_IO_read_ptr >= fp->_IO_read_end)
#else
#   ifdef USE_BSD_FBUFEMPTYP
#       define FBUFEMPTYP(fp)    (fp->_r <= 0)
#   else
#              define FBUFEMPTYP(fp)    (fp->_cnt == 0)
#   endif
#endif

/*
 * Disappear the inline keyword when it is inappropriate.
 */

#if ! __GNUC__
#   define inline
#   define __inline__
#endif

/*
 * Visual C++ puts alloca in malloc.h.
 */
#ifdef WIN32
#   include <malloc.h>
#endif

#endif        /* _STD_C_H_ */

