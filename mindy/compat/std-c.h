/*
 * std-c.h --
 *
 *	Provide emulations of standard c where parts might be lacking.
 *
 *	This file is adapted from tcl-7.3 tcl.h and tclInt.h and should
 *	be considered a highly derivative work covered by the following
 *	copyright.
 *
 *	Parts of this are irrelevant to mindy as it stands, but they're
 *	easier to keep than to go back and dig them up later.
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
 * $Header: /scm/cvs/src/mindy/compat/std-c.h,v 1.1 1998/05/03 19:55:18 andreas Exp $ SPRITE (Berkeley)
 * $Header: /scm/cvs/src/mindy/compat/std-c.h,v 1.1 1998/05/03 19:55:18 andreas Exp $ SPRITE (Berkeley)
 */

#ifndef _STD_C_H_
#define _STD_C_H_

/*
 * Definitions that allow this header file to be used either with or
 * without ANSI C features like function prototypes.
 */

#undef _ANSI_ARGS_
#undef CONST
#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#   define CONST const
#   ifdef __cplusplus
#       define VARARGS (...)
#   else
#       define VARARGS ()
#   endif
#else
#   define _ANSI_ARGS_(x)	()
#   define CONST
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

/*
 * Macro to use instead of "void" for arguments that must have
 * type "void *" in ANSI C;  maps them to type "char *" in
 * non-ANSI systems.
 */

#ifndef VOID
#   ifdef __STDC__
#       define VOID void
#   else
#       define VOID char
#   endif
#endif

/*
 * Miscellaneous declarations.
 */

#ifndef NULL
#   define NULL 0
#endif

/*
 * Common C language include files are included here, so that
 * system-dependent personalizations for the include files only
 * have to be made in once place.  This results in a few extra
 * includes, but greater modularity.
 */

#include <stdio.h>
#include <ctype.h>
#ifdef NO_LIMITS_H
#   include "std-limits.h"
#else
#   include <limits.h>
#endif
#ifdef NO_STDLIB_H
#   include "std-stdlib.h"
#else
#   include <stdlib.h>
#endif
#ifdef NO_STRING_H
#   include "std-string.h"
#else
#	include <string.h>
#	ifdef WIN32
#	    define strcasecmp	_stricmp
#	    define strncasecmp	_memicmp
#	endif
#endif
#if _USING_PROTOTYPES_
#   include <stdarg.h>
#else
#   include <varargs.h>
#endif

/*
 * At present (12/91) not all stdlib.h implementations declare strtod.
 * The declaration below is here to ensure that it's declared, so that
 * the compiler won't take the default approach of assuming it returns
 * an int.  There's no ANSI prototype for it because there would end
 * up being too many conflicts with slightly-different prototypes.
 */

#ifndef WIN32
    extern double strtod();
#endif

/*
 * hpux is claimed not to implement rint(), here's a declaration.
 */
extern double rint();

/*
 * Provide a way to determine if there are any characters in a
 * stdio buffer.  This is sure to break many more times.
 */
#ifdef USE_LINUX_FBUFEMPTYP
#   define FBUFEMPTYP(fp)        (fp->_IO_read_ptr >= fp->_IO_read_end)
#else
#   define FBUFEMPTYP(fp)        (fp->_cnt == 0)
#endif

/*
 * Disappear the inline keyword when it is inappropriate.
 */

#if ! __GNUC__
#   define inline
#   define __inline__
#endif

/*
 * Irix defines FD_ZERO() in sys/types.h using bzero(), but bzero() is
 * not declared.
 */
#if NO_BSTRING_H
#   include "std-bstring.h"
#else
#   include <bstring.h>
#endif

/*
 * Visual C++ puts alloca in malloc.h.
 */
#ifdef WIN32
#   include <malloc.h>
#endif

#endif	/* _STD_C_H_ */

