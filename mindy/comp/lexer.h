/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
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
*     University.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports, questions, comments, and suggestions should be sent by
*  E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/comp/lexer.h,v 1.1 1998/05/03 19:55:07 andreas Exp $
*
\**********************************************************************/


extern int internal_yylex(void);
extern FILE *yyin;

struct token {
    int length;
    int line;
    unsigned char chars[4];
};

extern struct token *make_token();

extern int line_count;

/*
 * This file is somewhat fragile.  The flex generated lexer introduces
 * all sorts of gotchas into the compatability code, because it includes
 * unsanitized versions of headers before and after it gives control to
 * user code.  This definition supplies boolean if mindycomp.h has not
 * been previously included.
 */

#ifdef WIN32
#   define boolean unsigned char
#else
#   define boolean int
#endif
#define TRUE 1
#define FALSE 0
