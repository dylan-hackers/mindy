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
* $Header: /scm/cvs/src/mindy/interp/print.h,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
\**********************************************************************/

extern void prin1(obj_t object);
extern void print(obj_t object);
extern void print_nonzero_in_binary(int number);
extern void print_number_in_binary(int number);
extern void format _ANSI_ARGS_((char *fmt, ...));
extern int count_format_args(char *fmt);
extern void vformat(char *fmt, obj_t *args, int nargs);

extern void def_printer(obj_t class, void (*print_fn)(obj_t object));
